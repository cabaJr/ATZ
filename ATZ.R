#### VX ####

# features To add
# 1. plotly to be used in age scatterplot, displaying AnimalID, CageID, 
#    possibly highlighting the points of other animals caged together
# 2. possibility to highlight cells in tables and copy selection (to be pasted in Atune or else)
# 2.5 when highlighting rows in cage table, highlight the animals in the age scatterplot.
# 3. additional barplot that differentiates between animals in breeding and not, + Sex and Strain

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(plotly)
library(RColorBrewer)

# version valid for data extracted after 27/06/2023

# Define UI
ui <- fluidPage(
  titlePanel("Animal Study Summary"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV or XLSX file",
                accept = c(".csv", ".xlsx")),
      actionButton("update", "Update")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pie Chart", plotOutput("pie")),
        tabPanel("Bar Plot", plotOutput("plot"), plotOutput("plot2")),
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Age Table", DT::dataTableOutput("age_table")),
        tabPanel("Age Scatterplot", plotlyOutput("scatterplot", height = "750px")),
        tabPanel("Cage Table", DT::dataTableOutput("cage_table"), DT::dataTableOutput("cage_summary_table")),
        tabPanel("Cage Bar Plot", plotOutput("barplot_cage"))
      )
    )
  )
)

# Define server function
server <- function(input, output) {
  
  # Read CSV or Excel file
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if(ext == "csv") {
      df <- read.csv(input$file$datapath, sep = ",", header = TRUE)
      colnames(df) <- c('IsDead', 'IsReservedByOrder', 'ParticipatesInActiveMating', 'IsReservedByReservation', 'IsLitter', 'IsWeaned', 'IsSick', 'HasTask', 'NotesExist', 'AttachmentExists', 'LockedForExperiment', 'IsPreviouslyUsed', 'IsLockedForEdit', 'HasHealthConcern', 'CageID', 'AnimalID', 'No. of animals', 'S', 'Species', 'DoB', 'Exit date', 'Age', 'Strain', 'Genotype', 'Room', 'Team', 'PPL', 'Responsible User', 'Project code', 'Status', 'protocol', 'Sire', 'Dam', 'Date of delivery', 'Tags', 'Last Study plan')
      df <- df %>%
        select(Strain, S, AnimalID, Sire, CageID, DoB, ParticipatesInActiveMating) %>%
        mutate(Age = as.numeric(difftime(Sys.Date(), dmy(DoB), units = "days")))
    } else if(ext == "xlsx") {
      df <- readxl::read_xlsx(input$file$datapath, sheet = 1)
      df <- as.data.frame(df)
      colnames(df) <- c('IsDead', 'IsReservedByOrder', 'ParticipatesInActiveMating', 'IsReservedByReservation', 'IsLitter', 'IsWeaned', 'IsSick', 'HasTask', 'NotesExist', 'AttachmentExists', 'LockedForExperiment', 'IsPreviouslyUsed', 'IsLockedForEdit', 'HasHealthConcern', 'CageID', 'AnimalID', 'No. of animals', 'S', 'Species', 'DoB', 'Exit date', 'Age', 'Strain', 'Genotype', 'Room', 'Team', 'PPL', 'Responsible User', 'Project code', 'Status', 'protocol', 'Sire', 'Dam', 'Date of delivery', 'Tags', 'Last Study plan')
      df <- df %>%
        select(Strain, S, AnimalID, Sire, CageID, DoB, ParticipatesInActiveMating) %>%
        mutate(Age = as.numeric(difftime(Sys.Date(), ymd(DoB), units = "days")))
    }
    print(class(df))
    return(df)
  })

  
  # Generate summary table
  summary_table <- reactive({
    data() %>%
      group_by(Strain, S, ParticipatesInActiveMating) %>%
      summarize(count = n())
  })
  
  # Generate summary table0
  summary_table0 <- reactive({
    data() %>%
      group_by(Strain, S) %>%
      summarize(count = n())
  })
  
  # Generate table of prevalent Strain for each unique CageID value
  cage_table <- reactive({
    cage <- data() %>%
      group_by(CageID) %>%
      summarize(n = dplyr::n_distinct(AnimalID),
                prevalent_strain = ifelse(sum(Strain == "C57BL/6J") == n(), "C57BL/6J",
                                          unique(na.omit(Strain[Strain != "C57BL/6J"]))),
                participates_in_mating = ifelse(any(ParticipatesInActiveMating), "Yes", "No"))
    
  })
  
  # Generate cage summary table
  cage_summary_table <- reactive({
    cage_table() %>%
      group_by(CageID, prevalent_strain, participates_in_mating) %>%
      summarize(n = n()) %>%
      distinct(CageID, .keep_all = TRUE) %>%
      group_by(prevalent_strain, participates_in_mating) %>%
      summarize(n_cages = n())
  })
  cage_sumDTable <- reactive({
    # create DT table
    datatable(cage_summary_table(), options = list(
      pageLength = 5,
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).css({'background-color': '#f0f0f0', 'font-weight': 'bold'});",
        "}"
      )
    ), selection = "single")
  })
  
  ## Generate all plots
  
  # Render pie chart
  output$pie <- renderPlot({
    df <- data() %>%
      select(Strain) %>%
      group_by(Strain) %>%
      summarise(n = n()) %>%
      arrange(desc(n))
    ggplot(df, aes(x = "", y = n, fill = Strain)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0)+
      geom_text(aes(label = paste0(Strain, ": ", n)), position = position_stack(vjust = 0.75)) +
      theme_void() +
      labs(fill = "Strain") +
      theme(legend.position = "bottom")
  })
  
  # Render bar plot
  output$plot <- renderPlot({
    ggplot(summary_table0(), aes(x = Strain, y = count, fill = S)) +
      ggtitle(label = "Animal per strain", subtitle = "grouped by Strain and Sex")+
      geom_bar(stat = "identity", position = "dodge") +
      theme_bw()
  })
  
  # Render bar plot mating 
  output$plot2 <- renderPlot({
    ggplot(summary_table(), aes(x = S, y = count, fill = S, alpha = ParticipatesInActiveMating)) +
      ggtitle(label = "Animal per strain", subtitle = "grouped by Strain, Sex, and animals in breeding")+
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("#E21A1C", "#339F2D", "#2078B4"), #c("red", "green","blue"), 
                        name = "Sex",
                        labels = c("F", "M", "u")) +
      scale_alpha_manual(values = c(0.5, 1), 
                         name = "Participates in Active Mating",
                         labels = c("No", "Yes")) +
      facet_grid(. ~ Strain) +
      theme_bw()
  })
  
  # Render scatterplot
  output$scatterplot <- renderPlotly({
    p <- ggplot(data(), 
                aes(x = Strain, y = Age, fill = S, color = ParticipatesInActiveMating,
                    text = paste("Animal ID: ", AnimalID,
                                 "<br>Cage ID: ", CageID,
                                 "<br>Mating: ", ParticipatesInActiveMating,
                                 "<br>Sire: ", Sire))) +
      geom_point(position = position_jitter(width = 0.33, height = 2.5), 
                 size = 3, alpha = 0.75, show.legend = FALSE) +
      scale_color_manual(values = c("TRUE" = "black"))+
      geom_hline(yintercept = 365, color = "red", linewidth = 1.5) +
      geom_hline(yintercept = 15, color = "yellow", linewidth = 1, linetype = "dashed") +
      theme(aspect.ratio = 3, legend.position="none", axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)) +
      scale_y_continuous(breaks = seq(0, max(data()$Age), by = 30))
    
    ggplotly(p, tooltip = c("text")) %>% 
      layout(yaxis = list(title = "Age"))
  })
  
  # action on scatterplot
  
  # observe selected data in DT and update plotly object
  # observeEvent(input$table_rows_selected, {
  #   selected_rows <- input$table_rows_selected
  #   if (length(selected_rows) > 0) {
  #     fig_data <- list(
  #       x = df$x[selected_rows],
  #       y = df$y[selected_rows],
  #       type = "scatter",
  #       mode = "markers",
  #       marker = list(size = 10, color = "red")
  #     )
  #   } else {
  #     fig_data <- list(
  #       x = df$x,
  #       y = df$y,
  #       type = "scatter",
  #       mode = "markers"
  #     )
  #   }
  #   plotlyProxyInvoke(p, "deleteTraces", 0)
  #   plotlyProxyInvoke(p, "addTraces", fig_data)
  # })
  
  
  # Render cage_plot
  
  output$barplot_cage <- renderPlot({
    ggplot(cage_summary_table(), aes(x = prevalent_strain, y = n_cages, fill = participates_in_mating)) +
      geom_bar(stat = "identity") +
      xlab("Strain") +
      ylab("Number of Cages") +
      ggtitle("Number of Cages per Strain") +
      scale_y_continuous(breaks = seq(0, max(data()$Age), by = 2))+
      scale_fill_manual(values = c("steelblue", "red"), 
                        name = "Mating Status", 
                        labels = c("No", "Yes")) 
  })
  
  ## Render all tables
  
  # Render DataTable
  output$table <- DT::renderDT({
    summary_table()
  })
  
  # Render Age DataTable
  output$age_table <- DT::renderDT({
    data()
  })
  
  # Render Cage Table
  output$cage_table <- DT::renderDT({
    cage_table()
  })
  
  # Render Cage summary Table
  output$cage_summary_table <- DT::renderDT({
    cage_summary_table()
  })
  
}

# Run the app
shinyApp(ui, server)
