#### VX ####

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Animal Study Summary"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV file",
                accept = c(".csv")),
      actionButton("update", "Update")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pie Chart", plotOutput("pie")),
        tabPanel("Bar Plot", plotOutput("plot")),
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Age Table", DT::dataTableOutput("age_table")),
        tabPanel("Age Scatterplot", plotOutput("scatterplot", height = "900px")),
        tabPanel("Cage Table", DT::dataTableOutput("cage_table"), DT::dataTableOutput("cage_summary_table")),
        tabPanel("Cage Bar Plot", plotOutput("barplot_cage")),
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
    } else if(ext == "xlsx") {
      df <- readxl::read_xlsx(input$file$datapath, sheet = 1)
      df <- as.data.frame(df)
    }
    colnames(df) <- c('IsDead', 'IsReservedByOrder', 'ParticipatesInActiveMating', 'IsReservedByReservation', 'IsLitter', 'IsWeaned', 'IsSick', 'HasTask', 'NotesExist', 'AttachmentExists', 'LockedForExperiment', 'IsPreviouslyUsed', 'CageID', 'AnimalID', 'No. of animals', 'S', 'Species', 'DoB', 'Exit date', 'Age', 'Strain', 'Genotype', 'Room', 'Team', 'PPL', 'Responsible User', 'Project code', 'Status', 'protocol', 'Sire', 'Dam', 'Date of delivery', 'Tags', 'Last Study plan')
    return(df)
  })
  
  ## Generate all tables
  
  # Extract relevant columns for age table
  age_data <- reactive({
    data() %>%
      select(Strain, S, AnimalID, Sire, CageID, DoB, ParticipatesInActiveMating) %>%
      mutate(Age = as.numeric(difftime(Sys.Date(), dmy(DoB), units = "days")))
    # Age to be moved to third place in col order
  })
  
  # Generate summary table
  summary_table <- reactive({
    data() %>%
      group_by(Strain, S) %>%
      summarize(count = n())
  })
  
  # Generate summary table for cages
  summary_table <- reactive({
    data() %>%
      group_by(Strain, S) %>%
      summarize(count = n())
  })
  
  # Generate table of prevalent Strain for each unique CageID value
  cage_table <- reactive({
    cage <- age_data() %>%
      group_by(CageID) %>%
      summarize(n = n_distinct(AnimalID),
                prevalent_strain = ifelse(sum(Strain == "C57BL/6J") == n(), "C57BL/6J",
                                          unique(na.omit(Strain[Strain != "C57BL/6J"]))),
                participates_in_mating = ifelse(all(ParticipatesInActiveMating), "Yes", "No"))
    
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
    ggplot(summary_table(), aes(x = Strain, y = count, fill = S)) +
      ggtitle(label = "Animal per strain", subtitle = "grouped by Strain and Sex")+
      geom_bar(stat = "identity", position = "dodge") +
      theme_bw()
  })
  
  # Render scatterplot
  output$scatterplot <- renderPlot({
    ggplot(age_data(), aes(x = Strain, y = Age, color = S)) +
      geom_point(position = position_jitter(width = 0.33, height = 2.5), size = 3) +
      geom_hline(yintercept = 365, color = "red", size = 1.5) +
      theme(aspect.ratio = 3)+
      theme_bw()+
      scale_y_continuous(breaks = seq(0, max(age_data()$Age), by = 30))
  })
  
  # Render cage_plot
  
  output$barplot_cage <- renderPlot({
    ggplot(cage_summary_table(), aes(x = prevalent_strain, y = n_cages, fill = participates_in_mating)) +
      geom_bar(stat = "identity") +
      xlab("Strain") +
      ylab("Number of Cages") +
      ggtitle("Number of Cages per Strain") +
      scale_y_continuous(breaks = seq(0, max(age_data()$Age), by = 2))+
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
    age_data()
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


#### ####

