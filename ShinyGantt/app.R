library(tidyverse)
library(ggplot2)
library(DT)
library(shiny)
library(glue)
library(WriteXLS)

## source application files
#source("global.R", local = TRUE)


ui <- fluidPage(
 sidebarLayout(
  sidebarPanel(
   tags$h3("Project Gantt Chart"),
   tags$h4("Add or remove as many tasks as you like for a single project."),
   tags$hr(),
   textInput(inputId = "projectName", label = "Project Name:", placeholder = "e.g., My Project"),
   textInput(inputId = "inTaskName", label = "Task:", placeholder = "e.g., Extract and Link Data"),
   dateInput(inputId = "inStartDate", value = Sys.Date(), min = Sys.Date() - 365, label = "Start Date:"),
   dateInput(inputId = "inEndDate", value = Sys.Date() + 10, min = Sys.Date() - 364, label = "End Date:"),
   actionButton(inputId = "btn", label = "Add Task")
  ),
  mainPanel(
   tags$h3("Task Table View"),
   tags$hr(),
   DTOutput(outputId = "tableTasks"),
   downloadButton("downloadExcel", "Download Table as Excel"),
   tags$h3("Gantt Chart"),
   tags$h4("Right-click the Gantt chart to copy or save as image."),
   tags$hr(),
   plotOutput(outputId = "plotTasks")
  )
 )
)


server <- function(input, output) {
 df <- reactiveValues(
  data = data.frame(
   Task = c("Task 1", "Task 2"),
   StartDate = as.Date(c("2023-10-10", "2023-10-30")),
   EndDate = as.Date(c("2023-11-30", "2023-12-15"))
  ) %>%
   # Add an ID column - used later to remove row with certain ID
   mutate(ID = row_number(), .before = Task) %>%
   # Add a column with a custom Remove button
   mutate(Remove = glue('<button id="custom_btn_{ID}" onclick="Shiny.onInputChange(\'button_id\', \'{ID}\')">Remove</button>'))
  
 )
 
 observeEvent(input$btn, {
  task_name <- input$inTaskName
  task_start_date <- input$inStartDate
  task_end_date <- input$inEndDate
  
  if (!is.null(task_name) && !is.null(task_start_date) && !is.null(task_end_date)) {
   # We also need a new row ID
   new_id <- nrow(df$data) + 1
   new_row <- data.frame(
    # Row ID
    ID = new_id,
    Task = task_name,
    StartDate = task_start_date,
    EndDate = task_end_date,
    # Remove button with a unique timestamp ID
    Remove = glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{new_id}_', Sys.time(), '\')">Remove</button>'),
    stringsAsFactors = FALSE
   )
   df$data <- rbind(df$data, new_row)
   df$data <- df$data[order(df$data$ID), ]
  }
 })
 
 # REMOVE A TASK
 observeEvent(input$button_id, {
  # Extract the actual ID from the combined ID_timestamp
  actual_id <- unlist(strsplit(input$button_id, "_"))[1]
  
  # Remove a row from the data.frame
  df$data <- df$data[-c(as.integer(actual_id)), ]
  
  # Renumber the IDs
  df$data$ID <- seq_len(nrow(df$data))
  
  # Sort the dataframe by StartDate
  df$data <- df$data[order(df$data$StartDate), ]
  
  # Reset row names
  rownames(df$data) <- NULL
  
  # Update the Remove buttons to reflect new IDs with a unique timestamp
  df$data$Remove <- sapply(df$data$ID, function(i) {
   glue('<button id="custom_btn_{i}" onclick="Shiny.onInputChange(\'button_id\', \'{i}_', Sys.time(), '\')">Remove</button>')
  })
 })
 
 
 
 output$tableTasks <- renderDT({
  datatable(data = df$data, escape = FALSE, caption = input$projectName)
 })
 
 output$plotTasks <- renderPlot({
  ggplot(df$data, aes(x = StartDate, xend = EndDate, y = fct_rev(fct_inorder(Task)), yend = Task)) +
   geom_segment(linewidth = 10, color = "#0198f9") +
   labs(
    title = input$projectName,
    x = "Duration",
    y = "Task"
   ) +
   theme_bw() +
   theme(legend.position = "none") +
   theme(
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
   )
 })
 
 
 output$downloadExcel <- downloadHandler(
  filename = function() {
   paste("TaskData-", Sys.Date(), ".xlsx", sep="")
  },
  content = function(file) {
   write_xlsx(df$data, path = file)
  }
 )
 
}


shinyApp(ui = ui, server = server)