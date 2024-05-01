Day	Amoeba	Toxin	reading	stdev
0	Vannella	High	6579.23	463.862049


# ui.R
library(shiny)

ui <- fluidPage(
  titlePanel("Drug Concentration vs. Cell Concentration"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("drug_conc", "Drug Concentration:",
                  min = 0, max = 100, value = c(0, 50), step = 5)
    ),
    mainPanel(
      plotOutput("cell_plot")
    )
  )
)


    # server.R
# server.R
library(shiny)
library(ggplot2)
library(readxl)
library(readr)
PowersAmoeba <- read_csv("PowersAmoeba.csv")
View(PowersAmoeba)

# Define server logic
server <- function(input, output) {
  # Read data from Excel file
  data <- reactive({
    excel_file <- "PowersAmoeba"  # Update with the actual file path
    read_excel(excel_file, sheet = "PowersAmoeba")
  })
  
  # Define output object within the server function
  output$cell_plot <- renderPlot({
    # Your plot generation code here
    ggplot(data(), aes(x = column_name)) + geom_histogram()
  })
}

# If this script is part of a Shiny application, it should be included within the shinyApp() function

        # Use the reactive data frame
        filtered_data <- subset(data(), 
                                Day >= input$day_range[1] & Day <= input$day_range[2] &
                                  Amoeba %in% input$selected_amoebas &
                                  Toxin >= input$toxin_range[1] & Toxin <= input$toxin_range[2])
        
        ggplot(filtered_data, aes(x = Toxin, y = reading, color = Amoeba, group = Amoeba)) +
          geom_point() +
          geom_errorbar(aes(ymin = reading - stdev, ymax = reading + stdev), width = 0.2) +
          geom_line() +
          labs(x = "Toxin", y = "Cell Concentration") +
          theme_minimal()
      })
      
      # Define inputs for user selection
      output$day_range_input <- renderUI({
        sliderInput("day_range", "Select Day Range:",
                    min = min(data()$Day), max = max(data()$Day),
                    value = c(min(data()$Day), max(data()$Day)))
      })
      
      output$amoeba_select <- renderUI({
        selectInput("selected_amoebas", "Select Amoeba:",
                    choices = unique(data()$Amoeba),
                    selected = unique(data()$Amoeba))
      })
      
      output$toxin_range_input <- renderUI({
        sliderInput("toxin_range", "Select Toxin Range:",
                    min = min(data()$Toxin), max = max(data()$Toxin),
                    value = c(min(data()$Toxin), max(data()$Toxin)))
      })
    }
    
    
    
    
    


# Run the application
shinyApp(ui = ui, server = server)




##########################part 2
rm(list=ls())

# Load libraries
library(shiny)
library(ggplot2)

# Load data
library(readr)

data <- read_csv("PowersAmoeba.csv")


# Define UI
ui <- fluidPage(
  titlePanel("Amoeba Reading Concentrations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("amoeba", "Select Amoeba Type:", choices = unique(data$Amoeba)),
      selectInput("toxin", "Select Toxin Level:", choices = unique(data$Toxin)),
      selectInput("plot_type", "Select Plot Type:", 
                  choices = c("dot" = "dot","bar" = "bar")),
      sliderInput("day", "Select Day:", min = 0, max = 9, value = 0)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Define server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    filtered_data <- data[data$Amoeba == input$amoeba &
                            data$Toxin == input$toxin, ]
    
    if(input$plot_type == "dot") {
      ggplot(filtered_data, aes(x = Day, y = reading)) +
        geom_point() +
        facet_wrap(~Amoeba) +
        labs(title = paste("Reading Concentrations for", input$amoeba, "Amoeba"),
             x = "Day", y = "Reading Concentration")
    } else if(input$plot_type == "bar") {
      ggplot(filtered_data, aes(x = Day, y = reading, fill = Amoeba)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Reading Concentrations for", input$amoeba, "Amoeba"),
             x = "Day", y = "Reading Concentration") +
        scale_fill_brewer(palette = "Set3")
    }
  })
}


# Run the app
shinyApp(ui, server)
