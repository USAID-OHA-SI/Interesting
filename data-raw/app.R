library(shiny)
library(shinythemes)
library(tidyverse)


# Global Options ----

  app_title <- "FEPFAR/USAID - Custom Indicators Reporting"
  nav_title <- "CIRG"

# Functions ----


# User Interface ----

  cirg_ui <- fluidPage(
    # Title
    titlePanel(title = app_title),

    # Navigation Bar * Pages
    navbarPage(
      id = ",",
      fluid = TRUE,
      title = nav_title,
      theme = shinytheme("sandstone"),
      tabPanel(
        id = "menuHome",
        title = "Home",
        titlePanel(title = "Welcome to CIRG"),
        mainPanel(
          textInput(inputId = "userName",
                    label = "What's your name?",
                    value = "world"),
          textOutput("greeting"),

        )
      ),
      tabPanel(
        id = "menuSubmissions",
        title = "Submissions",
        titlePanel(title = "Country/OU Data Submissions")
      ),
      tabPanel(
        id = "menuProcessing",
        title = "Processing",
        titlePanel(title = "Data Validation & Processing")
      ),
      tabPanel(
        id = "menuReports",
        title = "Reports",
        titlePanel(title = "Data Processing Reports")
      ),
      tabPanel(
        id = "menuData",
        title = "Data",
        titlePanel(title = "Processed Data")
      ),
      tabPanel(
        id = "menuHelp",
        title = "Help-Q&A",
        titlePanel(title = "Resources, Help & Q&A")
      )
    )
  )

# Server ----

  cirg_server <- function(input, output, session) {

    #message("The greeting is: ", output$greeting)

    curr_date <- lubridate::ymd(Sys.Date())
    #curr_date <- Sys.time()

    #output$greeting <- renderText({curr_date()})

    # output$greeting <- renderText({
    #   paste0("Hello ", input$userName, "!")
    # })

    output$greeting <- renderText({
      paste0("Hello ", input$userName, "! Today's date is ", curr_date())
    })
  }

# Run application

  shinyApp(ui = cirg_ui, server = cirg_server)
