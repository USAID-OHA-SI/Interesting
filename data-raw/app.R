library(shiny)
library(shinythemes)
library(tidyverse)
library(glamr)
library(glitr)
library(Interesting)
library(googledrive)
library(googlesheets4)
library(glue)



# Global Options ----

  app_title <- "FEPFAR/USAID - Custom Indicators Reporting"
  #nav_title <- "CUSTOM INDICATORS REPORTING"
  nav_title <- "FEPFAR/USAID - CUSTOM INDICATORS REPORTING"

# Functions ----


# User Interface ----

  cirg_ui <- fluidPage(

    # Navigation Bar & Pages
    navbarPage(
      id = "mainMenu",
      fluid = TRUE,
      collapsible = TRUE,
      title = nav_title,
      position = "fixed-top",
      theme = shinytheme("cerulean"),#cerulean, spacelab, yeti, sandstone
      #footer = titlePanel(title = app_title),
      tags$style(type="text/css",
        "body {padding-top: 70px;}
        .navbar-nav {float: right;}"),
      # Home Page ----
      tabPanel(
        id = "menuHome",
        title = "Home",
        titlePanel(title = "Welcome to Custom Indicators Reporting Page"),
        mainPanel(
          textInput(inputId = "userName",
                    label = "What's your name?",
                    value = "world"),
          textOutput("greeting"),
          htmltools::includeMarkdown("cirg_guidance.md")
        )
      ),
      # Submission Page ----
      tabPanel(
        id = "menuSubmissions",
        title = "Submissions",
        titlePanel(title = "OU / Country Data Submissions"),
        wellPanel(
          fluidRow(
            tags$style(type="text/css", "body {padding-bottom: 50px;}"),
            # 0: Input files ----
            column(
              width = 12,
              fileInput(
                inputId = "submRaw",
                label = "Select CIRG Submission file(s) to start ...",
                multiple = TRUE,
                accept = ".xlsx",
                width = NULL,
                buttonLabel = "Select...",
                placeholder = "No file selected"
              )
            ),
            # 1: Process input files ----
            column(
              width = 12,
              # Process files
              actionButton(
                inputId = "getSubmMeta",
                label="Metadata",
                icon = icon("info")
              ),
              # Read Contents
              actionButton(
                inputId = "importSubm",
                label="Import",
                icon = icon("table")
              ),
              # Augment Contents
              actionButton(
                inputId = "transformSubm",
                label="Transform",
                icon = icon("refresh")
              ),
              # Process files
              actionButton(
                inputId = "validateSubm",
                label="Validate",
                icon = icon("check")
              ),
              # Augment Contents
              actionButton(
                inputId = "augmentSubm",
                label="Augment",
                icon = icon("exchange")
              ),
              # Export Contents
              actionButton(
                inputId = "ingestSubm",
                label="Ingest",
                icon = icon("database")
              ),
              # Export Contents
              actionButton(
                inputId = "exportSubm",
                label="Export",
                icon = icon("share")
              )
            ),
            # Submission notifications
            column(
              width = 12,
              tags$style(type="text/css", "#submNotification {padding-top: 10px;}"),
              uiOutput(outputId = "submNotification")
            )
          )
        ),
        # File Status
        fluidRow(
          column(
            width = 12,
            uiOutput(outputId = "submProcessResults"),
            DT::dataTableOutput(outputId="submFilesList")
          ),
          column(
            width = 12,
            #tags$style(type="text/css", "#submFilesMeta {padding-top: 20px;}"),
            DT::dataTableOutput(outputId="submFilesMeta")
          ),
          column(
            width = 12,
            uiOutput(outputId = "submProcessData"),
            DT::dataTableOutput(outputId="submFilesData")
          )
        )
      ),
      # Processing Page ----
      tabPanel(
        id = "menuProcessing",
        title = "Processing",
        titlePanel(title = "Data Validation & Processing")
      ),
      # Reports Page ----
      tabPanel(
        id = "menuReports",
        title = "Reports",
        titlePanel(title = "Data Processing Reports")
      ),
      # Data Page ----
      tabPanel(
        id = "menuData",
        title = "Data",
        titlePanel(title = "Processed Data")
      ),
      # Other Pages ----
      navbarMenu(
        title = "Help-Q&A",
        tabPanel(
          id = "help",
          title = "Help Resources",
          titlePanel(title = "Help Resources")
        ),
        tabPanel(
          id = "questions",
          title = "Frequently Asked Questions",
          titlePanel(title = "Frequently Asked Questions")
        )
      )
    )
  )

# Server ----

  cirg_server <- function(input, output, session) {

    # Home Page ----
    output$greeting <- renderText({
      #curr_date <- lubridate::ymd(Sys.Date())
      paste0("Hello ", input$userName, "! Today's date is ", Sys.Date())
    })

    # Submissions Page ----

    subm_files <- NULL

    # List of selected submissions
    observeEvent(input$submRaw, {

      if (is.null(input$submRaw)) return(NULL)

      # Save for other processes
      subm_files <<- input$submRaw

      # Update header
      output$submProcessResults <- renderUI(
        HTML(as.character(h4("Selected submission(s) details")))
      )

      # Reformat files list for datatable
      output$submFilesList <- DT::renderDataTable({
        tbl_subm_files <- subm_files %>%
          mutate(type = tools::file_ext(datapath),
                 id = row_number()) %>%
          select(-datapath) %>%
          relocate(id, .before = 1) %>%
          select(-id) # DataTable takes care of the row id

        #return(subm_files)
        DT::datatable(
          tbl_subm_files,
          colnames = str_to_upper(names(tbl_subm_files)),
          height = "200px",
          options = list(
            dom = "t",
            scrollX = TRUE,
            scrollY = TRUE
          )
        )
      })
    })

    # Meta data from selected file
    df_metas <- NULL

    observeEvent(input$getSubmMeta, {

      if (is.null(subm_files)) {
        # Error
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - No submission file selected.",
                              style = "color:red")))
        })

        # Clear validations title
        output$submProcessResults <- renderUI({
          HTML(as.character(h4("")))
        })
        # Clear data title
        output$submProcessData <- renderUI({
          HTML(as.character(h4("")))
        })

        return(NULL)
      }

      # Clear Error Messages
      output$submNotification <- renderUI({HTML(as.character(p("")))})

      # Update validations header
      output$submProcessResults <- renderUI({
        HTML(as.character(h4("Selected submission(s) initial validation results")))
      })

      # Clear data header
      output$submProcessData <- renderUI({
        HTML(as.character(h4("")))
      })

      # Overwrite file list
      # Extract metadata and run initial validations
      output$submFilesList <- DT::renderDataTable({

        tbl_subm_files <- subm_files %>%
          mutate(filename = basename(datapath)) %>%
          select(filename, name, datapath)

        # Initial Validations
        metas <- tbl_subm_files %>%
          pull(datapath) %>%
          map_dfr(function(.file){

            meta <- validate_initial(filepath = .file)

            return(meta)
          })

        # Make metas available globally
        df_metas <<- metas %>%
          left_join(tbl_subm_files, by = "filename") %>%
          mutate(filename = name) %>%
          select(-datapath, -name)

        i_files <- df_metas %>%
          filter(!subm_valid) %>%
          pull(filename)

        if (length(i_files) > 0) {

          fnames <- paste(i_files, collapse = ", ")

          output$submNotification <- renderUI({
            HTML(as.character(p(glue("WARNING - These submission file(s) have some errors and can not be imported: {fnames}"),
                                style = glue("color:{glitr::burnt_sienna}"))))
          })
        }

        # Return DataTable
        DT::datatable(
          df_metas,
          colnames = str_to_upper(str_replace_all(names(df_metas), "_", " ")),
          filter = "top",
          height = "300px",
          options = list(
            dom = "tlpi",
            scrollX = TRUE,
            scrollY = TRUE
          )
        ) %>%
          DT::formatStyle(
            columns = "filename",
            valueColumns = "subm_valid",
            backgroundColor = DT::styleEqual(FALSE, glitr::burnt_sienna_light)
          )
      })

      # Clear subm data table
      output$submFilesData <- DT::renderDataTable({NULL})
    })


    # Imports Pre-validated Submissions

    df_imports <- NULL

    observeEvent(input$importSubm, {

      if (is.null(df_metas)) {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Make sure submission files are uploaded and pre-validated before proceeding with the ingestion.",
                              style = "color:red")))
        })
        return(NULL)
      }

      output$submNotification <- renderUI({
        HTML(as.character(p("")))
      })

      # Process data
      df_ref <- subm_files %>%
        mutate(datapath = basename(datapath)) %>%
        select(name, datapath)

      df_valids <- df_metas %>%
        filter(subm_valid == TRUE) %>%
        rename(subm_type = type) %>%
        left_join(subm_files, by = c("filename" = "name")) %>%
        select(-type, -size)

      v_files <- df_valids %>% pull(filename)

      if (length(v_files) > 0) {
        output$submNotification <- renderUI({
          HTML(as.character(p(glue("INFO - Files imported: {paste(v_files, collapse=', ')}"),
                              style = glue("color:{glitr::genoa}"))))
        })
      } else {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - No valid submission to ingest. Try fixing the errors.",
                              style = glue("color:{glitr::usaid_red}"))))
        })
        return(NULL)
      }

      # Update header - Checks
      output$submProcessResults <- renderText({
        HTML(as.character(h4("Selected submission(s) ingestion validation results")))
      })

      # Import data from submission files
      df_imports <<- df_valids %>%
        select(datapath, subm_type) %>%
        pmap(~cir_import(filepath = .x, template = .y))

      df_imports_checks <- df_imports %>%
        map_dfr(function(.imp){
          .imp$checks %>%
            left_join(df_ref, by = c("filename" = "datapath")) %>%
            select(-filename) %>%
            select(filename = name, everything())
        })

      df_imports_data <- df_imports %>%
        map_dfr(function(.imp){
          .imp$data
          # %>%
          #   left_join(df_ref, by = c("filename" = "datapath")) %>%
          #   select(-filename) %>%
          #   select(filename = name, everything())
        })

      if (nrow(df_imports_data > 0)) {
        # Update header - Data
        output$submProcessData <- renderText({
          HTML(as.character(h4("Selected submission(s) raw data")))
        })
      }
      else {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Unable to import data. See validations results for errors."),
                            style = "color:red"))
        })
      }

      #print(df_imports)
      # glimpse(df_imports[[1]]$checks)
      # glimpse(df_imports[[1]]$data)
      print(length(df_imports))

      output$submFilesList <- DT::renderDataTable({
        DT::datatable(
          df_imports_checks,
          colnames = str_to_upper(str_replace_all(names(df_imports_checks), "_", " ")),
          filter = "top",
          height = "300px",
          options = list(
            dom = "tlpi",
            scrollX = TRUE,
            scrollY = "250px"
          )
        )
      })

      output$submFilesData <- DT::renderDataTable({
        DT::datatable(
          df_imports_data,
          colnames = str_to_upper(str_replace_all(names(df_imports_data), "_", " ")),
          filter = "top",
          height = "300px",
          options = list(
            dom = "tlpi",
            scrollX = TRUE,
            scrollY = "300px"
          )
        )
      })
    })
  }

# Run application

  shinyApp(ui = cirg_ui, server = cirg_server)
