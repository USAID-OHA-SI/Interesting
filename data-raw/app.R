library(shiny)
library(shinyjs)
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

    # Navigation Bar & Pages ----
    navbarPage(
      useShinyjs(),
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
                label = "Select CIRG Submission file(s) to start processing ...",
                multiple = TRUE,
                accept = ".xlsx",
                width = NULL,
                buttonLabel = "Select...",
                placeholder = "No file selected"
              )
            ),
            # Processing flow indicator ----
            column(
              width = 12,
              tags$style(type="text/css", "#procFlow {padding-top: 1px;}"),
              uiOutput(outputId = "procFlow")
            ),
            # 1: Process input files ----
            column(
              width = 12,
              # Process files
              actionButton(
                inputId = "getSubmMeta",
                label="Check metadata",
                icon = icon("info"),
                disabled = ""
              ),
              # Read Contents
              hidden(actionButton(
                inputId = "importSubm",
                label="Import data",
                icon = icon("table")
              )),
              # Augment Contents
              hidden(actionButton(
                inputId = "transformSubm",
                label="Transform",
                icon = icon("refresh")
              )),
              # Process files
              hidden(actionButton(
                inputId = "validateSubm",
                label="Validate",
                icon = icon("check")
              )),
              # Augment Contents
              hidden(actionButton(
                inputId = "augmentSubm",
                label="Augment",
                icon = icon("exchange")
              )),
              # Export Contents
              hidden(actionButton(
                inputId = "exportSubm",
                label="Export",
                icon = icon("share")
              ))
            ),
            # Submission notifications ----
            column(
              width = 12,
              tags$style(type="text/css", "#submNotification {padding-top: 10px;}"),
              uiOutput(outputId = "submNotification")
            )
          )
        ),
        # File Status ----
        fluidRow(
          column(
            width = 12,
            tags$style(type="text/css", "#submFilesList {margin-bottom: 20px;}"),
            uiOutput(outputId = "submListResults"),
            DT::dataTableOutput(outputId="submFilesList")
          ),
          column(
            width = 12,
            tags$style(type="text/css", "#submFilesChecks {margin-bottom: 20px;}"),
            uiOutput(outputId = "submProcessResults"),
            DT::dataTableOutput(outputId="submFilesChecks")
          ),
          column(
            width = 12,
            tags$style(type="text/css", "#submFilesData {margin-bottom: 20px;}"),
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
      paste0("Hello ", input$userName, ", Today's date is ", Sys.Date(), "!")
    })

    # Submissions Page ----

    # Reactive Values
    rvalues <- reactiveValues(
      subms = NULL,
      metas = NULL,
      imports = NULL,
      transformed = NULL,
      validated = NULL,
      curr_step = NULL
    )

    # Process flow
    proc_flow <- c(
      "selection",
      "metadata",
      "import",
      "transformation",
      "validation",
      "augmentation",
      "export"
    )

    # Generate current position
    curr_pos <- reactive({
      if(is.null(rvalues$curr_step)) return("")

      pos <- which(proc_flow == rvalues$curr_step)

      flow <- proc_flow[1:pos] %>%
        stringr::str_to_sentence() %>%
        paste(c(1:pos), ., collapse = " => ")

      print(flow)

      return(flow)
    })

    # Processing flow
    output$procFlow <- renderUI({
      HTML(as.character(h4(curr_pos())))
    })

    # List of selected submissions ----

    subm_files <- NULL
    #subm_files <- reactive(input$submRaw)

    observeEvent(input$submRaw, {
      print("--- Selection of the submissions ---")

      # file selected
      subm_files <<- input$submRaw

      # Current step
      rvalues$curr_step <- proc_flow[1]

      # Clear Error Messages
      output$submNotification <- renderUI({
        HTML(as.character(p("")))
      })
      # Clear files list title
      output$submListResults <- renderUI({
        HTML(as.character(h4("")))
      })
      # Clear validations sections
      output$submProcessResults <- renderUI({
        HTML(as.character(h4("")))
      })
      output$submFilesChecks <- DT::renderDataTable({})

      # Clear data sections
      output$submProcessData <- renderUI({
        HTML(as.character(h4("")))
      })
      output$submFilesData <- DT::renderDataTable({})

      # Disable Read Metadata button if no file selected
      if (is.null(subm_files)) {
        shinyjs::disable(id = "getSubmMeta")
        return(NULL)
      }

      # Enable Read Metadata
      shinyjs::enable(id = "getSubmMeta")

      # Update header
      output$submListResults <- renderUI(
        HTML(as.character(h4("Submission(s) details")))
      )

      # Reformat files list for datatable
      output$submFilesList <- DT::renderDataTable({

        # Transform selected data
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

    # Meta data from selected file ----

    df_metas <- NULL

    observeEvent(input$getSubmMeta, {
      print("--- Metadata of the submissions ---")

      # Current step
      rvalues$curr_step <- proc_flow[2]

      # Hide import buttons
      shinyjs::hideElement(id = "importSubm")
      shinyjs::hideElement(id = "transformSubm")
      shinyjs::hideElement(id = "validateSubm")
      shinyjs::hideElement(id = "augmentSubm")
      shinyjs::hideElement(id = "exportSubm")

      # Check if submission is selected
      # TODO - This should not be executed given that button should be hidden
      if (is.null(subm_files)) {
        # Error
        output$submNotification <- renderUI({
          HTML(as.character(
            p("ERROR - No submission file selected.", style = "color:red"))
          )
        })

        # Clear validations section
        output$submProcessResults <- renderUI({
          HTML(as.character(h4("")))
        })
        output$submFilesChecks <- DT::renderDataTable({})

        # Clear data selection
        output$submProcessData <- renderUI({
          HTML(as.character(h4("")))
        })
        output$submFilesData <- DT::renderDataTable({})

        return(NULL)
      }

      # Clear Error Messages
      output$submNotification <- renderUI({HTML(as.character(p("")))})

      # Update validations header
      output$submProcessResults <- renderUI({
        HTML(as.character(h4("Initial validations results")))
      })

      # Clear data header
      output$submProcessData <- renderUI({
        HTML(as.character(h4("")))
      })

      # Overwrite file list
      # Extract metadata and run initial validations
      output$submFilesChecks <- DT::renderDataTable({

        tbl_subm_files <- subm_files %>%
          mutate(filename = basename(datapath)) %>%
          select(filename, name, datapath)

        # Initial Validations
        metas <- tbl_subm_files %>%
          pull(datapath) %>%
          map_dfr(function(.file){

            meta <- Interesting::validate_initial(filepath = .file)

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
            HTML(as.character(p(glue("WARNING - Submission(s) with errors: {fnames}"),
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
          )) %>%
          DT::formatStyle(
            columns = "filename",
            valueColumns = "subm_valid",
            backgroundColor = DT::styleEqual(FALSE, glitr::burnt_sienna_light)
          ) %>%
          DT::formatStyle(
            1:5,
            'vertical-align'='top'
          )
      })

      # Clear subm data table
      output$submFilesData <- DT::renderDataTable({NULL})

      # Show Import Button
      shinyjs::showElement(id = "importSubm")
    })

    # Imports Pre-validated Submissions ----

    df_imports <- NULL

    observeEvent(input$importSubm, {
      print("--- Import of the submissions ---")

      # Current step
      rvalues$curr_step <- proc_flow[3]

      # Hide import buttons
      shinyjs::hideElement(id = "transformSubm")
      shinyjs::hideElement(id = "validateSubm")
      shinyjs::hideElement(id = "augmentSubm")

      # Check dependencies
      if (is.null(df_metas)) {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Make sure submission files are uploaded and pre-validated before proceeding with the ingestion.",
                              style = "color:red")))
        })

        return(NULL)
      }

      # Clear Notification
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
        dplyr::left_join(subm_files, by = c("filename" = "name")) %>%
        dplyr::select(-type, -size)

      v_files <- df_valids %>% pull(filename)

      if (length(v_files) == 0) {

        # Notification
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - No valid submission to ingest. Try fixing the errors.",
                              style = glue("color:{glitr::usaid_red}"))))
        })

        return(NULL)
      }

      # Notification
      output$submNotification <- renderUI({
        HTML(as.character(p(glue("INFO - Files imported: {paste(v_files, collapse=', ')}"),
                            style = glue("color:{glitr::genoa}"))))
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
          .imp$data %>%
            left_join(df_ref, by = c("filename" = "datapath")) %>%
            select(-filename) %>%
            select(filename = name, everything())
        }) %>%
        relocate(sheet, row_id, .after = filename)

      if (nrow(df_imports_data) == 0) {

        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Unable to import data. See validations results for errors."),
                            style = "color:red"))
        })

        return(NULL)
      }

      # Update header - Checks
      output$submProcessResults <- renderText({
        HTML(as.character(h4("Import validations results")))
      })

      # Render validations table
      output$submFilesChecks <- DT::renderDataTable({
        DT::datatable(
          df_imports_checks,
          colnames = str_to_upper(str_replace_all(names(df_imports_checks), "_", " ")),
          filter = "top",
          height = "300px",
          options = list(
            dom = "tlpi",
            scrollX = TRUE,
            scrollY = "250px"
          )) %>%
          DT::formatStyle(TRUE, 'vertical-align'='top')
      })

      # Update header - Data
      output$submProcessData <- renderText({
        HTML(as.character(h4("Imported raw data")))
      })

      # Render data table
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
          )) %>%
          DT::formatStyle(TRUE, 'vertical-align'='top')
      })

      # Show
      shinyjs::showElement(id = "transformSubm")
    })

    # Transform Submissions ----

    df_transformed <- NULL

    observeEvent(input$transformSubm, {
      print("--- Transformation of the submissions ---")

      # Current step
      rvalues$curr_step <- proc_flow[4]

      # Hide import buttons
      shinyjs::hideElement(id = "validateSubm")
      shinyjs::hideElement(id = "augmentSubm")
      shinyjs::hideElement(id = "exportSubm")

      # Check dependencies
      if (is.null(df_imports)) {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Make sure submission data have been imported before proceeding with the transformation",
                              style = "color:red")))
        })

        return(NULL)
      }

      # Notification
      output$submNotification <- renderUI({
        HTML(as.character(p("")))
      })

      df_transformed <<- df_imports %>%
        map_dfr(function(.imp){
          .imp$data %>%
            left_join(subm_files[,c("name", "datapath")],
                      by = c("filename" = "datapath")) %>%
            select(-filename) %>%
            select(filename = name, everything())
        }) %>%
        cir_reshape()

      # Check data
      if (nrow(df_transformed) == 0) {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Unable to reshape data. See validations results for errors."),
                            style = "color:red"))
        })

        return(null)
      }

      # Update header - Data
      output$submProcessData <- renderText({
        HTML(as.character(h4("Transformed data")))
      })

      # Render Transformed Data
      output$submFilesData <- DT::renderDataTable({
        DT::datatable(
          df_transformed,
          colnames = str_to_upper(str_replace_all(names(df_transformed), "_", " ")),
          filter = "top",
          height = "300px",
          options = list(
            dom = "tlpi",
            scrollX = TRUE,
            scrollY = "300px"
          )
        )
      })

      # Show Validate Button
      shinyjs::showElement(id = "validateSubm")
    })

    # Validate Submissions' Content ----

    df_validated <- NULL
    df_refs <- NULL

    observeEvent(input$validateSubm, {
      print("--- Validation of the submissions ---")

      # Current step
      rvalues$curr_step <- proc_flow[5]

      # Hide import buttons
      shinyjs::hideElement(id = "augmentSubm")
      shinyjs::hideElement(id = "exportSubm")

      # Check dependencies
      if (is.null(df_transformed)) {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Make sure submission data have been transformed before proceeding with the content validation.",
                              style = "color:red")))
        })

        return(NULL)
      }

      # Notification
      output$submNotification <- renderUI({
        HTML(as.character(p("")))
      })

      # Get refs datasets
      df_refs <<- list(
        ou = df_metas$ou,
        pd = df_metas$period,
        orgs = NULL,
        mechs = NULL,
        de = data_elements
      )

      # Validate Outputs
      df_validated <<- df_transformed %>%
        validate_output(refs = df_refs, content = F)

      # Notification
      output$submNotification <- renderUI({
        HTML(as.character(p(glue("Output validations - {df_validated$status} - {df_validated$message}"),
                            style = glue("color:{glitr::burnt_sienna}"))))
      })

      # Update header - Data
      output$submProcessResults <- renderText({
        HTML(as.character(h4("Ouput validations results")))
      })

      # Validation Errors
      output$submFilesChecks <- DT::renderDataTable({
        DT::datatable(
          df_validated$checks,
          colnames = str_to_upper(str_replace_all(names(df_validated$checks), "_", " ")),
          filter = "top",
          height = "300px",
          options = list(
            dom = "tlpi",
            scrollX = TRUE,
            scrollY = "300px"
          )
        )
      })

      # Update header - Data
      output$submProcessData <- renderText({
        HTML(as.character(h4("Validated data")))
      })

      # Validated Data
      output$submFilesData <- DT::renderDataTable({
        DT::datatable(
          df_validated$data,
          colnames = str_to_upper(str_replace_all(names(df_validated$data), "_", " ")),
          filter = "top",
          height = "300px",
          options = list(
            dom = "tlpi",
            scrollX = TRUE,
            scrollY = "300px"
          )
        )
      })

      # Show augment button
      shinyjs::showElement(id = "augmentSubm")
    })

    # Extend Submissions data ----

    df_augmented <- NULL

    observeEvent(input$augmenteSubm, {
      print("--- Extending the submissions ---")

      # Current step
      rvalues$curr_step <- proc_flow[5]

      # Hide import buttons
      shinyjs::hideElement(id = "exportSubm")

      # Check dependencies
      if (is.null(df_validated)) {
        output$submNotification <- renderUI({
          HTML(as.character(p("ERROR - Make sure submission data have been validaded before adding reference datasets.",
                              style = "color:red")))
        })

        return(NULL)
      }

      # Notification
      output$submNotification <- renderUI({
        HTML(as.character(p("")))
      })


    })

  }

# Run application

  shinyApp(ui = cirg_ui, server = cirg_server)
