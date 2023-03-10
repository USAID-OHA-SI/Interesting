#' Import template sheet(s)
#'
#' @param filepath filepath to submitted template
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #identify whether template is long or wide
#'   filepath <- "~/CIRG_FY22_Q1_Zimbabwe_20220211.xlsx"
#'   df_hfr <- cir_import(filepath) }

cir_import <- function(filepath,
                       sheets = NULL,
                       template = NULL){

  # Notification
  if (interactive()) {
    cat("\n---- IMPORTING CIR SUBMISSION ----\n\n")
  }

  # Notification
  logger::log_info("\nSubmission: {basename(filepath)}")
  logger::log_info("\nTemplate: {null_to_chr(template)}")

  # Check valid sheets
  if (is.null(sheets)) {
    sheets <- filepath %>%
      cir_vsheets(return_names = T)
  }

  logger::log_info("\nValid data sheets: {paste(sheets, collapse=', ')}")

  # Stop here for file with missing valid sheets
  if (length(sheets) == 0) {
    logger::log_info("\nNo valid data sheets detected from {basename(filepath)} ...")
    #usethis::ui_stop("ERROR - Invalid submission file. Missing CIRG Tab.")

    # Return explicit error
    return(
      list(
        "checks" = tibble::tibble(filename = basename(filepath), file_imported = FALSE),
        "data" = NULL
      )
    )
  }

  # Store all validation checks here
  checks <- tibble::tibble()

  # Read data from submissions - 1 tab at the time
  df_imp <- sheets %>%
    purrr::map_dfr(function(.x) {

      # Notification
      if (interactive()) {
        cat("\n---- IMPORTING SHEET ----\n\n")
      }

      logger::log_info("\nSheet name: {.x}")

      # Read data from excel sheet
      df_tab <- readxl::read_excel(filepath,
                                   sheet = .x,
                                   skip = 2,
                                   col_types = "text")

      # Notification
      logger::log_info("Rows count = {nrow(df_tab)}")

      # Skip import validations
      if (is.null(template)) {
        logger::log_info("\nTemplate parameter is not set - skipping import validations >...>")

        df_tab <- df_tab %>%
          dplyr::mutate(filename = basename(filepath),
                        sheet = .x,
                        row_id = dplyr::row_number() + 2)

        return(df_tab)
      }

      # Pre-validations
      pvimp <- tibble::tibble(
        filename = basename(filepath),
        file_imported = TRUE,
        sheet = .x,
        sheet_imported = FALSE
      )

      # Check for valid core columns
      if (!all(template_cols_core %in% names(df_tab))) {
        # Notification
        logger::log_info("\nTemplate missing core columns [{paste(template_cols_core, collapse=', ')}] >...> skipping import")

        # Report errors
        checks <<- pvimp %>%
          dplyr::mutate(
            sheet_imported = FALSE,
            template_confirmed = FALSE,
            has_data = nrow(df_tab) > 0,
            notes = glue::glue("Template is missing core columns: {paste(template_cols_core, collapse=', ')}")
          ) %>%
          dplyr::bind_rows(checks, .)

        # Excluding data
        return(NULL)
      }

      # Make sure sheet has data
      if (nrow(df_tab) == 0) {
        # Notification
        logger::log_info("\n[{.x}] is empty >...> skipping import")

        # Report errors
        checks <<- pvimp %>%
          dplyr::mutate(
            sheet_imported = FALSE,
            template_confirmed = FALSE,
            has_data = nrow(df_tab) > 0,
            notes = glue::glue("CIRG Sheet [{.x}] is empty")
          ) %>%
          dplyr::bind_rows(checks, .)

        # Excluding data
        return(NULL)
      }

      # Validate only if template is provided
      vimp <- validate_import(df_tab, template = template)

      #print(glimpse(vimp))

      vimp_checks <- vimp$checks

      #checks <<- dplyr::bind_rows(checks, vimp_checks)

      #prinf(vimp_checks)

      sht_import <- vimp_checks %>%
        dplyr::pull(template_confirmed)

      #print(sht_import)

      checks <<- pvimp %>%
        dplyr::mutate(sheet_imported = vimp_checks$template_confirmed) %>%
        dplyr::bind_cols(vimp_checks) %>%              # expand errors
        dplyr::bind_rows(checks, .)                    # append to global errors

      # Exist for invalidated data structure
      if (!vimp_checks$template_confirmed) return(NULL)

      # Making sure records can be traced back to submissions
      df_data <- vimp$data %>%
        dplyr::mutate(filename = basename(filepath),
                      sheet = .x,
                      row_id = dplyr::row_number() + 2)

      return(df_data)
    })

  # Move tracking variable up front
  df_imp <- df_imp %>%
    dplyr::relocate(filename, sheet, row_id, .before = 1)

  # Check missing columns based on template
  subm_cols <- df_imp %>%
    dplyr::select(-c(filename, sheet, row_id)) %>%
    base::names()

  req_cols <- df_imp %>%
    dplyr::select(-c(filename, sheet, row_id)) %>%
    cir_template_cols(template = template)

  miss_cols <- setdiff(req_cols, subm_cols)

  checks <<- checks %>%
    tibble::add_column(cols_missing = paste(miss_cols, collapse = ", "))

  # Return data + checks
  return(list(
    "checks" = checks,
    "data" = df_imp
  ))
}
