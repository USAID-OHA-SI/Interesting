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

cir_import <- function(filepath, template = NULL){

  #tmp <- ifelse(is.null(template) | is.na(template), "[Not provided]", template)
  tmp <- template

  checks <- tibble::tibble()

  logger::log_info("\nSubmission template: {null_to_chr(tmp)} ...")

  df <- filepath %>%
<<<<<<< HEAD
    readxl::excel_sheets() %>% # TODO - Use sheets_valid from `vinit`
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(function(.x) {

      # Notification
      if (interactive()) {
        cat("\n---- IMPORTING SHEET ----",
            "\nCIRG sheet name: ", paint_blue(.x),
            "\n")
      }

      logger::log_info("\nImporting data from sheet: {.x} ...")

      # Read data from excel sheet
      df_tab <- readxl::read_excel(filepath,
                                   sheet = .x,
                                   skip = 2,
                                   col_types = "text")

      # Notification
      if (interactive()) {
        cat("\nRows count: ", paint_blue(nrow(df_tab)),
            "\n")
      }
=======
    readxl::excel_sheets() %>% # TODO - Return sheets_valid from `vinit`
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(function(.x) {

      logger::log_info("\nImporting data from sheet: {.x} ...")

      df_tab <- readxl::read_excel(filepath, sheet = .x, skip = 2, col_types = "text")
>>>>>>> feature/process-flow

      logger::log_info("Rows count = {nrow(df_tab)}")

      if("mechanismid" %in% names(df)) {
        logger::log_info("\nRenaming `mechanismid` to `mech_code`")
        df <- dplyr::rename(df, mech_code = mechanismid)
      }

      # Skip import validations
      if (is.null(tmp)) {
<<<<<<< HEAD
        logger::log_info("\nSkipping import validations ...")

        df_tab <- df_tab %>%
          dplyr::mutate(temp_type = NA_character_,
                        sheet = .x,
                        row_id = dplyr::row_number() + 2)

=======
        logger::log_info("Skipping import validations ...")
>>>>>>> feature/process-flow
        return(df_tab)
      }

      # Validate only if template is provided
      vimp <- validate_import(df_tab, template = tmp)

      vimp$checks <- vimp$checks %>%
        dplyr::mutate(filename = basename(filepath), sheet = .x)

      checks <<- dplyr::bind_rows(checks, vimp$checks)

<<<<<<< HEAD
      vimp$data <- vimp$data %>%
        dplyr::mutate(temp_type = template,
                      sheet = .x,
                      row_id = dplyr::row_number() + 2)

=======
>>>>>>> feature/process-flow
      #return(df_tab)
      return(vimp$data)
    })

  return(list(
    "checks" = checks,
    "data" = df
  ))
}
