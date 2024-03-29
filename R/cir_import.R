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

  # Store all validation checks here
  checks <- tibble::tibble()

  logger::log_info("\nSubmission template: {null_to_chr(template)} ...")

  df <- filepath %>%
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

      logger::log_info("Rows count = {nrow(df_tab)}")

      if("mechanismid" %in% names(df)) {
        logger::log_info("\nRenaming `mechanismid` to `mech_code`")
        df <- dplyr::rename(df, mech_code = mechanismid)
      }

      # Skip import validations
      if (is.null(template)) {
        logger::log_info("\nSkipping import validations ...")

        df_tab <- df_tab %>%
          dplyr::mutate(filename = basename(filepath),
                        #temp_type = NA_character_,
                        sheet = .x,
                        row_id = dplyr::row_number() + 2)

        return(df_tab)
      }

      # Validate only if template is provided
      vimp <- validate_import(df_tab, template = template)

      vimp$checks <- vimp$checks %>%
        dplyr::mutate(filename = basename(filepath), sheet = .x)

      checks <<- dplyr::bind_rows(checks, vimp$checks)

      # Making sure records can be traced back to submissions
      vimp$data <- vimp$data %>%
        dplyr::mutate(filename = basename(filepath),
                      sheet = .x,
                      row_id = dplyr::row_number() + 2)

      return(vimp$data)
    })

  return(list(
    "checks" = checks,
    "data" = df
  ))
}
