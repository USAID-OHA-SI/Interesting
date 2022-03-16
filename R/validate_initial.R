#' Initial Validation on Submitted template
#'
#' @param filepath filepath to sumbitted template
#'
#' @export


validate_initial <- function(filepath){

  #initial validation before proceeding to see if any CIRG named tabs
  is_cirgtab(filepath)

  check_meta(filepath)
  check_tabs(filepath)

}

# --------------------------------------------------------

#' Determine if there are tabs to import
#'
#' @param filepath filepath to sumbitted template
#'
#' @export

is_cirgtab <- function(filepath){

  if(missing(filepath))
    stop("No filepath provided.")

  shts <- readxl::excel_sheets(filepath)

  shts_cirg <- shts %>%
    stringr::str_subset("CIRG") %>%
    length()

  msg <- paste("No CIRG tabs included in file: ", basename(filepath))


  if(shts_cirg == 0)
    stop(msg)

}

# -----------------------------

#' Inspect Meta data
#'
#' @param filepath filepath to sumbitted template

check_meta <- function(filepath){
  #type
  if(is_metatab(filepath)){
    type <- cir_extract_meta(filepath, "type")
    temp_version <- cir_extract_meta(filepath, "version")
  } else {
    df <- cir_import(filepath)
    type <- ifelse(var_exists(df, "val"), "Long [no meta provided]", "Wide [no meta provided]")
    temp_version <- "[no meta provided]"
  }

  temp_version <- crayon::blue(paste(type, temp_version))

  #country/file
  ou_name <-  cir_extract_meta(filepath, "ou")
  ou_name <- ifelse(is.na(ou_name), crayon::yellow(ou_name), crayon::blue(ou_name))
  file_name <- crayon::blue(basename(filepath))

  #PRINT VALIDATION

  cat("\n--------------------------------------------",
      "\nCountry:", ou_name,
      "\nFile name:", file_name,
      "\nWhat template was submitted?", temp_version
  )
}

#' Validate Submission's tabs for import
#'
#' @param filepath filepath to sumbitted template

check_tabs <- function(filepath){

  #tabs
  tabs <- readxl::excel_sheets(filepath)

  tabs_imported <- tabs %>%
    stringr::str_subset("CIRG") %>%
    paste(collapse = ",")

  tabs_imported_ok <- ifelse(length(tabs_imported) > 0, crayon::green("TRUE"), crayon::green("FALSE"))

  tabs_imported <- crayon::blue(tabs_imported)

  tabs_excluded <- tabs %>%
    stringr::str_subset("CIRG", negate = TRUE) %>%
    paste(collapse = ",") %>%
    crayon::yellow()

  #PRINT VALIDATION

  cat("\nAre there tabs to import [must be labeled 'CIRG']?", tabs_imported_ok,
      "\nWhat tabs will be imported?", tabs_imported,
      "\nWhat tabs will be excluded?", tabs_excluded)
}




