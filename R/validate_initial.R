#' Initial Validation on Submitted template
#'
#' @param filepath filepath to sumbitted template
#'
#' @export

validate_initial <- function(filepath){

  # All submissions a bad till proven good
  valid_subm <- FALSE

  #Check Metadata
  meta <- check_meta(filepath)
  # Check data sheets
  cirg <- check_tabs(filepath)

  # Combine checks
  vinit <- meta %>%
    dplyr::bind_cols(cirg)

  # Update submission status
  if (vinit$has_valid_meta & vinit$has_cirg_sheets)
    valid_subm <- TRUE

  # Return submission status
  vinit <- vinit %>%
    dplyr::mutate(subm_valid = valid_subm)

  return(vinit)
}

#' Determine if there are tabs to import
#'
#' @note TODO - Change function name to `has_cirtab`
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

  # TODO - Make sure to return a boolean: Yes / No
  if(shts_cirg == 0) {
    #stop(msg)
    print(msg)
  }

  shts_cirg > 0
}

#' Determine whether meta tab exists
#'
#' @note TODO - Change function name to `has_metatab`
#'
#' @param filepath filepath to sumbitted template
#'
#' @export

is_metatab <- function(filepath){

  if(missing(filepath))
    stop("No filepath provided.")

  shts <- readxl::excel_sheets(filepath)

  # TODO - Make sure to capture all the cases
  "meta" %in% shts
}

#' Inspect Meta data
#'
#' @note TODO - This is doing more than checking the metadata.
#'
#' @param filepath filepath to sumbitted template
#'
#' @export

check_meta <- function(filepath){

  # response
  has_meta <- FALSE
  has_valid_meta <- FALSE

  #type
  if(is_metatab(filepath)){

    has_meta <- TRUE
    meta <- cir_extract_meta(filepath)

    # type <- cir_extract_meta(filepath, "type")
    # temp_version <- cir_extract_meta(filepath, "version")
    # reppd_meta <-  cir_extract_meta(filepath, "period")
    # ou_name <-  cir_extract_meta(filepath, "ou")
  }
  else {
    # # TODO - Should `cir_import` be checking `is_cirtab`?
    # df <- cir_import(filepath)
    #
    # # TODO - No need to guess, just reset all these variable to [no meta provided]
    # type <- ifelse(var_exists(df, "val"), "Long [no meta provided]", "Wide [no meta provided]")
    # temp_version <- "[no meta provided]"
    # reppd_meta <- "[no period provided]"

    meta <- tibble::tibble(
      ou = NA_character_,
      period = NA_character_,
      version = NA_character_,
      type = NA_character_
    ) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(),
                          names_to = 'mtype',
                          values_to = 'mvalue')
  }

  # Reshape metadata
  meta <- meta %>%
    tidyr::pivot_wider(names_from = mtype, values_from = mvalue) %>%
    tibble::add_column(filename = basename(filepath), .before = 1) %>%
    tibble::add_column(has_meta = has_meta) %>%
    tibble::add_column(has_valid_meta = !any(is.na(meta$mvalue)))

  #PRINT and/or LOG VALIDATION

  if (interactive()) {

    cat("\n--- METADATA ----",
        "\nFilename:", crayon::blue(meta$filename),
        "\nHas meta sheet?", paint_iftrue(meta$has_meta),
        "\nHas valid metadata?", paint_iftrue(meta$has_valid_meta),
        "\nOU/Country:", paint_ifna(meta$ou),
        "\nWhat template was submitted?", crayon::blue(paste(meta$type, meta$version)),
        "\nWhat reporting period?", paint_ifna(meta$period),
        "\n")

  }

  return(meta)
}

#' Validate Submission's tabs for import
#'
#' @param filepath filepath to sumbitted template
#'
#' @export

check_tabs <- function(filepath){

  has_cirg <- FALSE

  if(is_cirgtab(filepath)) {
    has_cirg <- TRUE
  }

  #tabs
  tabs <- readxl::excel_sheets(filepath)

  tabs_imported <- tabs %>%
    stringr::str_subset("CIRG") %>%
    paste(collapse = ", ")

  tabs_excluded <- tabs %>%
    stringr::str_subset("CIRG|meta", negate = TRUE) %>%
    paste(collapse = ", ")

  cirg <- tibble::tibble(
    has_cirg_sheets = has_cirg,
    sheets_count = length(tabs),
    sheets_valid = tabs_imported,
    sheets_exclude = tabs_excluded
  )

  #PRINT and/or LOG VALIDATION

  if (interactive()) {
    cat("\n---- DATA SHEETS ----",
        "\nHas CIRG Sheets? ", paint_iftrue(cirg$has_cirg_sheets),
        "\nNumber of sheets: ", cirg$sheets_count,
        "\nAll sheets [valid sheet must be labeled 'CIRG']: ", paint_blue(paste(tabs, collapse = ", ")),
        "\nWhat sheets will be imported?", paint_green(cirg$sheets_valid),
        "\nWhat sheets will be excluded?", paint_red(cirg$sheets_exclude),
        "\n")
  }

  return(cirg)
}




