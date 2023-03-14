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
#' @param filepath submission file path
#'
#' @export

has_cirgtab <- function(filepath){

  if(missing(filepath))
    stop("No filepath provided.")

  #shts <- readxl::excel_sheets(filepath)
  shts <- cir_vsheets(filepath) %>%
    dplyr::filter(visibility == "visible") %>%
    dplyr::pull(name)

  shts_cirg <- shts %>%
    stringr::str_subset("CIRG") %>%
    length()

  msg <- paste("No CIRG tabs included in file: ", basename(filepath))

  # TODO - Make sure to return a boolean: Yes / No
  if(shts_cirg == 0 & base::interactive()) {
    usethis::ui_warn(msg)
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

has_metatab <- function(filepath){

  if(missing(filepath))
    stop("No filepath provided.")

  #shts <- readxl::excel_sheets(filepath)
  shts <- cir_vsheets(filepath) %>%
    dplyr::filter(visibility == "visible") %>%
    dplyr::pull(name)

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
  meta <- NULL

  #type
  if(has_metatab(filepath)){
    has_meta <- TRUE
    meta <- cir_extract_meta(filepath)

  } else {

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

  # Check period validity
  rep_pd <- stringr::str_remove_all(meta[meta$mtype == "period", "mvalue"], " ")

  curr_dt <- glamr::curr_date()

  pd_valid <- glamr::pepfar_data_calendar %>%
    dplyr::mutate(pd = paste0("FY", str_sub(fiscal_year, 3,4), "Q", quarter)) %>%
    dplyr::filter(entry_close <= curr_dt) %>%
    dplyr::distinct(pd) %>%
    dplyr::pull(pd) %>%
    purrr::has_element(rep_pd)

  # Check Country
  rep_ou <- meta[meta$mtype == "ou", "mvalue"]

  ou_valid <- rep_ou %in% pepfar_countries$operatingunit | rep_ou %in% pepfar_countries$ou_country

  # Check template
  #temp_valid <- meta[meta$mtype == "type", "mvalue"] %in% names(templates)
  temp_valid <- meta %>%
    dplyr::filter(mtype == "type") %>%
    dplyr::pull(mvalue) %>%
    purrr::has_element(names(templates), .)

  # Reshape metadata
  meta <- meta %>%
    tidyr::pivot_wider(names_from = mtype, values_from = mvalue) %>%
    tibble::add_column(filename = basename(filepath), .before = 1) %>%
    tibble::add_column(has_meta = has_meta) %>%
    tibble::add_column(has_valid_ou = ou_valid) %>%
    tibble::add_column(has_valid_period = pd_valid) %>%
    tibble::add_column(has_valid_template = temp_valid) %>%
    tibble::add_column(has_valid_meta = !any(is.na(meta$mvalue) & ou_valid & pd_valid & temp_valid))

  #PRINT and/or LOG VALIDATION

  if (interactive()) {

    usethis::ui_info("--- METADATA ----")

    cat("\nFilename:", crayon::blue(meta$filename),
        "\nHas metadata sheet?", paint_iftrue(meta$has_meta),
        "\nOU/Country:", paint_ifna(meta$ou),
        "\nIs OU/Country valid:", paint_iftrue(meta$has_valid_ou),
        "\nReporting period:", paint_ifna(meta$period),
        "\nIs Reporting period valid?", paint_iftrue(meta$has_valid_period),
        "\nSubmission template:", crayon::blue(paste(meta$type, meta$version)),
        "\nIs template valid?", paint_iftrue(meta$has_valid_template),
        "\nIs metadata valid?", paint_iftrue(meta$has_valid_meta),
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

  if(has_cirgtab(filepath)) {
    has_cirg <- TRUE
  }

  #worksheets

  #tabs <- readxl::excel_sheets(filepath)
  tabs <- cir_vsheets(filepath)

  tabs_list <- tabs$name

  tabs_count <- nrow(tabs)

  tabs_imported <- tabs %>%
    dplyr::filter(visibility == "visible") %>%
    dplyr::pull(name) %>%
    stringr::str_subset("CIRG") %>%
    paste(collapse = ", ")

  tabs_excluded <- tabs %>%
    dplyr::filter(visibility == "visible") %>%
    dplyr::pull(name) %>%
    stringr::str_subset("CIRG|meta", negate = TRUE) %>%
    paste(collapse = ", ")

  tabs_hidden <- tabs %>%
    dplyr::filter(visibility != "visible") %>%
    dplyr::pull(name) %>%
    paste(collapse = ", ")

  cirg <- tibble::tibble(
    has_cirg_sheets = has_cirg,
    sheets_count = tabs_count,
    sheets_valid = ifelse(nchar(tabs_imported) > 0, tabs_imported, "None"),
    sheets_excluded = ifelse(nchar(tabs_excluded) > 0, tabs_excluded, "None"),
    sheets_hidden = ifelse(nchar(tabs_hidden) > 0, tabs_hidden, "None"),
  )

  #PRINT and/or LOG VALIDATION

  if (interactive()) {
    usethis::ui_info("---- DATA SHEETS ----")

    cat("\nHas CIRG Sheets? ", paint_iftrue(cirg$has_cirg_sheets),
        "\nNumber of sheets: ", cirg$sheets_count,
        "\nData sheets [Valid sheets contain 'CIRG']: ", paint_blue(paste(tabs_list, collapse = ", ")),
        "\nSheets to be imported?", paint_green(cirg$sheets_valid),
        "\nSheets to be excluded?", paint_ifempty(cirg$sheets_exclude),
        "\nSheets hidden?", paint_ifempty(cirg$sheets_hidden),
        "\n")
  }

  return(cirg)
}




