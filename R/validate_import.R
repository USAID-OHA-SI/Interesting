#' Validation on import
#'
#' @param df df create during `cir_import()`
#' @param template submission template
#'
#' @export

validate_import <- function(df, template){

  # Defaults
  cols <- names(df)
  req_cols <- cir_template_cols(df, template = template)

  # Check multi ous sheets
  ous <- check_distinct_ous(df)

  # Check if wide templates has mixed tech areas
  ta <- NULL

  # Check data structure based on template
  if (!is.null(req_cols)) {
    missing <- setdiff(req_cols, cols)
    extra <- setdiff(cols, req_cols)

    # Tech area
    if (template == "Wide") {
      ta <- cir_template_ta(df)
    }

    # Restrict Extract Columns
    if (length(extra) > 0) {
      df <- cir_restrict_cols(df)
    }

  } else {
    missing <- "[template could not be confirmed]"
    extra <- "[template could not be confirmed]"
  }

  # Validations
  vimp <- tibble::tibble(
    filename = NA_character_,
    sheet = NA_character_,
    template_confirmed = !is.null(req_cols),
    template_tech_areas = paste0(ta, collapse = ", "),
    cols_missing = paste0(missing, collapse = ", "),
    cols_extra = paste0(extra, collapse = ", "),
    cols_extra_restricted = length(extra) > 0,
    has_data = nrow(df) > 0,
    has_multi_ous = length(ous) > 1,
    ous = paste0(ous, collapse = ", ")
  )

  #PRINT VALIDATION
  if (interactive()) {
    cat("\n---- IMPORT VALIDATIONS ----",
        "\nDoes sheet have mixed technical areas?", paint_yellow(vimp$template_tech_areas),
        "\nAre there any missing columns on import?", paint_yellow(vimp$cols_missing),
        "\nAre there any extra columns on import?", paint_yellow(vimp$cols_extra),
        "\nIs sheet empty?", paint_iftrue(!vimp$has_data),
        "\nHas multiple ous?", paint_iftrue(vimp$has_multi_ous),
        "\nOUs: ", paint_iftrue(vimp$ous),
        "\n")
  }

  # Return data along with the validations
  return(list(
    "checks" = vimp,
    "data" = df
  ))
}


#' Check OUs listed in operatingunit
#'
#' @param df df create during `cir_import()`
#'
#' @export

check_distinct_ous <- function(df){

  ous <- df %>%
    dplyr::distinct(operatingunit) %>%
    dplyr::pull(operatingunit)

  ous_check <- length(ous) > 1

  ous_list <- paste(ous, collapse = ", ")

  ous_note <- ifelse(ous_check == TRUE, crayon::yellow(ous_list), crayon::blue(ous_list))

  #print validation
  if (interactive()) {
    cat("\nIs there just one OU (for non regional OUs)?", !ous_check,
        "\nOU(s): ", ous_note,
        "\n")
  }

  return(ous)
}

