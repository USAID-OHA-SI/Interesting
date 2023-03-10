#' Validation on import
#'
#' @param df_cir   df created during `cir_import()`
#' @param template submission template
#'
#' @export

validate_import <- function(df_cir, template){

  # Notification
  if (interactive()) {
    cat("\n---- IMPORT VALIDATIONS ----\n\n")
  }

  logger::log_info("\nValidating data against {template} template")

  # Defaults
  cols <- names(df_cir)
  ta <- cir_template_ta(df_cir)
  ta_inds <- NULL
  req_cols <- cir_template_cols(df_cir, template = template)

  # Missing & Extra Columns
  missing <- "[template could not be confirmed]"
  extra <- "[template could not be confirmed]"

  # Check multi ous sheets
  ous <- "[Unknown]"

  # List Indicators
  if (template_cols_ind %in% cols) {
    ta_inds <- df_cir %>%
      dplyr::distinct({template_cols_ind}) %>%
      dplyr::pull() %>%
      sort() %>%
      paste(collapse = ", ")
  }

  # Check data structure based on template
  if (!is.null(req_cols)) {

    # TA Indicators
    if (!template_cols_ind %in% cols) {
      ta_inds <- req_cols %>%
        setdiff(c(template_cols_core,
                  template_cols_ind,
                  template_cols_disaggs)) %>%
        stringr::str_extract("[^.]+") %>%
        base::unique() %>%
        base::sort() %>%
        paste0(collapse = ", ")
    }

    # Get missing and extra cols
    #missing <- setdiff(req_cols, cols)
    extra <- setdiff(cols, req_cols)

    # Restrict Extract Columns
    if (length(extra) > 0) {
      df_cir <- df_cir %>%
        dplyr::select(!tidyselect::all_of(extra))
    }

    # Get OUs
    ous <- df_cir %>%
      dplyr::distinct(operatingunit) %>%
      dplyr::pull(operatingunit)
  }

  # Validations
  vimp <- tibble::tibble(
    template_confirmed = !is.null(req_cols),
    template_tech_areas = paste0(ta, collapse = ", "),
    indicators = paste0(ta_inds, collapse = ", "),
    #cols_missing = paste0(missing, collapse = ", "),
    cols_extra = paste0(extra, collapse = ", "),
    cols_extra_restricted = length(extra) > 0 & all(extra %in% cols),
    has_data = nrow(df_cir) > 0,
    has_multi_ous = ifelse(str_detect(ous, "^\\[.*\\]$"), -999, length(ous) > 1),
    ous = paste0(ous, collapse = ", ")
  )

  # Notification
  if (interactive()) {
    cat("\nSheet Technical areas:", paint_yellow(vimp$template_tech_areas),
        "\nSheet Indicators?", paint_yellow(paste0(ta_inds, collapse = ", ")),
        "\nAre there missing columns?", paint_yellow(ifelse(vimp$cols_missing == "", "[None]", vimp$cols_missing)),
        #"\nAre there extra columns?", paint_yellow(ifelse(vimp$cols_extra == "", "[None]", vimp$cols_extra)),
        "\nSheet contains data?", paint_iftrue(vimp$has_data),
        "\nSheet has multiple OUs?", paint_iftrue(vimp$has_multi_ous),
        "\nOUs: ", paint_iftrue(vimp$ous),
        "\n")
  }

  # Return data along with the validations
  return(list(
    "checks" = vimp,
    "data" = df_cir
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
  # if (interactive()) {
  #   cat("\nIs there just one OU (for non regional OUs)?", !ous_check,
  #       "\nOU(s): ", ous_note,
  #       "\n")
  # }

  return(ous)
}

