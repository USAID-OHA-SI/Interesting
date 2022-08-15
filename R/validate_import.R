#' Validation on import
#'
#' @param df df create during `cir_import()`
#' @param template submission template
#'
#' @export

validate_import <- function(df, template){

  # Defaults
  tmp <- template
  req_cols <- NULL

  #check columns
  # TODO - Change this to case_when and move it to a separate function: eg: validate_template
  # TODO - Include the core columns into the validations
  if(tmp == "Long" & var_exists(df, template_cols_long)){
    req_cols <- template_cols_long
  } else if(tmp == "Semi-wide" & var_exists(df, c(template_cols_core, template_cols_disaggs)) & !var_exists(df, template_cols_ind)){
    req_cols <- template_cols_semiwide
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "dreams")){
    req_cols <- template_wide_dreams
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "gend_gbv")){
    req_cols <- template_wide_gender
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "^[ct]_verify")){
    req_cols <- template_wide_kp
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "eligible|sample|result")){
    req_cols <- template_wide_lab
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "ovc")){
    req_cols <- template_wide_ovc
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "prep")){
    req_cols <- template_wide_prep
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "sc")){
    req_cols <- template_wide_sch
  } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "vmmc")){
    req_cols <- template_wide_vmmc
  }

  # Check multi ous sheets
  ous <- check_distinct_ous(df)

  # Check data structure based on template
  if (!is.null(req_cols)) {
    #missing <- flag_missing(req_cols, names(df))
    missing <- setdiff(req_cols, names(df))

    #extra <- flag_extra(req_cols, names(df))
    extra <- setdiff(names(df), req_cols)

  } else {
    missing <- "[template could not be confirmed]"
    extra <- missing
  }

  # Restrict Extract Columns
  if (length(extra) > 0 & !is.null(req_cols)) {
    df <- cir_restrict_cols(df)
  }

  # Validations
  vimp <- tibble::tibble(
    filename = NA_character_,
    sheet = NA_character_,
    template_confirmed = !is.null(req_cols),
    cols_missing = paste(missing, collapse = ", "),
    cols_extra = paste(extra, collapse = ", "),
    cols_extra_restricted = length(extra) > 0,
    has_data = nrow(df) > 0,
    has_multi_ous = length(ous) > 1,
    ous = paste0(ous, collapse = ", ")
  )

  #PRINT VALIDATION
  if (interactive()) {
    cat("\n---- IMPORT VALIDATIONS ----",
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
  # if (interactive()) {
  #   cat("\nIs there just one OU (for non regional OUs)?", !ous_check,
  #       "\nOU(s): ", ous_note,
  #       "\n")
  # }

  return(ous)
}

