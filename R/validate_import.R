#' Validation on import
#'
#' @param df df create during `cir_import()`
#'
#' @export

validate_import <- function(df){

  # Defaults
  req_cols <- c()
  tmp <- NULL

  #check columns
  # TODO - Change this to case_when and move it to a separate function: eg: validate_template
  # TODO - Include the core columns into the validations
  if(var_exists(df, "val")){
    req_cols <- template_cols_long
  } else if(var_exists(df, "dreams_fp.....")){
    req_cols <- template_cols_semiwide
  } else if(var_exists(df, "dreams_fp.15_19.f...d")){
    req_cols <- template_wide_dreams
  } else if(var_exists(df, "gend_gbv.10_14.f.sexualviolence..n")){
    req_cols <- template_wide_gender
  } else if(var_exists(df, "tx_new_verify....fsw.n")){
    req_cols <- template_wide_kp
  } else if(var_exists(df, "pmtct_eid_eligible.0_12m..eideligible..n")){
    req_cols <- template_wide_lab
  } else if(var_exists(df, "ovc_offer.u1.m...n")){
    req_cols <- template_wide_ovc
  } else if(var_exists(df, "prep_screen.10_14.m...n")){
    req_cols <- template_wide_prep
  } else if(var_exists(df, "sc_arvdisp...tld30_countbottles..n")){
    req_cols <- template_wide_sch
  } else if(var_exists(df, "vmmc_ae.20_24.male...n")){
    req_cols <- template_wide_vmmc
  }

  missing <- flag_missing(req_cols, names(df))
  extra <- flag_extra(req_cols, names(df))

  # Check multi ous sheets
  ous <- check_distinct_ous(df)

  # Restrict Extract Columns
  if (length(extra) > 0) {
    df <- cir_restrict_cols(df)
  }

  # Validations
  vimp <- tibble::tibble(
    sheet = NA_character_,
    cols_missing = paste(missing, collapse = ", "),
    cols_extra = paste(extra, collapse = ", "),
    cols_extra_restricted = length(extra) > 0,
    has_multi_ous = length(ous) > 1,
    ous = paste0(ous, collapse = ", ")
  )

  #PRINT VALIDATION
  if (interactive()) {
    cat("\nAre there any missing columns on import?", vimp$cols_missing,
        "\nAre there any extra columns on import?", vimp$cols_extra)
  }

  return(list(
    "validation" = vimp,
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

