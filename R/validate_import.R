#' Validation on import
#'
#' @param df df create during `cir_import()`
#'
#' @export

validate_import <- function(df){

  #check columns
  if(var_exists(df, "val")){
    req_cols <- template_cols_long
  } else if(var_exists(df, "dreams_fp.....")){
    req_cols <- template_cols_semiwide
  } else if(var_exists(df, "dreams_fp.15_19.f...d")){
    req_cols <- temp_wide_dreams
  } else if(var_exists(df, "gend_gbv.10_14.f.sexualviolence..n")){
    req_cols <- temp_wide_gender
  } else if(var_exists(df, "tx_new_verify....fsw.n")){
    req_cols <- temp_wide_kp
  } else if(var_exists(df, "pmtct_eid_eligible.0_12m..eideligible..n")){
    req_cols <- temp_wide_lab
  } else if(var_exists(df, "ovc_offer.u1.m...n")){
    req_cols <- temp_wide_ovc
  } else if(var_exists(df, "prep_screen.10_14.m...n")){
    req_cols <- temp_wide_prep
  } else if(var_exists(df, "sc_arvdisp...tld30_countbottles..n")){
    req_cols <- temp_wide_sch
  } else if(var_exists(df, "vmmc_ae.20_24.male...n")){
    req_cols <- temp_wide_vmmc
  }

  missing <- flag_missing(req_cols, names(df))
  extra <- flag_extra(req_cols, names(df))

  #PRINT VALIDATION

  cat("\nAre there any missing columns on import?", missing,
      "\nAre there any extra columns on import?", extra)

  check_distinct_ous(df)

}


#' Check OUs listed in operatingunit
#'
#' @param df df create during `hfr_import()`
#'
#' @export

check_distinct_ous <- function(df){

  ous <- df %>%
    dplyr::distinct(operatingunit) %>%
    dplyr::pull(operatingunit)

  multi_ous <- length(ous) > 1

  ous <- ous %>%
    paste(collapse = ", ")

  ou_out <- ifelse(multi_ous == TRUE, crayon::yellow(ous), crayon::blue(ous))

  #print validation
  cat("\nIs there just one OU (for non regional OUs)?", ou_out)
}

