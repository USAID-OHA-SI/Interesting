#' Restrict CIR data frame columns
#'
#' @param df CIR data frame imported via `cir_import()`
#'
#' @export

cir_restrict_cols <- function(df){

  # defaults cols
  cols <- template_cols_long[1:7]

  if(var_exists(df, "val")){
    cols <- intersect(template_cols_long, names(df))
  } else if(var_exists(df, "dreams_fp.....")){ #change this!
    cols <- intersect(template_cols_semiwide, names(df))
  } else if(var_exists(df, "dreams_fp.15_19.f...d")){
    cols <- intersect(template_wide_dreams, names(df)) #change this!
  } else if(var_exists(df, "gend_gbv.10_14.f.sexualviolence..n")){
    cols <- intersect(template_wide_gender, names(df)) #change this!
  } else if(var_exists(df, "tx_new_verify....fsw.n")){
    cols <- intersect(template_wide_kp, names(df)) #change this!
  } else if(var_exists(df, "pmtct_eid_eligible.0_12m..eideligible..n")){
    cols <- intersect(template_wide_lab, names(df)) #change this!
  } else if(var_exists(df, "ovc_offer.u1.m...n")){
    cols <- intersect(template_wide_ovc, names(df)) #change this!
  } else if(var_exists(df, "prep_screen.10_14.m...n")){
    cols <- intersect(template_wide_prep, names(df))
  } else if(var_exists(df, "sc_arvdisp...tld30_countbottles..n")){
    cols <- intersect(template_wide_sch, names(df))
  } else if(var_exists(df, "vmmc_ae.20_24.male...n")){
    cols <- intersect(template_wide_vmmc, names(df))
  }

  df <- dplyr::select_at(df, .vars = vars(all_of(cols)))

  return(df)
}

