#' Restrict CIR data frame columns
#'
#' @param df CIR data frame imported via `cir_import()`
#'
#' @export

cir_restrict_cols <- function(df){

  if(var_exists(df, "val")){
    cols <- intersect(template_cols_long, names(df))
  } else if(var_exists(df, "dreams_fp.....")){ #change this!
    cols <- intersect(template_cols_semiwide, names(df))
  } else if(var_exists(df, "tx_new_verify....fsw.n")){
    cols <- intersect(template_cols_wide_kp, names(df)) #change this!
  }
  df <- dplyr::select_at(df, cols)

  return(df)
}

