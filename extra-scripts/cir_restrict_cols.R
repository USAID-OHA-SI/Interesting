#' Restrict CIR data frame columns
#'
#' @param df CIR data frame imported via `cir_import()`
#'
#' @export

cir_restrict_cols <- function(df){

  if(var_exists(df, "val")){
    cols <- intersect(template_cols_long, names(df))
  } else if(var_exists(df, "hts_tst.NA.NA")){ #change this!
    cols <- intersect(template_cols_wide_lim, names(df))
  } else {
    cols <- intersect(template_cols_wide, names(df)) #change this!
  }
  df <- dplyr::select_at(df, cols)

  return(df)
}
