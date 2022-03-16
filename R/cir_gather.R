#' Reshape CIR Data frame long
#'
#' @param df CIR data frame imported via `hfr_import()`
#'
#' @export

cir_gather <- function(df){

  #only need to gather if the data set is wide (will not have indicator as column)

  if(!var_exists(df, "val")){

    #identify data columns
    data_cols <- setdiff(names(df), template_cols_meta)

    #reshape from wide to long
    df <- tidyr::gather(df, indicator, val, all_of(data_cols), na.rm = TRUE)

    if(any(stringr::str_detect(df$indicator, ".n"))) {
      #seperate former col names into indicator & disaggregates
      df <- tidyr::separate(df, indicator, c("indicator", "age", "sex", "otherdisaggregate", "population", "numdenom"),
                             sep = "\\.", fill = "right")
    }

    #reorganize
    df <- dplyr::select(df, reportingperiod:psnu, indicator,sex, age, population, otherdisaggregate, numdenom, val)
  }

  return(df)

}
