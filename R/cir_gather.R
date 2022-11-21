#' Reshape CIR Data frame long
#'
#' @param df CIR data frame imported via `cir_import()`
#'
#' @export

cir_gather <- function(df){

  # No need to reshape data in long format
  if(var_exists(df, "val")){
    #df <- df %>% mutate(temp_type = "Long")
    return(df)
  }

  #only need to gather if the data set is wide (will not have indicator as column)

  # identify data columns
  meta_cols <- c("filename", "sheet", "row_id")
  core_cols <- c(template_cols_meta, meta_cols)

  data_cols <- setdiff(names(df), core_cols)

  #reshape from wide to long
  df <- df %>%
    #tidyr::gather(indicator, val, all_of(data_cols), na.rm = TRUE)
    tidyr::pivot_longer(cols = all_of(data_cols),
                        names_to = "indicator",
                        values_to = "val")

  # seperate former col names into indicator & disaggregates
  if(any(stringr::str_detect(df$indicator, ".n"))) {

    df <- tidyr::separate(df,
                          col = indicator,
                          into = c("indicator", "age", "sex",
                                   "otherdisaggregate", "population",
                                   "numdenom"),
                          sep = "\\.",
                          fill = "right")
  }

  #reorganize
  df <- df %>%
    select(any_of(meta_cols), all_of(core_cols), val)

  # df <- df %>%
  #   dplyr::select(
  #     reportingperiod:psnu, indicator, sex, age,
  #     population, otherdisaggregate, numdenom, val) %>%
  #   mutate(temp_type = ifelse(endsWith(indicator, "....."),
  #                             "Semi-wide", "Wide"))

  return(df)
}

