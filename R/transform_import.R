#' @title Transform CIR Data Frame
#'
#' @node This can also be accomplished with `cir_gather()` + `cir_munge_string()`
#'
#' @param df CIR raw and restricted data frame
#'
#' @export
#' @return Cleaned and tidy data frame

cir_reshape <- function(df) {
  df %>%
    cir_gather() %>%
    cir_munge_string()
}

#' Reshape CIR Data frame to long
#'
#' @param df CIR data frame imported via `cir_import()`
#'
#' @export

cir_gather <- function(df){

  # No need to reshape data in long format
  if(var_exists(df, "val")){
    return(df)
  }

  #only need to gather if the data set is wide (will not have indicator as column)

  # identify data columns
  meta_cols <- c("filename", "sheet", "row_id")
  core_cols <- c(template_cols_meta, meta_cols)

  data_cols <- setdiff(names(df), core_cols)

  #reshape from wide to long
  df <- df %>%
    tidyr::pivot_longer(cols = all_of(data_cols),
                        names_to = "indicator_code",
                        values_to = "val")

  # seperate former col names into indicator & disaggregates
  if(any(stringr::str_detect(df$indicator_code, "[\\.]+"))) {

    df <- tidyr::separate(
      df,
      col = indicator_code,
      into = c("indicator", "age", "sex",
               "otherdisaggregate", "population",
               "numdenom"),
      sep = "\\.",
      fill = "right",
      remove = FALSE
    )
  }

  if(any(meta_cols %in% names(df))) {
    df <- df %>%
      dplyr::relocate(meta_cols, .before = 1)
  }

  # if(any(c(meta_cols, "test_col") %in% names(df))) {
  #   df <- df %>%
  #     dplyr::relocate(meta_cols, .before = 1)
  # }

  return(df)
}


#' clean up/standardize string text for indicators and disaggs
#'
#' @param df HFR data frame imported via `cir_import()`
#'
#' @export
#'

cir_munge_string <- function(df){

  #reorganize
  df <- df %>%
    dplyr::rename(ageasentered = age,
                  numeratordenom = numdenom,
                  otherdisaggregate_sub = population)

  # Clean and/or recode values
  df <- df %>%
    dplyr::mutate(
      reportingperiod = stringr::str_replace(reportingperiod, " ", ""),
      reportingperiod = toupper(reportingperiod),
      indicator = toupper(indicator),
      indicator = stringr::str_replace(indicator, "PREP", "PrEP"),
      ageasentered = stringr::str_replace_all(ageasentered, " ", ""),
      ageasentered = stringr::str_replace(ageasentered, "_", "-"),
      ageasentered = case_when(
      stringr::str_detect(ageasentered, "unknown") ~ "Unknown Age",
        stringr::str_detect(ageasentered, "^u") ~
         stringr::str_replace(ageasentered, "^u", "<"),
        stringr::str_detect(ageasentered, "^o") ~
         paste0(stringr::str_remove(ageasentered, "^o"), "+"),
        TRUE ~ ageasentered
      ),
      sex = dplyr::recode(sex,
                          "f" = "Female",
                          "F" = "Female",
                          "m" = "Male",
                          "M" = "Male"),
      sex = ifelse(indicator == "DREAMS_FP" & is.na(sex), "Female", sex),
      numeratordenom = stringr::str_replace_all(numeratordenom, " ", ""),
      numeratordenom = toupper(numeratordenom),
      across(where(is.character), ~na_if(., "")),
      val = as.numeric(val)) %>%
    dplyr::filter(!is.na(val) & !is.na(indicator))

  return(df)
}

#' Join Meta to Dataset
#'
#' @param df      CIR data frame imported via `cir_import()`
#' @param df_meta CIR Meta data
#'
#' @export

cir_join_meta <- function(df, df_meta) {

  df %>%
    dplyr::left_join(df_meta,
                     by = c("operatingunit" = "ou_meta"
                            #,"reportingperiod" = "period_meta"
                     ))
}


# reportingperiod - make string + removing all spaces + making uppercase - Done
# orgunituid - make string + removing all space - Done
# mech_code - make string + removing all spaces
# indicator - make string + remove all spaces + make lowercase
# age - make string + remove all spaces + make lowercase
# sex - make string + remove all spaces + make lowercase
# val - make numeric - done
# filter val and indicator NAs
