#' @title Transform CIR Data Frame
#'
#' @note This can also be accomplished with `cir_gather()` + `cir_munge_string()`
#'
#' @param df_cir CIR raw and restricted data frame
#' @param clean  Clean indicators and disaggregates - default is TRUE
#'
#' @export
#' @return Cleaned and tidy data frame

cir_reshape <- function(df_cir, clean = TRUE) {

  df_cir <- df_cir %>% cir_gather()

  #
  if (!clean) return(df_cir)

  # Optional Clean
  cir_munge_string(df_cir)
}

#' Reshape CIR Data frame to long
#'
#' @param df_cir CIR data frame imported via `cir_import()`
#'
#' @export

cir_gather <- function(df_cir){

  # No need to reshape data in long format
  if (var_exists(df_cir, c("val", "value"))) return(df_cir)

  #only need to gather if the data set is wide (will not have indicator as column)

  # identify data columns
  track_cols <- c("filename", "sheet_name", "row_id")
  core_cols <- c(track_cols, template_cols_meta)

  data_cols <- setdiff(names(df_cir), core_cols)

  #reshape from wide to long
  df_cir <- df_cir %>%
    tidyr::pivot_longer(cols = all_of(data_cols),
                        names_to = "indicator_code",
                        values_to = "value")

  # Wide - Separate indicator codes into indicator & disaggregates
  if (!var_exists(df_cir,
                  c(template_cols_ind,
                    template_cols_disaggs[template_cols_disaggs != template_cols_kp]),
                  all = TRUE)) {

    df_cir <- df_cir %>%
      tidyr::separate(
        col = indicator_code,
        into = c("indicator",
                 "age", "sex",
                 "otherdisaggregate",
                 "population",
                 "numdenom"),
        sep = "\\.",
        fill = "right",
        remove = FALSE)

  } else {
    # Semi-wide
    df_cir <- df_cir %>%
      dplyr::mutate(indicator = stringr::str_remove(indicator_code, "[.]+$"))
  }

  # Re-position tracking columns
  if(any(track_cols %in% names(df_cir))) {
    df_cir <- df_cir %>%
      dplyr::relocate(track_cols, .before = 1)
  }

  # Re-position `population` column
  if (var_exists(df_cir, template_cols_kp)) {
    df_cir %>% dplyr::filter(!is.na(value) & !is.na(indicator)) %>%
      dplyr::relocate(template_cols_kp,
                      .before = dplyr::last(template_cols_disaggs))
  }

  # Drop empty values
  df_cir <- df_cir %>% dplyr::filter(!is.na(value) & !is.na(indicator)) %>%
    dplyr::relocate(numdenom, .before = value) %>%
    dplyr::relocate(indicator_code, indicator,
                    .before = dplyr::first(template_cols_disaggs))

  return(df_cir)
}


#' clean up/standardize string text for indicators and disaggs
#'
#' @param df_cir HFR data frame imported via `cir_import()`
#'
#' @export
#'

cir_munge_string <- function(df_cir){

  #reorganize
  df_cir <- df_cir %>%
    dplyr::rename(ageasentered = age,
                  numeratordenom = numdenom)

  # Clean and/or recode values
  df_cir <- df_cir %>%
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
      numeratordenom = str_sub(numeratordenom, 1, 1),
      value = as.numeric(value),
      across(where(is.character), ~na_if(., "")))

  return(df_cir)
}

#' Join Meta to Dataset
#'
#' @param df_cir      CIR data frame imported via `cir_import()`
#' @param df_meta CIR Meta data
#'
#' @export

cir_join_meta <- function(df_cir, df_meta) {

  df_cir %>%
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
