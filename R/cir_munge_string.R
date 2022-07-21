#' clean up/standardize string text for indicators and disaggs
#'
#' @param df HFR data frame imported via `cir_import()`
#'
#' @export
#'

cir_munge_string <- function(df){


  df <- df %>%
    rename(ageasentered = age,
           numeratordenom = numdenom,
           otherdisaggregate_sub = population)

  df <- df %>%
    mutate(reportingperiod = str_replace(reportingperiod, pattern=" ", repl=""),
           reportingperiod = toupper(reportingperiod),
           indicator = toupper(indicator),
           indicator = str_replace(indicator, pattern="PREP", repl="PrEP"),
           indicator = stringr::str_replace_all(indicator, " |  ", "_"),
           ageasentered = stringr::str_replace_all(ageasentered, " ", ""),
           numeratordenom = stringr::str_replace_all(numeratordenom, " ", ""),
           numeratordenom = toupper(numeratordenom),
           sex = dplyr::recode(sex,
                               "f" = "Female",
                               "F" = "Female",
                               "m" = "Male",
                               "M" = "Male"),
           #sex = na_if(sex, " "),
           ageasentered = stringr::str_replace(ageasentered, pattern= "_", repl = "-"),
           ageasentered = dplyr::recode(ageasentered,
                                        "u1" = "<1",
                                        "u10" = "<10",
                                        "unknownage" = "Unknown",
                                        "o50" = ">50"),
           across(where(is.character), ~na_if(., "")),
           #ageasentered = na_if(ageasentered, " "),
           sex = ifelse(indicator == "DREAMS_FP" & is.na(sex), "Female", sex),
           val = as.numeric(val)) %>%
    filter(!is.na(val) & !is.na(indicator))
   # left_join(meta_df, by = c("operatingunit" = "ou_meta", "reportingperiod" = "period_meta")



  return(df)
}


cir_join_meta <- function(df) {

df <-   df %>%
    left_join(meta_df, by = c("operatingunit" = "ou_meta"
                             # , "reportingperiod" = "period_meta"
                              ))

return(df)

}





# reportingperiod - make string + removing all spaces + making uppercase - Done
# orgunituid - make string + removing all space - Done
# mech_code - make string + removing all spaces
# indicator - make string + remove all spaces + make lowercase
# age - make string + remove all spaces + make lowercase
# sex - make string + remove all spaces + make lowercase
# val - make numeric - done
# filter val and indicator NAs
