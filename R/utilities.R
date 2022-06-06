#' Determine whether meta tab exists
#'
#' @export
#' @param filepath filepath to sumbitted template

is_metatab <- function(filepath){

  if(missing(filepath))
    stop("No filepath provided.")

  shts <- readxl::excel_sheets(filepath)
  "meta" %in% shts
}


#' Extract Meta Data Information about Template
#'
#' @description Useful for pulling information about the template, whether
#' It be the Operating Unit (OU), Period, template version, or type, eg wide or long.
#'
#' @param filepath filepath to sumbitted template
#' @param meta_type type of meta data requesting: ou, period, version, type (default)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #identify whether template is long or wide
#'   filepath <- "~/WeeklyData/Raw/KEN_Weekly.xlsx"
#'   cir_extract_meta(filepath, meta_type = "type")
#' #identify period
#'   cir_extract_meta(filepath, meta_type = "period")
#' #identify OU
#'   cir_extract_meta(filepath, meta_type = "ou") }

cir_extract_meta <- function(filepath, meta_type = "type"){

  if(is_metatab(filepath)){
    metatable <- readxl::read_excel(filepath, range = "meta!B1:E2") %>%
      stack() %>%
      rename(mvalue = values,
             mtype = ind) %>%
      select(mtype, mvalue)

    meta <- metatable %>%
      dplyr::mutate(mtype =
                      stringr::str_remove_all(mtype,
                                              "Template |CIRG Reporting |, eg 2020.1|perating |nit|\\/Country|\r\n")
                    %>% tolower) %>%
      dplyr::filter(mtype == meta_type) %>%
      dplyr::pull()


  } else {
    meta <- NA
  }

  return(meta)
}


#' Check if variable exist
#'
#' @export
#' @param df data frame to check against
#' @param var quoted variable of interest

var_exists <- function(df, var) {

  var %in% names(df)

}

#' Flag Missing Variables
#'
#' @export
#'
#' @param required list of required vars
#' @param submitted list of vars pulled from submission
#'
flag_missing <- function(required, submitted){

  missing <- setdiff(required, submitted)
  if(length(missing) > 0){
    missing <- crayon::yellow(missing)
  } else {
    missing <- crayon::green("No")
  }

  return(missing)
}

#' Flag Extra Variables
#' @export
#' @param required list of required vars
#' @param submitted list of vars pulled from submission

flag_extra <- function(required, submitted){

  extra <- setdiff(submitted, required)
  if(length(extra > 0)){
    extra <- crayon::red(extra)
  } else {
    extra <- crayon::green("No")
  }

  return(extra)
}



#' Count missing values
#'
#' @export
#' @param df data frame
#' @param var variable to count missing values

count_missing <- function(df, var){

  missing <- df %>%
    dplyr::filter(is.na({{var}})) %>%
    NROW()

  missing_pct <- round(missing/NROW(df), 2)*100
  missing_pct <- paste0("(",missing_pct, "%)")

  count <- ifelse(missing > 0, crayon::red(missing, "out of", NROW(df), "rows", missing_pct), crayon::green("No"))
  return(count)
}

#' Paint console text in red
#'
#' @param txt text to be printed
#' @export
#'
paint_red <- function(txt) {
  msg <- crayon::red(txt)
  return(msg)
}

#' Paint console text in green
#'
#' @param txt text to be printed
#' @export
#'
paint_green <- function(txt) {
  msg <- crayon::green(txt)
  return(msg)
}

#' Paint console text in blue
#'
#' @param txt text to be printed
#' @export
#'
paint_blue <- function(txt) {
  msg <- crayon::blue(txt)
  return(msg)
}

#' Paint console text in yellow
#'
#' @param txt text to be printed
#' @export
#'
paint_yellow <- function(txt) {
  msg <- rayon::yellow(txt)
  return(msg)
}
