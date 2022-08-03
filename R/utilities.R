
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

cir_extract_meta <- function(filepath, meta_type = NULL){

  if(!is_metatab(filepath))
    return(NA)

  # Read meta sheet
  metatable <- readxl::read_excel(
    path = filepath,
    #sheet = "meta",
    range = "meta!B1:E2") %>%
    utils::stack() %>%
    dplyr::rename(mvalue = values, mtype = ind) %>%
    dplyr::select(mtype, mvalue)

  metatable <- metatable %>%
    dplyr::mutate(
      mtype = stringr::str_remove_all(
        string = mtype,
        pattern = "Template |CIRG Reporting |, eg 2020.1|perating |nit|\\/Country|\r\n"
      ),
      mtype = base::tolower(mtype)
    )

  if (base::is.null(meta_type))
    return(metatable)

  # Extract specified value
  meta <- metatable%>%
    dplyr::filter(mtype == meta_type) %>%
    dplyr::pull()

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

#' Paint if na
#'
#' @param txt text to be painted and printed
#' @param true_paint crayon function to execute if ~is.na(txt)
#' @export
#'
paint_ifna <- function(txt,
                       true_paint = crayon::yellow,
                       false_paint = crayon::blue) {

  ifelse(base::is.na(txt), true_paint(txt), false_paint(txt))
}

#' Paint if null
#'
#' @param obj text to be painted and printed
#' @param true_paint crayon function to execute if ~is.na(txt)
#' @export
#'
paint_ifnull <- function(obj,
                         true_paint = crayon::red,
                         false_paint = crayon::blue) {

  ifelse(base::is.null(obj), true_paint(obj), false_paint(obj))
}

#' Paint if true
#'
#' @param value text to be painted and printed
#' @param true_paint crayon function to execute if ~is.na(txt)
#' @export
#'
paint_iftrue <- function(value,
                         true_paint = crayon::green,
                         false_paint = crayon::red) {

  ifelse(base::isTRUE(value), true_paint(value), false_paint(value))
}
