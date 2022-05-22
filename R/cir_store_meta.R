#' Store Meta Data Information about Template
#'
#' @description store meta data information for later validations
#'
#' @param filepath filepath to sumbitted template
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # meta_df <- cir_store_meta(filepath)

cir_store_meta <- function(filepath){

  if(is_metatab(filepath)){
    metatable <- readxl::read_excel(filepath, range = "meta!B1:E2") %>%
      stack() %>%
      rename(mvalue = values,
             mtype = ind) %>%
      select(mtype, mvalue)

    meta_df <- metatable %>%
      dplyr::mutate(mtype =
                      stringr::str_remove_all(mtype,
                                              "Template |CIRG Reporting |, eg 2020.1|perating |nit|\\/Country|\r\n")
                    %>% tolower,
                    mtype = stringr::str_c(mtype, "_meta")) %>%
      tidyr::pivot_wider(names_from = mtype, values_from = mvalue) %>%
      dplyr::mutate(filepaths = basename(filepath),
                    google_id = NA,
                    period_meta = str_replace(period_meta, pattern=" ", repl=""))


  } else {
    meta <- NA
  }

  return(meta_df)
}


