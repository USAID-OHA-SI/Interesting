#' Pull Partner/Mechanism Info from DATIM
#'
#' @param usaid_only specify if only USAID mechansism should be returned, default = TRUE
#' @param ou_sel option to specify an operating unit, default = NULL
#' @param folderpath_output provide the full path to the folder for saving
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #pull mechanism/partner information
#' df <- pull_mech() }

pull_mech <- function(usaid_only = TRUE, ou_sel = NULL, folderpath_output = NULL){

  package_check("curl")

  stopifnot(curl::has_internet())

  #url for SQL View stored as a csv
  sql_view_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"

  #pull from DATIM
  df2 <- readr::read_csv(sql_view_url,
                        col_types = readr::cols(.default = "c"))

  #fitler for OU
  if(!is.null(ou_sel))
    df2 <- dplyr::filter(df2, ou %in% c(ou_sel))

  #filter for USAID mechs if specified
  if(usaid_only == TRUE)
    df2 <- dplyr::filter(df2, agency == "USAID")

  #rename variables to match MSD
  df2 <- df2 %>%
    dplyr::select(operatingunit = ou,
                  fundingagency = agency,
                  mech_code = code,
                  primepartner = partner,
                  mech_name = mechanism)

  #remove mech_code from mech_name
  df2 <- df2 %>%
    dplyr::mutate(mech_name = stringr::str_remove(mech_name, "0000[0|1] |[:digit:]+ - "))

  #remove award information from mech_name
  df2 <- df2 %>%
    dplyr::mutate(mech_name = stringr::str_remove(mech_name, "^(720|AID|GHAG|U2GP).* - "))

  #export
  hfr_export(df2, folderpath_output, type = "mechanisms")

  #compile file name  and export data
  filename <- paste("CIR", ou_sel, "mech_list", sep = "_") %>%
    paste0(".csv") %>%
    stringr::str_replace_all("_{2,}", "_")

  #readr::write_csv(x = df2, file = file.path(folderpath_output, filename), na = "")

  return(df2)
}




#' Update USAID Mechanism meta table
#'
#' @param savefolder folderpath to save, default = "out/DATIM"
#' @param upload should the new table be pushed to Google Drive and s3? default = FALSE
#'
#' @export
update_meta_mechs <- function(savefolder = "out/DATIM", upload = FALSE){

  #pull updated mechanism table
  df_mechs <- pull_mech(folderpath_output = savefolder)

  #upload to Google Drive and s3
  if(upload == TRUE)
    upload_meta_table("mech")

  invisible(df_mechs)
}
