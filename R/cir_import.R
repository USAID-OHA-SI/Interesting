#' Import template sheet(s)
#'
#' @param filepath filepath to submitted template
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #identify whether template is long or wide
#'   filepath <- "~/CIRG_FY22_Q1_Zimbabwe_20220211.xlsx"
#'   df_hfr <- cir_import(filepath) }

cir_import <- function(filepath){

  df <- filepath %>%
    readxl::excel_sheets() %>%
    #setdiff("meta") %>%
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(.f = ~ readxl::read_excel(filepath, sheet = .x, skip = 2, col_types = "text"))
  # %>%
  #   rename('vmmc_ae..m.sitetype:unknown..n' = vmmc_ae..m.unknown..n...30,
  #          'vmmc_ae..m.aetype:unknown..n' = vmmc_ae..m.unknown..n...20)
  #
  if("mechanismid" %in% names(df))
    df <- dplyr::rename(df, mech_code = mechanismid)

  return(df)

}
