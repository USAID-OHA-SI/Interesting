#' Setup processing folders
#'
#' @param folder Folder path used for CIRG Data processing
#' @param dt     Processing date. If null, current date is used
#'
#' @export
#'
cir_setup <- function(folder = "cirg-submissions", dt = NULL) {

  # Date
  curr_dt <- ifelse(
    is.null(dt),
    base::format(base::Sys.Date(), "%Y-%m-%d"),
    dt
  )

  # Processing folder
  if (!base::dir.exists(file.path(".", folder)))
    base::dir.create(file.path(".", folder))

  # Current Processing folder
  dir_curr_proc <- file.path(".", folder) %>%
    base::file.path(paste0("CIRG-", curr_dt))

  dir_curr_proc %>% base::dir.create()

  # Sub-folders
  df_subfolders <- c("reference",
    "raw",
    "metadata",
    "validations",
    "processed",
    "transformed",
    "cleaned",
    "final",
    "archive") %>%
    tibble::tibble(folder = .) %>%
    dplyr::mutate(order = row_number() - 1)

  # Create all sub-folders
  df_subfolders %>%
    purrr::pwalk(~base::dir.create(
      path = base::file.path(".", dir_curr_proc, paste0(.y, "-", .x))
    ))
}


#' Get processing folder path
#'
#' @param type Folder type
#' @param dt   processing date
#'
#' @export
#'
cir_folder <- function(type = "raw", dt = NULL) {
  # Date
  curr_dt <- ifelse(is.null(dt),
                    base::format(base::Sys.Date(), "%Y-%m-%d"),
                    dt)

  # Processing folder
  folder <- base::file.path(paste0("./cirg-submissions/CIRG-", curr_dt)) %>%
    fs::dir_ls(regexp = paste0(type, "$"), recurse = TRUE)

  if (!base::dir.exists(folder)) {
    usethis::ui_warn(glue::glue("Folder does not exist: {folder}"))
    return(NULL)
  }

  return(folder)
}


#' Output intermediate data for files
#'
#' @param .df_out Data to be outputs
#' @param .subm   Submission file
#' @param .name   Output base name - no extension needed, default is `.csv`
#' @param type    Output data type, default is set to metadata
#'
#' @export
#'
cir_output <- function(.df_out, .subm, .name,
                       type = "metadata") {

  # Process Date
  pdate <- .subm %>%
    dirname() %>%
    dirname() %>%
    basename() %>%
    str_remove("CIRG-")

  # Output directories
  dir_out <- case_when(
    type == "metadata" ~ cir_folder(type = "metadata", dt = pdate),
    type == "validations" ~ cir_folder(type = "validations", dt = pdate),
    type == "processed" ~ cir_folder(type = "processed", dt = pdate),
    type == "transformed" ~ cir_folder(type = "transformed", dt = pdate),
    type == "cleaned" ~ cir_folder(type = "cleaned", dt = pdate),
    type == "final" ~ cir_folder(type = "final", dt = pdate),
    TRUE ~ .subm %>% dirname() %>% dirname()
  )

  file_out <- .subm %>%
    base::basename() %>%
    stringr::str_remove(".xlsx") %>%
    base::paste0(" - ", .name, ".csv")

  file_out %>%
    base::file.path(dir_out, .) %>%
    readr::write_csv(x = .df_out, file = ., na = "")
}


#' Archive submissions files
#'
#' @param .subm   Submission file
#'
#' @export
#'
cir_archive <- function(.subm) {

  # Process Date
  pdate <- .subm %>%
    dirname() %>%
    dirname() %>%
    basename() %>%
    str_remove("CIRG-")

  # archive directories
  dir_arch <- cir_folder(type = "archive", dt = pdate)

  destpath <- file.path(dir_arch, basename(.subm))

  fs::file_move(path = .subm,
                new_path = destpath)

  if (interactive()) {
    usethis::ui_info("Submission has been archived to: {destpath}")
  }
}


#' @title Get list of visible excel sheets
#'
#' @param .subm   Submission file
#'
#' @return Worksheet visibility as data frame
#' @export
#'
cir_vsheets <- function(.subm) {

  # Notification
  # if(base::interactive())
  #   usethis::ui_info("Checking worksheets visibility for: {.subm}")

  # load file as workbook and check sheets visibility
  wb <- openxlsx::loadWorkbook(file = .subm)

  .subm %>%
    openxlsx::getSheetNames() %>%
    tibble::tibble(
      filename = base::basename(.subm),
      name = .) %>%
    dplyr::mutate(
      visibility = openxlsx::sheetVisibility(wb)
    )
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

#' Store Meta Data Information about Template
#'
#' @description store meta data information for later validations
#'
#' @note TODO - This seems similar to `cir_extract_meta` function. Try this for meta: identify, extract, validate and store
#'
#' @param filepath filepath to sumbitted template
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # meta_df <- cir_store_meta(filepath)}

cir_store_meta <- function(filepath){

  if(is_metatab(filepath)){
    metatable <- readxl::read_excel(filepath, range = "meta!B1:E2") %>% # BK - What if the meta tab is moved?
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
                    file_size = file.size(filepath),
                    google_id = NA,
                    period_meta = str_replace(period_meta, pattern=" ", repl=""))


  } else {
    meta_df <- NA
  }

  return(meta_df)
}

#' @title Validate template
#'
#' @param df Data frame to check against
#' @param template Template name to validate against
#'
#' @export
#' @return List of required column names
#'
#' @examples
#' \dontrun{
#' tmp = "Semi-wide"
#' tmp_cols <- cir_template_cols(df, template = tmp)}

cir_template_cols <- function(df_cir, template = "long") {

  req_cols <- NULL

  # Long
  if(template == "Long" & var_exists(df_cir, template_cols_long)) {
    req_cols <- template_cols_long
  }

  # Semi-wide
  if(template == "Semi-wide" &
      var_exists(df_cir, c(template_cols_core, template_cols_disaggs)) &
      !var_exists(df_cir, template_cols_ind) &
      var_exists(df_cir, setdiff(template_cols_semiwide,
                                 c(template_cols_core, template_cols_disaggs)),
                 all = FALSE)) {
    req_cols <- template_cols_semiwide
  }

  # Wide
  if(template == "Wide" &
      var_exists(df_cir, template_cols_core) &
      !var_exists(df_cir, template_cols_ind) &
      var_exists(df_cir, setdiff(template_cols_wide, template_cols_core),
                 all = FALSE)) {

    ta <- cir_template_ta(df_cir)

    req_cols <- ta %>%
      stringr::str_to_lower() %>%
      purrr::map(function(.x) {
        template_cols_wgroups[[.x]]
      }) %>%
      base::unlist() %>%
      base::unique()
  }

  # if(tmp == "Long" & var_exists(df, template_cols_long)){
  #   req_cols <- template_cols_long
  # } else if(tmp == "Semi-wide" &
  #           var_exists(df, c(template_cols_core, template_cols_disaggs)) &
  #           !var_exists(df, template_cols_ind) &
  #           var_exists(df, setdiff(template_cols_semiwide,
  #                                  c(template_cols_core,
  #                                    template_cols_disaggs)),
  #                      all = FALSE)){
  #   req_cols <- template_cols_semiwide
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "dreams")){
  #   req_cols <- template_wide_dreams
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "gend_gbv")){
  #   req_cols <- template_wide_gender
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "^[ct]_verify")){
  #   req_cols <- template_wide_kp
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "eligible|sample|result")){
  #   req_cols <- template_wide_lab
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "ovc")){
  #   req_cols <- template_wide_ovc
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "prep")){
  #   req_cols <- template_wide_prep
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "sc")){
  #   req_cols <- template_wide_sch
  # } else if(tmp == "Wide" & var_exists(df, template_cols_core) & var_matches(df, "vmmc")){
  #   req_cols <- template_wide_vmmc
  # }

  return(req_cols)
}

#' @title Extract Technical Areas
#'
#' @param df_cir Data frame to extract columns from
#'
#' @export
#' @return Technical Area name(s)
#'
#' @examples
#' \dontrun{
#' ta <- cir_template_ta(df)}

cir_template_ta <- function(df_cir) {

  setdiff(names(df_cir),
          c(template_cols_core, "indicator",
            template_cols_disaggs)) %>%
    stringr::str_extract("[^.]+") %>%
    stringr::str_to_lower() %>%
    tibble::tibble(indicator = .) %>%
    dplyr::mutate(
      ta = dplyr::case_when(
        indicator == "val" ~ "ALL",
        indicator %in% c("dreams_fp", "dreams_gend_norm") ~ "DREAMS",
        indicator %in% c("gend_gbv") ~ "GENDER",
        indicator %in% c("ovc_enroll", "ovc_offer",
                 "ovc_vl_eligible", "ovc_vlr", "ovc_cls") ~ "OVC",
        indicator %in% c("tx_pvls_eligible", "tx_pvls_sample",
                 "tx_pvls_result_returned",
                 "pmtct_eid_eligible", "pmtct_eid_result_returned") ~ "LAB",
        indicator %in% c("prep_screen", "prep_eligible", "prep_new_verify",
                 "prep_1month", "prep_ct_verify") ~ "PrEP",
        indicator %in% c("sc_arvdisp", "sc_curr", "sc_lmis") ~ "SCH",
        indicator %in% c("tx_new_verify", "tx_rtt_verify",
                 "tx_curr_verify", "tx_pvls_verify") ~ "KP",
        indicator %in% c("vmmc_ae") ~ "VMMC",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::distinct(ta) %>%
    dplyr::pull()
}


#' Restrict CIR data frame columns
#'
#' @param df CIR data frame imported via `cir_import()`
#'
#' @export

cir_restrict_cols <- function(df){

  # defaults cols
  cols <- template_cols_core

  # Template's Tech Area
  ta <- cir_template_ta(df)

  # Valid Columns
  cols <- NULL

  if (ta == "ALL") {
    cols <- intersect(template_cols_long, names(df))
  } else if (is.na(ta)) {
    cols <- intersect(template_cols_semiwide, names(df))
  } else {
    cols <- intersect(template_cols_wgroups[ta], names(df))
  }

  if (is.null(cols)) return(NULL)

  df <- dplyr::select_at(df, .vars = vars(all_of(cols)))

  return(df)
}



#' Check if all variables exist
#'
#' @export
#' @param df data frame to check against
#' @param var quoted variable of interest

var_exists <- function(df, var, all = TRUE) {
  if (all) {
    all(var %in% names(df))}
  else {
    any(var %in% names(df))
  }
}

#' Check if any variable matches this pattern
#'
#' @param df data frame to check against
#' @param pattern quoted variable of interest
#'
#' @export
#'

var_matches <- function(df, pattern) {
  any(stringr::str_detect(string = names(df), pattern = pattern))
}

#' Flag Missing Variables
#'
#' @param required list of required vars
#' @param submitted list of vars pulled from submission
#'
#' @export
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
#'
#' @param required list of required vars
#' @param submitted list of vars pulled from submission
#'
#' @export

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

  nr <- nrow(df)

  missing <- df %>%
    dplyr::filter(is.na({{var}})) %>%
    nrow()

  missing_pct <- round(missing/nr, 2)*100
  missing_pct <- paste0("(", missing_pct, "%)")

  count <- ifelse(missing > 0,
                  crayon::red(missing, "out of", NROW(df), "rows", missing_pct),
                  crayon::green("No"))

  return(count)
}

#' @title Get rows with missing values
#'
#' @export
#' @param df data frame
#' @param var variable to count missing values

get_missing <- function(df, var){

  df_miss <- df

  if (!"row_id" %in% names(df_miss)) {
    df_miss <- df_miss %>%
      dplyr::mutate(row_id = row_number() + 2)
  }

  df_miss %>%
    dplyr::filter(is.na({{var}})) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)
}


#' @title Get rows with values not matching a pattern
#'
#' @export
#' @param df data frame
#' @param var variable to count missing values

match_value <- function(df, var, pattern = "FY\\d{2}Q\\d{1}"){

  df_miss <- df

  if (!"row_id" %in% names(df_miss)) {
    df_miss <- df_miss %>%
      dplyr::mutate(row_id = row_number() + 2)
  }

  df_miss %>%
    dplyr::filter(is.na({var})) %>%
    dplyr::mutate(vmatch = stringr::str_match({{var}}, pattern)) %>%
    dplyr::filter(isFALSE(vmatch)) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)
}



#' Not provided if null
#'
#' @param obj text to be printed
#' @export
#'
null_to_chr <- function(obj) {
  ifelse(is.null(obj), "[not provided]", obj)
}


#' Not available if na
#'
#' @param obj text to be printed
#' @export
#'
na_to_chr <- function(obj) {
  ifelse(is.null(obj), "[not available]", obj)
}


#' Return none if vector/list is empty
#'
#' @param obj text to be printed
#' @param type Type of the object, default set to atomic (vector or list). Options are atomic or string
#' @export
#'
empty_to_chr <- function(obj, type = "atomic") {
  if (type == "atomic") {
    ifelse(purrr::is_empty(obj), "None", paste0(obj, collapse = ", "))
  }
  else if (type == "string") {
    ifelse(base::is.character(obj) & base::nchar(obj) == 0, "None", obj)
  }
  else {
    base::toString(obj)
  }
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
  msg <- crayon::yellow(txt)
  return(msg)
}

#' Paint if na
#'
#' @param txt text to be painted and printed
#' @param true_paint crayon function to execute if ~is.na(txt)
#' @param false_patin crayon function to execute
#' @export
#'
paint_ifna <- function(txt,
                       true_paint = crayon::yellow,
                       false_paint = crayon::blue) {

  ifelse(base::is.na(txt), true_paint(txt), false_paint(txt))
}

#' Paint if empty
#'
#' @param txt text to be painted and printed
#' @param true_paint crayon function to execute if ~is_empty(txt) or txt == ""
#' @param false_patin crayon function to execute
#' @export
#'
paint_ifempty <- function(txt,
                          true_paint = crayon::blue,
                          false_paint = crayon::yellow) {

  ifelse(toString(txt) == "", true_paint("None"), false_paint(txt))
}

#' Paint if null
#'
#' @param obj text to be painted and printed
#' @param true_paint crayon function to execute if is.null(obj)
#' @param false_paint crayon function to execute if !is.null(ojb)
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
#' @param true_paint crayon function to execute
#' @param false_paint crayon function to execute
#' @export
#'
paint_iftrue <- function(value,
                         true_paint = crayon::green,
                         false_paint = crayon::red) {

  ifelse(base::isTRUE(value), true_paint(value), false_paint(value))
}
