#' Validation Checks
#'
#' @param df HFR data framed created by `hfr_process_template()`
#' @param content check full dataset
#' @param datim_path path to look up files
#'
#' @export

validate_output <- function(df, output_path, content=FALSE, datim_path=NULL){

  check_output_cols(df)
  check_orgunituids(df)
  check_mechs(df)
  check_content(df)
  #check_inds(df)
  #check_disaggs(df)

  # #optional check
  # if (content & !is.null(datim_path)) {
  #   df <- check_content(df, output_path, datim_path)
  # }

  return(df)
}


#' Validate columns for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_output_cols <- function(df){

  #check headers
  req_cols <- c("reportingperiod", "orgunit", "orgunituid", "mech_code","partner",
                "operatingunit", "psnu", "indicator",
                "sex", "ageasentered", "otherdisaggregate_sub",
                "otherdisaggregate", "numeratordenom", "val",
                "temp_type", "period_meta", "version_meta", "type_meta","filepaths")

  submitted <- names(df)

  #missing columns
  missing <- flag_missing(req_cols, submitted)

  #extra columns
  extra <- flag_extra(req_cols, submitted)

  #print validation
  cat("\nAre there any missing columns for export?", missing,
      "\nAre there any extra columns for export?", extra)
}


#' Validate dates
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_dates <-function(df){

  #missing dates?
  missing_dates <- count_missing(df, reportingperiod)

  #date range
  pds <- length(unique(df$reportingperiod))

  date_range <- df %>%
    dplyr::distinct(reportingperiod) %>%
    dplyr::pull(reportingperiod)

  date_range <- ifelse(pds > 1, crayon::red(date_range), crayon::green(date_range))
  pds <- ifelse(pds > 1, crayon::red("Yes"), crayon::green("No"))

  #print validation
  cat("\nAre there any missing dates?", missing_dates,
      "\nDoes the submission cover multiple period?", pds,
      "\nWhat dates does the submission cover?", date_range)
}



#' Validate orgunituids for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_orgunituids <-function(df){

  #missing orgunituid?
  missing_orgunituid <- count_missing(df, orgunituid)

  #print validation
  cat("\nAre there any missing orgunituids?", missing_orgunituid)
}


#' Validate mechanisms for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_mechs <-function(df){

  #missing mechanisms?
  missing_mechs<- count_missing(df, mech_code)

  #mechanisms
  mech_list <- unique(df$mech_code) %>% sort() %>% paste(collapse = ", ") %>% crayon::blue()

  #print validation
  cat("\nAre there any missing mech_codes?", missing_mechs,
      "\nWhat mechanism are included?", mech_list)
}


#' Validate indicators for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_inds <-function(df){

  missing_ind <- count_missing(df, indicator)

  #indicators
  req <- df %>%


  submitted <- unique(df$indicator)

  missing <- flag_missing(req, sumbitted)
  extra <- flag_extra(req, sumbitted)


  #print validation
  cat("\nAre there any unspecified indicators?", missing_ind,
      "\nAre there any missing indicators?", missing,
      "\nAre there any extra indicators?", extra)
}


#' Validate disaggs for export
#'
#' @param df HFR data framed created by `hfr_process_template()`

check_disaggs <- function(df){

  #age/sex
  req <- c("Female <15", "Female 15+", "Male <15", "Male 15+")
  submitted <- df %>%
    dplyr::distinct(ageasentered, sex) %>%
    tidyr::unite(agesex, c("sex", "agecoarse"), sep = " ") %>%
    dplyr::pull(agesex)

  missing <- flag_missing(req, submitted)
  extra <- flag_extra(req, submitted)

  #MMD months
  req_otherdisagg <- c("<3 months", "3-5 months", "6 months or more")
  sumbitted_otherdisagg <- unique(df$otherdisaggregate) %>% setdiff(NA)

  #extra otherdisaggs
  extra_otherdisagg <- flag_extra(req_otherdisagg, sumbitted_otherdisagg)

  #print validation
  cat( "\nAre there any missing age/sex disaggs?", missing,
       "\nAre there any extra age/sex disaggs?", extra,
       "\nAre there any extra other disaggs?", extra_otherdisagg, "\n")
}


#' Validate output content
#'
#' @param df HFR DataFrame
#' @param datim_path path of datim lookup files
#' @export
#' @return df updated HFR dataframe
#'
check_content <- function(df, output_path, datim_path) {

  # cat("\nLoading lookup tables ...\n")
  #
  # # Load lookup tables: load only once
  # if ( !exists("df_orgs") | !exists("df_") ) {
  #   load_lookups(datim_path)
  # }


  #LOAD SOMEWHERE --------------------------

  # cntry <- df %>% distinct(operatingunit) %>% pull()
  # uid <- glamr::get_ouuid(cntry)
  #
  # df_orgs <- Wavelength::pull_hierarchy(uid, username = datim_user(), password = datim_pwd())
  # df_mechs <- pull_mech(usaid_only = TRUE, ou_sel = cntry, folderpath_output = NULL)

  cat("\nChecking operatingunits values ...")

  # Check and update operatingunits
  err_ou <- df %>%
    is_ou_valid(df_orgs = df_orgs) %>%
    dplyr::filter(!valid_ou) %>%
    dplyr::select(-valid_ou) %>%
    dplyr::distinct(operatingunit) %>%
    dplyr::pull()

  if ( length(err_ou) > 1 ) {

    cat("\nAre there any invalid operatingunits?", ifelse(length(err_ou) > 0, paint_red("Yes"), paint_green("No")),
        "\nList of invalid operatingunits: ", paint_red(paste(err_ou, collapse = ", ")))

    cat("\nUpdating operatingunit from mech codes ...")

    # transform & extract unique mech codes
    ims_ou <- df_mechs %>%
      dplyr::mutate(mech_code = mech_code) %>%
      dplyr::select(mech_code , ou = operatingunit) %>%
      dplyr::distinct(mech_code, ou)

    df <- df %>%
      is_ou_valid(df_orgs = df_orgs) %>%
      dplyr::mutate(mech_code = mech_code) %>%
      dplyr::left_join(ims_ou, by = c("mech_code" = "mech_code")) %>%
      dplyr::mutate(operatingunit = ifelse(valid_ou == FALSE, ou, operatingunit)) %>%
      dplyr::select(-c(valid_ou, ou))

    #Check again after update
    err_ou <- df %>%
      is_ou_valid(df_orgs = df_orgs) %>%
      dplyr::filter(!valid_ou) %>%
      dplyr::select(-valid_ou) %>%
      dplyr::distinct(operatingunit) %>%
      dplyr::pull()

    cat("\nAre there still any invalid operatingunit?", ifelse(length(err_ou) > 0, paint_red("Yes"), paint_green("No")),
        "\nList of invalid operatingunit: ", ifelse(length(err_ou) > 0, paint_red(paste(err_ou, collapse = ", ")), paint_green("None")))
  }

  # Check the rest of the data
  cat("\nChecking the entire dataset ...")



  df <- df %>%
    is_ou_valid(df_orgs = df_orgs) %>%
     is_mech_valid(df_mechs = df_mechs) %>%
     is_mech4ou(df_mechs = df_mechs) %>%
    is_orgunituid_valid(df_orgs = df_orgs) %>%
    is_orgunituid4ou(df_orgs = df_orgs)


  valid_ou <- df %>% distinct(valid_ou) %>% pull()
  valid_mech <- df %>% distinct(valid_mech) %>% pull()
  mech_to_ou <- df %>% distinct(mech_to_ou) %>% pull()
  valid_uid <- df %>% distinct(valid_uid) %>% pull()
  uid_to_ou <- df %>% distinct(uid_to_ou) %>% pull()


  cat(
      "\nIs OU valid?", ifelse(length(valid_ou) > 1, paint_red("No"), paint_green("Yes")),
      "\nAre mechs valid?", ifelse(length(valid_mech) > 1, paint_red("No"), paint_green("Yes")),
      "\nIs orgunituid valid?", ifelse(length(valid_uid) > 1, paint_red("No"), paint_green("Yes")),
      "\nDo all org units exist in OU?", ifelse(length(uid_to_ou) > 1, paint_red("No"), paint_green("Yes")),
      "\nAre all mechs valid for the OU?", ifelse(length(mech_to_ou) > 1, paint_red("No"), paint_green("Yes")),
      "\n--------------------------------------------")



#
#   # Sum up invalid columns
#   grp <- df %>%
#     dplyr::select(-c(date:val)) %>%
#     names()
#
#   df <- df %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(errors = sum(tidyselect::all_of(grp) == FALSE)) %>%
#     dplyr::ungroup()
#
#   # Errors count
#   n_errors <- df %>%
#     dplyr::filter(errors > 0) %>%
#     dplyr::distinct(mech_code) %>%
#     dplyr::pull() %>%
#     length()
#
#   if ( n_errors > 0 ) {
#     msg_errors <- paint_red('Yes')
#   } else {
#     msg_errors <- paint_green('No')
#   }
#
#   cat("\nAre there any mechanism with invalid data?", msg_errors)
#
#   if (n_errors > 0) {
#
#     cat("\nList of mechanisms with errros: ",
#         paint_red(paste(errors, collapse = ", ")))
#
#     df %>%
#       dplyr::group_by(mech_code) %>%
#       dplyr::mutate(row_id = dplyr::row_number()) %>%
#       dplyr::ungroup() %>%
#       dplyr::filter(errors > 0) %>%
#       readr::write_csv(., paste0(output_path,
#                                  "/HFR_ERRORS_", curr_fy, ".",
#                                  stringr::str_pad(dplyr::first(df$hfr_pd), 2, pad = "0"), "_",
#                                  paste(errors, collapse = "_"), "_",
#                                  format(Sys.Date(),"%Y%m%d"), ".csv"), na = "")
#
#     cat("\nThe errors file is located here: ", paint_blue(datim_path))
#   }
#
#   df <- df %>%
#     dplyr::select(date:val, errors) %>%
#     dplyr::mutate(errors = ifelse(errors > 0, TRUE, FALSE))

  return(df)
}
