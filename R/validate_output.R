#' Validation Checks
#'
#' @param df HFR data framed created by `cir_process_template()`
#' @param refs Reference datasets to include orgs, mechs, and data elements
#' @param content check full dataset
#'
#' @return list containing validated data along with results of the checks
#' @export

validate_output <- function(df, refs, content=FALSE){

  # Extract Meta Info
  subm_file <- df %>%
    dplyr::filter(!is.na(filename)) %>%
    dplyr::distinct(filename) %>%
    dplyr::pull() %>%
    dplyr::first()

  subm_sheet <- df %>%
    dplyr::filter(!is.na(sheet)) %>%
    dplyr::distinct(sheet) %>%
    dplyr::pull() %>%
    dplyr::first()

  subm_pds <- df %>%
    dplyr::filter(!is.na(reportingperiod)) %>%
    dplyr::distinct(reportingperiod) %>%
    dplyr::pull()

  # Notifications
  if (interactive()) {
    cat("\n---- OUTPUT VALIDATIONS ----",
        "\n---- Missing Values ----")
  }

  ou_miss <- df %>% get_missing("operatingunit")
  pd_miss <- df %>% get_missing("reportingperiod")
  pd_fmt <- df %>% match_value("reportingperiod")

  curr_dt <- glamr::curr_date()

  pd_post <- glamr::pepfar_data_calendar %>%
    mutate(pd = paste0("FY", str_sub(fiscal_year, 3,4), "Q", quarter)) %>%
    filter(entry_close <= curr_dt) %>%
    pull(pd) %>%
    has_element(subm_pds)

  org_miss <- df %>% get_missing("orgunituid")
  mech_miss <- df %>% get_missing("mech_code")
  ind_miss <- df %>% get_missing("indicator")
  nd_miss <- df %>% get_missing("numeratordenom")

  # Validations
  vout <- tibble::tibble(
    filename = subm_file,
    sheet = subm_sheet,
    #period = paste0(subm_pds, collapse = ", "),
    pd_missing = paste0(pd_miss, collapse = ", "),
    pd_valid = paste0(pd_fmt, collapse = ", "),
    pd_future = NA,
    ou_missing = paste0(ou_miss, collapse = ", "),
    ou_valid = NA,
    org_missing = paste0(org_miss, collapse = ", "),
    org_valid = NA,
    mech_missing = paste0(mech_miss, collapse = ", "),
    mech_valid = NA,
    ind_missing = paste0(ind_miss, collapse = ", "),
    ind_valid = NA,
    nd_missing = paste0(nd_miss, collapse = ", "),
    nd_valid = NA,
    disagg_valid = NA
  )

  if (interactive()) {
    cat("\n",
        "\nOperaringunit: ", empty_to_chr(vout$ou_missing, type="string"),
        "\nReporting Period: ", empty_to_chr(vout$pd_missing, type="string"),
        "\nOrgunit: ", empty_to_chr(vout$org_missing, type="string"),
        "\nMechanisms: ", empty_to_chr(vout$mech_missing, type="string"),
        "\nIndicator: ", empty_to_chr(vout$ind_missing, type="string"),
        "\nNum/Denominator: ", empty_to_chr(vout$nd_missing, type="string"),
        "\n")
  }

  # Skip content validation

  if (content == FALSE) {

    return(
      list(
        status = "success",
        message = "Content validation was skipped as instructed",
        checks = dplyr::select(vout, filename, sheet, dplyr::ends_with("missing")),
        data = df
      )
    )
  }

  if (content == TRUE & (is.null(refs$orgs) | is.null(refs$mechs) | is.null(refs$de))) {
    return(
      list(
        status = "failed",
        message = "Missing some (if not all) of the reference datasets needed for this validation",
        checks = dplyr::select(vout, filename, sheet, dplyr::ends_with("missing")),
        data = df
      )
    )
  }

  # Skip content validation => parameter needs to be set to true + ref datasets

  if (interactive()) {
    cat("\n---- Invalid Values ----")
  }

  # Check operatingunit
  ou_valid <- check_operatingunit(df, refs$ou)

  vout$ou_valid <- paste0(ou_valid, collapse = ", ")

  # Check orgunits
  org_valid <- check_orgunituids(df, ref_orgs = refs$orgs)

  vout$org_valid <- paste0(org_valid, collapse = ", ")

  # Check mechanisms
  mech_valid <- check_mechs(df, ref_mechs = refs$mechs)

  vout$mech_valid <- paste0(mech_valid, collapse = ", ")

  # Check Indicators
  ind_valid <- check_inds(df, ref_de = refs$de)

  vout$ind_valid <- paste0(ind_valid, collapse = ", ")

  # Check Disaggs
  disagg_valid <- check_disaggs(df, ref_de = refs$de)

  vout$disagg_valid <- paste0(disagg_valid, collapse = ", ")

  # Check numerator / denom
  nd_valid <- check_numdenom(df)

  vout$na_valid <- paste0(nd_valid, collapse = ", ")

  # Notification

  if (interactive()) {
    cat("\n",
        "\nOperaringunit: ", empty_to_chr(vout$ou_valid, type="string"),
        "\nReporting Period: ", empty_to_chr(vout$pd_valid, type="string"),
        "\nOrgunit: ", empty_to_chr(vout$org_valid, type="string"),
        "\nMechanisms: ", empty_to_chr(vout$mech_valid, type="string"),
        "\nIndicator: ", empty_to_chr(vout$ind_valid, type="string"),
        "\nNum/Denominator: ", empty_to_chr(vout$nd_valid, type="string"),
        "\nDisaggregation: ", empty_to_chr(vout$disagg_valid, type="string"),
        "\n")
  }

  # Return data, message and checks
  return(list(
    status = "success",
    message = "All validations successfully completed",
    checks = vout,
    data = df
  ))
}


#' Validate columns for export
#'
#' @param df HFR data framed created by `cir_process_template()`

check_output_cols <- function(df){

  #check headers
  req_cols <- c("reportingperiod", "orgunit", "orgunituid", "mech_code","partner",
                "operatingunit", "psnu", "indicator",
                "sex", "ageasentered", "otherdisaggregate_sub",
                "otherdisaggregate", "numeratordenom", "val",
                "temp_type", "period_meta", "version_meta", "type_meta","filepaths",
                "file_size", "google_id")

  submitted <- names(df)

  #missing columns
  missing <- flag_missing(req_cols, submitted)

  #extra columns
  extra <- flag_extra(req_cols, submitted)

  #print validation
  cat("\nAre there any missing columns for export?", missing,
      "\nAre there any extra columns for export?", extra)
}


#' @title Check OU
#'
#' @param df Data frame from transformed submission
#' @param ou Name of OU/Country submitting data
#'
#' @return list of row ids with invalid operatingunit
#' @export

check_operatingunit <- function(df, ou) {

  # Countries Reference List
  cntries <- pepfar_countries %>%
    dplyr::filter(ou_country == ou | operatingunit == ou) %>%
    dplyr::pull(country)

  # Check if Operating Unit listed is valid
  df %>%
    dplyr::filter(!is.na(operatingunit)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(valid_ou = operatingunit %in% cntries) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!valid_ou) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)
}


#' Validate orgunituids
#'
#' @param df       HFR data frame containing reshaped submission
#' @param ref_orgs Datim OU Orgunits Reference Data as data frame
#'
#' @export
#' @return list of row ids with invalid orgunituid

check_orgunituids <- function(df, ref_orgs){

  df %>%
    dplyr::filter(!is.na(orgunituid)) %>%
    dplyr::left_join(ref_orgs, by = "orgunituid") %>%
    dplyr::filter(is.na(orgunit_level)) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)

}


#' Validate Implementing Mechanisms for export
#'
#' @param df        HFR data frame containing reshaped submission
#' @param ref_mechs Datim OU Mechanisms Reference Data as data frame
#'
#' @export
#' @return list of row ids with invalid mechanism code

check_mechs <- function(df, ref_mechs){

  df %>%
    dplyr::filter(!is.na(mech_code)) %>%
    dplyr::left_join(ref_mechs, by = "mech_code") %>%
    dplyr::filter(is.na(mech_name)) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)
}


#' Validate indicators for export
#'
#' @param df     HFR data frame containing reshaped submission
#' @param ref_de CIR Data Element Reference Data as data frame
#'
#' @export
#' @return list of row ids with invalid indicators

check_inds <- function(df, ref_de){

  df %>%
    dplyr::filter(!is.na(indicator)) %>%
    dplyr::left_join(
      ref_de[, c("indicator", "field_marking")], by = "indicator") %>%
    dplyr::filter(is.na(field_marking)) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)
}

#' Validate Numerator/Denominator for export
#'
#' @param df     HFR data frame containing reshaped submission
#'
#' @export
#' @return list of row ids with invalid numerator/denominator values

check_numdenom <- function(df){

  df %>%
    dplyr::filter(!is.na(numeratordenom)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      nd_valid = numeratordenom %in% c("N", "Numerator", "D", "Denominator")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!nd_valid) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)

}


#' Validate disaggs for export
#'
#' @param df     HFR data frame containing reshaped submission
#' @param ref_de CIR Data Element Reference Data as data frame
#'
#' @export
#' @return list of row ids with invalid indicators' disaggregations

check_disaggs <- function(df, ref_de){

  df %>%
    dplyr::filter(!is.na(indicator)) %>%
    dplyr::left_join(
      dplyr::select(ref_de, !c("tech_area", "disaggregate_group")),
      by = c("indicator", "ageasentered" = "age",
             "sex", "otherdisaggregate",
             "otherdisaggregate_sub", "numeratordenom")
    ) %>%
    dplyr::filter(is.na(field_marking)) %>%
    dplyr::distinct(row_id) %>%
    dplyr::pull(row_id)

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


  # LOAD SOMEWHERE

  cntry <- df %>%
    dplyr::distinct(operatingunit) %>%
    dplyr::pull()

  uid <- grabr::get_ouuid(cntry)

  df_orgs <- Wavelength::pull_hierarchy(uid, username = glamr::datim_user(), password = glamr::datim_pwd())
  df_mechs <- Wavelength::pull_mech(usaid_only = TRUE, ou_sel = cntry, folderpath_output = NULL)

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
