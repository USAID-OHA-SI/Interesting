# Libraries

  library(tidyverse)
  library(glamr)
  library(Interesting)
  library(googledrive)
  library(googlesheets4)


# SETUP

  # Processing folder
  proc_folder <- "../../CIRG-SUBMISSIONS"

  # Processing Date
  #proc_date <- glamr::curr_date()
  proc_date <- "2023-03-06"

  if (!dir.exists(file.path(proc_folder, paste0("CIRG-", proc_date)))) {
    cir_setup(folder = proc_folder, dt = proc_date)
  }

  dir_raw <- paste0("CIRG-", proc_date) %>%
    file.path(proc_folder, .) %>%
    cir_folder(folderpath = ., type = "raw")

# GOOGLE DRIVE ----
#TODO Function to pull latest/resubmisions

  cir_subm_sheet <- "1amabNYu1HF9rZ1Y-Hy5p8lQ73Ynll744HZxPyZuVEgI"

  cir_subm_id <- as_id(cir_subm_sheet)

  googledrive::drive_get(cir_subm_id)

  #googlesheets4::gs4_browse(cir_subm_id)
  #googledrive::drive_browse(cir_subm_id)

  df_cir_subm <- googlesheets4::read_sheet(cir_subm_id)

  # CIR Submissions

  df_cir_subm <- df_cir_subm %>%
    janitor::clean_names() %>%
    select(subm_time = timestamp,
           subm_poc = email_address,
           subm_ou = operating_unit_country,
           subm_period = custom_indicator_fy_and_period,
           subm_type = what_type_of_submission_is_this,
           subm_tmp_type = which_template_s_are_you_submitting,
           subm_tech_areas = what_technical_area_s_are_you_submitting_data_for,
           subm_files = upload_your_custom_indicator_template_file_s_here_excel_sheets_only_no_google_sheets,
           subm_processed = processed_by_cirg,
           subm_rejected = file_s_rejected_or_accepted,
           subm_frozen = frozen_for_processing_yes_no,
           subm_notes = cirg_notes,
           subm_feedb_sent = feedback_sent_back_to_mission_c_cs) %>%
    rowwise() %>%
    mutate(subm_files_count = length(unlist(str_split(subm_files, ", "))),
           subm_id = as.numeric(lubridate::as_datetime(subm_time, tz = "EST"))) %>%
    ungroup() %>%
    relocate(subm_id, .before = 1) %>%
    relocate(subm_files_count, .after = last_col())

  # Check duplicates
  df_cir_subm %>%
    count(subm_id) %>%
    filter(n > 1)

  # CIR Submission files

  df_cir_subm %>% distinct(subm_period)

  df_cir_files <- df_cir_subm %>%
    filter(subm_period == last(subm_period)) %>%
    select(subm_id, subm_file = subm_files) %>%
    separate_rows(subm_file, sep = ",\\s") %>%
    mutate(subm_file_id = str_extract(subm_file, "(?<=\\?id\\=).*"),
           subm_filename = pull(drive_get(as_id(subm_file_id)), name),
           subm_file_valid = str_detect(subm_filename, ".xlsx$")) %>%
    relocate(subm_file_id, .after = subm_id)

  # DOWNLOAD Submissions

  df_cir_files %>%
    filter(subm_file_valid = TRUE) %>%
    select(subm_file_id, subm_filename) %>%
    pwalk(~drive_download(file = as_id(.x),
                          path = file.path(dir_raw, .y),
                          overwrite = T))

# LOCAL FILES ----

  #subms <- fs::dir_ls("~/Downloads", regexp = "CIRG_.*.xlsx$")
  subms <- fs::dir_ls(dir_raw, regexp = "CIRG_.*.xlsx$")

  # Worksheets Visibility

  shts <- subms %>%
    dplyr::first() %>%
    cir_vsheets()

  vshts <- subms %>%
    dplyr::first() %>%
    cir_vsheets(return_names = T)

  #subms %>% map_dfr(cir_vsheets)

  # Metadata

  # Initial Validation

  metas <- subms %>%
    map_dfr(validate_initial)

  #  Get random file by template
  subm_file <- metas %>%
    #filter(!if_any(!filename, ~is.na(.)), type == "Wide") %>%     # test wide templates
    #filter(!if_any(!filename, ~is.na(.)), type == "Semi-wide") %>% # Test semi-wide templates
    filter(!if_any(!filename, ~is.na(.)), type == "Long") %>%      # Test semi-wide templates
    filter(str_detect(filename, "FY23")) %>%
    pull(filename) %>%
    sample(1) %>%
    #first() %>%
    #nth(2) %>%
    file.path(dir_raw, .)

  # open_path(subm_file)

  meta <- subm_file %>%
    validate_initial()

  # Import & 2nd round of validations
  df_subm <- subm_file %>%
    cir_import(template = meta$type)

  df_subm$checks %>% glimpse
  df_subm$data %>% glimpse

  df_subm$data %>%
    select(-(1:filename)) %>%
    cir_template_ta()

  df_subm$data %>%
    select(-(1:filename)) %>%
    cir_template_cols(template = meta$type)

  df_subm$checks %>%
    mutate(across(where(is.logical), as.character)) %>%
    mutate(across(where(is.integer), as.character)) %>%
    cir_reshape_checks(vname = "value")

  # Transformation

  #df_trans <- df_subm$data %>% cir_reshape(clean = F)
  df_trans <- df_subm$data %>% cir_reshape(clean = T)

  df_trans %>% glimpse()

  # Template DataElement Pattern
  # <indicator>.<age>.<sex>.<otherdisaggs>.<population>.<numdenom>

  # Validate outputs -- Missing

  # df_trans %>% get_missing("operatingunit")
  # df_trans %>% get_missing("reportingperiod")
  # df_trans %>% match_value("reportingperiod", "FY\\d{2}Q\\d{1}")
  #
  # curr_dt <- glamr::curr_date()
  #
  # glamr::pepfar_data_calendar %>%
  #   mutate(pd = paste0("FY", str_sub(fiscal_year, 3,4), "Q", quarter)) %>%
  #   filter(entry_close <= curr_dt) %>%
  #   distinct(pd) %>%
  #   pull(pd) %>%
  #   has_element("FY22Q4")
  #
  # df_trans %>% get_missing("orgunituid")
  # df_trans %>% get_missing("mech_code")
  # df_trans %>% get_missing("indicator")
  # df_trans %>% get_missing("numeratordenom")


  # Validate outputs -- wrong values

  #df_orgs <- meta$ou %>%
  df_orgs <- "Ethiopia" %>%
    purrr::map_dfr(~datim_orgunits(
      username = glamr::datim_user(),
      password = glamr::datim_pwd(),
      cntry = .x,
      base_url = "https://datim.org"))

  df_orgs %>% glimpse()

  df_mechs <- meta$ou %>%
    purrr::map_dfr(~datim_mechs(
      username = glamr::datim_user(),
      password = glamr::datim_pwd(),
      cntry = .x,
      base_url = "https://datim.org"))

  df_mechs %>% glimpse()

  # df_trans %>%
  #   check_operatingunit(ou = meta$ou)
  #
  # df_trans %>%
  #   check_orgunituids(ref_orgs = df_orgs)
  #
  # df_trans %>%
  #   check_mechs(ref_mechs = df_mechs)
  #
  # df_trans %>%
  #   check_inds(ref_de = data_elements)
  #
  # df_trans %>%
  #   check_numdenom()
  #
  # df_trans %>%
  #   check_disaggs(ref_de = data_elements)

  # ref datasets
  refs <- list(
    ou = meta$ou,
    pd = meta$period,
    orgs = df_orgs,
    mechs = df_mechs,
    de = data_elements
  )

  df_vout <- df_trans %>%
    validate_output(refs = refs, content = F)

  df_vout <- df_trans %>%
    validate_output(refs = refs, content = T)

  df_vout$status
  df_vout$message
  df_vout$checks
  df_vout$data

  vname = "location"

  df_vout$checks %>%
    cir_reshape_checks(vname = "location") %>%
    mutate(location = as.double(location)) %>%
    dplyr::left_join(
      df_vout$data, .,
      by = c("filename", "sheet", "row_id" = "location")) %>%
    dplyr::filter(is.na(validations)) %>%
    dplyr::select(-validations)

  # Import, Import Validations, Transformations, Content Validation
  df_subm <- subms %>%
    dplyr::first() %>%
    cir_processing(vcontent = F, archive = F)

  df_subm <- subms %>%
    dplyr::first() %>%
    cir_processing(archive = F,
                   vcontent = T,
                   base_url = "https://datim.org")

  df_subm <- subms %>%
    dplyr::first() %>%
    cir_processing(archive = T, vcontent = F)



