# Libraries

  library(tidyverse)
  library(glamr)
  library(Interesting)
  library(googledrive)
  library(googlesheets4)


# SETUP

  # Processing folder
  proc_folder <- "cirg-submissions"

  # Processing Date
  #proc_date <- glamr::curr_date()
  proc_date <- "2022-08-15"

  cir_setup(folder = proc_folder, dt = proc_date)

# GOOGLE DRIVE - TODO Function to pull latest/resubmisions

  cir_subm_sheet <- "1amabNYu1HF9rZ1Y-Hy5p8lQ73Ynll744HZxPyZuVEgI"

  cir_subm_id <- as_id(cir_subm_sheet)

  googledrive::drive_get(cir_subm_id)

  #googlesheets4::gs4_browse(cir_subm_id)
  #googledrive::drive_browse(cir_subm_id)

  df_cir_subm <- googlesheets4::read_sheet(cir_subm_id)

# CIR Submissions

  df_cir_subm <- df_cir_subm %>%
    janitor::clean_names() %>% #glimpse()
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

  df_cir_subm %>%
    count(subm_id) %>%
    filter(n > 1)

# CIR Submission files

  df_cir_subm %>% distinct(subm_period)

  df_cir_files <- df_cir_subm %>%
    filter(subm_period == "FY22 Q4") %>%
    select(subm_id, subm_file = subm_files) %>%
    separate_rows(subm_file, sep = ",\\s") %>%
    mutate(subm_file_id = str_extract(subm_file, "(?<=\\?id\\=).*"),
           subm_filename = pull(drive_get(as_id(subm_file_id)), name),
           subm_file_valid = str_detect(subm_filename, ".xlsx$")) %>%
    relocate(subm_file_id, .after = subm_id)

# DOWNLOAD Submissions

  dir_raw <- cir_folder(type = "raw", dt = proc_date)

  df_cir_files %>%
    filter(subm_file_valid = TRUE) %>%
    select(subm_file_id, subm_filename) %>%
    #filter(row_number() == 1) %>%
    #pull(subm_file_id) %>% first() %>%
    pwalk(~drive_download(file = as_id(.x),
                          path = file.path(dir_raw, .y),
                          overwrite = T))

# LOCAL FILES

  #subms <- fs::dir_ls("~/Downloads", regexp = "CIRG_.*.xlsx$")
  subms <- fs::dir_ls(dir_raw, regexp = "CIRG_.*.xlsx$")

  subms

  # Metadata
  # subms %>%
  #   dplyr::first() %>%
  #   cir_extract_meta()
  #
  # subms %>%
  #   dplyr::first() %>%
  #   check_meta()
  #
  # subms %>%
  #   dplyr::first() %>%
  #   check_tabs()

  # Initial Validation
  meta <- subms %>%
    dplyr::first() %>%
    validate_initial()

  # metas <- subms %>%
  #   map_dfr(validate_initial)

  # Import & 2nd round of Validation
  df_subm <- subms %>%
    dplyr::first() %>%
    cir_import(template = meta$type)

  df_subm$checks
  df_subm$data

  # Import all
  df_imp_checks <- metas %>%
    filter(subm_valid == TRUE) %>%
    select(filename, type) %>%
    pmap_dfr(function(filename, type) {
      subm <- cir_import(filepath = file.path(dir_raw, filename), template = type)
      return(subm$checks)
    })

  df_imp_data <- metas %>%
    filter(subm_valid == TRUE) %>%
    select(filename, type) %>%
    pmap_dfr(function(filename, type) {
      subm <- cir_import(filepath = file.path(dir_raw, filename), template = type)
      return(subm$data)
    })

  # Transformation

  #Use template Metadata as lookup table
  tmp_meta <- template_metadata %>%
    select(-tech_area, -ends_with("_header")) %>%
    mutate(indicator_code = case_when(
      str_detect(indicator_code, "pep|sexual violence") ~
        str_remove_all(indicator_code, " "),
      str_detect(indicator_code, "physical and") ~
        str_replace_all(indicator_code, " | and\\/or ", "_"),
      TRUE ~ indicator_code
    )) %>% distinct()

  df_subm$data %>%
    cir_gather() %>%
    left_join(tmp_meta, by = "indicator_code")

  df_trans <- df_subm$data %>%
    cir_gather() %>%
    cir_munge_string()

  df_trans <- df_subm$data %>%
    cir_reshape()

  # Template DataElement Pattern
  # <indicator>.<age>.<sex>.<otherdisaggs>.<population>.<numdenom>
  df_trans %>% distinct(indicator)
  df_trans %>% distinct(sex)
  df_trans %>% distinct(ageasentered)
  df_trans %>% distinct(otherdisaggregate)
  df_trans %>% distinct(otherdisaggregate_sub)


  # Validate outputs -- Missing

  df_trans %>% glimpse()

  df_trans %>% validate_output()

  df_trans %>% get_missing("operatingunit")
  df_trans %>% get_missing("reportingperiod")
  df_trans %>% match_value("reportingperiod")

  curr_dt <- glamr::curr_date()

  pepfar_data_calendar %>%
    mutate(pd = paste0("FY", str_sub(fiscal_year, 3,4),
                       "Q", quarter)) %>%
    filter(entry_close <= curr_dt) %>%
    pull(pd) %>%
    has_element("FY22Q4")


  df_trans %>% get_missing("orgunituid")

  df_trans %>% get_missing("mech_code")

  df_trans %>% get_missing("indicator")

  df_trans %>% get_missing("numeratordenom")


  # Validate outputs -- wrong values

  df_trans %>%
    check_operatingunit(ou = meta$ou)

  datim_sqlviews(username = glamr::datim_user(),
                 password = glamr::datim_pwd())

  df_orgs <- meta$ou %>%
    purrr::map_dfr(~datim_orgunits(
      username = glamr::datim_user(),
      password = glamr::datim_pwd(),
      cntry = .x,
      base_url = "https://datim.org"))

  "Ghana" %>%
    purrr::map_dfr(~datim_orgunits(
      username = glamr::datim_user(),
      password = glamr::datim_pwd(),
      cntry = .x,
      base_url = "https://datim.org"))

  df_orgs %>% glimpse()

  df_trans %>% glimpse()

  df_trans %>%
    check_orgunituids(ref_orgs = df_orgs)

  df_mechs <- meta$ou %>%
    purrr::map_dfr(~datim_mechs(
      username = glamr::datim_user(),
      password = glamr::datim_pwd(),
      cntry = .x,
      base_url = "https://datim.org"))

  df_mechs %>% glimpse()

  df_trans %>%
    check_mechs(ref_mechs = df_mechs)






  # Import, Import Validations, Transformations, Content Validation
  df_subm <- subm %>%
    dplyr::first() %>%
    cir_processing()



