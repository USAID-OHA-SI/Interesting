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

  if (!dir.exists(file.path(proc_folder, paste0("CIRG-", proc_date))))
    cir_setup(folder = proc_folder, dt = proc_date)

  dir_raw <- cir_folder(type = "raw", dt = proc_date)

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

  df_cir_files %>%
    filter(subm_file_valid = TRUE) %>%
    select(subm_file_id, subm_filename) %>%
    pwalk(~drive_download(file = as_id(.x),
                          path = file.path(dir_raw, .y),
                          overwrite = T))

# LOCAL FILES ----

  #subms <- fs::dir_ls("~/Downloads", regexp = "CIRG_.*.xlsx$")
  subms <- fs::dir_ls(dir_raw, regexp = "CIRG_.*.xlsx$")

  subms

  # Worksheets Visibility

  subms %>%
    dplyr::first() %>%
    cir_vsheets()

  #subms %>% map_dfr(cir_vsheets)

  # Metadata

  # Initial Validation

  meta <- subms %>%
    dplyr::first() %>%
    validate_initial()

  metas <- subms %>% map_dfr(validate_initial)

  # Import & 2nd round of Validation
  df_subm <- subms %>%
    dplyr::first() %>%
    cir_import(template = meta$type)

  df_subm$checks %>% glimpse
  df_subm$data %>% glimpse

  # Import all
  # df_imp_checks <- NULL
  #
  # df_imp_data <- metas %>%
  #   filter(subm_valid == TRUE) %>%
  #   select(filename, type) %>%
  #   pmap_dfr(function(filename, type) {
  #     subm <- cir_import(filepath = file.path(dir_raw, filename), template = type)
  #
  #     df_imp_checks <<- bind_rows(subm$checks)
  #
  #     return(subm$data)
  #   })

  # Transformation

  # df_trans <- df_subm$data %>%
  #   cir_gather() %>%
  #   cir_munge_string()

  df_trans <- df_subm$data %>%
    cir_reshape()

  df_trans %>% glimpse()

  # Template DataElement Pattern
  # <indicator>.<age>.<sex>.<otherdisaggs>.<population>.<numdenom>


  # Validate outputs -- Missing

  df_trans %>% get_missing("operatingunit")
  df_trans %>% get_missing("reportingperiod")
  df_trans %>% match_value("reportingperiod", "FY\\d{2}Q\\d{1}")

  curr_dt <- glamr::curr_date()

  glamr::pepfar_data_calendar %>%
    mutate(pd = paste0("FY", str_sub(fiscal_year, 3,4), "Q", quarter)) %>%
    filter(entry_close <= curr_dt) %>%
    distinct(pd) %>%
    pull(pd) %>%
    has_element("FY22Q4")

  df_trans %>% get_missing("orgunituid")
  df_trans %>% get_missing("mech_code")
  df_trans %>% get_missing("indicator")
  df_trans %>% get_missing("numeratordenom")


  # Validate outputs -- wrong values

  df_orgs <- meta$ou %>%
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

  df_trans %>%
    check_operatingunit(ou = meta$ou)

  df_trans %>%
    check_orgunituids(ref_orgs = df_orgs)

  df_trans %>%
    check_mechs(ref_mechs = df_mechs)

  df_trans %>%
    check_inds(ref_de = data_elements)

  df_trans %>%
    check_numdenom()

  df_trans %>%
    check_disaggs(ref_de = data_elements)

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

  # Import, Import Validations, Transformations, Content Validation
  df_subm <- subms %>%
    dplyr::first() %>%
    cir_processing()



