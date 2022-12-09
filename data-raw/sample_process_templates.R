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
  googledrive::drive_browse(cir_subm_id)

  df_cir_subm <- googlesheets4::read_sheet(cir_subm_id)

  df_cir_subm %>% glimpse()

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

  metas <- subms %>%
    map_dfr(validate_initial)

  subms %>%
    dplyr::first() %>%
    readxl::read_excel(.,
                       sheet = 2,
                       skip = 2,
                       col_types = "text") %>%
    #cir_template_cols(template = "Wide")
    cir_template_ta()

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

  # Import, Validations & Transformations
  df_subm <- subm %>%
    dplyr::first() %>%
    cir_processing()







