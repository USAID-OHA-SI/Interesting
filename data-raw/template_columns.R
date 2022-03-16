#Template Columns ------------------------------

#store column names for long template
template_cols_long <- readxl::read_excel("data-raw/templates/FY22_CIRG_Submission_Long_All_Technical_Areas.xlsx",
                                         sheet = "CIRG", col_types = "text", n_max = 0) %>%
  names()

usethis::use_data(template_cols_long, overwrite = TRUE)


#store column names for semi_wide template
  template_cols_semiwide <- readxl::excel_sheets("data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx") %>%
    #setdiff("meta") %>%
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(.f = ~ readxl::read_excel("data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx", sheet = .x, skip = 2, col_types = "text")) %>%
    names()

  #Store column names for wide template -------------------

  #Store path

  template_path <- "data-raw/templates/Wide - by Technical Area/"

  template_files <- dir({template_path}, pattern = "*.xlsx")

  #loop through and grab names
  temp_names <- template_files %>%
    purrr::map(~readxl::excel_sheets(file.path(template_path, .))) %>%
    purrr::map(~stringr::str_subset(., "CIRG"))

  #map dfr to get template names for each wide template
  purrr::map2_dfr(.y = glue::glue("{template_path}{template_files}"), .x = temp_names, .f = ~ readxl::read_excel(.y, sheet = .x, skip = 2, col_types = "text")) %>%
    names()



  #KP only
  template_cols_wide_kp <- readxl::excel_sheets("data-raw/templates/FY22 CIRG Submission - Wide - KEY POPULATIONS.xlsx") %>%
    #setdiff("meta") %>%
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(.f = ~ readxl::read_excel("data-raw/templates/FY22 CIRG Submission - Wide - KEY POPULATIONS.xlsx", sheet = .x, skip = 2, col_types = "text")) %>%
    names()

  #-----


  #store meta data columns
  template_cols_meta <- template_cols_long %>% setdiff("val")

  usethis::use_data(template_cols_meta, overwrite = TRUE)

