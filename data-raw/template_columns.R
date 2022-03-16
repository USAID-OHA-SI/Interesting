#Template Columns

#store column names for long template
template_cols_long <- readxl::read_excel("data-raw/templates/FY22_CIRG_Submission_Long_All_Technical_Areas.xlsx",
                                         sheet = "CIRG", col_types = "text", n_max = 0) %>%
  names()

usethis::use_data(template_cols_long, overwrite = TRUE)

#store column names for semi_wide template
  # template_cols_semiwide <- readxl::read_excel("data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx",
  #                                          sheet = "CIRG", col_types = "text", n_max = 0) %>%
  #   names()

  template_cols_semiwide <- readxl::excel_sheets("data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx") %>%
    #setdiff("meta") %>%
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(.f = ~ readxl::read_excel("data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx", sheet = .x, skip = 2, col_types = "text")) %>%
    names()

  #Wide Key POp

  template_cols_wide_kp <- readxl::excel_sheets("data-raw/templates/FY22 CIRG Submission - Wide - KEY POPULATIONS.xlsx") %>%
    #setdiff("meta") %>%
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(.f = ~ readxl::read_excel("data-raw/templates/FY22 CIRG Submission - Wide - KEY POPULATIONS.xlsx", sheet = .x, skip = 2, col_types = "text")) %>%
    names()

  #-----

  semiwide_sheet <- readxl::excel_sheets("data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx")[2:9]

  semiwide_df <- lapply(semiwide_sheet, readxl::read_excel, path = "data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx", col_types = "text", n_max = 0)

  semiwide_names <- lapply(semiwide_df, names)


  #store meta data columns
  template_cols_meta <- template_cols_long %>% setdiff("val")




  usethis::use_data(template_cols_long, overwrite = TRUE)

