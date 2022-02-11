#Template Columns

#store column names for long template
template_cols_long <- readxl::read_excel("data-raw/templates/FY22_CIRG_Submission_Long_All_Technical_Areas.xlsx",
                                         sheet = "CIRG", col_types = "text", n_max = 0) %>%
  names()

usethis::use_data(template_cols_long, overwrite = TRUE)

#store column names for semi_wide template
  # template_cols_long <- readxl::read_excel("data-raw/templates/FY22_CIRG_Submission_Long_All_Technical_Areas.xlsx",
  #                                          sheet = "CIRG", col_types = "text", n_max = 0) %>%
  #   names()
  #
  #
  # semiwide_sheet <- readxl::excel_sheets("data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx")[2:9]
  #
  # semiwide_df <- lapply(semiwide_sheet, readxl::read_excel, path = "data-raw/templates/FY22_CIRG_Submission_Semi_Wide_ All_Technical_Areas.xlsx", col_types = "text", n_max = 0)
  #
  # lapply(semiwide_df, names)
  #
  # usethis::use_data(template_cols_long, overwrite = TRUE)

