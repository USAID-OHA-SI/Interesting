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

  template_path <- "data-raw/templates/Wide - by Technical Area/"

  files <- dir({template_path}, pattern = "*.xlsx", full.names = TRUE)

  df_tabs <- map_dfr(files,
                     ~ tibble(filename = .x,
                              tabname = map(filename, readxl::excel_sheets)) %>%
                       flatten()) %>%
    filter(tabname != "meta")

  #type of template
  tab_type <- purrr::map(.x = files, ~tail(strsplit(.x,split=" ")[[1]],1)) %>%
    unlist()

  tab_type <- tab_type %>% stringr::str_extract("[^.]+")


  #loop

  lst <- as.list(rep(NA, length(tab_type)))
  lst2 <- as.list(rep(NA, length(tab_type)))

  for (i in 1:length(tab_type)) {
    lst[[i]] <- df_tabs %>%
      filter(str_detect(filename, tab_type[i]))

    lst2[[i]] <-  lst[[i]] %>%
      pmap_dfr(.f = ~readxl::read_excel(..1, ..2)) %>%
      names()

  }

#can I use glue?

 # glue("temp_wide_{tab_type[2]}") <- lst2[[2]]

 temp_wide_dreams <- lst2[[1]]
 temp_wide_gender <- lst2[[2]]
 temp_wide_kp <- lst2[[3]]
 temp_wide_lab <- lst2[[4]]
 temp_wide_ovc <- lst2[[5]]
 temp_wide_prep <- lst2[[6]]
 temp_wide_sch <- lst2[[7]]
 temp_wide_vmmc <- lst2[[8]]


  #-----


  #store meta data columns
  template_cols_meta <- template_cols_long %>% setdiff("val")

  usethis::use_data(template_cols_meta, overwrite = TRUE)

