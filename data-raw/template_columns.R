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

  usethis::use_data(template_cols_semiwide, overwrite = TRUE)

 #Store column names for wide template -------------------

  template_path <- "data-raw/templates/Wide - by Technical Area/"

  files <- dir({template_path}, pattern = "*.xlsx", full.names = TRUE)

  template_wide_techareas <- files %>%
    basename() %>%
    stringr::str_extract("(?<=Wide -).*(?=.xlsx)") %>%
    stringr::str_trim(side = "both")

  df_tabs <- map_dfr(files,
                     ~ tibble(filename = .x,
                              tabname = map(filename, readxl::excel_sheets)) %>%
                       flatten()) %>%
    filter(tabname != "meta")

  #type of template
  tab_type <- purrr::map(
    .x = files,
    ~tail(strsplit(.x, split=" ")[[1]], 1)) %>%
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

  template_wide_dreams <- lst2[[1]]
  template_wide_gender <- lst2[[2]]
  template_wide_kp <- lst2[[3]]
  template_wide_lab <- lst2[[4]]
  template_wide_ovc <- lst2[[5]]
  template_wide_prep <- lst2[[6]]
  template_wide_sch <- lst2[[7]]
  template_wide_vmmc <- lst2[[8]]

  template_cols_wide <- c(
    template_wide_dreams,
    template_wide_gender,
    template_wide_kp,
    template_wide_lab,
    template_wide_ovc,
    template_wide_prep,
    template_wide_sch,
    template_wide_vmmc
  ) %>%
    unique()

  template_cols_wgroups <- list(
    "dreams" = template_wide_dreams,
    "gender" = template_wide_gender,
    "kp" = template_wide_kp,
    "lab" = template_wide_lab,
    "ovc" = template_wide_ovc,
    "prep" = template_wide_prep,
    "sch" = template_wide_sch,
    "vmmc" = template_wide_vmmc
  )

  usethis::use_data(template_cols_wide, overwrite = TRUE)

  usethis::use_data(template_wide_techareas, overwrite = TRUE)

  usethis::use_data(template_cols_wgroups, overwrite = TRUE)

  usethis::use_data(template_wide_dreams, overwrite = TRUE)
  usethis::use_data(template_wide_gender, overwrite = TRUE)
  usethis::use_data(template_wide_kp, overwrite = TRUE)
  usethis::use_data(template_wide_lab, overwrite = TRUE)
  usethis::use_data(template_wide_ovc, overwrite = TRUE)
  usethis::use_data(template_wide_prep, overwrite = TRUE)
  usethis::use_data(template_wide_sch, overwrite = TRUE)
  usethis::use_data(template_wide_vmmc, overwrite = TRUE)


  #-----


  #store meta data columns
  template_cols_value <- "val"
  template_cols_ind <- "indicator"
  template_cols_disaggs <- c("sex", "age", "population", "otherdisaggregate", "numdenom")

  template_cols_meta <- template_cols_long %>% setdiff(template_cols_value)

  template_cols_core <- template_cols_long %>%
    setdiff(c(template_cols_ind, template_cols_disaggs, template_cols_value))

  usethis::use_data(template_cols_value, overwrite = TRUE)
  usethis::use_data(template_cols_ind, overwrite = TRUE)
  usethis::use_data(template_cols_disaggs, overwrite = TRUE)
  usethis::use_data(template_cols_meta, overwrite = TRUE)
  usethis::use_data(template_cols_core, overwrite = TRUE)

