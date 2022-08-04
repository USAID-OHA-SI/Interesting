
library(tidyverse)
#library(glamr)
library(Interesting)

subm <- fs::dir_ls("~/Downloads", regexp = "CIRG_.*.xlsx$")

subm

subm %>%
  dplyr::first() %>%
  readxl::read_excel(path = ., range = "meta!B1:E2") %>%
  utils::stack() %>%
  dplyr::rename(mvalue = values, mtype = ind) %>%
  dplyr::select(mtype, mvalue)

subm %>%
  dplyr::first() %>%
  cir_extract_meta()

subm %>%
  dplyr::first() %>%
  check_meta()

subm %>%
  dplyr::first() %>%
  check_tabs()

subm %>%
  dplyr::first() %>%
  validate_initial()

subm %>%
  dplyr::first() %>%
  cir_store_meta()

meta <- subm %>%
  dplyr::first() %>%
  check_meta()

df_subm <- subm %>%
  dplyr::first() %>%
  cir_import(template = meta$type)

df_subm$checks


subm %>%
  dplyr::first() %>%
  #Interesting::cir_extract_meta()
  #cir_extract_meta("type")
  cir_extract_meta()

subm %>%
  #dplyr::first() %>%
  dplyr::nth(2) %>%
  walk(cir_processing)
  #Interesting::cir_processing()




