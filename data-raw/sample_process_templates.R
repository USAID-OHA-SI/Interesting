
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

# Metadata
subm %>%
  dplyr::first() %>%
  cir_extract_meta()

subm %>%
  dplyr::first() %>%
  check_meta()

subm %>%
  dplyr::first() %>%
  check_tabs()

# Initial Validation
meta <- subm %>%
  dplyr::first() %>%
  validate_initial()

# Import & 2nd round of Validation
df_subm <- subm %>%
  dplyr::first() %>%
  cir_import(template = meta$type)

df_subm$checks
df_subm$data

# Gather
df_cirg <- df_subm$data %>% cir_gather()






