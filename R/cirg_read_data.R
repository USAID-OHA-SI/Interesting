library(tidyverse)
library(readxl)


# location of data: change this
#ci_data_folder <- "<full-or-relative-path-data-folder>"
ci_data_folder <- "C:/Users/nmaina/Documents/Data/CI_data"
#ci_data_folder <- "../../Downloads"

files <- list.files(path = ci_data_folder,
                    pattern = "^CIRG.*.xlsx$",
                    full.names = TRUE)


#' @title Read CIRG Submissions
#'
#' @param path full path of data file
#' @param source Should the file/sheets details be appended? default is FALSE
#'
#' @return data frame
#'
cirg_read <- function(ci_data_folder, source = FALSE) {

  # File base name
  name <- base::basename(ci_data_folder)
  print("data")
  print("Filename:")
  print(name)

  # List of all sheets
  sheets <- ci_data_folder %>% readxl::excel_sheets()

  if (length(sheets) == 0) {
    stop("There are no valid data sheet in this file")
  }

  # Metadata from meta sheet
  meta <- sheets %>% stringr::str_subset("meta")

  if (length(meta) != 1) {
    stop("There is no valid or multiple meta sheet(s) in this file")
  }

  df_meta <- meta %>%
    readxl::read_excel(ci_data_folder, sheet = .) %>%
    dplyr::select(2:3) %>%
    dplyr::filter(row_number() <= 4) %>%
    purrr::set_names(c("meta", "value"))

  # Metadata Notification
  for(r in 1:nrow(df_meta)) {
    print(glue::glue("{df_meta[r,1]}: {df_meta[r,2]}"))
  }

  # Data from cirg-like sheets
  sheets <- sheets %>% stringr::str_subset("CIRG")

  if (length(sheets) == 0) {
    stop("There are no valid data sheets in this file")
  }

  # Data from cirg-like sheets
  # Append file/sheets details
  if (source) {
    df_ci <- sheets %>%
      set_names() %>%
      #map_dfr(.x, .f = ~read_excel(path = path, sheet = .x, skip = 1), .id = "sheet")
      # same as above
      map_dfr(read_excel,
              path = ci_data_folder,
              skip = 1,
              col_types = c(.default = "text"),
              .id = "sheet") %>%
      mutate(filename = name) %>%
      relocate(filename, .before = 1)
  }
  # Only data
  else {
    df_ci <- sheets %>%
      map_dfr(read_excel,
              path = ci_data_folder,
              skip = 1,
              col_types = c(.default = "text"))
  }

  #TODO: Pivot wide and semi-wide templates into long
  if (!"val" %in% names(df_ci)) {
    print("WARNING - This file contains non-tidy data. Consider reshaping ...")
  }

  return(df_ci)
}


# examples - 1 file at the time
files[1] %>% cirg_read()
files[1] %>% cirg_read(source = T)


#' @title Read all CIRG Submissions
#'
#' @param folderpath full or relative path of data folder
#' @param pattern    Regex of name of files to be targetted
#' @param source     Should the file/sheets datails be appended? default is FALSE
#'
#' @return data frame
#'
cirg_read_all <- function(ci_data_folder,
                          pattern = "^CIRG.*.xlsx$",
                          source = FALSE) {

  files <- list.files(path = ci_data_folder,
                      pattern = "^CIRG.*.xlsx$",
                      full.names = TRUE)

  if (length(files) == 0) {
    stop("There are no valid CIRG files")
  }

  print(glue::glue("Files to be processed: {length(files)}"))

  df_all <- files %>%
    map_dfr(cirg_read, source = source)

  return(df_all)
}


# examples - batching
ci_data_folder %>% cirg_read_all()

ci_data_folder %>% cirg_read_all(source = T)



