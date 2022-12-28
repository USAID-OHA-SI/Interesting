#' Process CIR Submissions
#'
#' @param filepath   Full path of filename
#' @param archive    Should the file be archived if successfully processed? Default is false
#' @param vcontent   Should the content of submission be validated?
#' @param datim_user Datim username
#' @param datim_pass Datim password
#' @param base_url   Datim API Base URL
#'
#' @export
#' @return CIR Submission as Tibble

cir_processing <- function(filepath,
                           archive = FALSE,
                           vcontent = FALSE,
                           datim_user = NULL,
                           datim_pass = NULL,
                           base_url = NULL) {

  # TODO - run template column storing from Interesting/data-raw/template_columns.R

  # TODO - Setup processing folder structure
  # Note - This folder should be excluded from commit

  # Get user creds for content validation
  if (vcontent & (is.null(datim_user) | is.null(datim_pass))) {
    user <- glamr::datim_user()
    pass <- glamr::datim_pwd()
  }

  # Initial validation checks
  vinit <- validate_initial(filepath)

  # Save metadata output to file
  cir_output(.df_out = vinit,
             .subm = filepath,
             .name = "metadata",
             type = "metadata")

  # STOP HERE IF SUBMISSION IS NOT VALID
  if (!vinit$subm_valid) return(vinit$subm_valid)

  #import template sheet(s) - this is being being built locally right now (can we do this from drive and use the filename to identify)
  #in order for this to work, we need the naming conventions to be consistent
  #validation checks - VMMC does not work on this one because of the names issue - come back to that!
  # TODO - Pass valid cirg sheets to cir_import. No need to use `excel_sheets()`
  df_cirg <- cir_import(filepath, template = vinit$type)

  # TODO - Store sheet level validations

  # Save vimp output to file
  cir_output(.df_out = df_cirg$checks,
             .subm = filepath,
             .name = "import validations",
             type = "validations")

  # Save imported data to file
  cir_output(.df_out = df_cirg$data,
             .subm = filepath,
             .name = "processed",
             type = "processed")

  # Transformed processed data
  df_cirg <- cir_reshape(df_cirg$data)

  # Save transformed data output to file
  cir_output(.df_out = df_cirg,
             .subm = filepath,
             .name = "transformed",
             type = "transformed")

  # Validate output
  # TODO - Return data along with output validations list(checks = vout, data = df_cirg)
  refs <- list(
    ou = vinit$ou,
    pd = vinit$period
  )

  # Get OU's Orgs & Mechs reference datasets for validate output
  # TODO: Try to leverage the local storage for speed - extract once, store and re-use if needed
  if (vcontent) {

    refs$de <- data_elements

    refs$orgs <- datim_orgunits(username = user,
                                password = pass,
                                cntry = vinit$ou,
                                base_url = base_url)

    refs$mechs <- datim_mechs(username = user,
                              password = pass,
                              cntry = vinit$ou,
                              base_url = base_url)
  }

  # Validate output dataset
  df_cirg <- validate_output(df_cirg, refs, content = vcontent)

  #usethis::ui_info("Output validations - {df_cirg$status} - {df_cirg$message}")

  #Save output validations to file
  cir_output(.df_out = df_cirg$checks,
             .subm = filepath,
             .name = "output validations",
             type = "validations")

  #Save cleaned output data to file
  df_cirg$checks <- df_cirg$checks %>%
    tidyr::pivot_longer(cols = !c(filename, sheet),
                        names_to = "validations",
                        values_to = "location") %>%
    dplyr::filter(!is.na(location) & location != "")

  if (nrow(df_cirg$checks) > 0) {
    df_cirg$data <- df_cirg$checks %>%
      tidyr::separate_rows(location, sep = ", ") %>%
      dplyr::filter(!is.na(location)) %>%
      dplyr::mutate(location = as.double(location)) %>%
      dplyr::left_join(
        df_cirg$data, .,
        by = c("filename", "sheet", "row_id" = "location")) %>%
      dplyr::filter(is.na(validations)) %>%
      dplyr::select(-validations)
  }

  #Save cleaned output data to file
  cir_output(.df_out = df_cirg$data,
             .subm = filepath,
             .name = "cleaned",
             type = "cleaned")

  # Move file from raw to archive folder when done
  if(archive) {
    cir_archive(filepath)
  }

  # return cleaned data only
  return(df_cirg$data)
}


