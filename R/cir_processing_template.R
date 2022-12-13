#' Process CIR Submissions
#'
#' @param filepath Full filename
#'
#' @export
#' @return CIR Submission as Tibble

cir_processing <- function(filepath) {

  # TODO - run template column storing from Interesting/data-raw/template_columns.R

  # TODO - Setup processing folder structure
  # Note - This folder should be excluded from commit

  # TODO - Identify & download submissions

  # Initial validation checks
  vinit <- validate_initial(filepath)

  # Save metadata output to file
  cir_output(.df_out = vinit,
             .subm = filepath,
             .name = "metadata",
             type = "metadata")

  # Notification
  if (interactive()) {
    cat("\n---- STATUS ----",
        "\nIs submission valid? ", paint_iftrue(vinit$subm_valid),
        "\n")
  }

  # STOP HERE IS SUBMISSION IS NOT VALID
  if (!vinit$subm_valid)
    return(vinit$subm_valid)

  #store meta df - come back to google ID
  # TODO - There are inconsistencies in the output. See `cir_extract_meta`
  # Alternative - Use `validate_initial()` result and join by `ou`
  #meta_df <- cir_store_meta(filepath)

  #import template sheet(s) - this is being being built locally right now (can we do this from drive and use the filename to identify)
  #in order for this to work, we need the naming conventions to be consistent
  # TODO - Pass valid cirg sheets to cir_import. No need to use `excel_sheets()`
  df_cirg <- cir_import(filepath, template = vinit$type)

  # TODO - Store sheet level validations

  df_vimp <- df_cirg$checks

  # Save vimp output to file
  cir_output(.df_out = df_cirg$checks,
             .subm = filepath,
             .name = "import validations",
             type = "validations")

  # Save data output to file
  cir_output(.df_out = df_cirg$data,
             .subm = filepath,
             .name = "processed",
             type = "processed")

  # TODO - Store sheet level validation
  df_vimp <- df_cirg$checks

  #validation checks - VMMC does not work on this one because of the names issue - come back to that!
  # TODO - This validation needs to be done at the tab level given the differences in wide templates
  # ACTION - Do the validation within the import
  # ACTION - Do the validation against the specified template type
  # ACTION - What's the fall back for when data structure does meet the template?

  #vimp <- validate_import(df_data)


  #remove any extra columns
  # TODO - Same as above. This needs to be moved in `cir_import()`
  #df_cirg <- cir_restrict_cols(df_cirg)

  #join to reference table - need to update with the new reference table from Nashiva
  # df_cirg <- cir_wide_refjoin(df_cirg)

  #reshape wide to match long df (only affects wide format)
  df_cirg <- cir_gather(df_cirg$data)

  #Munge string
  df_cirg <- cir_munge_string(df_cirg)



  # Save transformed data output to file
  cir_output(.df_out = df_cirg,
             .subm = filepath,
             .name = "transformed",
             type = "transformed")

  # NOTE - See notes in meta_df section
  #df_cirg <- cir_join_meta(df_cirg, df_meta = meta_df)
  df_cirg <- df_cirg %>%
    dplyr::left_join(vinit, by = c("operatingunit" = "ou"))

  # Validate output
  # TODO - Return data along with output validations list(checks = vout, data = df_cirg)
  df_cirg <- validate_output(df_cirg)

  #df_vout <- df_cirg$checks

  # Save transformed data output to file
  # cir_output(.df_out = df_cirg$checks,
  #            .subm = filepath,
  #            .name = "output validations",
  #            type = "validations")

  # TODO - Move file from raw to archive folder when done
  # cir_archive(filepath)

  return(vinit)

  # Save outputs
  # TODO - Save processing logs & data: vinit, vimp, vout and df_cirg
  #

  return(df_cirg)
}


