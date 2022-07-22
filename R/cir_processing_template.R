#' Process CIR Submissions
#'
#' @param filepath Full filename
#'
#' @export
#' @return CIR Submission as Tibble

cir_processing <- function(filepath) {

  #run template column storing from Interesting/data-raw/template_columns.R

  #validation checks
  # TODO - Validate Initial should return something to help with next steps

  validate_initial(filepath)

  #store meta df - come back to google ID
  meta_df <- cir_store_meta(filepath)

  # TODO - Validate Meta before proceeding
  # Eg: check ou/country & reporting period
  if (is.na(meta_df$ou_meta) | is.na(meta_df$period_meta)) {
    cat(crayon::red("\n---- METADATA ERRORS ----"),
        "\nMissing OU/Country name and/or Reporting period",
        "\nCountry:", meta_df$ou_meta,
        "\nReporting period:", meta_df$period_meta,
        "\n"
    )

    return(tibble::tibble())
  }

  #import template sheet(s) - this is being being built locally right now (can we do this from drive and use the filename to identify)
  #in order for this to work, we need the naming conventions to be consistent
  df <- cir_import(filepath)

  #validation checks - VMMC does not work on this one because of the names issue - come back to that!
  validate_import(df)

  #remove any extra columns
  df <- cir_restrict_cols(df)

  #join to reference table - need to update with the new reference table from Nashiva
       # df <-  cir_wide_refjoin(df)

  #reshape wide to match long df (only affects wide format)
  df <- cir_gather(df)
  #str(df)


  #Munge string
  df <- cir_munge_string(df)


  df <- cir_join_meta(df, df_meta = meta_df)


  validate_output(df)

  return(df)
}


