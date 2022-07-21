cir_processing <- function(filepath) {

  #run template column storing from Interesting/data-raw/template_columns.R

  #validation checks
  validate_initial(filepath)

  #store meta df - come back to google ID
  meta_df <- cir_store_meta(filepath)

  #import template sheet(s) - this is being being built locally right now (can we do this from drive and use the filename to identify)
  #in order for this to work, we need the naming conventions to be consistent
  df <- cir_import(filepath)

  #validation checks - VMMC doesnt work on this one because of the names issue - come back to that!
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


  df <- cir_join_meta(df)


  validate_output(df)

  return(df)

}


