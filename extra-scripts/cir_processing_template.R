#validation checks
validate_initial(filepath)

#import template sheet(s) - this is being being built locally right now (can we do this from drive and use the filename to identify)
  #in order for this to work, we need the naming conventions to be consistent
df <- cir_import(filepath)

#validation checks
validate_import(df)

#remove any extra columns
df <- cir_restrict_cols(df)
