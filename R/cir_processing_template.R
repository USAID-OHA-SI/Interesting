#validation checks
validate_initial(filepath)

#import template sheet(s) - this is being being built locally right now (can we do this from drive and use the filename to identify)
  #in order for this to work, we need the naming conventions to be consistent
df <- cir_import(filepath)

#validation checks - VMMC doesnt work on this one because of the names issue - come back to that!
validate_import(df)

#remove any extra columns
df <- cir_restrict_cols(df)

#reshape wide to match long df (only affects wide format)
df <- cir_gather(df)
str(df)
