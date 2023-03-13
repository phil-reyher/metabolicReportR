################################ Regex_search ##################################
##function to perform regex search x must be a regular expression as a string,
##which is then searched for within the dataset
##y is optional, input 1 so the value 'left' to the searched cell is extracted,
##instead of the default: 'right'
regex_s <- function (df,x,y=0,unit="kg"){
  
  if (unit != "kg" & unit != "lb") {
    stop("Invalid value for unit. Must be 'kg' or 'lbs'.")
  }
  ##Find column name and row index of demographics with regex
  ##Outputs a list with elemts containing row index and the name of the
  ##elements containing the columnname!
  tmp <- sapply(df, function(col) {
    grep(x,col,perl = T,ignore.case = T)
  })
  ##find column index within list as column name is impractical for the
  ##following steps
  tmp_col <- as.integer(which(tmp != 0))
  
  
  if(all(sapply(tmp, is.null)) == 1){
    return(NA_character_)
    stop()
  }
  ##Find row index within list using column index
  tmp_row <- as.integer(tmp[tmp_col])
  ##special case weight, z specifies unit, default "kg"
  if(grepl("weight", x)){tmp_col <- grep(unit,tolower(df[tmp_row,])) - 1}
  ##Should the value be left of the demographics' name
  else if(y=="1"){tmp_col <- tmp_col - 1}
  ##Should the value be right to the demographics' name (default)
  else{tmp_col <- tmp_col + 1}
  ##extract from dataframe using both indices
  
  return(as.character(df[tmp_row,..tmp_col])
  )}

