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

########################### MM:SS to seconds converter #########################
mmss_to_ss <- function(time){
  time_components <- strsplit(time, "[:]")
  time_components <- lapply(time_components, as.numeric)
  TIME_S <- sapply(time_components, function(x) {
    (x[1] * 60 + x[2])
    })
return(TIME_S)
}
######################### Matlabs Changepoints function ########################
findchangepts_std <- function(x) {
  m <- nrow(x)
  n <- ncol(x)
  max_log_likelihood <- -Inf
  change_point <- 0
  for (i in 3:(n-2)) {
    log_likelihood <- 0
    for (j in 1:m) {
      region1 <- x[j, 1:(i-1)]
      region2 <- x[j, i:n]
      std1 <- sd(region1)
      std2 <- sd(region2)
      mean1 <- mean(region1)
      mean2 <- mean(region2)
      log_likelihood1 <- sum(dnorm(region1, mean = mean1, sd = std1,
                                   log = TRUE))
      log_likelihood2 <- sum(dnorm(region2, mean = mean2, sd = std2,
                                   log = TRUE))
      log_likelihood <- log_likelihood + log_likelihood1 + log_likelihood2
    }
    if (log_likelihood > max_log_likelihood) {
      max_log_likelihood <- log_likelihood
      change_point <- i
    }
  }
  return(change_point)
}


########################### Predict work, linear model #########################
predict_work <- function(df,VO2_VAL){
  df <- df %>% slice(1:which.max(df$WORK))
  model <- lm(WORK ~ VO2_ABS_LOW,data = df)
  new_observations <- data.frame(VO2_ABS_LOW=VO2_VAL)
  predicted_vals <- predict(model,newdata = new_observations)
  return(predicted_vals)
}