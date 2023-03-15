################### Extract participant names out of dataList ##################
extract_participant_names <- function(dataList){
  names <- sapply(dataList, function(df) {
    name <- regex_s(df,"\\bname\\b")
  })
  return(names)
}
################### Extract demographics and test parameters ###################
extract_metadata <- function(dataList){
  metadata <- lapply(dataList, function(df) {
    name <- regex_s(df,"\\bname\\b")
    age <- regex_s(df,"\\bage\\b")
    sex <- regex_s(df,"\\bsex\\b")
    bodyMass <- regex_s(df,"\\bweight\\b",unit="kg")
    device <- regex_s(df,"^(?=.*(exercise))(?=.*(device)).*$")
    barometricPressure <- regex_s(df,"^(?=.*(baro))(?=.*(press)).*$")
    temperature <- regex_s(df,"^(?=.*(insp))(?=.*(temp)).*$")
    relativeHumidity <- regex_s(df,"^(?=.*(insp))(?=.*(humid)).*$")
    startWarmUp <- regex_s(df,"^(?=.*(warm))(?=.*(up)).*$",1)
    ifelse(length(startWarmUp)==0,startWarmUp <- NA,startWarmUp)
    startExercise <- regex_s(df,"^(?=.*(start))(?=.*(exercise)).*$",1)
    ifelse(length(startExercise)==0,startExercise <- NA,startExercise)
    endExercise <- regex_s(df,"^(?=.*(cool))(?=.*(down)).*$",1)
    ifelse(length(endExercise)==0,endExercise <- NA,endExercise)
    
    df <- data.frame(name, age, sex, bodyMass, device, barometricPressure,
                     temperature, relativeHumidity, startWarmUp, startExercise,
                      endExercise)
    df <- df %>% tidytable::select(tidytable::where(~any(!is.na(.))))
  })
return(metadata)
}

################ Extract testdate append to demographicsList ###################
extract_test_date <- function(extractFrom,appendTo){
  testDates <- mapply(df = appendTo, vec = extractFrom, SIMPLIFY = F,
                      FUN = function(df,vec){
    testDate <- regmatches(vec, regexpr("\\d{8}", vec))
    df$testDate <- as.Date(testDate, format = "%Y%m%d")
    df
  })
return(testDates)
}
