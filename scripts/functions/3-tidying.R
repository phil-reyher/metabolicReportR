################################ Clean dataset #################################
#rename columns to appropriate names.
#remove first and last rows
tidy_up <- function(dataList){
  dataList <- lapply(dataList, function(df) {
    ##find indices of string "time", this is where the spiro data starts
    tmp <- which(sapply(df, function(x) {grepl("TIME",fixed = T,x)}) )-1
    ##finally remove the mess from the top
    df <- slice(df,-(1:tmp) )
    ##Column names
    ##combine the two header rows
    columnNames <- paste(df[1,],df[2,],sep = "")
    ## clean up NAs
    columnNames <- gsub("NA","",columnNames)
    ##remove whitespace
    columnNames <- gsub(" ","",columnNames)
    ##standardize colnames
    columnNames <- sub("^(?=.*(VO2))(?=.*(kg)).*$","vo2Rel",columnNames,
                       perl = T,ignore.case = T)
    columnNames <- sub("VO2STPD","vo2Abs",columnNames,perl = T,ignore.case = T)
    columnNames <- sub(".*work.*","work",columnNames,perl = T,ignore.case = T)
    columnNames <- sub(".*hr.*","heartrate",columnNames,perl = T,
                       ignore.case = T)
    columnNames <- tolower(gsub("STPD","",columnNames))
    colnames(df) <- columnNames
    ##remove anything but data
    df <- slice(df,-(1:4) )
    ##find first and last instance of NA to remove mess at the bottom
    naIndex <- which(is.na(df[,1]))
    firstNa <- min(naIndex)
    l <- nrow(df)
    df <- slice(df,-(firstNa:l) )
    ##convert time from m:s format to s
    df$time <- mmss_to_ss(df$time)
    ##convert all to numeric, to character first to preserve factors
    df <-df %>% mutate(across(.cols = everything(), ~ as.character(.x) %>% 
                                as.numeric(.x) ) )
    ##rename problematic column names
    df <- df %>% rename('vevo2'=`ve/vo2`,'vevco2'=`ve/vco2`)
    
    df
  })
return(dataList)
}
############################# extend metadata ##################################
#add indices of exercise beginning and end do demographicList
extract_start_end_indices <- function(extractFrom,appendTo){
  metadata <- mapply(df=extractFrom, meta=appendTo, SIMPLIFY = F,
    FUN= function(df,meta){
    startExercise <- as.numeric(meta$startExercise)*60
    endExercise <- as.numeric(meta$endExercise)*60
     beg <- which.max(df$time >= startExercise)
    end <- which.max(df$time >= endExercise)
    meta$startExerciseIndex <- beg
    meta$endExerciseIndex <- end
    meta
  })
return(metadata)
}
