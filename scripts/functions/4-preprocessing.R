############################# Apply Lowpassfilter ##############################
low_pass_filter <- function(dataList){
  filteredData <- lapply(dataList,function(df) {
    bf <- butter(3, 0.04, type= 'low')
    df$vo2absLow <- signal::filtfilt(bf, df$vo2abs)
    df$vo2relLow <- signal::filtfilt(bf, df$vo2rel)
    
    df$vco2Low <- signal::filtfilt(bf, df$vco2)
    
    df$veLow <- signal::filtfilt(bf, df$ve)
    
    df$vevo2Low <- signal::filtfilt(bf, df$vevo2)
    
    df$vevco2Low <- signal::filtfilt(bf, df$vevco2)
    df
  })
return(filteredData)
}

######################## Compute Ventilatory Variables #########################
compute_ventilatory_vars <- function(dataList){
  dataList <- lapply(dataList, function(df){
    df$exco2 <- ( ( (df$vco2*df$vco2)/df$vo2abs) - df$vco2)
    df$exve <- ( ( (df$ve*df$ve)/df$vco2) - df$ve)
    #no forgetti removi!!!
    df$heartrate <- 100
    ###################
    df$vo2maxPercentage <- df$vo2relLow/max(df$vo2relLow)
    df$hrmax <- df$heartrate/max(df$heartrate)
    df
  })
return(dataList)
}

################################ Truncate Data #################################
truncate_data <- function(dataList,metadata){
  testDataTruncated <- mapply(df=dataList, meta=metadata, SIMPLIFY = F,
    FUN= function(df,meta){
      df <- df%>% slice(meta$startExerciseIndex:meta$endExerciseIndex)
      df
    })
return(testDataTruncated)
}

############################ Interpolate to Seconds ############################
interpolate_to_seconds <- function(dataList){
  testDataInterpolated <- lapply(dataList, function(df){
    interpolate <- function(df) {
      ## first make sure data only contains numeric columns
      dataNum <- df %>%
        select(where(is.numeric))
      
      out <- lapply(dataNum, \(i) {
        approx(
        x = dataNum[[1]],
        y = i,
        xout = seq(min(dataNum[[1]]), max(dataNum[[1]], na.rm = TRUE), 1)
      )$y
      }) %>%
        as.data.frame()
      out
    }
    out <- interpolate(df)
    out
  })
return(testDataInterpolated)
}

################################ Bin Averaging #################################
apply_bin_average <- function(dataList,bin){
  testDataBinned <- lapply(dataList, function(df,...){
    dataNum <- df %>%
      select(where(is.numeric))
    out <- dataNum %>%
      mutate(across(1,\(x) round(x / bin) * bin)) %>% 
      group_by(1) %>%
      summarise(across(everything(),mean, na.rm = TRUE) )
    out
  })
return(testDataBinned)
}
