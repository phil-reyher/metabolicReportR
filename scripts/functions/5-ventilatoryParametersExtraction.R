##################### Extract ventilatory thresholds data ######################
find_ventilatory_thresholds_data <- function(dataList){
  thresholdsTables <- lapply(dataList, function(df){
    ##truncate dataframe to ranges in which VT1/VT2 can occur (NASA Paper,2021)
    vt1IndexStart <- which.min( abs(df$time-quantile(df$time,0.3)))
    vt1IndexEnd <- which.min( abs(df$time-quantile(df$time,0.8)))
    vt2IndexStart <- which.min( abs(df$time-quantile(df$time,0.5)))
    
    dfVt1 <- df %>% slice(vt1IndexStart:vt1IndexEnd)
    dfVt2 <- df %>% slice_tail(n=vt2IndexStart+1)
    ##breakpointanalyses
    ##VT1##
    ##V-Slope##
    vSlope <- dfVt1 %>% select(vo2abs,vco2) %>% as.matrix(.) %>% t(.)
    vSlopeIndex <- findchangepts_std(vSlope)+vt1IndexStart-1
    #EXCO2##
    excessCo2 <- dfVt1 %>% select(exco2) %>% as.matrix(.) %>% t(.)
    excessCo2Index <- findchangepts_std(excessCo2)+vt1IndexStart-1
    ##VT2##
    ##V-Slope##
    vSlope2 <- dfVt2 %>% select(vco2,ve) %>% as.matrix(.) %>% t(.)
    vSlope2Index <- findchangepts_std(vSlope2)+vt2IndexStart-1
    ##EXVE##
    excessVentilation <- dfVt2 %>% select(exve) %>% as.matrix(.) %>% t(.)
    excessVentIndex <- findchangepts_std(excessVentilation)+
                              vt2IndexStart-1
    ##combine
    vt1_indexVslope <- vSlopeIndex
    vt1_indexExCo <- excessCo2Index
    vt2_indexVslope <- vSlope2Index
    vt2_indexExVent <- excessVentIndex
    vt1_index <- round((vSlopeIndex+excessCo2Index)/2)
    vt2_index <- round((vSlope2Index+excessVentIndex)/2)
    #´_´ seperator for pivot longer later
    vt1_time <- df$time[vt1_index]
    vt2_time <- df$time[vt2_index]
    vt1_vo2abs <- df$vo2absLow[vt1_index]
    vt2_vo2abs <- df$vo2absLow[vt2_index]
    vt1_vo2rel <- df$vo2relLow[vt1_index]
    vt2_vo2rel <- df$vo2relLow[vt2_index]
    
    vt1_work <- predict_work(df,vt1_vo2abs)
    vt2_work <- predict_work(df,vt2_vo2abs)
    
    vt1_heartrate <- df$heartrate[vt1_index]
    vt2_heartrate <- df$heartrate[vt2_index]
    
    vt1_vo2Percentage <- vt1_vo2abs
    vt2_vo2Percentage <- vt2_vo2abs
    vt1_workPercentage <- vt1_work
    vt2_workPercentage <- vt2_work
    vt1_hrPercentage <- vt1_heartrate
    vt2_hrPercentage <- vt2_heartrate
    
    out <- data.frame(vt1_indexVslope , vt1_indexExCo , vt2_indexVslope ,
                      vt2_indexExVent , vt1_index , vt2_index , vt1_time ,
                      vt2_time , vt1_vo2abs , vt2_vo2abs , vt1_vo2rel ,
                      vt2_vo2rel , vt1_work , vt2_work , vt1_heartrate ,
                      vt2_heartrate , vt1_vo2Percentage , vt2_vo2Percentage ,
                      vt1_workPercentage , vt2_workPercentage ,
                      vt1_hrPercentage , vt2_hrPercentage)
    out
  })
return(thresholdsTables)
}
############################# Extract VO2Max data ##############################
find_vo2max_data <- function(dataList){
  vo2maxTables <- lapply(dataList, function(df){
    max_index <- which.max(df$vo2absLow)
    max_time <- df$time[max_index]
    max_vo2abs <- max(df$vo2absLow)
    max_vo2rel <- max(df$vo2relLow)
    max_work <- predict_work(df,max_vo2abs)
    max_heartrate <- max(df$heartrate)
    
    max_vo2Percentage <- 1
    max_workPercentage <- 1
    max_hrPercentage <- 1
    
    out <- data.frame(max_index,max_time,max_vo2abs,max_vo2rel,max_work,
                      max_heartrate,max_vo2Percentage,max_workPercentage,max_hrPercentage)
    out
  })
return(vo2maxTables)
}

################ Create summary table of threshold and max data ################
create_summary_tables <- function(thresholdsTables,vo2maxTables){
  summaryTable <- mapply(vtTable=thresholdsTables,maxTable=vo2maxTables,
    SIMPLIFY = F,FUN = function(vtTable,maxTable){
    vtTable$vt1_vo2Percentage <- vtTable$vt1_vo2Percentage/maxTable$max_vo2abs
    vtTable$vt2_vo2Percentage <- vtTable$vt2_vo2Percentage/maxTable$max_vo2abs
    
    vtTable$vt1_workPercentage <- vtTable$vt1_workPercentage/maxTable$max_work
    vtTable$vt2_workPercentage <- vtTable$vt2_workPercentage/maxTable$max_work
    
    vtTable$vt1_hrPercentage <- vtTable$vt1_hrPercentage/maxTable$max_heartrate
    vtTable$vt2_hrPercentage <- vtTable$vt2_hrPercentage/maxTable$max_heartrate
    
    vtTable <- vtTable %>% select(-c(vt1_indexVslope , vt1_indexExCo ,
                                     vt2_indexVslope , vt2_indexExVent ))
    vtTable <- cbind(vtTable,maxTable)
    vtTable <- vtTable %>%
     pivot_longer(everything(),names_sep = "_",
                  names_to = c("variable","measurement")) %>%
     pivot_wider(id_cols =variable, names_from = measurement, values_from = value,
                 names_repair = "check_unique")
    
    out <- vtTable[order(vtTable$vo2abs),]
    out
  })
return(summaryTable)
}