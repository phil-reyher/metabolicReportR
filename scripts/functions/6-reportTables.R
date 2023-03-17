############################# Format Test Details ##############################
format_test_details <- function(metadataList){
  testDetails <- lapply(metadataList,function(meta){
    out <- meta %>% select(testDate,temperature,relativeHumidity,
                           barometricPressure) %>%
      mutate(across("testDate",~format(.x, format = "%d-%b-%Y"))) %>% 
      mutate(across(-"testDate",~format(round(as.numeric(.),1), nsmall=1))) 
    out
  })
return(testDetails)
}

############################ Format Summary Tables #############################
format_summary_table <- function(summaryTables){
  formattedSummaryTable <- lapply(summaryTables, function(df){
    df <- df %>% mutate(across(ends_with("percentage",ignore.case =T),
                               ~round(.*100,digits = 0) ) ) %>%
      mutate(across(c(work,heartrate),~round(.,digits = 0) )) %>% 
      mutate(across(c(vo2abs,vo2rel),~format(round(.,digits = 1),
                                                 nsmall = 1)))
    out <- df %>% select(c(variable,work,workPercentage,vo2abs,vo2rel,
                           vo2Percentage,heartrate,hrPercentage))
    out
  })
return(formattedSummaryTable)
}

################################## GXT Table ###################################
create_gxt_table <- function(dataList,vo2maxDataList){
  gxtTable <- mapply(df = dataList, maxTable = vo2maxDataList, SIMPLIFY = F,
    function(df,maxTable){
    #extract work, truncating to workmax for linear model
    df <- df %>% slice(1:which.max(df$work))
    work <- df$work
    #select columns to loop through
    df <- df %>% select(vo2absLow,vo2relLow,vo2maxPercentage,heartrate,
                 hrmaxPercentage)
    #loop through columns, for linear models
    resultsList <- lapply(df, function(vec){
      model <- lm(vec ~ work)
      workIn <- seq(100,max(work),by=25)
      newObservations <- data.frame(work =  workIn)
      predictedValues <- predict(model,newdata = newObservations)
    })
    gxtTable <- bind_cols(resultsList)
    work <- seq(100,max(work),by=25 )
    gxtTable <- cbind(work,gxtTable)
    #exchange last row with values of VO2MAX
    maxTable <- maxTable %>% select('work'=max_work,'vo2absLow'=max_vo2abs,
                          'vo2relLow'=max_vo2rel,
                          'vo2maxPercentage'=max_vo2Percentage,
                          'heartrate'=max_heartrate,
                          'hrmaxPercentage'=max_hrPercentage)
    if (max(gxtTable$work) >= maxTable$work) {
      gxtTable <- gxtTable[-nrow(gxtTable),]
    }
    gxtTable <- rbind(gxtTable,maxTable)
    gxtTable <- gxtTable %>% 
      mutate(across(ends_with("percentage",ignore.case = T),
                    ~round(.*100,digits = 0))) %>% 
      mutate(across(c("work","heartrate"),~round(.,digits = 0)) ) %>% 
      mutate(across(c("vo2absLow","vo2relLow"),~format(round(.,digits = 1)
                                                           ,nsmall = 1)) )
    gxtTable
  })
return(gxtTable)
}

############################## Coggan Power Zones ##############################
create_coggans_zones_table <- function(vt2DataList){
  cogganTable <- lapply(vt2DataList, function(df){
    
    lvl1WorkLow <- "-"
    lvl1WorkUp <- round(df$vt2_work*0.55)
    lvl1HeartrateLow <- "-"
    lvl1HeartrateUp <- round(df$vt2_heartrate*0.68)
    lvl1RpeLow <- "-"
    lvl1RpeUp <- 9
    
    lvl2WorkLow <- round(df$vt2_work*0.56)
    lvl2WorkUp <- round(df$vt2_work*0.75)
    lvl2HeartrateLow <- round(df$vt2_heartrate*0.69)
    lvl2HeartrateUp <- round(df$vt2_heartrate*0.83)
    lvl2RpeLow <- 9
    lvl2RpeUp <- 11
    
    lvl3WorkLow <- round(df$vt2_work*0.76)
    lvl3WorkUp <- round(df$vt2_work*0.90)
    lvl3HeartrateLow <- round(df$vt2_heartrate*0.84)
    lvl3HeartrateUp <- round(df$vt2_heartrate*0.94)
    lvl3RpeLow <- 11
    lvl3RpeUp <- 13
    
    lvl4WorkLow <- round(df$vt2_work*0.91)
    lvl4WorkUp <- round(df$vt2_work*1.05)
    lvl4HeartrateLow <- round(df$vt2_heartrate*0.95)
    lvl4HeartrateUp <- round(df$vt2_heartrate*1.05)
    lvl4RpeLow <- 13
    lvl4RpeUp <- 15
    
    lvl5WorkLow <- round(df$vt2_work*1.06)
    lvl5WorkUp <- round(df$vt2_work*1.2)
    lvl5HeartrateLow <- round(df$vt2_heartrate*1.06)
    lvl5HeartrateUp <- "-"
    lvl5RpeLow <- 15
    lvl5RpeUp <- 17
    
    lvl6WorkLow <- round(df$vt2_work*1.21)
    lvl6WorkUp <- "-"
    lvl6HeartrateLow <- "-"
    lvl6HeartrateUp <- "-"
    lvl6RpeLow <- 17
    lvl6RpeUp <- 19
    
    lvl7WorkLow <- "-"
    lvl7WorkUp <- "-"
    lvl7HeartrateLow <- "-"
    lvl7HeartrateUp <- "-"
    lvl7RpeLow <- 20
    lvl7RpeUp <- "-"
    
    
    lvl1 <- c(1,"Active Recovery",lvl1WorkLow,lvl1WorkUp,lvl1HeartrateLow,
              lvl1HeartrateUp,lvl1RpeLow,lvl1RpeUp)
    lvl2 <- c(2,"Endurance",lvl2WorkLow,lvl2WorkUp,lvl2HeartrateLow,
              lvl2HeartrateUp,lvl2RpeLow,lvl2RpeUp)
    lvl3 <- c(3,"Tempo",lvl3WorkLow,lvl3WorkUp,lvl3HeartrateLow,lvl3HeartrateUp,
              lvl3RpeLow,lvl3RpeUp)
    lvl4 <- c(4,"Lactate Threshold",lvl4WorkLow,lvl4WorkUp,lvl4HeartrateLow,
              lvl4HeartrateUp,lvl4RpeLow,lvl4RpeUp)
    lvl5 <- c(5,"VO\\textsubscript{2max}",lvl5WorkLow,lvl5WorkUp,
              lvl5HeartrateLow,lvl5HeartrateUp,lvl5RpeLow,lvl5RpeUp)
    lvl6 <- c(6,"Anaerobic Capacity",lvl6WorkLow,lvl6WorkUp,lvl6HeartrateLow,
              lvl6HeartrateUp,lvl6RpeLow,lvl6RpeUp)
    lvl7 <- c(7,"Neuromuscular Power",lvl7WorkLow,lvl7WorkUp,lvl7HeartrateLow,
              lvl7HeartrateUp,lvl7RpeLow,lvl7RpeUp)
    trainingZones <- as.data.frame(rbind(lvl1,lvl2,lvl3,lvl4,lvl5,lvl6,lvl7))
    colnames(trainingZones) <- c("Zone","Intensity","Lower \\par Range",
                            "Upper \\par Range","Lower \\par Range",
                            "Upper \\par Range","Lower \\par Range",
                            "Upper \\par Range")
    rownames(trainingZones) <- NULL
    out <- trainingZones
    out
  })
return(cogganTable)
}
############################### AIS Power Zones ################################
create_ais_zones_table <- function(vo2maxDataList){
  aisTable <- lapply(vo2maxDataList, function(df){
    lvl0WorkLow <-round(df$max_work * 0.4)
    lvl0WorkUp <- round(df$max_work * 0.5)
    lvl0HeartrateLow <- "-"
    lvl0HeartrateUp <- round(df$max_heartrate * 0.65)
    lvl0RpeLow <- "-"
    lvl0RpeUp <- 11
    
    lvl1WorkLow <-round(df$max_work * 0.5)
    lvl1WorkUp <- round(df$max_work * 0.65)
    lvl1HeartrateLow <- round(df$max_heartrate * 0.65)
    lvl1HeartrateUp <- round(df$max_heartrate * 0.75)
    lvl1RpeLow <- 12
    lvl1RpeUp <- 13
    
    lvl2WorkLow <- round(df$max_work * 0.65)
    lvl2WorkUp <- round(df$max_work * 0.725)
    lvl2HeartrateLow <- round(df$max_heartrate * 0.75)
    lvl2HeartrateUp <- round(df$max_heartrate * 0.8)
    lvl2RpeLow <- 13
    lvl2RpeUp <- 15
    
    lvl3WorkLow <- round(df$max_work * 0.725)
    lvl3WorkUp <- round(df$max_work * 0.80)
    lvl3HeartrateLow <- round(df$max_heartrate * 0.8)
    lvl3HeartrateUp <- round(df$max_heartrate * 0.85)
    lvl3RpeLow <- 15
    lvl3RpeUp <- 16
    
    lvl4WorkLow <- round(df$max_work * 0.8)
    lvl4WorkUp <- round(df$max_work * 0.9)
    lvl4HeartrateLow <- round(df$max_heartrate  *0.85)
    lvl4HeartrateUp <- round(df$max_heartrate * 0.92)
    lvl4RpeLow <- 16
    lvl4RpeUp <- 17
    
    lvl5WorkLow <- round(df$max_work * 0.9)
    lvl5WorkUp <- round(df$max_work * 1)
    lvl5HeartrateLow <- round(df$max_heartrate * 0.92)
    lvl5HeartrateUp <- round(df$max_heartrate * 1)
    lvl5RpeLow <- 17
    lvl5RpeUp <- 19
    
    lvl0 <- c(0,"Recovery",lvl0WorkLow,lvl0WorkUp,lvl0HeartrateLow,
              lvl0HeartrateUp,lvl0RpeLow,lvl0RpeUp)
    lvl1 <- c(1,"Aerobic",lvl1WorkLow,lvl1WorkUp,lvl1HeartrateLow,
              lvl1HeartrateUp,lvl1RpeLow,lvl1RpeUp)
    lvl2 <- c(2,"Extensive Endurance",lvl2WorkLow,lvl2WorkUp,lvl2HeartrateLow,lvl2HeartrateUp,
              lvl2RpeLow,lvl2RpeUp)
    lvl3 <- c(3,"Intensive Endurance",lvl3WorkLow,lvl3WorkUp,lvl3HeartrateLow,lvl3HeartrateUp,
              lvl3RpeLow,lvl3RpeUp)
    lvl4 <- c(4,"Threshold",lvl4WorkLow,lvl4WorkUp,lvl4HeartrateLow,
              lvl4HeartrateUp,lvl4RpeLow,lvl4RpeUp)
    lvl5 <- c(5,"VO\\textsubscript{2max}",lvl5WorkLow,lvl5WorkUp,lvl5HeartrateLow,lvl5HeartrateUp,
              lvl5RpeLow,lvl5RpeUp)
    trainingZones <- as.data.frame(rbind(lvl0,lvl1,lvl2,lvl3,lvl4,lvl5))
    colnames(trainingZones) <- c("Zone","Intensity","Lower \\par Range",
                            "Upper \\par Range","Lower \\par Range",
                            "Upper \\par Range","Lower \\par Range",
                            "Upper \\par Range")
    rownames(trainingZones) <- NULL
    out <- trainingZones
    out
  })
return(aisTable)
}
