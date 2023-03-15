############################# Format Test Details ##############################
format_test_details <- function(metadata){
  testDetails <- lapply(metadata,function(meta){
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
    out <- df %>% select(-c(index,time))
    out
  })
return(formattedSummaryTable)
}