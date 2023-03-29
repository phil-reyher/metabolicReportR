############################## Create Large List ###############################
create_combined_list <- function(){
bigList <- mapply(x=metadata,y=testDetailsTablesFormatted,
                  z=summaryTablesFormatted,a=gxtTablesFormatted,
                  b=coggansTablesFormatted,c=aisTablesFormatted,
                  d=participantNameList, SIMPLIFY = F,
                  FUN = function(x,y,z,a,b,c,d){
                    list(metadata=x,testDetails=y,
                         summaryTable=z,gxtTable=a,cogganTable=b,aisTable=c,
                         participantName=d)})
return(bigList)
}
###################################let's render#################################
create_reports <- function(bigList){
  purrr::walk(bigList,function(participant){
    rmarkdown::render(
      input = "layout/report_layout.Rmd",
      intermediates_dir = "output/intermediates",
      output_file = paste0(participant$participantName,".pdf"),
      output_dir = "finished_reports",
      output_format = outputFormat,
      clean = TRUE
    )
  })
}

