############################## Create Large List ###############################
create_combined_list <- function(){
bigList <- mapply(x=metadata,y=testDetailsTablesFormatted,
                  z=summaryTablesFormatted,a=coggansTablesFormatted,
                  b=aisTablesFormatted,c=participantNameList, SIMPLIFY = F,
                  FUN = function(x,y,z,a,b,c){
                    list(metadata=x,testDetails=y,
                         summaryTable=z,cogganTable=a,aisTable=b,
                         participantName=c)})
return(bigList)
}
###################################let's render#################################
create_reports <- function(bigList){
  purrr::walk(bigList,function(participant){
    rmarkdown::render(
      input = "layout/report_layout.Rmd",
      intermediates_dir = "finished_reports/intermediates",
      output_file = paste0(participant$participantName,".pdf"),
      output_dir = "finished_reports",
      clean = TRUE
    )
  })
}

