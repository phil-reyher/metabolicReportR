
import_filelist <- function(fileList){
  dataList <- lapply(fileList, function(fileList){ 
    df <- tidytable::fread(fileList, header = F, fill = T, sep = ",",
                           quote = "\"", dec = ".", na.strings = "")
    df
  })
  return(dataList)
}
