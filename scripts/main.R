################################################################################
#                                                                              #
# Purpose:       Parvo Plot Script                                             #
#                                                                              #
# Author:        Philipp Reyher                                                #
# Contact:       reyher.philipp@gmail.com                                      #
# Client:        Philipp Reyher                                                #
#                                                                              #
# Code created:  2022-10-28                                                    #
# Last updated:  2022-10-28                                                    #
# Source:        /metabolic_reportR/scripts/                                   #
#                                                                              #
# Comment:       Script aims to automise the creation of plots using the Parvo #
#                Metabolic Cart data                                           #
#                                                                              #
################################################################################
################################### Packages ###################################
library(data.table)
library(tidytable)
library(ggplot2)
library(signal)
library(gridExtra)
library(grid)
library(here)
############################# Global Vars/Options ##############################
dir <- here::here()
setwd(dir)
gxtPlotsPath <- file.path("output","plots","gxt_plots")
thresholdPlotsPath <- file.path("output","plots","threshold_plots")
filePath <- file.path("data","single")
fileList <- list.files(path = filePath, pattern = "*.csv",
                        ignore.case = T, full.names = T)
################################## Functions ###################################
source(here::here("scripts/functions/0-globalFuns.R"))
source(here::here("scripts/functions/1-import.R"))
source(here::here("scripts/functions/2-extractRegex.R"))
source(here::here("scripts/functions/3-tidying.R"))
source(here::here("scripts/functions/4-preprocessing.R"))
source(here::here("scripts/functions/5-ventilatoryParametersExtraction.R"))
source(here::here("scripts/functions/6-reportTables.R"))

#################################### Import ####################################
testData <- import_filelist(fileList)
################### Extract Demographics and Test-parameters ###################
participantNames <- extract_participant_names(testData)
participantNames <- gsub("[^[:alnum:]]", "", participantNames)
names(testData) <- participantNames
metadata <- extract_metadata(testData)
metadata <- extract_test_date(extractFrom = fileList,
                                      appendTo = metadata)
############################### Tidy Up Dataset ################################
testData <- tidy_up(testData)
############################### Extend Metadata ################################
metadata <- extract_start_end_indices(extractFrom = testData,
                                       appendTo = metadata)
################################ Preprocessing #################################
#filtering
testData <- low_pass_filter(testData)
#variable computation
testData <- compute_ventilatory_vars(testData)
#Truncation
testDataTruncated <- truncate_data(dataList = testData,metadata = metadata)
#interpolation
testDataInterpolatedSeconds <- interpolate_to_seconds(testDataTruncated)
#binning
testData10Binned <- apply_bin_average(testDataInterpolatedSeconds,bin = 10)
########################## Calculation of VT1,VT2,Max ##########################
changepointsData <- find_ventilatory_thresholds_data(testData10Binned)
vo2maxData <- find_vo2max_data(testData)
summaryTables <- create_summary_tables(changepointsData,vo2maxData)
################################ Report-Tables #################################
summaryTablesFormatted <- format_summary_table(summaryTables)
testDetailsTablesFormatted <-format_test_details(metadata)
gxtTablesFormatted <- create_gxt_table(testDataTruncated,vo2maxData)
coggansTablesFormatted <- create_coggans_zones_table(changepointsData)
aisTablesFormatted <- create_ais_zones_table(vo2maxData)
#################################### Plots #####################################
plist_cps_10bin <- plist_cps_func(test_data_10bin,cps_10bin)

# #save individual plots
purrr::pwalk(list(partnames_formatted,plist_cps_10bin), function(name,p){
  ggsave(paste0("./plots/individual_plots/",name,".pdf"), p, width = 11,
         height = 8.5, units = "in")
})
partnames_formatted <- as.list(partnames_formatted)

################################################################################
biglist <- mapply(function(x,y,z){list(test_data=x,demo_data=y,changepoints=z)},
            x=test_data,y=demo_data,z=changepoints, SIMPLIFY = F)


###################################let's render#################################
rmarkdown::render(
  input = loc,
  output_file = paste0(current$branch, "_", this_year, "_", this_month ,".pdf"),
  output_dir = "finished_reports",
  intermediates_dir = "finished_reports/tex",
  clean = TRUE,
  output_options = list(
    pdf_document = list(
      keep_tex = TRUE,
      includes = list(
        in_header = "path/to/additional_latex_styling.tex"
      )
    )
  )
)
