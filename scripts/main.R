################################################################################
#                                                                              #
# Purpose:       Parvo Plot Script                                             #
#                                                                              #
# Author:        Philipp Reyher                                                #
# Contact:       reyher.philipp@gmail.com                                      #
# Client:        Philipp Reyher                                                #
#                                                                              #
# Code created:  2022-10-28                                                    #
# Last updated:  2023-06-06                                                    #
# Source:        /metabolic_reportR/scripts/                                   #
#                                                                              #
# Comment:       Script aims to automate the creation of plots using the Parvo #
#                Metabolic Cart data                                           #
#                                                                              #
################################################################################
############################## ONLY RUN THIS ONCE ##############################
library(tinytex)
tinytex::install_tinytex()
################################### Packages ###################################
library(purrr)
library(data.table)
library(tidytable)
library(ggplot2)
library(signal)
library(gridExtra)
library(grid)
library(here)
library(kableExtra)
############################# Global Vars/Options ##############################
dir <- here::here()
setwd(dir)
gxtPlotsPath <- file.path("output","plots","gxt_plots")
thresholdPlotsPath <- file.path("output","plots","threshold_plots")
latexPreamblePath <- file.path(dir,"layout","latex_input",
                               "preamble.tex")
filePath <- file.path("data")
fileList <- list.files(path = filePath, pattern = "*.csv",
                        ignore.case = T, full.names = T)
#yaml-header input for rmarkdown
outputFormat <- rmarkdown::pdf_document(
  includes = list(
    in_header = latexPreamblePath
  )
)
################################## Functions ###################################
source(here::here("scripts/functions/0-globalFuns.R"))
source(here::here("scripts/functions/1-import.R"))
source(here::here("scripts/functions/2-extractRegex.R"))
source(here::here("scripts/functions/3-tidying.R"))
source(here::here("scripts/functions/4-preprocessing.R"))
source(here::here("scripts/functions/5-ventilatoryParametersExtraction.R"))
source(here::here("scripts/functions/6-reportTables.R"))
source(here::here("scripts/functions/7-plots.R"))
source(here::here("scripts/functions/8-export.R"))
#################################### Import ####################################
testData <- import_filelist(fileList)
################### Extract Demographics and Test-parameters ###################
participantNames <- extract_participant_names(testData)
participantNames <- gsub("[^[:alnum:]]", "", participantNames)
participantNameList <- as.list(participantNames)
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
#IMPORTANT - CHOOSE ONE FILTER AND ONLY EXECUTE THAT LINE;
#THE FILTERS OVERWRITE EACH OTHER
testData <- apply_low_pass_filter(testData)
testData <- apply_moving_average_filter(testData)
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
create_gxt_plots(testData,metadata,changepointsData,participantNameList,
                 gxtPlotsPath)
create_threshold_plots(testData10Binned,changepointsData,
                       participantNameList,thresholdPlotsPath)
#################################### Export ####################################
combinedList <- create_combined_list()
create_reports(combinedList)

