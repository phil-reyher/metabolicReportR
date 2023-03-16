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
library(purrr)
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
fpath <- file.path("data","single")
fileList <- list.files(path = fpath, pattern = "*.csv",
                        ignore.case = T, full.names = T)
################################## Functions ###################################
source(here::here("scripts/functions/0-globalFuns.R"))
source(here::here("scripts/functions/1-import.R"))
source(here::here("scripts/functions/2-extractRegex.R"))
source(here::here("scripts/functions/3-tidying.R"))
source(here::here("scripts/functions/4-preprocessing.R"))
source(here::here("scripts/functions/5-vo2ThresholdsMaxExtraction.R"))
source(here::here("scripts/functions/6-reportTables.R"))

#################################### Import ####################################
testData <- import_filelist(fileList)
################### Extract Demographics and Test-parameters ###################
participantNames <- extract_participant_names(testData)
names(testData) <- participantNames
metadata <- extract_metadata(testData)
metadata <- extract_test_date(extractFrom = fileList,
                                      appendTo = metadata)
############################### Tidy Up Dataset ################################
testData <- tidy_up(testData)
################################## Extend metadata #############################
metadata <- extract_start_end_indices(extractFrom = testData,
                                       appendTo = metadata)
################################# Preprocessing ################################
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
##################### Calculation of VT1, VT2, Max #############################
changepointsData <- find_ventilatory_thresholds_data(testData10Binned)
vo2maxData <- find_vo2max_data(testData)
summaryTables <- create_summary_tables(changepointsData,vo2maxData)
################################ Report-Tables #################################
summaryTablesFormatted <- format_summary_table(summaryTables)
testDetailsTablesFormatted <-format_test_details(metadata)
gxtTablesFormatted <- create_gxt_table(testDataTruncated,vo2maxData)
coggansTablesFormatted <- create_coggans_zones_table(changepointsData)
aisTablesFormatted <- create_ais_zones_table(vo2maxData)
####################### Changepoints plotting ##################################
#plotting function
plist_cps_func <- function(test_data,cps_data){
  plist <- mapply(df=test_data, vt=cps_data, SIMPLIFY = F,
                  FUN = function(df,vt)
                  {
                    exco2 <- ggplot(df, aes(x=TIME_S))+
                      geom_point(aes(y=EXCO2),colour='blue')+
                      geom_vline(xintercept = df$TIME_S[vt$VT1EXCO2_I], colour='green')+
                      annotate(x=df$TIME_S[vt$VT1EXCO2_I],y=+Inf,
                               label=paste0("VT1=",df$TIME_S[vt$VT1EXCO2_I]," s"),
                               vjust=2,geom="label")+
                      theme_bw()
                    
                    vslop1 <- ggplot(df, aes(x=VO2_ABS))+
                      geom_point(aes(y=VCO2),colour='blue')+
                      geom_vline(xintercept = df$VO2_ABS[vt$VT1VSLOP_I], colour='green')+
                      annotate(x=df$VO2_ABS[vt$VT1VSLOP_I],y=+Inf,
                               label=paste0("VT1=",df$TIME_S[vt$VT1VSLOP_I]," s"),
                               vjust=2,geom="label")+
                      theme_bw()
                    
                    exve <- ggplot(df, aes(x=TIME_S))+
                      geom_point(aes(y=EXVE),colour='blue')+
                      geom_vline(xintercept = df$TIME_S[vt$VT2EXVE_I], colour='green')+
                      annotate(x=df$TIME_S[vt$VT2EXVE_I],y=+Inf,
                               label=paste0("VT2=",df$TIME_S[vt$VT2EXVE_I]," s"),
                               vjust=2,geom="label")+
                      theme_bw()
                    
                    vslop2 <- ggplot(df, aes(x=VCO2))+
                      geom_point(aes(y=VE),colour='blue')+
                      geom_vline(xintercept = df$VCO2[vt$VT2VSLOP_I], colour='green')+
                      annotate(x=df$VCO2[vt$VT2VSLOP_I],y=+Inf,
                               label=paste0("VT2=",df$TIME_S[vt$VT2VSLOP_I]," s"),
                               vjust=2,geom="label")+
                      theme_bw()
                    
                    bigplot <- ggplot(df, aes(x=TIME_S))+
                      coord_cartesian(xlim = c(300, 1100),ylim = c(7.5,45))+
                      scale_x_continuous(name="Time (s)",
                                         breaks=seq(300,1150,50) )+
                      scale_y_continuous(name="VE/VO2 | VE/VCO2", breaks=seq(10,45,5),
                                         sec.axis = sec_axis(~.*10,name='Work' ) )+
                      geom_point(aes(y=VE_VO2, colour='VE/VO2') )+
                      geom_point(aes(y=VE_VCO2, colour='VE/VCO2') )+
                      geom_vline(xintercept = df$TIME_S[vt$VT1_I],colour='black',
                                 linetype = "dotted")+
                      annotate(x=df$TIME_S[vt$VT1_I],y=+Inf,
                               label=paste0("VT1=",df$TIME_S[vt$VT1_I]," s"),
                               vjust=2,geom="label")+
                      geom_vline(xintercept = df$TIME_S[vt$VT2_I],colour='green',
                                 linetype = "longdash")+
                      annotate(x=df$TIME_S[vt$VT2_I],y=+Inf,
                               label=paste0("VT2=",df$TIME_S[vt$VT2_I]," s"),
                               vjust=2,geom="label")+
                      geom_area(aes(y = (WORK/10),colour="Work"), fill ="lightblue", 
                                alpha = 0.4) +
                      scale_color_manual(name=' ',
                                         breaks=c('VE/VO2', 'VE/VCO2', 'Work'),
                                         values=c('VE/VO2'='blue', 'VE/VCO2'='red', 'Work'='lightblue'),
                                         guides(colour = guide_legend(override.aes = list(size = 8) ) ) )+
                      theme_bw()+
                      guides(shape = guide_legend(override.aes = list(size = 1)))+
                      guides(color = guide_legend(override.aes = list(size = 1)))+
                      theme(legend.title = element_blank(),
                            legend.text = element_text(size = 8),
                            legend.position = c(.05, .95),
                            legend.justification = c("left", "top"),
                            legend.box.just = "right",
                            legend.margin = margin(6, 6, 6, 6) )
                    
                    plots <- list(exco2,vslop1,exve,vslop2,bigplot)
                    lay <- rbind(c(1,1,2,2),
                                 c(1,1,2,2),
                                 c(3,3,4,4),
                                 c(3,3,4,4),
                                 c(5,5,5,5),
                                 c(5,5,5,5),
                                 c(5,5,5,5),
                                 c(5,5,5,5),
                                 c(5,5,5,5))
                    
                    out <- arrangeGrob(grobs = plots, layout_matrix = lay)
                  })
  return(plist)
}
# #create plots
plist_cps_10bin <- plist_cps_func(test_data_10bin,cps_10bin)
# #save individual plots
partnames_formatted <- gsub(",", "_", partnames)
partnames_formatted <- gsub(" ", "", partnames_formatted)
purrr::pwalk(list(partnames_formatted,plist_cps_10bin), function(name,p){
  ggsave(paste0("./plots/individual_plots/",name,".pdf"), p, width = 11,
         height = 8.5, units = "in")
})
partnames_formatted <- as.list(partnames_formatted)

############################## Exercise plots ##################################
ex_plots <- mapply(df=test_data,dem=demo_data,vt=cps_10bin,SIMPLIFY = F,
                  FUN=function(df,dem,vt){
  p <-  ggplot(df,aes(x=TIME_S))+
  
  geom_vline(xintercept = vt$VT1_TIME)+
  annotate(x=vt$VT1_TIME,y=+Inf,
  label="VT1",vjust=2,geom="label")+
  
  geom_vline(xintercept = vt$VT2_TIME)+
  annotate(x=vt$VT2_TIME,y=+Inf,
  label="VT2",vjust=2,geom="label")+
  
  geom_vline(xintercept = df$TIME_S[dem$EV_EX_I])+
  annotate(x=df$TIME_S[dem$EV_EX_I],y=+Inf,
  label="Start",vjust=2,geom="label")+
  
  geom_vline(xintercept = df$TIME_S[dem$EV_CD_I])+
  annotate(x=df$TIME_S[dem$EV_CD_I],y=+Inf,
  label="Cooldown",vjust=2,geom="label")+
  
  geom_line(aes(y=VO2_ABS_LOW,group=1, colour='VO2'))+
  guides(color = guide_legend(override.aes = list(size = 1.5)))+
  labs(color="Measurement")+
  geom_line(aes(y=VCO2_LOW,group=2, colour='VCO2'))+
  geom_area(aes(y = (WORK/100)), fill ="lightblue", group=3, alpha = 0.4 ) +
  scale_color_manual(name='Measurement',
                     breaks=c('VO2', 'VCO2', 'VO2', 'WORK'),
                     values=c('VO2'='green', 'VCO2'='red', 'WORK'='blue'))
 p
})

ex_plotlist <- marrangeGrob(ex_plots, nrow=1,ncol=1)
ggsave("multipage.pdf", ex_plotlist, width = 11, height = 8.5, units = "in")

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
