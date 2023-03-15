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
testDetailsTable <-format_test_details(metadata)
gxt_tbl <- lapply(test_data_trunc, function(df){
  df <- df %>% slice(1:which.max(df$WORK))
  WORK <- df$WORK
  df <- select(df,VO2_ABS_LOW,VO2_REL_LOW,VO2MAX_PERC,HR,HRMAX_PERC)
  results_list <- lapply(df, function(vec){
    model <- lm(vec ~ WORK)
    WORK_IN <- seq(100,max(WORK),by=25)
    new_observations <- data.frame(WORK =  WORK_IN)
    predicted_vals <- predict(model,newdata = new_observations)
    })
  
  WORK = seq(100,max(WORK),by=25 )
  out <- bind_cols(results_list)
  out <- cbind(WORK,out)
  out <- out %>% 
    mutate(across(c("VO2MAX_PERC","HRMAX_PERC"), ~round(.*100,digits = 0))) %>% 
    mutate(across(c("WORK","HR"),~round(.,digits = 0)) ) %>% 
    mutate(across(c("VO2_ABS_LOW","VO2_REL_LOW"),~format(round(.,digits = 1),
                                                         nsmall = 1)) )
  out
})
#change last stage with max
gxt_tbl <- mapply(gxt=gxt_tbl,max=max_tbl,SIMPLIFY = F,function(gxt,max){
  gxt <- gxt[-nrow(gxt),]
  max <- max %>% select('WORK'=MAX_WORK,'VO2_ABS_LOW'=MAX_VO2ABS,
                        'VO2_REL_LOW'=MAX_VO2REL,'VO2MAX_PERC'=MAX_VO2PERC,
                        'HR'=MAX_HR,'HRMAX_PERC'=MAX_HRPERC) %>% 
    mutate(across(c("VO2MAX_PERC","HRMAX_PERC"), ~round(.*100,digits = 0))) %>% 
    mutate(across(c("WORK","HR"),~round(.,digits = 0)) ) %>% 
    mutate(across(c("VO2_ABS_LOW","VO2_REL_LOW"),~format(round(.,digits = 1)
                                                         ,nsmall = 1)) )
  out <- rbind(gxt,max)
  out
})

########################### Coggan Power Zones #################################
coggan_tbl <- lapply(cps_10bin, function(df){
  
  lvl1_work_low <- "-"
  lvl1_work_up <- round(df$VT2_WORK*0.55)
  lvl1_hr_low <- "-"
  lvl1_hr_up <- round(df$VT2_HR*0.68)
  lvl1_rpe_low <- "-"
  lvl1_rpe_up <- 9
  
  lvl2_work_low <- round(df$VT2_WORK*0.56)
  lvl2_work_up <- round(df$VT2_WORK*0.75)
  lvl2_hr_low <- round(df$VT2_HR*0.69)
  lvl2_hr_up <- round(df$VT2_HR*0.83)
  lvl2_rpe_low <- 9
  lvl2_rpe_up <- 11
  
  lvl3_work_low <- round(df$VT2_WORK*0.76)
  lvl3_work_up <- round(df$VT2_WORK*0.90)
  lvl3_hr_low <- round(df$VT2_HR*0.84)
  lvl3_hr_up <- round(df$VT2_HR*0.94)
  lvl3_rpe_low <- 11
  lvl3_rpe_up <- 13
  lvl4_work_low <- round(df$VT2_WORK*0.91)
  lvl4_work_up <- round(df$VT2_WORK*1.05)
  lvl4_hr_low <- round(df$VT2_HR*0.95)
  lvl4_hr_up <- round(df$VT2_HR*1.05)
  lvl4_rpe_low <- 13
  lvl4_rpe_up <- 15
  
  lvl5_work_low <- round(df$VT2_WORK*1.06)
  lvl5_work_up <- round(df$VT2_WORK*1.2)
  lvl5_hr_low <- round(df$VT2_HR*1.06)
  lvl5_hr_up <- "-"
  lvl5_rpe_low <- 15
  lvl5_rpe_up <- 17
  
  lvl6_work_low <- round(df$VT2_WORK*1.21)
  lvl6_work_up <- "-"
  lvl6_hr_low <- "-"
  lvl6_hr_up <- "-"
  lvl6_rpe_low <- 17
  lvl6_rpe_up <- 19
  
  lvl7_work_low <- "-"
  lvl7_work_up <- "-"
  lvl7_hr_low <- "-"
  lvl7_hr_up <- "-"
  lvl7_rpe_low <- 20
  lvl7_rpe_up <- "-"

  
  lvl1 <- c(1,"Active Recovery",lvl1_work_low,lvl1_work_up,lvl1_hr_low,
            lvl1_hr_up,lvl1_rpe_low,lvl1_rpe_up)
  lvl2 <- c(2,"Endurance",lvl2_work_low,lvl2_work_up,lvl2_hr_low,lvl2_hr_up,
            lvl2_rpe_low,lvl2_rpe_up)
  lvl3 <- c(3,"Tempo",lvl3_work_low,lvl3_work_up,lvl3_hr_low,lvl3_hr_up,
            lvl3_rpe_low,lvl3_rpe_up)
  lvl4 <- c(4,"Lactate Threshold",lvl4_work_low,lvl4_work_up,lvl4_hr_low,
            lvl4_hr_up,lvl4_rpe_low,lvl4_rpe_up)
  lvl5 <- c(5,"VO\\textsubscript{2max}",lvl5_work_low,lvl5_work_up,lvl5_hr_low,lvl5_hr_up,
            lvl5_rpe_low,lvl5_rpe_up)
  lvl6 <- c(6,"Anaerobic Capacity",lvl6_work_low,lvl6_work_up,lvl6_hr_low,
            lvl6_hr_up,lvl6_rpe_low,lvl6_rpe_up)
  lvl7 <- c(7,"Neuromuscular Power",lvl7_work_low,lvl7_work_up,lvl7_hr_low,
            lvl7_hr_up,lvl7_rpe_low,lvl7_rpe_up)
  tr_zones <- as.data.frame(rbind(lvl1,lvl2,lvl3,lvl4,lvl5,lvl6,lvl7))
  colnames(tr_zones) <- c("Zone","Intensity","Lower \\par Range",
                          "Upper \\par Range","Lower \\par Range",
                          "Upper \\par Range","Lower \\par Range",
                          "Upper \\par Range")
  rownames(tr_zones) <- NULL
  out <- tr_zones
  out
})

############################# AIS Power Zones ##################################
ais_tbl <- lapply(max_tbl, function(df){
  
  lvl0_work_low <-round(df$MAX_WORK*0.4)
  lvl0_work_up <- round(df$MAX_WORK*0.5)
  lvl0_hr_low <- "-"
  lvl0_hr_up <- round(df$MAX_HR*0.65)
  lvl0_rpe_low <- "-"
  lvl0_rpe_up <- 11
  
  lvl1_work_low <-round(df$MAX_WORK*0.5)
  lvl1_work_up <- round(df$MAX_WORK*0.65)
  lvl1_hr_low <- round(df$MAX_HR*0.65)
  lvl1_hr_up <- round(df$MAX_HR*0.75)
  lvl1_rpe_low <- 12
  lvl1_rpe_up <- 13
  
  lvl2_work_low <- round(df$MAX_WORK*0.65)
  lvl2_work_up <- round(df$MAX_WORK*0.725)
  lvl2_hr_low <- round(df$MAX_HR*0.75)
  lvl2_hr_up <- round(df$MAX_HR*0.8)
  lvl2_rpe_low <- 13
  lvl2_rpe_up <- 15
  
  lvl3_work_low <- round(df$MAX_WORK*0.725)
  lvl3_work_up <- round(df$MAX_WORK*0.80)
  lvl3_hr_low <- round(df$MAX_HR*0.8)
  lvl3_hr_up <- round(df$MAX_HR*0.85)
  lvl3_rpe_low <- 15
  lvl3_rpe_up <- 16
  
  lvl4_work_low <- round(df$MAX_WORK*0.8)
  lvl4_work_up <- round(df$MAX_WORK*0.9)
  lvl4_hr_low <- round(df$MAX_HR*0.85)
  lvl4_hr_up <- round(df$MAX_HR*0.92)
  lvl4_rpe_low <- 16
  lvl4_rpe_up <- 17
  
  lvl5_work_low <- round(df$MAX_WORK*0.9)
  lvl5_work_up <- round(df$MAX_WORK*1)
  lvl5_hr_low <- round(df$MAX_HR*0.92)
  lvl5_hr_up <- round(df$MAX_HR*1)
  lvl5_rpe_low <- 17
  lvl5_rpe_up <- 19
  
  lvl0 <- c(0,"Recovery",lvl0_work_low,lvl0_work_up,lvl0_hr_low,
            lvl0_hr_up,lvl0_rpe_low,lvl0_rpe_up)
  lvl1 <- c(1,"Aerobic",lvl1_work_low,lvl1_work_up,lvl1_hr_low,
            lvl1_hr_up,lvl1_rpe_low,lvl1_rpe_up)
  lvl2 <- c(2,"Extensive Endurance",lvl2_work_low,lvl2_work_up,lvl2_hr_low,lvl2_hr_up,
            lvl2_rpe_low,lvl2_rpe_up)
  lvl3 <- c(3,"Intensive Endurance",lvl3_work_low,lvl3_work_up,lvl3_hr_low,lvl3_hr_up,
            lvl3_rpe_low,lvl3_rpe_up)
  lvl4 <- c(4,"Threshold",lvl4_work_low,lvl4_work_up,lvl4_hr_low,
            lvl4_hr_up,lvl4_rpe_low,lvl4_rpe_up)
  lvl5 <- c(5,"VO\\textsubscript{2max}",lvl5_work_low,lvl5_work_up,lvl5_hr_low,lvl5_hr_up,
            lvl5_rpe_low,lvl5_rpe_up)
  tr_zones <- as.data.frame(rbind(lvl0,lvl1,lvl2,lvl3,lvl4,lvl5))
  colnames(tr_zones) <- c("Zone","Intensity","Lower \\par Range",
                          "Upper \\par Range","Lower \\par Range",
                          "Upper \\par Range","Lower \\par Range",
                          "Upper \\par Range")
  rownames(tr_zones) <- NULL
  out <- tr_zones
  out
})

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
