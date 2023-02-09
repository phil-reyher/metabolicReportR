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
# Source:        C:/Users/reyhe/Documents/Parvo                                #
#                                                                              #
# Comment:       Script aims to automise the creation of plots using the Parvo #
#                Metabolic Cart data                                           #
#                                                                              #
################################################################################

################################### Packages ###################################
library(readxl)
library(tidyverse)
library(data.table)
library(ggplot2)
library(signal)
library(gridExtra)
library(grid)
#################################### Import ####################################

##get dir that script is in
dir <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dir)
##go up a folder (now Parvo folder is the wd)
setwd("..")
file.list = list.files(path = "data/single", pattern = "*.csv", ignore.case = T,
    full.names = T)

##read data
##apply over file-list
test_data <- lapply(file.list, function(x){ 
  df <- fread(x, header = F, fill = T, sep = ",", quote = "\"", dec = ".",
  na.strings = "")
  df
  })

################### Extract demographics and test parameters ###################
##function to perform regex search x must be a regular expression as a string,
##which is then searched for within the dataset
##y is optional, input 1 so the value 'left' to the searched cell is used, not the
##default: 'right'
regex_s <- function (df,x,y=0){
  ##Find column name and row index of demographics with regex
  ##Outputs a list with elemts containing row index and the name of the
  ##elements containing the columnname!
  tmp <- sapply(df, function(col) {
    str_which(col, regex(x, ignore_case = T))
  })
  ##find column index within list as column name is impractical for the
  ##following steps
  tmp_col <- as.integer(which(tmp != 0))
  ##Find row index within list using column index
  tmp_row <- as.integer(tmp[tmp_col])
  ##special case weight
  if(x=="weight"){tmp_col <- str_which(tolower(df[tmp_row]), "kg") - 1}
  ##Should the value be left of the demographics' name
  else if(y=="1"){tmp_col <- tmp_col - 1}
  ##Should the value be right to the demographics' name
  else{tmp_col <- tmp_col + 1}
  ##extract from dataframe using both indices
  as.character(df[tmp_row, ..tmp_col]
  )}

##save participant names in array for later
partnames <- sapply(test_data, function(df) {
  NAME <- regex_s(df,"\\bname\\b")
})

names(test_data) <- partnames

demo_data <- lapply(test_data, function(df) {
  NAME <- regex_s(df,"\\bname\\b")
  AGE <- regex_s(df,"\\bage\\b")
  SEX <- regex_s(df,"\\bsex\\b")
  MASS <- regex_s(df,"\\bweight\\b")
  PB <- regex_s(df,"^(?=.*(baro))(?=.*(press)).*$")
  TEMP <- regex_s(df,"^(?=.*(insp))(?=.*(temp)).*$")
  RH <- regex_s(df,"^(?=.*(insp))(?=.*(humid)).*$")
  EV_WU <- regex_s(df,"^(?=.*(warm))(?=.*(up)).*$",1)
  ifelse(is_empty(EV_WU)==1,EV_WU <- NA,EV_WU)
  EV_EX <- regex_s(df,"^(?=.*(start))(?=.*(exercise)).*$",1)
  ifelse(is_empty(EV_EX)==1,EV_EX <- NA,EV_EX)
  EV_CD <- regex_s(df,"^(?=.*(cool))(?=.*(down)).*$",1)
  ifelse(is_empty(EV_CD)==1,EV_CD <- NA,EV_CD)
  
  df1 <- data.frame(NAME, AGE, SEX, MASS, PB, TEMP, RH, EV_WU, EV_EX, EV_CD)
  df1 <- df1 %>% dplyr::select(where(~any(!is.na(.))))
  })
  
##function to extract the dates from file.list and append it to the demographics
##list
##apply over both lists
demo_data <- mapply(df = demo_data, x = file.list, SIMPLIFY = F,
  FUN = function(df,x){
  dat <- lubridate::as_date(str_extract(x, "\\d{8}"))
  df$TEST_DAT <- dat
  df
  })

################################ Clean dataset #################################
test_data <- lapply(test_data, function(df) {
  ##find indices of string "time", this is where the spiro data starts
  tmp <- sapply(df, function(x) {
  str_which(x, fixed("time", ignore_case = T))
                                })
  ##row index -1, as the current row is holding the headers
  tmp <- as.numeric(tmp[which(tmp != 0)]) - 1
  ##finally remove the mess from the top
  df <- df[-(1:tmp)]
  ##Column names
  ##combine the two header rows
  coln <- paste(df[1],df[2],sep = "")
  ## clean up NAs
  coln <- gsub("NA","",coln)
  ##remove whitespace
  coln <- gsub(" ","",coln)
  ##standardize colnames
  coln <- str_replace(coln,regex("^(?=.*(VO2))(?=.*(kg)).*$",ignore_case = T),"VO2_REL")
  coln <- str_replace(coln,regex("VO2STPD",ignore_case = T),"VO2_ABS")
  coln <- str_replace(coln,regex(".*work.*",ignore_case = T),"WORK")
  coln <- str_replace(coln,regex(".*hr.*",ignore_case = T),"HR")

  coln <- toupper(gsub("STPD","",coln))
  colnames(df) <- coln
  ##remove anything but data
  df <- df[-(1:4)]
  ##find first and last instance of NA to remove mess at the bottom
  NAindex <- which(is.na(df[,1]))
  firstNA <- min(NAindex)
  l <- nrow(df)
  df <- df[-(firstNA:l)]
  ##convert time from m:s format to s
  TIME_S <- lubridate::ms(df$TIME)
  TIME_S <- lubridate::period_to_seconds(TIME_S)
  df <- add_column(df, TIME_S, .after = "TIME") 
  ##change to POSIXct for graphing later
  df$TIME <- as.POSIXct(strptime(df$TIME, format= "%M:%S"))
  ##convert all to numeric, to character first to preserve factors
  df <-df %>% mutate(across(.cols = !TIME, ~ as.character(.x) %>% 
  as.numeric(.x) ) )
  ##rename problematic column names
  df <- df %>% rename('VE_VO2'=`VE/VO2`,'VE_VCO2'=`VE/VCO2`)
  
  df
  })

########################## Smoothing, computation of vars ######################
test_data <- lapply(test_data,function(df) {
  bf <- butter(3, 0.04, type= 'low')
  df$VO2_ABS_LOW <- signal::filtfilt(bf, df$VO2_ABS)
  df$VO2_REL_LOW <- signal::filtfilt(bf, df$VO2_REL)
  
  df$VCO2_LOW <- signal::filtfilt(bf, df$VCO2)
  
  df$VE_LOW <- signal::filtfilt(bf, df$VE)
  
  df$VE_VO2_LOW <- signal::filtfilt(bf, df$VE_VO2)
  
  df$VE_VCO2_LOW <- signal::filtfilt(bf, df$VE_VCO2)
  
  
  f15 <- rep(1/15,15)
  df$VO2_ABS_SMA <- stats::filter(df$VO2_ABS, f15, method = "convolution",
                                  sides = 2, circular = TRUE)
  
  f30 <- rep(1/30,30)
  df$VE_VO2_SMA <- stats::filter(df$VE_VO2, f30, method = "convolution",
                                 sides = 2, circular = TRUE)
  
  df$VE_VCO2_SMA <- stats::filter(df$VE_VCO2, f30, method = "convolution",
                                  sides = 2, circular = TRUE)
  
  #df$VE_VO2_LOW <- df$VE_LOW/df$VO2_ABS_LOW
  #df$VE_VCO2_LOW <- df$VE_LOW/df$VCO2_LOW
  
  
  df$EXCO2 <- ( ( (df$VCO2*df$VCO2)/df$VO2_ABS) - df$VCO2)
  df$EXVE <- ( ( (df$VE*df$VE)/df$VCO2) - df$VE)
  
  df$EXCO2_LOW <- ( ( (df$VCO2_LOW*df$VCO2_LOW)/df$VO2_ABS_LOW) - df$VCO2_LOW)
  df$EXVE_LOW <- ( ( (df$VE_LOW*df$VE_LOW)/df$VCO2_LOW) - df$VE_LOW)
  
  #no forgetti removi!!!
  df$HR <- 100
  
  df
})
############################# extend demo data #################################
demo_data <- mapply(df=test_data, dem=demo_data, SIMPLIFY = F,
                    FUN= function(df,dem){
                      ev_ex <- as.numeric(dem$EV_EX)*60
                      ev_cd <- as.numeric(dem$EV_CD)*60
                      beg <- which.max(df$TIME_S >= ev_ex)
                      end <- which.max(df$TIME_S >= ev_cd)
                      dem$EV_EX_I <- beg
                      dem$EV_CD_I <- end
                      dem$VO2MAX_ABS <- max(df$VO2_ABS_LOW)
                      dem$VO2MAX_REL <- max(df$VO2_REL_LOW)
                      dem$HRMAX <- max(df$HR)
                      dem$WORKMAX <- max(df$WORK)
                      dem
                    })
################################## Truncation ##################################
test_data_trunc <- mapply(df=test_data, dem=demo_data, SIMPLIFY = F,
                    FUN= function(df,dem){
                    
                    out <- df%>% slice(dem$EV_EX_I:dem$EV_CD_I)
                    
                    out
                  })

############################ Interpolation, Binning ############################

##5point
test_data_sec <- lapply(test_data_trunc, function(df){
  interpolate <- function(df) {
    ## first make sure data only contains numeric columns
    data_num <- df %>%
      dplyr::select(where(is.numeric))
    
      out <- lapply(data_num, function (i) approx(
        x = data_num[[1]],
        y = i,
        xout = seq(min(data_num[[1]]), max(data_num[[1]], na.rm = TRUE), 1)
      )$y
      ) %>%
        as.data.frame()
    
    out
  }
  out <- interpolate(df)
  out
  })

test_data_10bin <- lapply(test_data_sec, function(df){
  
  data_num <- df %>%
    dplyr::select(where(is.numeric))
  
  out <- data_num %>%
    dplyr::group_by(across(1, function(x) round(x / 10) * 10)) %>%
    dplyr::summarise_all(mean, na.rm = TRUE)
})

test_data_5bin <- lapply(test_data_sec, function(df){
  
  data_num <- df %>%
    dplyr::select_if(is.numeric)
  
  out <- data_num %>%
    dplyr::group_by(across(1, function(x) round(x / 5) * 5)) %>%
    dplyr::summarise_all(mean, na.rm = TRUE)
})

test_data_15bin <- lapply(test_data_sec, function(df){
  
  data_num <- df %>%
    dplyr::select_if(is.numeric)
  
  out <- data_num %>%
    dplyr::group_by(across(1, function(x) round(x / 15) * 15)) %>%
    dplyr::summarise_all(mean, na.rm = TRUE)
})

####################### Calculation of VT1, VT2 ################################

findchangepts_std <- function(x) {
  m <- nrow(x)
  n <- ncol(x)
  max_log_likelihood <- -Inf
  change_point <- 0
  for (i in 3:(n-2)) {
    log_likelihood <- 0
    for (j in 1:m) {
      region1 <- x[j, 1:(i-1)]
      region2 <- x[j, i:n]
      std1 <- sd(region1)
      std2 <- sd(region2)
      mean1 <- mean(region1)
      mean2 <- mean(region2)
      log_likelihood1 <- sum(dnorm(region1, mean = mean1, sd = std1,
                                   log = TRUE))
      log_likelihood2 <- sum(dnorm(region2, mean = mean2, sd = std2,
                                   log = TRUE))
      log_likelihood <- log_likelihood + log_likelihood1 + log_likelihood2
    }
    if (log_likelihood > max_log_likelihood) {
      max_log_likelihood <- log_likelihood
      change_point <- i
    }
  }
  return(change_point)
}

cps_input <- function(test_data){
  lapply(test_data, function(df){
  ##truncate dataframe to ranges in which VT1/VT2 can occur (NASA Paper,2021)
  vt1_i_beg <- which.min( abs(df$TIME_S-quantile(df$TIME_S,0.3)))
  vt1_i_end <- which.min( abs(df$TIME_S-quantile(df$TIME_S,0.8)))
  vt2_i_beg <- which.min( abs(df$TIME_S-quantile(df$TIME_S,0.5)))
  
  df_vt1 <- df %>% slice(vt1_i_beg:vt1_i_end)
  df_vt2 <- df %>% slice_tail(n=vt2_i_beg+1)
  ##breakpointanalyses
  ##VT1##
  ##V-Slope##
  vslop <- df_vt1 %>% select(VO2_ABS,VCO2) %>% as.matrix(.) %>% t(.)
  VT1_VSLOP_I <- findchangepts_std(vslop)+vt1_i_beg-1
  #EXCO2##
  exco2 <- df_vt1 %>% select(EXCO2) %>% as.matrix(.) %>% t(.)
  VT1_EXCO2_I <- findchangepts_std(exco2)+vt1_i_beg-1
  ##VT2##
  ##V-Slope##
  vslop2 <- df_vt2 %>% select(VCO2,VE) %>% as.matrix(.) %>% t(.)
  VT2_VSLOP_I <- findchangepts_std(vslop2)+vt2_i_beg-1
  ##EXVE##
  exve <- df_vt2 %>% select(EXVE) %>% as.matrix(.) %>% t(.)
  VT2_EXVE_I <- findchangepts_std(exve)+vt2_i_beg-1
  ##combine
  VT1_I <- round((VT1_EXCO2_I+VT1_VSLOP_I)/2)
  VT2_I <- round((VT2_EXVE_I+VT2_VSLOP_I)/2)
  VT1_TIME <- df$TIME_S[VT1_I]
  VT2_TIME <- df$TIME_S[VT2_I]
  VT1_VO2 <- df$VO2_ABS[VT1_I]
  VT2_VO2 <- df$VO2_ABS[VT2_I]
  VT1_WORK <- df$WORK[VT1_I]
  VT2_WORK <- df$WORK[VT2_I]
  VT1_HR <- df$HR[VT1_I]
  VT2_HR <- df$HR[VT2_I]
  
  VT1_VO2_PERC <- df$VO2_ABS[VT1_I]
  VT2_VO2_PERC <- df$VO2_ABS[VT2_I]
  VT1_WORK_PERC <- df$WORK[VT1_I]
  VT2_WORK_PERC <- df$WORK[VT2_I]
  VT1_HR_PERC <- df$HR[VT1_I]
  VT2_HR_PERC <- df$HR[VT2_I]
  
  df <- data.frame(VT1_EXCO2_I, VT1_VSLOP_I,VT2_EXVE_I, VT2_VSLOP_I,VT1_I,VT2_I,
                   VT1_TIME,VT2_TIME,VT1_VO2,VT2_VO2,VT1_WORK,VT2_WORK,VT1_HR,
                   VT2_HR,VT1_VO2_PERC,VT2_VO2_PERC,VT1_WORK_PERC,VT2_WORK_PERC,
                   VT1_HR_PERC,VT2_HR_PERC)
  df
    })
}

cps_5bin <- cps_input(test_data_5bin)
cps_10bin <- cps_input(test_data_10bin)
cps_15bin <- cps_input(test_data_15bin)
cps_sec <- cps_input(test_data_sec)
####################### Changepoints plotting ##################################

#plotting function
plist_cps_func <- function(test_data,cps_data){
  plist <- mapply(df=test_data, vt=cps_data, SIMPLIFY = F,
                  FUN = function(df,vt)
        {
         exco2 <- ggplot(df, aes(x=TIME_S))+
           geom_point(aes(y=EXCO2),colour='blue')+
           geom_vline(xintercept = df$TIME_S[vt$VT1_EXCO2_I], colour='green')+
           annotate(x=df$TIME_S[vt$VT1_EXCO2_I],y=+Inf,
                    label=paste0("VT1=",df$TIME_S[vt$VT1_EXCO2_I]," s"),
                    vjust=2,geom="label")+
           theme_bw()
         
         vslop1 <- ggplot(df, aes(x=VO2_ABS))+
           geom_point(aes(y=VCO2),colour='blue')+
           geom_vline(xintercept = df$VO2_ABS[vt$VT1_VSLOP_I], colour='green')+
           annotate(x=df$VO2_ABS[vt$VT1_VSLOP_I],y=+Inf,
                    label=paste0("VT1=",df$TIME_S[vt$VT1_VSLOP_I]," s"),
                    vjust=2,geom="label")+
           theme_bw()
         
         exve <- ggplot(df, aes(x=TIME_S))+
           geom_point(aes(y=EXVE),colour='blue')+
           geom_vline(xintercept = df$TIME_S[vt$VT2_EXVE_I], colour='green')+
           annotate(x=df$TIME_S[vt$VT2_EXVE_I],y=+Inf,
                    label=paste0("VT2=",df$TIME_S[vt$VT2_EXVE_I]," s"),
                    vjust=2,geom="label")+
           theme_bw()
         
         vslop2 <- ggplot(df, aes(x=VCO2))+
           geom_point(aes(y=VE),colour='blue')+
           geom_vline(xintercept = df$VCO2[vt$VT2_VSLOP_I], colour='green')+
           annotate(x=df$VCO2[vt$VT2_VSLOP_I],y=+Inf,
                    label=paste0("VT2=",df$TIME_S[vt$VT2_VSLOP_I]," s"),
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
              guides(colour = guide_legend(override.aes = list(size = 10) ) ) )+
          theme_bw()+
           guides(shape = guide_legend(override.aes = list(size = 1)))+
           guides(color = guide_legend(override.aes = list(size = 1)))+
           theme(legend.title = element_blank(),
                 legend.text = element_text(size = 10),
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
         
         out <- grid.arrange(grobs = plots, layout_matrix = lay)
         
         out
         
       })
  return(plist)
}

#create plots
plist_cps_5bin <- plist_cps_func(test_data_5bin,cps_5bin)
plist_cps_10bin <- plist_cps_func(test_data_10bin,cps_10bin)
plist_cps_15bin <- plist_cps_func(test_data_15bin,cps_15bin)
plist_cps_sec <- plist_cps_func(test_data_sec,cps_sec)

#marrange grobs, spread grobs over pages.
plots_cps_5bin <- marrangeGrob(plist_cps_5bin, nrow=1,ncol=1)
plots_cps_10bin <- marrangeGrob(plist_cps_10bin, nrow=1,ncol=1)
plots_cps_15bin <- marrangeGrob(plist_cps_15bin, nrow=1,ncol=1)
plots_cps_sec <- marrangeGrob(plist_cps_sec, nrow=1,ncol=1)


#export plots
ggsave("plots/5bin_cps.pdf", plots_cps_5bin, width = 11,
       height = 8.5, units = "in")
ggsave("plots/10bin_cps.pdf", plots_cps_10bin, width = 11,
       height = 8.5, units = "in")
ggsave("plots/15bin_cps.pdf", plots_cps_15bin, width = 11,
       height = 8.5, units = "in")
ggsave("plots/sec_cps.pdf", plots_cps_sec, width = 11,
       height = 8.5, units = "in")

ggsave("plots/parv_cps.pdf", plots_cps_parv, width = 11,
       height = 8.5, units = "in")

############################# Table summary ####################################

tbl_sum <- mapply(cp=cps_10bin,dem=demo_data,SIMPLIFY = F,FUN = function(cp,dem){
  
  cp$VT1_VO2_PERC <- cp$VT1_VO2_PERC/dem$VO2MAX_ABS
  cp$VT2_VO2_PERC <- cp$VT2_VO2_PERC/dem$VO2MAX_ABS
  
  cp$VT1_WORK_PERC <- cp$VT1_WORK_PERC/dem$WORKMAX
  cp$VT2_WORK_PERC <- cp$VT2_WORK_PERC/dem$WORKMAX
  
  cp$VT1_HR_PERC <- cp$VT1_HR_PERC/dem$HRMAX
  cp$VT2_HR_PERC <- cp$VT2_HR_PERC/dem$HRMAX
  cp
})

########################### Coggan Power Zones #################################

lapply(demo_data, function(df){
  
  lvl1_work <- round(df$WORKMAX*0.55,digits = 2) %>% as.character(.)
  lvl1_hr <- round(df$HRMAX*0.68,digits = 2) %>% as.character(.)
  
  lvl2_work_low <- round(df$WORKMAX*0.56,digits = 2) %>% as.character(.)
  lvl2_work_up <- round(df$WORKMAX*0.75,digits = 2) %>% as.character(.)
  lvl2_hr_low <- round(df$HRMAX*0.69,digits = 2) %>% as.character(.)
  lvl2_hr_up <- round(df$HRMAX*0.83,digits = 2) %>% as.character(.)
  
  lvl3_work_low <- round(df$WORKMAX*0.76,digits = 2) %>% as.character(.)
  lvl3_work_up <- round(df$WORKMAX*0.90,digits = 2) %>% as.character(.)
  lvl3_hr_low <- round(df$HRMAX*0.84,digits = 2) %>% as.character(.)
  lvl3_hr_up <- round(df$HRMAX*0.94,digits = 2) %>% as.character(.)
  
  lvl4_work_low <- round(df$WORKMAX*0.91,digits = 2) %>% as.character(.)
  lvl4_work_up <- round(df$WORKMAX*1.05,digits = 2) %>% as.character(.)
  lvl4_hr_low <- round(df$HRMAX*0.95,digits = 2) %>% as.character(.)
  lvl4_hr_up <- round(df$HRMAX*1.05,digits = 2) %>% as.character(.)
  
  lvl5_work_low <- round(df$WORKMAX*1.06,digits = 2) %>% as.character(.)
  lvl5_work_up <- round(df$WORKMAX*1.2,digits = 2) %>% as.character(.)
  lvl5_hr <- round(df$HRMAX*1.06,digits = 2) %>% as.character(.)

  
  lvl1 <- c(1,"Active Recovery",paste0("<",lvl1_work),paste0("<",lvl1_hr))
  lvl2 <- c(2,"Endurance",paste(lvl2_work_low,lvl2_work_up,sep = '-'),
            paste(lvl2_hr_low,lvl2_hr_up,sep = '-') )
  lvl3 <- c(3,"Tempo",paste(lvl3_work_low,lvl3_work_up,sep = '-'),
            paste(lvl3_hr_low,lvl3_hr_up,sep = '-') )
  lvl4 <- c(4,"Lactate Threshold",paste(lvl4_work_low,lvl4_work_up,sep = '-'),
            paste(lvl4_hr_low,lvl4_hr_up,sep = '-') )
  lvl5 <- c(5,"VO2 Max",paste(lvl5_work_low,lvl5_work_up,sep = '-'),
            paste0(">",lvl5_hr) )
  tr_zones <- as.data.frame(rbind(lvl1,lvl2,lvl3,lvl4,lvl5))
  colnames(tr_zones) <- c("Level","Name","Average Power","Average HR")
  rownames(tr_zones) <- NULL
  out <- tr_zones
  out
})


################################ Bland Altmann #################################

binder <- function(list,method){
  bind_rows(list) %>%
  select(VT1_TIME,VT2_TIME) %>% 
  add_column(ID= partnames,.before = "VT1")%>%
  add_column(method= method,.before = "VT1")
}
df_sec <- binder(cps_sec,"sec")
df_5bin <- binder(cps_5bin,"5bin")
df_10bin <- binder(cps_5bin,"10bin")
df_15bin <- binder(cps_5bin,"15bin")

df_big <- bind_rows(df_sec,df_5bin,df_10bin,df_15bin)
library(blandr)


##################################### WIP ######################################

mapply(df = test_data, cp = changepoints, 
       FUN = function(df,cp){
         
         
         vt1 <- cp$VT1_VO2/max(df$VO2_ABS)
         vt2 <- cp$VT2_VO2/max(df$VO2_ABS)
         
         df <- tibble(vt1,vt2)
       })


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



biglist <- mapply(function(x,y,z){list(test_data=x,demo_data=y,changepoints=z)},
            x=test_data,y=demo_data,z=changepoints, SIMPLIFY = F)

scale_x_datetime(date_labels = "%R")
