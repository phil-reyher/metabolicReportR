create_gxt_plots <- function(dataList,metadataList,vt2DataList){
  gxtPlots <- mapply(df=dataList,dem=metadataList,vt=vt2DataList,SIMPLIFY = F,
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
return(gxtPlots)
}

create_threshold_plots <- function(test_data,cps_data){
  plist <- mapply(df=test_data, vt=cps_data, SIMPLIFY = F,
  FUN = function(df,vt) {
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
      scale_color_manual(name=' ',breaks=c('VE/VO2', 'VE/VCO2', 'Work'),
                         values=c('VE/VO2'='blue', 'VE/VCO2'='red',
                                  'Work'='lightblue'),
                         guides(colour = guide_legend(
                                override.aes = list(size = 8) ) ) )+
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
