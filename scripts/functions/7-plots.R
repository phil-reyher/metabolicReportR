create_gxt_plots <- function(dataList,metadataList,vtDataList){
  gxtPlots <- mapply(df=dataList,meta=metadataList,vt=vtDataList,
  SIMPLIFY = F, FUN=function(df,meta,vt){
    
    plot <-  ggplot(df,aes(x=time))+
      geom_vline(xintercept = vt$vt1_time)+
      annotate(x=vt$vt1_time,y=+Inf,
              label="VT1",vjust=2,geom="label")+
      
      geom_vline(xintercept = vt$vt2_time)+
      annotate(x=vt$vt2_time,y=+Inf,
              label="VT2",vjust=2,geom="label")+
      
      geom_vline(xintercept = df$time[meta$startExerciseIndex])+
      annotate(x=df$time[meta$startExerciseIndex],y=+Inf,
              label="Start",vjust=2,geom="label")+
      
      geom_vline(xintercept = df$time[meta$endExerciseIndex])+
      annotate(x=df$time[meta$endExerciseIndex],y=+Inf,
              label="Cooldown",vjust=2,geom="label")+
      
      geom_line(aes(y=vo2absLow,group=1, colour='VO2'))+
      guides(color = guide_legend(override.aes = list(size = 1.5)))+
      labs(color="Measurement")+
      geom_line(aes(y=vco2Low,group=2, colour='VCO2'))+
      geom_area(aes(y = (work/100)), fill ="lightblue", group=3, alpha = 0.4 ) +
      scale_color_manual(name='Measurement',
                        breaks=c('VO2', 'VCO2', 'VO2', 'WORK'),
                        values=c('VO2'='green', 'VCO2'='red', 'WORK'='blue'))
    plot
  })
return(gxtPlots)
}

create_threshold_plots <- function(dataList,vtDataList){
  plist <- mapply(df=dataList, vt=vtDataList, SIMPLIFY = F,
  FUN = function(df,vt) {
    plotExco2 <- ggplot(df, aes(x=time))+
      geom_point(aes(y=exco2),colour='blue')+
      geom_vline(xintercept = df$time[vt$VT1EXCO2_I], colour='green')+
      annotate(x=df$time[vt$VT1EXCO2_I],y=+Inf,
               label=paste0("VT1=",df$time[vt$VT1EXCO2_I]," s"),
               vjust=2,geom="label")+
      theme_bw()
    
    plotVslope <- ggplot(df, aes(x=vo2abs))+
      geom_point(aes(y=vco2),colour='blue')+
      geom_vline(xintercept = df$vo2abs[vt$VT1VSLOP_I], colour='green')+
      annotate(x=df$VO2_ABS[vt$VT1VSLOP_I],y=+Inf,
               label=paste0("VT1=",df$time[vt$VT1VSLOP_I]," s"),
               vjust=2,geom="label")+
      theme_bw()
    
    plotExVe <- ggplot(df, aes(x=time))+
      geom_point(aes(y=exve),colour='blue')+
      geom_vline(xintercept = df$time[vt$VT2EXVE_I], colour='green')+
      annotate(x=df$time[vt$VT2EXVE_I],y=+Inf,
               label=paste0("VT2=",df$time[vt$VT2EXVE_I]," s"),
               vjust=2,geom="label")+
      theme_bw()
    
    plotVslope2 <- ggplot(df, aes(x=vco2))+
      geom_point(aes(y=ve),colour='blue')+
      geom_vline(xintercept = df$vco2[vt$VT2VSLOP_I], colour='green')+
      annotate(x=df$vco2[vt$VT2VSLOP_I],y=+Inf,
               label=paste0("VT2=",df$time[vt$VT2VSLOP_I]," s"),
               vjust=2,geom="label")+
      theme_bw()
    
    plotVentiEquiv <- ggplot(df, aes(x=time))+
      coord_cartesian(xlim = c(300, 1100),ylim = c(7.5,45))+
      scale_x_continuous(name="Time (s)",
                         breaks=seq(300,1150,50) )+
      scale_y_continuous(name="VE/VO2 | VE/VCO2", breaks=seq(10,45,5),
                         sec.axis = sec_axis(~.*10,name='Work' ) )+
      geom_point(aes(y=vevo2, colour='VE/VO2') )+
      geom_point(aes(y=vevco2, colour='VE/VCO2') )+
      geom_vline(xintercept = df$time[vt$VT1_I],colour='black',
                 linetype = "dotted")+
      annotate(x=df$time[vt$VT1_I],y=+Inf,
               label=paste0("VT1=",df$time[vt$VT1_I]," s"),
               vjust=2,geom="label")+
      geom_vline(xintercept = df$time[vt$VT2_I],colour='green',
                 linetype = "longdash")+
      annotate(x=df$time[vt$VT2_I],y=+Inf,
               label=paste0("VT2=",df$time[vt$VT2_I]," s"),
               vjust=2,geom="label")+
      geom_area(aes(y = (work/10),colour="Work"), fill ="lightblue", 
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
    
    plots <- list(plotExco2,plotVslope,plotExVe,plotVslope2,plotVentiEquiv)
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
