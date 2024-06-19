rm(list=ls())
library(ggplot2)
library(plyr)
library(cowplot)

mainDir <- "/result/data/"
mainDir1 <- "/result/plot/"


setwd(mainDir)
filenames<-dir(mainDir,pattern = "*.csv")
filepath<-sapply(filenames,function(x){
  paste(mainDir,x,sep='')
})

my_plot <- function(i){
  dt <- read.csv(filepath[i],row.names = NULL)
  colnames(dt)[1] <- 'fac'
  
  if (i<3){
    dt$fac <- factor(dt$fac, 
                     levels = c('SLA','Pm','Nm',
                                'VPD_PN_CV','VPD_EP_CV',
                                'SR_PN_CV','SR_EP_CV',
                                'CWD_PN_CV','CWD_EP_CV',
                                'TMP_PN_CV','TMP_EP_CV',
                                'EM','CEC','AWC','TD',
                                'CH','RD','P50'
                     ))
  }else{
    dt$fac <- factor(dt$fac, 
                     levels = c('SLA','Pm','Nm',
                                'VPD_LP_CV','VPD_EP_CV',
                                'SR_LP_CV','SR_EP_CV',
                                'CWD_LP_CV','CWD_EP_CV',
                                'TMP_LP_CV','TMP_EP_CV',
                                'EM','CEC','AWC','TD',
                                'CH','RD','P50'))
    
  }
  
  dt <- dt[order(dt$fac),]
  
  dt$var <- c(rep('leaf',times=3),rep('clim',times=8),
              rep('site',times=4),rep('hydro',times=3))
  
  sum_dt<-reshape2::melt(dt,id.vars=c("fac",'var'),variable.name="times",value.name="rc")
  
  
  p <- ggplot(sum_dt, aes(x = fac, y = rc, fill=var))+
    geom_violin(width=1,color="white")+
    geom_boxplot(width=0.2,cex=0.4, position=position_dodge(0.9),outlier.size=1 
                 # ,notch = T, notchwidth = 0.7
    )+ 
    stat_boxplot(geom = 'errorbar',width=0.1,cex=0.4)+
    ylab('Relative contribution (%)')+xlab('')+
    scale_fill_manual(values = c("#0d898a","#5494cc","#f9cc52","#e18283"))+
    coord_flip()+
    theme_bw()+
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x  = element_line(size=0.5, linetype = 'dashed'),
      panel.border = element_rect(color="black", size=1),
      axis.ticks.length.x = unit(0.2, "cm"),
      axis.ticks.length.y = unit(0.2, "cm"),
      axis.ticks.x = element_line(color="black",size=1,lineend = 1),
      axis.ticks.y = element_line(color="black",size=1,lineend = 1),
      axis.text.x = element_text(size=16,color = "black"),
      axis.text.y = element_text(size=16,color = "black"),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      legend.position="none")
}

my_list <- lapply(1:4, my_plot) 

p <- plot_grid(plotlist = my_list, nrow = 1, ncol = 4,
               labels = "auto",label_size = 20,
               scale = c(0.9))
p

save_plot(paste(mainDir1,'Fig2 Violin.jpg',sep=""), p,
          base_height = 10,
          base_width = 20) 
save_plot(paste(mainDir1,'Fig2 Violin.pdf',sep=""), p,
          base_height = 10,
          base_width = 20) 
