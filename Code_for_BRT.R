
rm(list=ls())
library(dplyr)

############## BRT #############################
source("/jane_1390_sm_appendixs/brt.functions.R")

mainDir <-"/noswitch/RWI/"
mainDir1 <- "/noswitch/data/"
mainDir2 <- "/noswitch/plot/"

setwd(mainDir)
filenames<-dir(mainDir,'*.csv')
filepath<-sapply(filenames,function(x){
  paste(mainDir,x,sep='')
})

region <- c('Arid','Humid')


func.sample <- function(i,r){
  
  savename <- substring(filenames[i],1,nchar(filenames[i])-4)  
  
  # create empty data.frames for relative contribution
  if  (substr(savename,1,2) == "ew"){
    mycontri <- data.frame(row.names = c('AWC','EM','CH','CEC',
                                         'CV_EP_CWD','CV_EP_SR','CV_EP_TMP','CV_EP_VPD',
                                         'CV_PN_CWD', 'CV_PN_SR','CV_PN_TMP','CV_PN_VPD',
                                         'Nm','P50','Pm','RD','SLA','TD'))
  }else{
    mycontri <- data.frame(row.names = c('AWC','EM','CH','CEC',
                                         'CV_EP_CWD','CV_EP_SR','CV_EP_TMP','CV_EP_VPD',
                                         'CV_LP_CWD', 'CV_LP_SR','CV_LP_TMP','CV_LP_VPD',
                                         'Nm','P50','Pm','RD','SLA','TD'))
  }
  
  
  # read my data
  mydt <- read.csv(filepath[i]) %>% 
    filter(!is.na(cv)) %>%
    .[.$AI==region[r],] %>%
    .[,c(35,12:20,22,36:43)]
  
  
  # brt models with 100 sampling
  for (t in 1:100){
    
    brt.dt <- mydt[sample(row.names(mydt),nrow(mydt),replace=T),]
    
    # 80% train set + 20% test set
    index <- sort(sample(nrow(brt.dt), nrow(brt.dt)*.8))
    train.set <- brt.dt[index,]
    test.set <-  brt.dt[-index,]
    
    # brt model
    rwi.tc5.lr005 <- gbm.step(data=train.set,
                              gbm.x = 2:19,gbm.y = 1,
                              family = "gaussian",
                              tree.complexity = 5,
                              learning.rate = 0.005,
                              bag.fraction = 0.5)
    
    # put the relative contribution into mycontri
    mycontri <- cbind(mycontri, 
                      rc=merge(rwi.tc5.lr005$contributions['rel.inf'], 
                               mycontri, 
                               by = 'row.names', 
                               all.x=TRUE)[,2])
    

  # output data
  write.table(mycontri, paste(mainDir1, savename,'_',region[r], "_contri.csv", sep=""),
              sep=",")

  }
  return(mycontri)
}

ew_a <- func.sample(1,1)

ew_h <- func.sample(1,2)

lw_a <- func.sample(2,1)

lw_h <- func.sample(2,2)

# outout all data
save.image(paste(mainDir1, "alldt.RData", sep=""))

############## Figure 2 ##############
mainDir <- "/noswitch/data/"
mainDir1 <- "/noswitch/plot/"


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
    # ylim(0, 25)+
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

save_plot(paste(mainDir1,'Figure 2 Violin.jpg',sep=""), p,
          base_height = 10,
          base_width = 20) 
save_plot(paste(mainDir1,'Figure 2 Violin.pdf',sep=""), p,
          base_height = 10,
          base_width = 20) 
