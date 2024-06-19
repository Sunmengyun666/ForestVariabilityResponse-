
rm(list=ls())
library(dplyr)
install.packages(gbm)

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
  
  # create empty data.frames for plotting relative contribution and partial dependent plot
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

  pred_cv <- list(rep(NA,3))
  n.plots <- nrow(mycontri)
  pred1 <- list(rep(NA,n.plots))
  p1 <- list(rep(NA,n.plots))

  
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
    
    pp <- predict.gbm(rwi.tc5.lr005, test.dt, n.trees=rwi.tc5.lr005$gbm.call$best.trees, type="response")

    cor <- cor.test(pp,test.dt$cv,method="pearson")

    ss <- data.frame(R2 = cor[["estimate"]][["cor"]]^2,
                     p = cor[["p.value"]])
    if (cor[["p.value"]] < 0.05){
      ss[1,3] <- 'Y'
    }

    if (t==1){
      pred_cv[[1]] <- data.frame(n=pp)
      pred_cv[[2]] <- data.frame(n=test.dt$cv)
      pred_cv[[3]] <- ss
      if (cor[["p.value"]] < 0.05){
        pred_cv[[3]][t,3] <- 'Y'
      }
    }else{
      pred1[[b]] <- cbind(pred1[[b]],n=resp.matrix[,1])
      p1[[b]] <- cbind(p1[[b]],n=resp.matrix[,2])
      pred_cv[[1]] <- cbind(pred_cv[[1]],n=pp)
      pred_cv[[2]] <- cbind(pred_cv[[2]],n=test.dt$cv)
      pred_cv[[3]] <- rbind(pred_cv[[3]],ss)
    }


    # PDP
    pred.names <- rwi.tc5.lr005$gbm.call$predictor.names

    for (b in 1:n.plots){
      resp.matrix <- plot.gbm(rwi.tc5.lr005, i.var = b,
                              n.trees = rwi.tc5.lr005$n.trees,
                              return.grid = TRUE)
      if (t==1){
        pred1[[b]] <- data.frame(n=resp.matrix[,1])
        p1[[b]] <- data.frame(n=resp.matrix[,2])
      }else{
        pred1[[b]] <- cbind(pred1[[b]],n=resp.matrix[,1])
        p1[[b]] <- cbind(p1[[b]],n=resp.matrix[,2])
      }
    }
  }

  # rename
  names(pred_cv)[1:3] <- c("pred", "cv",'cor')

  for (x in 1:length(pred1)){
    names(pred1)[x] <- pred.names[x]
    names(p1)[x] <- pred.names[x]
  }
  
  # output data
  write.table(mycontri, paste(mainDir1, savename,'_',region[r], "_contri.csv", sep=""),
              sep=",")
  # write.xlsx(pred_cv, paste(mainDir1, savename,'_',region[r], "_predcv.xlsx", sep=""),
  #            row.names=FALSE)
  # write.xlsx(pred1,paste(mainDir1, savename,'_',region[r], "_pdp_x.xlsx", sep=""))
  # write.xlsx(p1,paste(mainDir1, savename,'_',region[r], "_pdp_y.xlsx", sep=""))


  }
  mydt <- list(mycontri = mycontri, pred_cv = pred_cv,
               pred1 = pred1, p1 = p1)
  return(mydt)
}

ew_a <- func.sample(1,1)

ew_h <- func.sample(1,2)

lw_a <- func.sample(2,1)

lw_h <- func.sample(2,2)

# outout all data
save.image(paste(mainDir1, "alldt.RData", sep=""))
