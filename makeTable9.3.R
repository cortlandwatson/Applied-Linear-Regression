makeTable9.3 <- function(thedata, nbest=1){
  
  if (nbest < 1 | nbest > dim(thedata)[2]-1){
    warning("nbest recoded to 1")
    nbest <- 1
  }
  
  cnames <- colnames(thedata)

  Yat <- grep("Y",cnames)
  
  if (length(Yat)>1)
    stop("Currently you have: ", paste(cnames[Yat], ", ", sep=""), 
               " in your data set.\n There can be only one response variable.\n",
               sep="")

  colnames(thedata)[Yat] <- "Y"
  Xat <- grep("X",cnames)
  
  require("leaps")
  require("qpcR")

  reg <- regsubsets(thedata[,Xat],thedata[,Yat], nbest=nbest, nvmax=length(cnames)-1)

  regs <- summary(reg)
  
  models <- sapply(1:length(row.names(regs$which)), function(i){
    colnames(regs$which)[which(regs$which[i,])][-1]
  })
  
  pressdata <<- thedata
  
  PRESS <- sapply(models, function(model){
    tmpf <- formula(paste("Y~",
                          paste(model,
                                c(rep("+",length(model)-1), ""), 
                                sep="", 
                                collapse=""), 
                          sep=""))
    tmp <- lm(tmpf, data=pressdata)
    tmp$call$formula <- eval(tmpf)
    PRESS(tmp, verbose=FALSE)$stat
  })
  
  n <- nrow(thedata)
  p <- as.numeric(row.names(regs$which))+1
 
  
  AIC <- regs$bic-log(n)*p-2*p

  
  AIC2 <- sapply(models, function(model){
    tmpf <- formula(paste("Y~",
                          paste(model,
                                c(rep("+",length(model)-1), ""), 
                                sep="", 
                                collapse=""), 
                          sep=""))
    tmp <- lm(tmpf, data=pressdata)
    tmp$call$formula <- eval(tmpf)
    AIC(tmp)
  })
  
  tmp <- data.frame(p=p, 
             model= sapply(1:length(models), function(i) {
                      paste(models[[i]], collapse=",")
                    }),
             SSEp=regs$rss,
             R2p=regs$rsq,
             R2ap=regs$adjr2,
             Cp=regs$cp,
             AICp=AIC2,
             SBCp=regs$bic,
             PRESSp=PRESS)
  
  rbind(data.frame(p=1,
                   model="(intercept)",
                   SSEp=regs$rss[1]/(1-regs$rsq[1]),
                   R2p=0,
                   R2ap=0,
                   Cp=NA,
                   AICp=NA,
                   SBCp=NA,
                   PRESSp=NA),tmp)
}



