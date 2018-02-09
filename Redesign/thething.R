setwd("~/SP 15/STAT 515/Redesign")


library(micromapST)

micromapSTprep <- function(stateDF,stateId=NULL,
                           ref=stateNamesFips){
  if(is.null(stateId)) nam <- row.names(stateDF) else
    nam <- stateDF[,stateId]
  nam <- ifelse(nam=="District of Columbia","D.C.",nam)
  check <-  match(nam,row.names(ref)) 
  bad <- is.na(check)
  good <- !bad
  nbad <- sum(bad)
  if(nbad>0){
    warning(paste(nbad,"Unmatch Names Removed",nam[bad])) 
    stateDF <- stateDF[!bad,]
    nam <- nam[!bad]
    check <- check[!bad]
    good <- good[!bad]
  }
  ngood <- sum(good)
  if(ngood < 51)warning(paste("Only",ngood,"State Ids"))
  row.names(stateDF) <- ref[check,2]
  return(stateDF)
}

data <- read.csv(file = 'cleaned data.csv', header = T, as.is = TRUE)

feesbystate <- micromapSTprep(data, "State")
feesbystate[3:13]<- (feesbystate[3:13])/(1000)


colNumbers <- 1:ncol(data)
names(colNumbers)=colnames(data)
colNumbers

panelDesc <- data.frame(
  type=c('map','id','dot','dot','dot','dot'),
  lab1=c('' ,'','2004','2014','Ave 1 Yr % Change','Ave 5 Yr % Change'),
  lab2=c('' ,'','(in thousands)','(in thousands)','',''),
  lab3=c('','','Possible $3k-$9k','Possible $4k-$14k',
         ' ',''),
  col1 = c(NA,NA,3,12,"oneyear","fiveyear"))

t(panelDesc)



pdf("trial1.pdf",
    width=7.5,height=10)
micromapST(feesbystate, panelDesc,
           sortVar=15,ascend=TRUE,
           title=c("Tuition changes over ten years",
                   "by state"))
dev.off()


