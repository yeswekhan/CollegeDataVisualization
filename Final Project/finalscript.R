setwd("~/SP 15/STAT 515/Final Project")

#Section 1: reading in the data
#http://cran.r-project.org/web/packages/ISLR/ISLR.pdf
#data taken from CMU, taken from US News&World Report

library(ISLR)
library(xlsx)
College
summary(College)
#write.xlsx(College, "College.xlsx")
colleges <- read.csv("College.csv")
colleges
summary(colleges)



#Section 2: Setup
library(ggplot2)
library(grid)
library(lattice)
library(MASS)
library(hexbin)
library(randomForest)
library(MASS)
library(car)
library(ellipse)


hw <- theme_gray() + theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.50)),
  axis.text=element_text(colour="black"),
  axis.ticks=element_blank(),
  axis.ticks.margin=unit(-0.05,"cm"),
  panel.grid.minor = element_blank()
)

#Section 3: densities of percent alumni who donate

ggplot(colleges, aes(x = perc.alumni)) + geom_density(fill = "blue") + hw + labs(y="Density",x="Percent of Alumni Who Donate",
                                                                                title="Density of Percent Alumni Who Donate")

ggplot(colleges, aes(x = perc.alumni)) + geom_density(aes(fill = factor(Private))) + hw+ labs(y="Density",x="Percent of Alumni Who Donate",
                                                                                              title="Density of Percent Alumni Who Donate, By School Type")

#Section 4: pairing quantiles of the data

x <- colleges$perc.alumni[colleges$Private=="Yes"]
y <- colleges$perc.alumni[colleges$Private=="No"]
qqplot(x,y,las=1, xlab="Private",ylab="Public",
       main="Q-Q Plot for Percent Alumni")

n <- min(length(x),length(y))
probs <- seq(0,1,length=n)
qx <- quantile(x,probs=probs)
qy <- quantile(y,probs=probs)
df <- data.frame(qx,qy)

ab <- coef(rlm(qy~qx))

rx <- range(colleges$perc.alumni)

ggplot(df,aes(x=qx,y=qy))+
  geom_abline(intercept=0,slope=1,col="black")+
  geom_abline(int=ab[1],sl=ab[2],
              size=1.5,col=rgb(0,.8,0))+
  geom_point(size=3.2,col="blue")+
  xlim(rx)+ylim(rx)+
  labs(x="Private Schools",y="Public Schools",
       title="Q-Q Plot of Percent Alumni Who Donate")+
  hw

qMean <- (qx + qy)/2
qDiff <- qx - qy
df <- data.frame(qMean,qDiff)
rFit <- rlm(qDiff~qMean)$coef

ggplot(df,aes(x=qMean,y=qDiff))+
  geom_hline(yint=mean(qDiff),size=1.2)+
  geom_abline( int=rFit[1],sl=rFit[2],
               size=1,col=rgb(0,.8,0))+
  geom_point(col="blue",size=3.2)+
  labs(x="Mean of Percent Alumni Who Donate",
       y="Public - Private",
       title="MD Plot of Paired Private and Public Quantiles")+
  hw

#Section 5: Boxplots
ggplot(data=colleges, aes(x=Private,y=Tot.Exp)) +
  geom_boxplot(width=.6,col="blue",
               outlier.colour='orange',outlier.size=3.5, lwd=.1)+
  coord_flip() + hw + labs(x="Private",y="Total Expense per Student in Dollars",
                           title="Boxplot of Total Expense per Student by Type of School")


#Section 6: Redesign tuitions

#Section 7: scatterplot matrix

c <- data.frame(colleges$perc.accept, colleges$Top10perc, colleges$perc.alumni)

myPanel <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.points(x,y,...,pch=21,fill=rgb(0,.9,0),col="black")
  panel.loess(x , y, ..., lwd=3,col='purple')
}


windows(width=8,height=8) 
splom(c,
      varnames=c("Acceptance Rate",
                 "Percent that \nwas top 10%",
                 "Percent Alumni\n that Donate"),
      xlab='',main="College Data",
      pscale=4, varname.cex=0.8,axis.text.cex=0.6,
      axis.text.col="red",axis.text.font=2,
      axis.line.tck=.5,
      panel=function(x,y,...){
        panel.grid(h=-1,v=-1,...)
        panel.hexbinplot(x,y,...,border=gray(.7),trans=function(x)x^.2)
        panel.loess(x , y, ..., lwd=2,col='purple')
      },
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm=TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d,col=gray(.8),lwd=2)
        diag.panel.splom(x, ...)
      }
)

#Section 8: importance (see Week 10)


set.seed(4543)
collegeRF <- randomForest(x = colleges[ , 2:20], y=colleges[, 21],
                        importance=TRUE,  proximity=FALSE, ntree=500,
                        keepForest=TRUE)
collegeRF

dep <- colleges[, 21]
sc <- diff(range(dep))
sc
scaledRes <- (dep - predict(collegeRF))/sc 
mean(abs(scaledRes)) # 0.026

imp <- importance(collegeRF)
imp
varImpPlot(collegeRF,cex=.8)




#Section 9: color correlation (see Week 10)



subs <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
collegedat <- colleges[,subs]
corMat <- cor(collegedat)   # get correlations
x <- cmdscale(1-abs(corMat), k=1)     # seriation 
ord <- rev(order(x))
corMatOrd <- corMat[ord, ord]
round(corMatOrd, 2)

# define colors
mat <- matrix(c(
  120,  60, 180, 
  175, 141, 195, 
  231, 212, 232, 
  255, 255, 255,
  217, 240, 211,
  127, 191, 123, 
  40,  140, 100), ncol=3, byrow=TRUE)
colors=rgb(mat[, 1], mat[, 2], mat[, 3], max=255)

# assign color classes

tmp <- as.vector(corMatOrd)
brk <- c(-1.01, -.70 , -.40, -.10, .10, .40, .70,  1.01)
colorSub <- cut(tmp, brk, label=FALSE)

# plot

windows(w=9, h=7)
par(mai=c(.4, .5, .3, .2))
nr <- nrow(corMatOrd)
x <- 1:nr-.5
cen <- expand.grid(list(y=rev(x), x=x))
plot(c(-10, nr+2), c(0, nr+.3), type='n', axes="FALSE", ylab='')
mtext(side=3,line=0,
      'Diverging Scale:  Shades of purple negative, Shades of green positive')
mtext(side=1, line=0, 
      'Seriation by 1D MDS, Pearson Distance: 1-abs(cor)')
# could use image()
rect(cen$x-.5, cen$y-.5, cen$x+.5, cen$y+.5, col=colors[colorSub],
     border=rgb(.2, .2, .2))
text(rep(-10.5, 35), y=rev(x), rownames(corMatOrd), adj=0, cex=.75)
