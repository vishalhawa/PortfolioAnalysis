require(graphics)

rfpdata = read.table("C:\\Users\\vhawa\\Dropbox\\Projects\\Ticket Analysis\\RFP Info.csv", header = TRUE,sep=",")

rfpdata$Approx.Number.of.Users <- as.numeric(rfpdata$Approx.Number.of.Users)
rfpdata$App.Size <- as.factor(rfpdata$App.Size)

rfpdata$Average.MOnthly.Ticket.Volume.Incidents <- as.numeric(rfpdata$Average.Monthly.Ticket.Volume.Incidents)

rfpdata$Application.Complexity = factor(rfpdata$Application.Complexity,levels=c("Low","Medium","High"))


rfpdata$App.Size=mapply(FUN=appsize,rfpdata$Approx.Number.of.Users, rfpdata$Application.Complexity,rfpdata$Average.Monthly.Ticket.Volume.Incidents)
rfpdata$App.Score=mapply(FUN=appscore,rfpdata$Approx.Number.of.Users, rfpdata$Application.Complexity,rfpdata$Average.Monthly.Ticket.Volume.Incidents)

distanceMatrix <- dist(rfpdata[,c("App.Score")])
hclustering <- hclust(distanceMatrix)
plot(hclustering)

source("http://dl.dropbox.com/u/7710864/courseraPublic/myplclust.R")
myplclust(hclustering,lab.col=Service.entitlement)

# Cluster package 
require(cluster)
dclust = daisy(rfpdata[,c("Platform.OS.Type","Service.entitlement","Region","Bus.Area","App.Score")])
agn <- agnes(dclust)
agn
plot(agn)
d2 <- as.dendrogram(agn)
d2[[1]] # the first branch
str(d2)
plot(d2)
rect.hclust(agn, 4)
rfpdata$groups <- cutree(agn, k=10) # cut tree into 10 clusters

library(tree)
tree1 <- tree(groups ~  Service.entitlement + Region + Bus.Area + App.Score,data=rfpdata)
summary(tree1)
plot(tree1)
text(tree1)

write.csv(rfpdata,file="C:\\Users\\vhawa\\Dropbox\\Projects\\Ticket Analysis\\rfpprocessed.csv")

plot( rfpdata$App.Size,rfpdata$Service.entitlement)
text(rfpdata$App.Size,rfpdata$Service.entitlement,round(rfpdata$Service.entitlement, 2), cex=0.45)

library(ggplot2)
ggplot(data=dat, aes(x = x, y = y, label = text))  + geom_point() + geom_text(size=4, hjust = 1, vjust = 1)))
  
# rfpdata<- read.table("clipboard")


appsize <- function(users,complexity, volume){

 # print(users)
  
  if(users >10000 | complexity == "High"| volume>200 ) return ("large")
  
  if( users >500 | complexity == "Medium" | volume>50 )  return ("medium")   else   return ("small")
}

appscore<-function(users,complexity, volume){
  
  scaledusers = users/3333
  scaledvolume = volume/67
  scaledcomplexity = as.numeric(complexity) 
  
  return(scaledusers*scaledvolume*scaledcomplexity)
  
}

appscore(3,"Low",20)

lapply(rfpdata,FUN=appsize(Approx.Number.of.Users,Application.Complexity,Average.MOnthly.Ticket.Volume.Incidents))

appsize(rfpdata$Approx.Number.of.Users,rfpdata$Application.Complexity,rfpdata$Average.MOnthly.Ticket.Volume.Incidents)

# *** _____________________________***

sdf<-df[3:21]
sdf[order(sdf$Open.Date)]
 class(as.date-time(sdf$Open.Date ))

 
p1df<-sdf[sdf$Priority=='P1',]
p2df<-sdf[sdf$Priority=='P2',]


h<-hist(as.Date(sdf$Open.Date,"%m/%d/%y"),breaks="days",freq=TRUE,format="%b-%d",col="Blue",main="Ticket Frequency by Day",xlab="Date (Days)")

abline(v=as.Date("4/30/2013","%m/%d/%y"), col="Orange",lwd=3)
text(as.Date("4/30/2013","%m/%d/%y"),30, labels="Hard Launch",col="Orange",cex=.8,font=3,lwd=30)

# min(as.Date(sdf$Open.Date,"%m/%d/%y"))


xvals<- c(min(as.Date(sdf$Open.Date,"%m/%d/%y")):min(as.Date(sdf$Open.Date,"%m/%d/%y"))+124)
lines(h$breaks,dpois(h$breaks, lambda=h$breaks[1]+4.105634))



x.poi<-rpois(n=200,lambda=4)
plot(density(x.poi))
#curve(dpois(x, lambda = 4))


curve(dpois(x,lambda=4),main="Poisson distribution",from=0,to=5,add=TRUE,lwd=3,lty=2,col=2)

h1<-hist(as.Date(p1df$Open.Date,"%m/%d/%y"),breaks="days",freq=TRUE,format="%b-%d",col="red",main="Ticket Frequency P1 by Day",xlab="Date (Days)")

h2<-hist(as.Date(p2df$Open.Date,"%m/%d/%y"),breaks="days",freq=TRUE,format="%b-%d",col="Green",main="Ticket Frequency P2 by Day",xlab="Date (Days)")

# A graphical technique to evaluate the goodness of fit can be drawing pdf curve and histogram together

diffDate=as.Date(sdf$Open.Date,"%m/%d/%y")-min(as.Date(sdf$Open.Date,"%m/%d/%y"))
x<-seq(min(as.Date(sdf$Open.Date,"%m/%d/%y")),max(as.Date(sdf$Open.Date,"%m/%d/%y")),length=100)
y<-dpois(xfit,lambda=1)


xfit<-h$breaks-min(h$breaks)
yfit<h$counts

# Estimating Lambda
poidist=fitdistr(yfit,"Poisson")
# through MLE it does not support
nLL <- function(lambda) -sum(stats::dpois(yfit, lambda, log=TRUE))
est = mle(minuslogl=nLL, start=list(lambda=2))
summary(est)

t<-diff(as.Date(sdf$Open.Date,"%m/%d/%y"))
 
# --- Spare -- 
plot(density(as.Date(sdf$Open.Date,"%m/%d/%y"))) 

 strptime(sdf$Open.Date,tz="EST5EDT",format="%Y-%m-%d")
# becasue the last two are grand totoal and blank
#kClust <- kmeans(sdf,centers=3,nstart=10)

#distanceMatrix <- dist(sdf)
#hclustering <- hclust(distanceMatrix)

#table(kClust$cluster,samsungData$activity[samsungData$subject==1])

 
#sdf1<-sum(df[,"BDS"]==3 & df[,"RMS"]==4,na.rm=TRUE)
#sdf2<-sum(df[,"BDS"]==2 & df[,"RMS"]==5,na.rm=TRUE)
#sdf3<-sum(df[,"BDS"]==2 & df[,"RMS"]==7,na.rm=TRUE)

#agricultureLogical<-(df[,"ACR"]==3 & df[,"AGS"]==6)
#indexes=which(agricultureLogical)
 
#subsetDataFrame  = df[indexes,] 
#sum(is.na(subsetDataFrame$MRGX))
#(subsetDataFrame[,"MRGX"]==NA)
#download.file("https://dl.dropbox.com/u/7710864/data/csv_hid/ss06pid.csv ","C:\\Users\\vhawa\\Dropbox\\Study\\Data Analysis - R\\Data Analysis -Advanced\\POPULATION.csv")

#populationData<-read.csv("C:\\Users\\vhawa\\Dropbox\\Study\\Data Analysis - R\\Data Analysis -Advanced\\POPULATION.csv")
#cdf<-merge(populationData,df,by="SERIALNO")

