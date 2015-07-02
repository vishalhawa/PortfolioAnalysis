require(graphics)


# ------------------------------------------------incidents ------------------------------------

incidentsdf = read.csv("C:\\Users\\vhawa\\Dropbox\\Projects\\RCode\\ticketAnalysis\\incidents.csv")

incidentsdf$Incident.Reported.Date.Time = strptime(incidentsdf$Incident.Reported.Date.Time , format="%m/%d/%y %I:%M %p")

incidentsdf = incidentsdf[order(incidentsdf$Incident.Reported.Date.Time) , ]

timeDiff =	aggregate( incidentsdf$"Incident.Reported.Date.Time", by= list(incidentsdf$"Region"), FUN=diff )  

timeDiff$sd = mapply(sd, timeDiff$x, na.rm=TRUE)
timeDiff$mean = mapply(mean, timeDiff$x, na.rm=TRUE)
 
timeDiff$CV = timeDiff$sd/timeDiff$mean


# ------------------------------------------------Clustering ------------------------------------
rfpdata = read.table("C:\\Users\\vhawa\\Dropbox\\Projects\\RCode\\ticketAnalysis\\RFP Info.csv", header = TRUE,sep=",")
rfpdata$Approx.Number.of.Users <- as.numeric(rfpdata$Approx.Number.of.Users)
rfpdata$App.Size <- as.factor(rfpdata$App.Size)

rfpdata$Average.MOnthly.Ticket.Volume.Incidents <- as.numeric(rfpdata$Average.Monthly.Ticket.Volume.Incidents)

rfpdata$Application.Complexity = factor(rfpdata$Application.Complexity,levels=c("Low","Medium","High"))
rfpdata$Service.entitlement = factor(rfpdata$Service.entitlement,levels=c("Bronze","Silver","Gold"))

rfpdata$App.Score=as.numeric(as.numeric(rfpdata$Service.entitlement)*(as.numeric(rfpdata$Application.Complexity))*scale(rfpdata$Approx.Number.of.Users)*scale(rfpdata$Average.Monthly.Ticket.Volume.Incidents))

rfpdata$App.Size=mapply(FUN=appsize,rfpdata$Approx.Number.of.Users, rfpdata$Application.Complexity,rfpdata$Average.Monthly.Ticket.Volume.Incidents)
#rfpdata$App.Score=mapply(FUN=appscore,rfpdata$Approx.Number.of.Users, rfpdata$Application.Complexity,rfpdata$Average.Monthly.Ticket.Volume.Incidents)

# distanceMatrix <- dist(rfpdata[,c("App.Score")])
# hclustering <- hclust(distanceMatrix)
# plot(hclustering)

# Cluster package 
require(cluster)
dclust = daisy(rfpdata[,c("Platform.OS.Type","Service.entitlement","Region","Bus.Area","App.Score")])
agn <- agnes(dclust)
agn
plot(agn)

rect.hclust(agn, 10)
rfpdata$groups <- as.factor(cutree(agn, k=10))  # cut tree into 10 clusters
table(rfpdata$groups)
write.csv(rfpdata,file="C:\\Users\\vhawa\\Dropbox\\Projects\\RCode\\ticketAnalysis\\rfpprocessed.csv")

d2 <- as.dendrogram(agn)
d2[[1]] # the first branch
str(d2)
plot(d2)

# ---------------------------PLOTS----------------------------
pairs(~Region + Bus.Area,data=rfpdata, main="Simple Scatterplot Matrix")

library(ggplot2)

	qplot(groups,Region,data=rfpdata, color=Region,size= 2*table(Region,groups)[groups]) 
	qplot(groups,App.Score,data=rfpdata, color=Region,size= I(5)) 
	qplot(groups,Bus.Area,data=rfpdata, color=Region,size= I(5)) 
	qplot(groups,Platform.OS.Type,data=rfpdata, color=Region,size= I(5)) 


# -----------------------Totals ---------------------------------
	
	aggregate(data=rfpdata, Average.Monthly.Ticket.Volume..P1~groups+Service.entitlement,sum)
	
# ---------------------------Tree Check---------------------
library(rpart)
tree1 <- rpart(groups ~  Platform.OS.Type + Region + Bus.Area + App.Score,data=rfpdata)
summary(tree1)
plot(tree1)
text(tree1,use.n=TRUE)


plot( rfpdata$App.Size,rfpdata$Service.entitlement)
text(rfpdata$App.Size,rfpdata$Service.entitlement,round(rfpdata$Service.entitlement, 2), cex=0.45)

#-----------------FUN _______________________________

appsize <- function(users,complexity, volume){

 # print(users)
  
  if(users >10000 | complexity == "High"| volume>200 ) 
  return ("large")
  
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

