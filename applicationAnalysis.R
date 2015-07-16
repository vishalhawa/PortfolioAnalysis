
appsdata<-read.csv("DR11Apps.csv")
appsdata.model <- appsdata[,c("Age","Users","Regions","Platform","Version","Instances","Incidents.SRs")]

appsdata.complete = appsdata.model[complete.cases(appsdata.model),]

# Ordering of Factors

appsdata.complete$Regions = factor(appsdata.complete$Regions, levels = c("Low","Medium","High"), ordered = TRUE)
appsdata.complete$Platform = factor(appsdata.complete$Platform, levels = c("Cloud","Custom","ERP"), ordered = TRUE)


# Normalize Data 
scale(appsdata.complete$Regions)


# FA

factanal(appsdata.complete[,c("Age","Users","Instances","Incidents.SRs","Regions")],factors=1,rotation="varimax")
