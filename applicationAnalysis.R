
appsdata<-read.csv("DR11Apps.csv")
appsdata.model <- appsdata[,c("Age","Users","Regions","Platform","Version.Support","Instances","Incidents.SRs")]

appsdata.model$Agef = as.numeric(cut(appsdata.model$Age,breaks=c(0,2,5,8,Inf), labels=c(1,2,3,4),ordered_result = TRUE, right = FALSE))
appsdata.model$Usersf = as.numeric(cut(appsdata.model$Users,breaks=c(0,25,100,300,1000,2000,5000,Inf), labels=c("0-25","25-100","100-300","300-1000","1000-2000","2000-5000","5000+"),ordered_result = TRUE, right = FALSE))
appsdata.model$Instancesf = as.numeric(cut(appsdata.model$Instances,breaks=c(0,2,Inf), labels=c("Single","Multiple"),ordered_result = TRUE, right = FALSE))
appsdata.model$Incidents.SRf = as.numeric(cut(appsdata.model$Incidents.SRs,breaks=7, ordered_result = TRUE, right = FALSE))



# Complete Cases
sum(complete.cases(appsdata.model))


appsdata.complete = appsdata.model[complete.cases(appsdata.model),]

# No of Rows
nrow(appsdata.complete)

# Ordering of Factors

appsdata.complete$Regions = as.numeric(factor(appsdata.complete$Regions, levels = c("Low","Medium","High"), ordered = TRUE))
appsdata.complete$Platform = as.numeric(factor(appsdata.complete$Platform, levels = c("Cloud","Custom","ERP"), ordered = TRUE))

# TEST: REcode  to see effects of Data Coding done differently------------------------------

 appsdata.complete$UsersGroup =   round(5.38*exp(1.09*appsdata.complete$Usersf))


# Normalize Data 
# scale(appsdata.complete$Regions) # Not necessary 
# scale(appsdata.complete$Incidents.SRs) # Not Necessary as they are now Category variables


#  ---------------------- Exploratory -FA  -----------------------------------------------------------------

# Change number of Facotrs - Done
fit <- factanal(appsdata.complete[,c("Agef","Usersf","Instancesf","Incidents.SRf","Regions","Platform","Version.Support")],factors=3,rotation="varimax",scores = "regression")

(fit)
fit.scores = as.matrix(appsdata.complete[,c("Agef","Usersf","Instancesf","Incidents.SRf", "Regions","Platform","Version.Support")]) %*%loadings(fit)
appsdata.complete <-cbind (appsdata.complete,fit.scores) # These are RAW Scores
appsdata.complete$Sum.Factors = appsdata.complete$Factor1+appsdata.complete$Factor2+appsdata.complete$Factor3

# Change Rotation to Promax

fit2 <- factanal(appsdata.complete[,c("Agef","Usersf","Instancesf","Incidents.SRf","Regions","Platform","Version.Support")],factors=3,rotation="promax",scores = "regression")

(fit2)   # Results mostly same
fit2.scores = as.matrix(appsdata.complete[,c("Agef","Usersf","Instancesf","Incidents.SRf", "Regions","Platform","Version.Support")]) %*%loadings(fit2)
colnames(fit2.scores)<- c("F1","F2","F3")

appsdata.complete <-cbind (appsdata.complete,fit2.scores) # These are Raw scores
appsdata.complete$Sum.FS = appsdata.complete$F1+appsdata.complete$F2+appsdata.complete$F3


# Combining Factors - to be skipped



x.Agef = sum( loadings(fit)[1,])
x.Usersf = sum(loadings(fit)[2,])
x.Instancesf = sum( loadings(fit)[3,])
x.Incidents.SRf = sum( loadings(fit)[4,])
x.Regions = sum( loadings(fit)[5,])
x.Platform = sum( loadings(fit)[6,])
x.Version.Support = sum( loadings(fit)[7,])


# # to Apply proportionality Variance weights, which is sum of square of respective loadings: SS Loadings
# 
# x.Agef = crossprod(colSums(fit$loadings^2) , loadings(fit)[1,])
# x.Usersf = crossprod(colSums(fit$loadings^2) , loadings(fit)[2,])
# x.Instancesf = crossprod(colSums(fit$loadings^2) , loadings(fit)[3,])
# x.Incidents.SRf = crossprod(colSums(fit$loadings^2) , loadings(fit)[4,])
# x.Regions = crossprod(colSums(fit$loadings^2) , loadings(fit)[5,])
# x.Platform = crossprod(colSums(fit$loadings^2) , loadings(fit)[6,])
# x.Version.Support = crossprod(colSums(fit$loadings^2) , loadings(fit)[7,])


# --------------------------------Apply LR and check fitness----------------------------------------

appsdata.complete$Points = x.Agef*appsdata.complete$Agef + x.Usersf*appsdata.complete$Usersf + x.Instancesf*appsdata.complete$Instancesf + x.Incidents.SRf*appsdata.complete$Incidents.SRf + 
  x.Regions*appsdata.complete$Regions + x.Platform*appsdata.complete$Platform + x.Version.Support*appsdata.complete$Version.Support
  
# lm.model = lm(Points~data=appsdata.complete,) => PERFECT FIT BELOW

lm.model =lm(Sum.FS~ Agef + Usersf + Instancesf + Incidents.SRf + Regions + Platform + Version.Support -1,data=appsdata.complete)
summary(lm.model)

lm.model.Factors =lm(Sum.Factors~ Agef + Usersf + Instancesf + Incidents.SRf + Regions + Platform + Version.Support -1,data=appsdata.complete)
summary(lm.model.Factors)


# --------------------Coeff or Weights ---------------------------------------------

seq(1:max(appsdata.complete$Incidents.SRf))*x.Incidents.SRf
seq(1:max(appsdata.complete$Instancesf))*x.Instancesf
unique(appsdata.complete$Usersf)*x.Usersf
seq(1:max(appsdata.complete$Agef))*x.Agef
seq(1:max(appsdata.complete$Regions))*x.Regions



#----------------------------------SEM-OR CFA-------------------------------------------
# -------------------------------------------------------------------------------------

require("lavaan")

# CORRELATION
cor(appsdata.complete[,c("Agef","Usersf","Instancesf","Incidents.SRf","Regions","Platform","Version.Support")])

# try unscaled version ...
lvmod.1 <- 'Sum.FS ~ Agef + Usersf + Instancesf + Incidents.SRf + Regions + Platform + Version.Support '
lvmod.1.fit = sem(lvmod.1, data = appsdata.complete)

lvmod.1.fit

summary(lvmod.1.fit)

modindices(lvmod.1.fit)

# --------------------------------------------------------lvmod.1.fit------------
lvmod.F <- 'points =~ F1 + F2 + F3 
           '

lvmod.F <- 'points =~ Factor1 + Factor2 + Factor3 
           '

lvmod.F.fit = sem(lvmod.F, data = appsdata.complete)

lvmod.F.fit

summary(lvmod.F.fit)
varTable(lvmod.F.fit)

mi = modindices(lvmod.F.fit)
mi[mi$op=="~"]
mi[mi$op=="~~"]

# -------------SEM with Usersf----*** Best fit so far-----------------------------------------

lvmod.userf <- 'points =~ Agef + Usersf + Instancesf + Incidents.SRf + Regions + Platform + Version.Support 
              # Add Covariances
                Usersf ~~ Regions 
                 Usersf ~~   Incidents.SRf
                Instancesf ~~   Regions 
                 Agef ~~  Usersf '


lvmod.UF.fit = sem(lvmod.userf, data = appsdata.complete)

lvmod.UF.fit

summary(lvmod.UF.fit)



inspect(lvmod.UF.fit,"theta")

mi.UF = modindices(lvmod.UF.fit)
mi.UF
#---------------SEM with Starts  -----------------
lvmod.f1 <- 'points =~ start(0.195)*Agef + Usersf + start(0)*Instancesf + start(.283)*Incidents.SRf + start(.64)*Regions + start(.157)*Platform + start(0)*Version.Support 
             Agef ~~ Version.Support
               '


lvmod.Users.fit = sem(lvmod.f1, data = appsdata.complete)

lvmod.Users.fit

summary(lvmod.Users.fit)  #Users variances is too high 

mi.Users = modindices(lvmod.Users.fit)
mi.Users

# -------Compare Models ------------------

anova(lvmod.UF.fit,lvmod.UG.fit,lvmod.Users.fit)


# ---------------------------Assignment of Applications to Price Band--------------------------------


appsdata.complete$Points.LR = appsdata.complete$Sum.FS*10

appsdata.complete$Points.SEM = 10* as.matrix(appsdata.complete[,c("Agef","Usersf","Instancesf","Incidents.SRf", "Regions","Platform","Version.Support")]) %*%inspect(lvmod.UF.fit,what="est")$lambda 

# Points Diff : This is Not Correct --
appsdata.complete$Diff.LR_SEM = appsdata.complete$Points.LR - appsdata.complete$Points.SEM

appsdata.complete$PriceBand.LR =  cut(appsdata.complete$Points.LR,breaks=c(47,74,101,129,156,184), labels=c("A","B","C","E","F"),ordered_result = TRUE, right = FALSE)

appsdata.complete$PriceBand.SEM =  cut(appsdata.complete$Points.SEM,breaks=c(-3,11,25,39,53,67), labels=c("A","B","C","E","F"),ordered_result = TRUE, right = FALSE)

table(appsdata.complete$PriceBand.SEM,appsdata.complete$PriceBand.LR)

# Match Percentage 

100*sum(diag(table(appsdata.complete$PriceBand.SEM,appsdata.complete$PriceBand.LR)))/sum(table(appsdata.complete$PriceBand.SEM,appsdata.complete$PriceBand.LR))

write.csv(appsdata.complete,"Output.csv")
# ------------------------------EOF --------------


  