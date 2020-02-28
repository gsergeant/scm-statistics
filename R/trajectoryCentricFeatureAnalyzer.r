## DO TRAJECTORY-CENTRIC ANALYSIS
#do the features already need to be there???
#or calculate features myself... :/

# read data
trajectory_data = read.table("data/features_trajectory.txt", sep = "\t", header = TRUE)
head(trajectory_data)






### Permutation tests ###-------------------------------------------------------------------

#function to shuffle treatments within patient
shuffle <- function(dataset, number) {
  A <- subset(dataset, patientnumber == number)
  newtreatment<-sample(A[,'treatment'])
  perm_data<-A
  perm_data$treatment = newtreatment
  perm_data
}

patients <- unique(trajectory_data$patientnumber)


#set PBS as baseline for the model to compare with
traj_relevelled <- within(trajectory_data, treatment <- relevel(treatment, ref = "PBS"))

# Linear model + Standard Anova on these data
mod1 <- lm(accumulated_distance ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
ANOVA
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval
hist(FT, breaks = 100)

treatment = permutated_dataset$treatment


#post hoc test met multcomp library
model1.mcp<-glht(mod1,linfct=mcp(treatment="Tukey"))
summary(model1.mcp)


#4mean_speed

#run function over data, for number of times you want to do iteration
nreps <- 5000
FT <- numeric(nreps)    #Set up space to store F values as calculated.
FP <- numeric(nreps)
for (i in 2:nreps) {
  permutated_dataset = data.frame()
  for (patient in patients){
    newtreatment_dataset <- shuffle(features2, patient)
    permutated_dataset = rbind(permutated_dataset, newtreatment_dataset)
  }
  mod2 <- lm(permutated_dataset$mean_speed ~ permutated_dataset$treatment + permutated_dataset$patientnumber)
  b <- summary(aov(mod2))
  FT[i] <- b[[1]]$"F value"[1]
  FP[i] <- b[[1]]$"F value"[2]
}

# Standard Anova on these data
mod1 <- lm(mean_speed ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval

sum(FT)
sum(FT>=Ftreatment)

#post hoc test
model1.mcp<-glht(mod1,linfct=mcp(treatment="Tukey"))
summary(model1.mcp)






#1. CXCL1 - CXCL8
set.seed(1)
nreps <- 2500
FT <- numeric(nreps)    #Set up space to store F values as calculated.
FP <- numeric(nreps)
for (i in 2:nreps) {
  permutated_dataset = data.frame()
  for (patient in patients){
    newtreatment_dataset <- shuffle4(features2, patient, "CXCL1", "CXCL8")
    permutated_dataset = rbind(permutated_dataset, newtreatment_dataset)
  }
  mod2 <- lm(permutated_dataset$mean_speed ~ permutated_dataset$treatment + permutated_dataset$patientnumber)
  b <- summary(aov(mod2))
  FT[i] <- b[[1]]$"F value"[1]
  FP[i] <- b[[1]]$"F value"[2]
}

# Standard Anova on these data
mod1 <- lm(mean_speed ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
ANOVA
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval
hist(FT, breaks = 100)
sum(FT>=Ftreatment)

# -> niet significant (p = 0.6565374)


#2. CXCL1, PBS
set.seed(1)
nreps <- 2500
FT <- numeric(nreps)    #Set up space to store F values as calculated.
FP <- numeric(nreps)
for (i in 2:nreps) {
  permutated_dataset = data.frame()
  for (patient in patients){
    newtreatment_dataset <- shuffle4(features2, patient, "CXCL1", "PBS")
    permutated_dataset = rbind(permutated_dataset, newtreatment_dataset)
  }
  mod2 <- lm(permutated_dataset$mean_speed ~ permutated_dataset$treatment + permutated_dataset$patientnumber)
  b <- summary(aov(mod2))
  FT[i] <- b[[1]]$"F value"[1]
  FP[i] <- b[[1]]$"F value"[2]
}

# Standard Anova on these data
mod1 <- lm(mean_speed ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
ANOVA
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval
hist(FT, breaks = 100)
sum(FT>=Ftreatment)

# -> significant (p = 0.0003998401)

# CXCL1 fMLP

set.seed(1)
nreps <- 2500
FT <- numeric(nreps)    #Set up space to store F values as calculated.
FP <- numeric(nreps)
for (i in 2:nreps) {
  permutated_dataset = data.frame()
  for (patient in patients){
    newtreatment_dataset <- shuffle4(features2, patient, "CXCL1", "fMLP")
    permutated_dataset = rbind(permutated_dataset, newtreatment_dataset)
  }
  mod2 <- lm(permutated_dataset$mean_speed ~ permutated_dataset$treatment + permutated_dataset$patientnumber)
  b <- summary(aov(mod2))
  FT[i] <- b[[1]]$"F value"[1]
  FP[i] <- b[[1]]$"F value"[2]
}

# Standard Anova on these data
mod1 <- lm(mean_speed ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
ANOVA
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval
hist(FT, breaks = 100)
sum(FT>=Ftreatment)

#niet significant (p = 0.05877649)

#CXCL8 PBS

set.seed(1)
nreps <- 2500
FT <- numeric(nreps)    #Set up space to store F values as calculated.
FP <- numeric(nreps)
for (i in 2:nreps) {
  permutated_dataset = data.frame()
  for (patient in patients){
    newtreatment_dataset <- shuffle4(features2, patient, "CXCL8", "PBS")
    permutated_dataset = rbind(permutated_dataset, newtreatment_dataset)
  }
  mod2 <- lm(permutated_dataset$mean_speed ~ permutated_dataset$treatment + permutated_dataset$patientnumber)
  b <- summary(aov(mod2))
  FT[i] <- b[[1]]$"F value"[1]
  FP[i] <- b[[1]]$"F value"[2]
}

# Standard Anova on these data
mod1 <- lm(mean_speed ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
ANOVA
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval
hist(FT, breaks = 100)
sum(FT>=Ftreatment)

##heel significant (p = 0.0003998401)

#CXCL8 fMLP
set.seed(1)
nreps <- 2500
FT <- numeric(nreps)    #Set up space to store F values as calculated.
FP <- numeric(nreps)
for (i in 2:nreps) {
  permutated_dataset = data.frame()
  for (patient in patients){
    newtreatment_dataset <- shuffle4(features2, patient, "CXCL8", "fMLP")
    permutated_dataset = rbind(permutated_dataset, newtreatment_dataset)
  }
  mod2 <- lm(permutated_dataset$mean_speed ~ permutated_dataset$treatment + permutated_dataset$patientnumber)
  b <- summary(aov(mod2))
  FT[i] <- b[[1]]$"F value"[1]
  FP[i] <- b[[1]]$"F value"[2]
}

# Standard Anova on these data
mod1 <- lm(mean_speed ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
ANOVA
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval
hist(FT, breaks = 100)
sum(FT>=Ftreatment)

#significant (p = 0.00239904)

#fMLP PBS
set.seed(1)
nreps <- 2500
FT <- numeric(nreps)    #Set up space to store F values as calculated.
FP <- numeric(nreps)
for (i in 2:nreps) {
  permutated_dataset = data.frame()
  for (patient in patients){
    newtreatment_dataset <- shuffle4(features2, patient, "PBS", "fMLP")
    permutated_dataset = rbind(permutated_dataset, newtreatment_dataset)
  }
  mod2 <- lm(permutated_dataset$mean_speed ~ permutated_dataset$treatment + permutated_dataset$patientnumber)
  b <- summary(aov(mod2))
  FT[i] <- b[[1]]$"F value"[1]
  FP[i] <- b[[1]]$"F value"[2]
}

# Standard Anova on these data
mod1 <- lm(mean_speed ~ treatment + patientnumber, data = features2_relevelled)
ANOVA <- summary(aov(mod1))
ANOVA
cat( " The standard ANOVA for these data follows ","\n")
Ftreatment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fpatientnumber <-  ANOVA[[1]]$"F value"[2]


pval=(sum(FT>=Ftreatment)+1)/(nreps+1)
pval
hist(FT, breaks = 100)
sum(FT>=Ftreatment)

#significant (p = 0.009196321)


## other way to try permutation
set.seed(150)
B=5000
fOrig=anova(lm(mean_speed~treatment+patientnumber,data=features2_relevelled))$F[1]
fStar=sapply(X=1:B, FUN=function(b,y,groep)
{anova(lm(features2_relevelled$mean_speed~shuffle3(features2)+features2_relevelled$patientnumber))$F[1]})
fOrig

pval2=(sum(fStar>=fOrig)+1)/(B+1)
pval2

sum(fStar>=fOrig)

hist(fStar, breaks = 100)
