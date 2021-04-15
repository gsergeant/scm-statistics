# these libraries give problems when using double colon, so load them here
library(ggplot2)
library(pim)
# essen trajectory data = trajectory_data
# essen step data = step_data
# ghent trajectory data = data.trajectory
# ghent step data = data.step
# --------- Data Setup -----------
head(data.trajectory)
data.trajectory$treatment <- as.factor(data.trajectory$treatment)
data.trajectory$treatment <- relevel(data.trajectory$treatment, "pbs")
data.trajectory$patient <- as.factor(data.trajectory$patient)
str(data.trajectory)
head(trajectory_data)
str(trajectory_data)
trajectory_data$patientnumber <- as.factor(trajectory_data$patientnumber)
trajectory_data$treatment <- as.factor(trajectory_data$treatment)
trajectory_data$treatment <- relevel(trajectory_data$treatment, "PBS")
trajectory_data$status <- as.factor(trajectory_data$status)
colnames(step_data)[colnames(step_data) == "cellType_treatment"] <- "treatment"
str(step_data)
step_data$patientnumber <- as.factor(step_data$patientnumber)
step_data$treatment <- as.factor(step_data$treatment)
step_data$treatment <- relevel(step_data$treatment, "PBS")
step_data$status <- as.factor(step_data$status)
# function to not keep created subsets in memory
getEssen.trackLengths <- function() {
  track_lenghts <- c()
  for(patient in unique(step_data$patientnumber)){
    for(treatment in unique(step_data$treatment)){
      tempsubset <- step_data[which(step_data[["patientnumber"]] == patient), ]
      realsubset <- tempsubset[which(tempsubset[["treatment"]] == treatment), ]
      for(track in unique(realsubset$track_id)){
        track_lenghts <- c(track_lenghts, nrow(realsubset[which(realsubset[["track_id"]] == track), ]))
      }
    }
  }
  return(track_lenghts)
}
essen.trackLengths <- getEssen.trackLengths()
#add track lenghts to trajectory data frame
trajectory_data$track_length <- essen.trackLengths


# ----------- Exploration: Track length vs track amount -------
#plot track length -- this grouped plot is not very informative
ggplot(data = data.trajectory, aes(x = track_length, group = treatment, fill = treatment))+
  geom_histogram(alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent")
ggplot(data = trajectory_data, aes(x = track_length, group = treatment, fill = treatment))+
  geom_histogram(alpha=.4) +
  labs(title = "Grouped trajectory data - Essen")

# Track amounts per unique patient - treatment combination
trackamounts <- c()
#Ghent
for(patient in unique(data.trajectory$patient)){
  for(treatment in unique(data.trajectory$treatment)){
    count <- sum(data.trajectory[,7]== treatment & data.trajectory[,8]== patient)
    trackamounts <- c(trackamounts, count)
  }
}
matrix.trackamounts.Ghent <- matrix(trackamounts, nrow = 4, ncol = 4, dimnames = list(unique(data.trajectory$treatment),unique(data.trajectory$patient)))
#Now visualize the matrices and compare vs track length
meltmatrix.Ghent <- reshape2::melt(matrix.trackamounts.Ghent)
ggplot(meltmatrix.Ghent, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Patient", y="Treatment", title="Amount of unique tracks - Ghent") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
for (patient in unique(data.trajectory$patient)) {
  tempSubset <- data.trajectory[which(data.trajectory[["patient"]] == patient), ]
  tempSubset <- na.omit(tempSubset)
  print(ggplot(tempSubset, aes(x = track_length, group = treatment, fill = treatment)) +
          geom_histogram(alpha=.4) +
          labs(title = paste("Trajectory data: patient ", patient," - Ghent")))
}

#Essen
trackamounts <- c()
for(patient in unique(trajectory_data$patientnumber)){
  for(treatment in unique(trajectory_data$treatment)){
    count <- sum(trajectory_data[,10]== treatment & trajectory_data[,9]== patient)
    trackamounts <- c(trackamounts, count)
  }
}
matrix.trackamounts.Essen <- matrix(trackamounts, nrow = 4, ncol = 9, dimnames = list(unique(trajectory_data$treatment),unique(trajectory_data$patientnumber)))
#Now visualize the matrix
meltmatrix.Essen <- reshape2::melt(matrix.trackamounts.Essen)
# must re-restablish patient number as factor (else the interval between numbers is too large)
meltmatrix.Essen$Var2 <- as.factor(meltmatrix.Essen$Var2)
ggplot(meltmatrix.Essen, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Patient", y="Treatment", title="Amount of unique tracks - Essen") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
#compare vs track length, separate plot per patient
for (patient in unique(trajectory_data$patientnumber)) {
  tempSubset <- trajectory_data[which(trajectory_data[["patientnumber"]] == patient), ]
  tempSubset <- na.omit(tempSubset)
  print(ggplot(tempSubset, aes(x = track_length, group = treatment, fill = treatment)) +
    geom_histogram(alpha=.4) +
    labs(title = paste("Trajectory data: patient ", patient," - Essen")))
}
#limit x axis to 200
for (patient in unique(trajectory_data$patientnumber)) {
  tempSubset <- trajectory_data[which(trajectory_data[["patientnumber"]] == patient), ]
  tempSubset <- na.omit(tempSubset)
  print(ggplot(tempSubset, aes(x = track_length, group = treatment, fill = treatment)) +
          geom_histogram(alpha=.4) +
          labs(title = paste("Trajectory data: patient ", patient," - Essen")) +
          xlim(0, 200))
}

# Ghent - average directness per treatment (patients grouped)
ggplot(data=data.trajectory, aes(x=average_directness, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")
# -------- TRAJECTORY-CENTRIC ANALYSES ------

# -------  TYPICAL ROUTINE ANALYSIS: grouped data -------
# ------- Ghent ------
# Mean speed: investigation of normality
# Ghent data, pbs & flmp, all patients grouped. Density followed by Shapiro Wilk test
# p < 0.05 indicates data does NOT follow a normal distribution
ggplot(data=data.trajectory, aes(x=mean_speed, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")
#only pbs and fmlp
subGhent.pbsfmlp <- subset(data.trajectory, treatment == "pbs" | treatment == "fmlp")
ggplot(data=subGhent.pbsfmlp, aes(x=mean_speed, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")
plot(density(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"]), main = "Ghent PBS mean speed density distribution")
shapiro.test(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"])
plot(density(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"]), main = "Ghent FMLP mean speed density distribution")
shapiro.test(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"])

# NOT normally distributed means non-parametric group comparison
# Location test: Wilcoxon
wilcox.test(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"], subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"])
# Investigate distribution scale: mood test (2-sample)
mood.test(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"], subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"])
# in routine publications, features are reported as mean +- sd, boxplot
mean(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"])
sd(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"])
mean(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"])
sd(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"])
ggplot(data=subGhent.pbsfmlp, aes(x = treatment, y=mean_speed, group=treatment, fill=treatment)) +
  geom_boxplot() +
  labs(title = "Ghent PBS-FMLP - grouped")

# -------- Essen ----
# Essen data, pbs, patient groups per disease status
traj.essen.pbs <- trajectory_data[which(trajectory_data[["treatment"]] == "PBS"), ]
ggplot(data=traj.essen.pbs, aes(x=mean_speed, group=status, fill=status)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data - Essen", y = "# of tracks")
plot(density(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"]), main = "Essen PBS-control mean speed density")
shapiro.test(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"])
plot(density(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"]), main = "Essen PBS-mild mean speed density")
shapiro.test(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"])
plot(density(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"), "mean_speed"]), main = "Essen PBS-severe mean speed density")
shapiro.test(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"), "mean_speed"])

# NOT normally distributed means non-parametric group comparison
# Location
kruskal.test(traj.essen.pbs$mean_speed, traj.essen.pbs$status)
# Test for homogeneity of variances (non-parametric)
fligner.test(x = traj.essen.pbs$mean_speed, g = traj.essen.pbs$status)
# K-sample mood test for scale: use coin package
coin::mood_test(mean_speed ~ status, data = traj.essen.pbs)

# one of the most popular post-hoc tests is Dunn, works for unequal numbers of observations
FSA::dunnTest(mean_speed ~ status, data = traj.essen.pbs, method = "bh")
# again, mean +- sd and boxplot
mean(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"])
sd(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"])
mean(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"])
sd(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"])
mean(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"), "mean_speed"])
sd(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"), "mean_speed"])
ggplot(data=traj.essen.pbs, aes(x = status, y=mean_speed, group=status, fill=status)) +
  geom_boxplot() +
  labs(title = "Essen PBS mean speed - grouped")

# ------ small upgrade: test multiple moments of distribution----
#Ghent data, PBS vs CXCL , patients grouped.
boxplot(data.trajectory[which(data.trajectory[["treatment"]] == "pbs"), "mean_speed"],data.trajectory[which(data.trajectory[["treatment"]] == "cxcl1"), "mean_speed"], main = "Ghent PBS-CXCL1 mean speed")
#Density followed by Shapiro Wilk test
# p < 0.05 indicates data does NOT follow a normal distribution
ggplot(data=data.trajectory, aes(x=mean_speed, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..),  alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")
plot(density(data.trajectory[which(data.trajectory[["treatment"]] == "pbs"), "mean_speed"]), main = "Ghent PBS mean speed density distribution")
shapiro.test(data.trajectory[which(data.trajectory[["treatment"]] == "pbs"), "mean_speed"])
plot(density(data.trajectory[which(data.trajectory[["treatment"]] == "cxcl1"), "mean_speed"]), main = "Ghent CXCL-1 mean speed density distribution")
shapiro.test(data.trajectory[which(data.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])
# another normality check is plotting QQ-plot
# we here use the car package because it gives us interval lines
car::qqPlot(data.trajectory[which(data.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])

# NOT normally distributed means non-parametric group comparison
tempSubset <- subset (data.trajectory, treatment == "pbs" | treatment == "cxcl1")
# Location
wilcox.test(data.trajectory[which(data.trajectory[["treatment"]] == "pbs"), "mean_speed"], data.trajectory[which(data.trajectory[["treatment"]] == "cxcl8"), "mean_speed"])
# Variance
fligner.test(mean_speed ~ treatment, data = tempSubset)
car::leveneTest(mean_speed ~ treatment, data = tempSubset)
# Skewness, no real test available, will do permutation calculations
#function to shuffle treatment around for permutations
shuffleTreatment <- function(dataframe){
  newvalues = sample(dataframe[,'treatment']) #only shuffle 1 column
  permutation = dataframe
  permutation$treatment =  newvalues # then stick shuffled values on the original groups
  return(permutation)
}
#calculate skewness and difference
skewPBS <- moments::skewness(data.trajectory[which(data.trajectory[["treatment"]] == "pbs"), "mean_speed"])
skewCXCL1 <- moments::skewness(data.trajectory[which(data.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])
skewdifference <- skewPBS - skewCXCL1
#do permutation
set.seed(1)
nreps <- 5000
skeww <- numeric(nreps)    #Set up space to store the skewness results as calculated.
for (i in 1:nreps) {
  permutedFrame <- shuffleTreatment(tempSubset)
  grouppbs <- subset(permutedFrame, treatment == "pbs", select = "mean_speed")
  groupcxcl1 <- subset(permutedFrame, treatment == "cxcl1", select = "mean_speed")
  skewdiff <- moments::skewness(grouppbs) - moments::skewness(groupcxcl1)
  skeww[i] <- skewdiff
}
plot(density(skeww), main = "Permutations of skewness difference \n Ghent PBS-CXCL1")
abline(v = skewdifference, col = "red")
# Kurtosis, same method as skewness as there is no test available
kurtPBS <- moments::kurtosis(data.trajectory[which(data.trajectory[["treatment"]] == "pbs"), "mean_speed"])
kurtCXCL1 <- moments::kurtosis(data.trajectory[which(data.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])
kurtdifference <- kurtPBS - kurtCXCL1
#do permutation
set.seed(1)
nreps <- 5000
permkurt <- numeric(nreps)    #Set up space to store the kurtosis results as calculated.
for (i in 1:nreps) {
  permutedFrame <- shuffleTreatment(tempSubset)
  grouppbs <- subset(permutedFrame, treatment == "pbs", select = "mean_speed")
  groupcxcl1 <- subset(permutedFrame, treatment == "cxcl1", select = "mean_speed")
  kurtdiff <- moments::kurtosis(grouppbs) - moments::kurtosis(groupcxcl1)
  permkurt[i] <- kurtdiff
}
plot(density(permkurt), main = "Permutations of kurtosis difference \n Ghent PBS-CXCL1")
abline(v = kurtdifference, col = "red")

# Now test additional moments in Essen dataset: control vs mild mds
# Post hoc test did not show significant diference between these
# Re-test location (although not necessary - same result as posthoc dunn test)
wilcox.test(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"], traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"])
#Variance
#We only want control and mild statuses
tempSubset <- subset (traj.essen.pbs, status == "control" | status == "mild")
fligner.test(mean_speed ~ status, data = tempSubset)
car::leveneTest(mean_speed ~ status, data = tempSubset)
#Skewness
#function to shuffle status around for permutations
shuffleStatus <- function(dataframe){
  newvalues = sample(dataframe[,'status']) #only shuffle 1 column
  permutation = dataframe
  permutation$status =  newvalues # then stick shuffled values on the original groups
  return(permutation)
}
#calculate skewness and difference
skewcontrol <- moments::skewness(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"])
skewmild <- moments::skewness(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"])
skewdifference <- skewcontrol - skewmild
#do permutation
set.seed(1)
nreps <- 5000
skeww <- numeric(nreps)    #Set up space to store the skewness results as calculated.
for (i in 1:nreps) {
  permutedFrame <- shuffleStatus(tempSubset)
  groupcontrol <- subset(permutedFrame, status == "control", select = "mean_speed")
  groupmild <- subset(permutedFrame, status == "mild", select = "mean_speed")
  skewdiff <- moments::skewness(groupcontrol) - moments::skewness(groupmild)
  skeww[i] <- skewdiff
}
plot(density(skeww), main = "Permutations of skewness difference \n Essen healthy-mild MDS")
abline(v = skewdifference, col = "red")
#from this table we can see 4887 permutations out of 5000 are smaller than the original skewness difference
table(skeww < skewdifference)
4887/5000  #0.9774 Outside the 95th percentile, so significant effect of mild status vs healthy on mean speed skewness
# Kurtosis, same method as skewness as there is no test available
kurtcontrol <- moments::kurtosis(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"])
kurtmild <- moments::kurtosis(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"])
kurtdifference <- kurtcontrol - kurtmild
#do permutation
set.seed(1)
nreps <- 5000
permkurt <- numeric(nreps)    #Set up space to store the kurtosis results as calculated.
for (i in 1:nreps) {
  permutedFrame <- shuffleStatus(tempSubset)
  groupcontrol <- subset(permutedFrame, status == "control", select = "mean_speed")
  groupmild <- subset(permutedFrame, status == "mild", select = "mean_speed")
  kurtdiff <- moments::kurtosis(groupcontrol) - moments::kurtosis(groupmild)
  permkurt[i] <- kurtdiff
}
plot(density(permkurt), main = "Permutations of kurtosis difference \n Essen healthy-mild MDS")
abline(v = kurtdifference, col = "red")

# ---------- EXPAND ROUTINE: ungroup patients - MODELING ---------
#Some exploratory plots
#Ghent data
ggplot(data=data.trajectory[which(data.trajectory[["treatment"]] == "pbs"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Ghent PBS", y = "# of tracks")
ggplot(data=data.trajectory[which(data.trajectory[["treatment"]] == "fmlp"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Ghent FMLP", y = "# of tracks")
ggplot(data=data.trajectory[which(data.trajectory[["treatment"]] == "cxcl1"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Ghent CXCL-1", y = "# of tracks")
ggplot(data=data.trajectory[which(data.trajectory[["treatment"]] == "cxcl8"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Ghent CXCL-8", y = "# of tracks")

# Linear mixed (effect) model: model effect of treatment on mean speed for each individual patient
lmm.Ghent.allTreat <- lme4::lmer(mean_speed ~ treatment + (treatment | patient), data.trajectory)
summary(lmm.Ghent.allTreat)
## compare resuduals between lmm and normal lm
sqrt(sum(residuals(lm(mean_speed~treatment * patient,data=data.trajectory))^2)/(dim(data.trajectory)[1]-2))
sqrt(sum(resid(lmm.Ghent.allTreat)^2)/(dim(data.trajectory)[1]-2))
# compare models using AIC
fit1 <- lm(mean_speed~treatment * patient,data=data.trajectory)
fit2 <- lmm.Ghent.allTreat
fit3 <- lme4::lmer(mean_speed ~ treatment + (1 | patient), data.trajectory)
# REML models will be automatically recalculated onder ML (max likelihood)
anova(fit2, fit1)
# plot prediction of mixed effect models using sjPlot package
sjPlot::plot_model(lmm.Ghent.allTreat,  type = "pred", title = "Predicted mean speed: random treatment|patient")
sjPlot::plot_model(fit1, type = "pred", title = "Predicted mean speed with random patient effect")
# most intervals seem to shrink when removing random treatment|patient interaction, but cxcl1 prediction interval widens

# GENERALIZED linear mixed effects model: less assumptions about data
glmm.Ghent.allTreat <- lme4::glmer(mean_speed ~ treatment + (treatment | patient), data.trajectory, family = Gamma(link = "identity"))
sjPlot::plot_model(glmm.Ghent.allTreat,  type = "pred", title = "Predicted mean speed: random treatment|patient")
summary(glmm.Ghent.allTreat)


#Essen data
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"),], aes(x=mean_speed, group=patientnumber, fill=patientnumber)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Essen control patients", y = "# of tracks")
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"),], aes(x=mean_speed, group=patientnumber, fill=patientnumber)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Essen mild patients", y = "# of tracks")
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"),], aes(x=mean_speed, group=patientnumber, fill=patientnumber)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Essen severe patients", y = "# of tracks")
# final one again with extra layer that shows data points (because there are a lot of low measurement points in this case)
# not very helpful plot when looking at it
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"),], aes(x=mean_speed, group=patientnumber, fill=patientnumber)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  geom_rug(aes(x = mean_speed, y = 0), position = position_jitter(height = 0))+
  labs(title = "Essen severe patients", y = "# of tracks")

# Linear Mixed Model
lmm.Essen.pbs <- lme4::lmer(mean_speed ~ status + (status|patientnumber), data = traj.essen.pbs)
summary(lmm.Essen.pbs)
lmm.Essen.allTreat <- lme4::lmer(mean_speed ~ status * treatment + (treatment|patientnumber) + (status|patientnumber), data = trajectory_data)
summary(lmm.Essen.allTreat)
# compare residuals of simple and complex model
sqrt(sum(resid(lmm.Essen.pbs)^2)/(dim(traj.essen.pbs)[1]-2))
sqrt(sum(resid(lmm.Essen.allTreat)^2)/(dim(trajectory_data)[1]-2))
# lower residuals in the simpler model makes sense: smaller dataframe with a lot less dimensionality
# anova does not make sense with these two models, as they were fit on different size datasets
sjPlot::plot_model(lmm.Essen.pbs,  type = "pred", title = "Predicted mean speed: random status|patient")
sjPlot::plot_model(lmm.Essen.allTreat,  type = "pred",terms = c("treatment", "status"), title = "Predicted mean speed: random treatment|patient")



# ---------- Probabilistic Index models --------
#Ghent data, full complexity
pimGhent_traj <- pim(mean_speed ~ treatment * patient + 1, data = data.trajectory)
# without intercept
pimGhent_traj <- pim(mean_speed ~ treatment * patient, data = data.trajectory)

summary(pimGhent_traj)

#Essen data
pimEssen <- pim(mean_speed ~ treatment * patientnumber * status + 1, data = trajectory_data) # vector too large 38.6Gb
pimEssen <- pim(mean_speed ~ treatment * status + patientnumber + 1, data = trajectory_data) # vector 7.1 gig also impossible
pimEssen <- pim(mean_speed ~ treatment * status + patientnumber, data = trajectory_data)
summary(pimEssen)
coef(pimEssen)
plot(pimEssen)




# -------- STEP-CENTRIC ANALYSES ------
# --------- Data Setup -----------
head(step_data)
str(step_data)
head(data.step)
str(data.step)
