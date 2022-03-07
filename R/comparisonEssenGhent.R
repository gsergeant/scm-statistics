# Packages used in this file: reshape2, coin, FSA, car, moments, lme4, sjPlot, ggplot2, pim
# Please install these using the install.packages("") command
# Some libraries give problems when using double colon, so load them here instead
library(ggplot2)
library(pim)

# --------- Data Setup -----------
ghent.trajectory <- readRDS("data.Ghent.trajectory.rds")
head(ghent.trajectory)
str(ghent.trajectory)
essen.trajectory <- readRDS("data.Essen.trajectory.rds")
head(essen.trajectory)
str(essen.trajectory)
# Note: Speed is measured in pixels/min and is derived from the step data's delta z

# ----------- Exploration: Track length vs track amount -------
#plot track length -- this grouped plot is not very informative
ggplot(data = ghent.trajectory, aes(x = track_length, group = treatment, fill = treatment))+
  geom_histogram(alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent")
ggplot(data = essen.trajectory, aes(x = track_length, group = treatment, fill = treatment))+
  geom_histogram(alpha=.4) +
  labs(title = "Grouped trajectory data - Essen")

# Track amounts per unique patient - treatment combination
trackamounts <- c()
#Ghent
for(patient in unique(ghent.trajectory$patient)){
  for(treatment in unique(ghent.trajectory$treatment)){
    count <- sum(ghent.trajectory[,7]== treatment & ghent.trajectory[,8]== patient)
    trackamounts <- c(trackamounts, count)
  }
}
matrix.trackamounts.Ghent <- matrix(trackamounts, nrow = 4, ncol = 4, dimnames = list(unique(ghent.trajectory$treatment),unique(ghent.trajectory$patient)))
#Now visualize the matrices and compare vs track length
meltmatrix.Ghent <- reshape2::melt(matrix.trackamounts.Ghent)
ggplot(meltmatrix.Ghent, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Patient", y="Treatment", title="Amount of unique tracks - Ghent") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
for (patient in unique(ghent.trajectory$patient)) {
  tempSubset <- ghent.trajectory[which(ghent.trajectory[["patient"]] == patient), ]
  tempSubset <- na.omit(tempSubset)
  print(ggplot(tempSubset, aes(x = track_length, group = treatment, fill = treatment)) +
          geom_histogram(alpha=.4) +
          labs(title = paste("Trajectory data: patient ", patient," - Ghent")))
}

#Essen
trackamounts <- c()
for(patient in unique(essen.trajectory$patient)){
  for(treatment in unique(essen.trajectory$treatment)){
    count <- sum(essen.trajectory[,7]== treatment & essen.trajectory[,6]== patient)
    trackamounts <- c(trackamounts, count)
  }
}
matrix.trackamounts.Essen <- matrix(trackamounts, nrow = 4, ncol = 9, dimnames = list(unique(essen.trajectory$treatment),unique(essen.trajectory$patient)))
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
for (patient in unique(essen.trajectory$patient)) {
  tempSubset <- essen.trajectory[which(essen.trajectory[["patient"]] == patient), ]
  tempSubset <- na.omit(tempSubset)
  print(ggplot(tempSubset, aes(x = track_length, group = treatment, fill = treatment)) +
    geom_histogram(alpha=.4) +
    labs(title = paste("Trajectory data: patient ", patient," - Essen")))
}
#limit x axis to 200
for (patient in unique(essen.trajectory$patient)) {
  tempSubset <- essen.trajectory[which(essen.trajectory[["patient"]] == patient), ]
  tempSubset <- na.omit(tempSubset)
  print(ggplot(tempSubset, aes(x = track_length, group = treatment, fill = treatment)) +
          geom_histogram(alpha=.4) +
          labs(title = paste("Trajectory data: patient ", patient," - Essen")) +
          xlim(0, 200))
}
# Correlation matrix of all features, with coefficient
ggstatsplot::ggcorrmat(
  data = ghent.trajectory,

  p.adjust.method = "holm", # p-value adjustment method for multiple comparisons
  cor.vars = c(mean_speed:track_length), # a range of variables can be selected
  cor.vars.names = c(
    "Mean Speed", # variable names
    "Average Directness",
    "Cumulative Distance",
    "Euclidian Distance",
    "Track Length"
  ),
  matrix.type = "upper", # type of visualization matrix
  colors = c("blue", "white", "red"),
  title = "Correlalogram for migration features",
  subtitle = "Ghent dataset",
  ggcorrplot.args = list(tl.cex = 9) # smaller text labels
)



# ------- Exploration of directness feature -----
# Ghent - average directness per treatment (patients grouped)
ggplot(data=ghent.trajectory, aes(x=average_directness, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")

# mean speed vs average directness
ggplot(data = ghent.trajectory, aes(x=mean_speed, y=average_directness, group=treatment, color=treatment)) +
  geom_point(size = 0.9)+
  labs(title = "Speed vs directionality - Ghent")


# -------- TRAJECTORY-CENTRIC ANALYSES ------

# -------  TYPICAL ROUTINE ANALYSIS: grouped data -------
# ------- Ghent ------
# Mean speed: investigation of normality
# Ghent data, pbs & flmp, all patients grouped. Density followed by Shapiro Wilk test
# p < 0.05 indicates data does NOT follow a normal distribution
ggplot(data=ghent.trajectory, aes(x=mean_speed, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")
#only pbs and fmlp
subGhent.pbsfmlp <- subset(ghent.trajectory, treatment == "pbs" | treatment == "fmlp")
ggplot(data=subGhent.pbsfmlp, aes(x=mean_speed, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")
plot(density(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"]), main = "Ghent PBS mean speed density distribution")
shapiro.test(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "pbs"), "mean_speed"])
plot(density(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"]), main = "Ghent FMLP mean speed density distribution")
shapiro.test(subGhent.pbsfmlp[which(subGhent.pbsfmlp[["treatment"]] == "fmlp"), "mean_speed"])
#QQ-plots
car::qqPlot(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"), "mean_speed"], ylab= "mean_speed")
title("QQplot Ghent PBS", adj = 0)
car::qqPlot(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "fmlp"), "mean_speed"], ylab= "mean_speed")
title("QQplot Ghent fMLP", adj = 0)


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
ggplot(data=essen.trajectory, aes(x=mean_speed, group=status, fill=status)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data -all treatments- Essen", y = "# of tracks")
# Essen data, pbs, patient groups per disease status
traj.essen.pbs <- essen.trajectory[which(essen.trajectory[["treatment"]] == "pbs"), ]
ggplot(data=traj.essen.pbs, aes(x=mean_speed, group=status, fill=status)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Grouped trajectory data -PBS only- Essen", y = "# of tracks")
plot(density(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"]), main = "Essen PBS-control mean speed density")
shapiro.test(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"])
plot(density(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"]), main = "Essen PBS-mild mean speed density")
shapiro.test(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"])
plot(density(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"), "mean_speed"]), main = "Essen PBS-severe mean speed density")
shapiro.test(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"), "mean_speed"])
#QQplots
car::qqPlot(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"), "mean_speed"], ylab= "mean_speed")
title("Essen healthy, PBS only", adj = 0)
car::qqPlot(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"), "mean_speed"], ylab= "mean_speed")
title("Essen mild MDS, PBS only", adj = 0)
car::qqPlot(traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"), "mean_speed"], ylab= "mean_speed")
title("Essen severe MDS, PBS only", adj = 0)

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

# ------ Upgrade: test multiple moments of distribution----
#---- Ghent ----
#Ghent data, PBS vs CXCL1 , patients grouped.
boxplot(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"), "mean_speed"],ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl1"), "mean_speed"], main = "Ghent PBS-CXCL1 mean speed")
#Density followed by Shapiro Wilk test
# p < 0.05 indicates data does NOT follow a normal distribution
ggplot(data=ghent.trajectory, aes(x=mean_speed, group=treatment, fill=treatment)) +
  geom_density(aes(y = ..count..),  alpha=.4) +
  labs(title = "Grouped trajectory data - Ghent", y = "#of tracks")
plot(density(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"), "mean_speed"]), main = "Ghent PBS mean speed density distribution")
shapiro.test(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"), "mean_speed"])
plot(density(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl1"), "mean_speed"]), main = "Ghent CXCL-1 mean speed density distribution")
shapiro.test(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])
# another normality check is plotting QQ-plot
# we here use the car package because it gives us interval lines
car::qqPlot(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])

# NOT normally distributed means non-parametric group comparison
tempSubset <- subset (ghent.trajectory, treatment == "pbs" | treatment == "cxcl1")
# Location
wilcox.test(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"), "mean_speed"], ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl8"), "mean_speed"])
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
skewPBS <- moments::skewness(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"), "mean_speed"])
skewCXCL1 <- moments::skewness(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])
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

#from this table we can see 4223 permutations out of 5000 are smaller than the original skewness difference
table(skeww < skewdifference)
4223/5000  #0.8446 Not outside the 95th percentile, so no significant effect of cxcl1 vs pbs on mean speed skewness

# Kurtosis, same method as skewness as there is no test available
kurtPBS <- moments::kurtosis(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"), "mean_speed"])
kurtCXCL1 <- moments::kurtosis(ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl1"), "mean_speed"])
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
table(permkurt < kurtdifference)
2471/5000 #0.4942

#----- Essen----
# Now test additional moments in Essen dataset: control vs mild MDS,
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
plot(density(skeww), main = "Permutations of skewness difference \n Essen healthy-mild MDS", xlab= "Skewness difference")
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
plot(density(permkurt), main = "Permutations of kurtosis difference \n Essen healthy-mild MDS", xlab = "Kurtosis difference")
abline(v = kurtdifference, col = "red")
table(permkurt < kurtdifference)
4437/5000 #0.8874

# ---------- EXPAND ROUTINE: ungroup patients - MODELING ---------
#----Some exploratory plots of the ungrouped datasets----
#Ghent data
ggplot(data=ghent.trajectory[which(ghent.trajectory[["treatment"]] == "pbs"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Ghent PBS", y = "# of tracks")
ggplot(data=ghent.trajectory[which(ghent.trajectory[["treatment"]] == "fmlp"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Ghent FMLP", y = "# of tracks")
ggplot(data=ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl1"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Ghent CXCL-1", y = "# of tracks")
ggplot(data=ghent.trajectory[which(ghent.trajectory[["treatment"]] == "cxcl8"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Ghent CXCL-8", y = "# of tracks")

#Essen data
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "control"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  labs(title = "Essen control patients", y = "# of tracks")
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "mild"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Essen mild patients", y = "# of tracks")
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4)+
  labs(title = "Essen severe patients", y = "# of tracks")
# final one again with extra layer that shows data points (because there are a lot of low measurement points in this case)
# not very helpful plot when looking at it
ggplot(data=traj.essen.pbs[which(traj.essen.pbs[["status"]] == "severe"),], aes(x=mean_speed, group=patient, fill=patient)) +
  geom_density(aes(y = ..count..), alpha=.4) +
  geom_rug(aes(x = mean_speed, y = 0), position = position_jitter(height = 0))+
  labs(title = "Essen severe patients", y = "# of tracks")

# ---------- Probabilistic Index models --------
#Ghent data, full complexity
pimGhent_traj <- pim(mean_speed ~ treatment * patient + 1, data = ghent.trajectory)
# without intercept
pimGhent_traj2 <- pim(mean_speed ~ treatment * patient, data = ghent.trajectory)

summary(pimGhent_traj)
coef(pimGhent_traj)

#Plot probabilities and their confidence intervals
#create prob/confint dataframe with logical column names
pimconf <- confint(pimGhent_traj)
pimconf <- as.data.frame(pimconf)
data.table::setDT(pimconf, keep.rownames = TRUE)[]
colnames(pimconf) <- c("Parameter", "Min", "Max")
pimconf$Probability <- locfit::expit(coef(pimGhent_traj))
pimconf$Min<-locfit::expit(pimconf$Min)
pimconf$Max<-locfit::expit(pimconf$Max)
# Plot intervals with rotated x axes values
ggplot(pimconf,aes(Parameter,y=Probability,ymin=Min,ymax=Max,color="red")) +
  geom_pointrange() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept = 0.5, linetype = 2)

#Essen data
# Unfortunately, using PIMs on very large datasets, such as our Essen set, quickly becomes a very computationally demanding task
# Especially because of the three levels of hierarchy that are present and their interactions.
# For more information on this subject and possible workarounds, we recommend https://github.com/HBossier/BigDataPIM
# The dissertation associated with that repository can be found here: https://libstore.ugent.be/fulltxt/RUG01/002/376/287/RUG01-002376287_2017_0001_AC.pdf
pimEssen <- pim(mean_speed ~ treatment * patient * status + 1, data = essen.trajectory) # vector too large 38.6Gb
pimEssen <- pim(mean_speed ~ treatment * status + patient + 1, data = essen.trajectory) # vector 7.1 gig also impossible
pimEssen <- pim(mean_speed ~ treatment + status + patient + 1, data = essen.trajectory)
summary(pimEssen)
coef(pimEssen)

# ------- GLMM Ghent ----
# Linear mixed (effect) model: model effect of treatment on mean speed for each individual patient
lmm.Ghent.allTreat <- lme4::lmer(mean_speed ~ treatment + (treatment | patient), ghent.trajectory)
summary(lmm.Ghent.allTreat)
## compare resuduals between lmm and normal linear model
sqrt(sum(residuals(lm(mean_speed~treatment * patient,data=ghent.trajectory))^2)/(dim(ghent.trajectory)[1]-2))
sqrt(sum(resid(lmm.Ghent.allTreat)^2)/(dim(ghent.trajectory)[1]-2))

# Standard Fitted vs Residuals plot
plot(lmm.Ghent.allTreat, type = c("p", "smooth"))
# Scale-location (based on raw residuals instead of standardized)
plot(lmm.Ghent.allTreat, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))
# QQ plot (also based on raw residuals)
lattice::qqmath(lmm.Ghent.allTreat, id = 0.05)

# compare models using AIC
fit1 <- lm(mean_speed~treatment * patient,data=ghent.trajectory)
fit2 <- lmm.Ghent.allTreat
# (fit3 =  simpler mixed model with only variable intercept, not slope)
fit3 <- lme4::lmer(mean_speed ~ treatment + (1 | patient), ghent.trajectory)
# REML models will be automatically recalculated onder ML (max likelihood) when using this anova
anova(fit2, fit1)
# plot prediction of mixed effect models using sjPlot package
sjPlot::plot_model(lmm.Ghent.allTreat,  type = "pred", title = "Predicted mean speed: random treatment|patient")
sjPlot::plot_model(fit1, type = "pred", title = "Predicted mean speed with random patient effect")
# most intervals seem to shrink when removing random treatment|patient interaction, but cxcl1 prediction interval widens

# GENERALIZED linear mixed effects model: less assumptions about data
# Try other family such as Gamma or inverse.Gaussian
# Does not seem to converge with this dataset
glmm.Ghent.allTreat <- lme4::glmer(mean_speed ~ treatment + (treatment | patient), ghent.trajectory, family = inverse.gaussian(link = "identity"))
sjPlot::plot_model(glmm.Ghent.allTreat,  type = "pred", title = "Predicted mean speed: random treatment|patient")
summary(glmm.Ghent.allTreat)

#---- GLMM Essen ----
# Linear Mixed Model
lmm.Essen.pbs <- lme4::lmer(mean_speed ~ status + (status|patient), data = traj.essen.pbs)
summary(lmm.Essen.pbs)
lmm.Essen.allTreat <- lme4::lmer(mean_speed ~ status * treatment + (treatment|patient) + (status|patient), data = essen.trajectory)
summary(lmm.Essen.allTreat)

# Using interactive random effects is also possible,
# although status:treatment interactions are then present in both fixed and random effect results
lmm.Essen.allTreat.interaction <- lme4::lmer(mean_speed ~ status * treatment + (treatment*status|patient), data = essen.trajectory)

# Simple linear models for both pbs and full dataset
linMod <-  lm(mean_speed~status * patient,data=traj.essen.pbs)
sqrt(sum(resid(linMod)^2)/(dim(traj.essen.pbs)[1]-2))
linMod2 <- lm(mean_speed~status * patient * treatment,data= essen.trajectory)
sqrt(sum(resid(linMod2)^2)/(dim(essen.trajectory)[1]-2))

# Compare residuals of simple and complex model
sqrt(sum(resid(lmm.Essen.pbs)^2)/(dim(traj.essen.pbs)[1]-2))
sqrt(sum(resid(lmm.Essen.allTreat)^2)/(dim(essen.trajectory)[1]-2))
# Mixed models only: lower residuals in the simpler model makes sense: smaller dataframe with a lot less dimensionality
# Residuals of normal linear models with mixed models on both datasets are comparable

# Standard Fitted vs Residuals plot
plot(lmm.Essen.pbs, type = c("p", "smooth"))
plot(lmm.Essen.allTreat, type = c("p", "smooth"), main = "C")
# Scale-location (based on raw residuals instead of standardized)
plot(lmm.Essen.allTreat, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))
# QQ plot (also based on raw residuals)
lattice::qqmath(lmm.Essen.allTreat, id = 0.05)
# anova does not make sense with these two mixed models, as they were fit on different size datasets
# ANOVA with simple linear model and mixel model
anova(lmm.Essen.pbs, linMod)
anova(lmm.Essen.allTreat, linMod2)

# Some prediction plots using sjPlot package
sjPlot::plot_model(lmm.Essen.pbs,  type = "pred", title = "Predicted mean speed: random status|patient")
sjPlot::plot_model(lmm.Essen.allTreat,  type = "pred",terms = c("treatment", "status"), title = "Predicted mean speed: random treatment|patient")
# another type of plot: interaction
sjPlot::plot_model(lmm.Essen.allTreat, type = "int")
# sjPlot package can also summarize models in html format
sjPlot::tab_model(lmm.Essen.pbs)
sjPlot::tab_model(lmm.Essen.allTreat)
# a combination table can also be made but is quite bulky
sjPlot::tab_model(lmm.Essen.pbs, lmm.Essen.allTreat)

# Try out inverse gaussian/gamma distribution (family = inverse.gaussian / Gamma), gamma makes more sense for this dataset
# In the dataset, there is a non-positive value (a zero) somewhere
# thus, create dataset copies that do not have this zero and perform glmm with these
traj.essen.pbs.nonzero <- dplyr::filter(traj.essen.pbs, mean_speed > 0)
essen.trajectory.nonzero <- dplyr::filter(essen.trajectory, mean_speed > 0)
glmm.Essen.pbs <- lme4::glmer(mean_speed ~ status + (status|patient), traj.essen.pbs.nonzero, family = inverse.gaussian(link = "identity"))
glmm.Essen.allTreat <- lme4::glmer(mean_speed ~ status * treatment + (treatment|patient) + (status|patient), data = essen.trajectory.nonzero, family = Gamma(link = "identity"))

# Residuals plot
plot(glmm.Essen.allTreat, type = c("p", "smooth"))
# Scale-location
plot(glmm.Essen.allTreat, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))
# QQ plot
lattice::qqmath(glmm.Essen.allTreat, id = 0.05)


#Summary and prediction
summary(glmm.Essen.pbs)
sjPlot::plot_model(glmm.Essen.pbs,  type = "pred", title = "Predicted mean speed: random status|patient")
summary(glmm.Essen.allTreat)
sjPlot::plot_model(glmm.Essen.allTreat,  type = "pred",terms = c("treatment", "status"), title = "Predicted mean speed: random treatment|patient")
# Conclusion: Gamma-based seems better than inverse gaussian. But overall the result is very similar to standard gaussian modelling.

