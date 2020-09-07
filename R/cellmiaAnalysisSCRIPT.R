# better tracked data
#samples MIA: 3=control, 4=fmlp, 5=cxcl8, 6=cxcl1
datalocations <-
  c("D:\\CellMIA\\p2_pbs.txt", "D:\\CellMIA\\p2_fmlp.txt", "D:\\CellMIA\\p2_cxcl1.txt", "D:\\CellMIA\\p2_cxcl8.txt",
    "D:\\CellMIA\\p3_pbs.txt", "D:\\CellMIA\\p3_fmlp.txt", "D:\\CellMIA\\p3_cxcl1.txt", "D:\\CellMIA\\p3_cxcl8.txt",
    "D:\\CellMIA\\p4_pbs.txt", "D:\\CellMIA\\p4_fmlp.txt", "D:\\CellMIA\\p4_cxcl1.txt", "D:\\CellMIA\\p4_cxcl8.txt",
    "D:\\CellMIA\\p5_pbs.txt", "D:\\CellMIA\\p5_fmlp.txt", "D:\\CellMIA\\p5_cxcl1.txt", "D:\\CellMIA\\p5_cxcl8.txt") #
data_original = lapply(datalocations,
                  FUN = read.table,
                  header = TRUE,
                  sep = "\t")

#----------------------- IN CASE OF GUI LATER ----------------------------
#read single table, read class1 and class2 text fields (2x2 fields, column name & specific factor)
#create new column for the classifier, append dataframe to list for later rbind
# on button load new dataset, clear fields, prompt new file selection popup
# save classifier column names in memory for later possible presentation to user and subsetting options
dataclassifier1 <- NULL
dataclassifier2 <- NULL

datalocationtext <- "" # later: read text field in gui
temp.table <- read.table(datalocationtext, header = TRUE, sep = "\t")

if (classifiersPresent){
  #when classifiers are already present in the selected file, ONLY ENABLE classifier type editing. This HAS to correspond to a header element.

} else{
  temp.table[classifier1typetext] <- "" # read classifier 1 text field
  if (classifier2text != empty){
    temp.table[classifier2typetext] <- "" # read classifier 2 text field
  }
}


# Make sure user can set classifiers for data
# what if classifier is already a column?
# Classifier 1 here = treatment  (lowest level classifier / separate image, separate well)
# Classifier 2 here = patient    (higher level classifier, concerns a group of classifier 1's / cell line)
# Lowest level qualifiers can be grouped with column identifier, higher level in separate dataframe
#------------ END ---------------------------------------------------


# SCRIPT funtion for adding classifier columns (so that it is collapsable)
addClassifierData <- function(listOfReadDataframes) {
  #currently 16 datasets
  listOfReadDataframes[[1]]$treatment <- "pbs"
  listOfReadDataframes[[2]]$treatment <- "fmlp"
  listOfReadDataframes[[3]]$treatment <- "cxcl1"
  listOfReadDataframes[[4]]$treatment <- "cxcl8"
  listOfReadDataframes[[5]]$treatment <- "pbs"
  listOfReadDataframes[[6]]$treatment <- "fmlp"
  listOfReadDataframes[[7]]$treatment <- "cxcl1"
  listOfReadDataframes[[8]]$treatment <- "cxcl8"
  listOfReadDataframes[[9]]$treatment <- "pbs"
  listOfReadDataframes[[10]]$treatment <- "fmlp"
  listOfReadDataframes[[11]]$treatment <- "cxcl1"
  listOfReadDataframes[[12]]$treatment <- "cxcl8"
  listOfReadDataframes[[13]]$treatment <- "pbs"
  listOfReadDataframes[[14]]$treatment <- "fmlp"
  listOfReadDataframes[[15]]$treatment <- "cxcl1"
  listOfReadDataframes[[16]]$treatment <- "cxcl8"
  listOfReadDataframes[[1]]$patient <- "2"
  listOfReadDataframes[[2]]$patient <- "2"
  listOfReadDataframes[[3]]$patient <- "2"
  listOfReadDataframes[[4]]$patient <- "2"
  listOfReadDataframes[[5]]$patient <- "3"
  listOfReadDataframes[[6]]$patient <- "3"
  listOfReadDataframes[[7]]$patient <- "3"
  listOfReadDataframes[[8]]$patient <- "3"
  listOfReadDataframes[[9]]$patient <- "4"
  listOfReadDataframes[[10]]$patient <- "4"
  listOfReadDataframes[[11]]$patient <- "4"
  listOfReadDataframes[[12]]$patient <- "4"
  listOfReadDataframes[[13]]$patient <- "5"
  listOfReadDataframes[[14]]$patient <- "5"
  listOfReadDataframes[[15]]$patient <- "5"
  listOfReadDataframes[[16]]$patient <- "5"
  return(listOfReadDataframes)
}
data_original <- addClassifierData(data_original)
# bind separate dataframes together by row, assuming the same header/columns for all files
data.step = do.call("rbind", data_original)

# ------------------ IDEA GUI and standardization------------------------------
#if user uploads several small dataframes, show them the bound head() of all uploads before continuing
# + add option to start over if they did something wrong in that case?

# ----------- using this function for standardising columns might be difficult as it would require all names
# ----------- (also ones that wouldn't need to be changed) AND in the right order
# dataframe <- lapply(dataframe, function(x){
#   colnames(x) <- c("NewName1", "NewName2")
#   x
# } )
# --------------------------------------------------------

data.step <- standardise_names_cellmia(data.step)
data.step$treatment <- as.factor(data.step$treatment)
data.step$patient <- as.factor(data.step$patient)

# first: subset the big dataframe by treatment/patient/...
split_list <- split_by_factor(data.step, "patient")
#make trajectory centric datafile(s), currently only one classifier possible internally
traj.list <- lapply(split_list, create_trajectory_table, "treatment")
# add higher classifier
traj.list[[1]]$patient <- "2"
traj.list[[2]]$patient <- "3"
traj.list[[3]]$patient <- "4"
traj.list[[4]]$patient <- "5"
data.trajectory <- do.call("rbind", traj.list)


## Trajectory-centric analysis

#data exploration: plots


# Reminder: par(mfrow) does not work with ggplot
plot_coordinates(data.step, "treatment", "pbs")
plot_coordinates(data.step, "treatment", "fmlp")
plot_turningangles(data.step, "treatment", "pbs")
plot_turningangles(data.step, "treatment", "fmlp")


plot(density(data.trajectory$mean_speed), main= "Mean speed, all treatments")
plot(density((quick_subset_classifier(data.trajectory, "treamtent", "pbs"))$mean_speed), main= "Mean speed, pbs")
plot(density((quick_subset_classifier(data.trajectory, "treamtent", "fmlp"))$mean_speed), main= "Mean speed, fmlp")

plot(density(data.trajectory$average_directness))
plot(density((quick_subset_classifier(data.trajectory, "treamtent", "pbs"))$average_directness), main= "Avg directness, pbs")
plot(density((quick_subset_classifier(data.trajectory, "treamtent", "fmlp"))$average_directness), main= "Avg directness, fmlp")
plot(data.trajectory$average_directness,
     data.trajectory$mean_speed, main="All conditions")
plot(quick_subset_classifier(data.trajectory, "treamtent", "pbs")$average_directness, quick_subset_classifier(data.trajectory, "treamtent", "pbs")$mean_speed, main = "PBS", xlab = "Avg directness", ylab = "Mean speed")
plot(quick_subset_classifier(data.trajectory, "treamtent", "fmlp")$average_directness, quick_subset_classifier(data.trajectory, "treamtent", "fmlp")$mean_speed, main = "FMLP", xlab = "Avg directness", ylab = "Mean speed")


#Automatically detect highly skewed features using skewness()?
#go through all columns, remember feature name
#skewness(feature, na.rm = TRUE)
# if <-1 or >1 --> highly skewed left/right, show feature name
# others are moderately skewed to symmetrical

#--------------------Exploration: PCA and MDS---------------------------------------
# princomp(): Spectral decomposition; examines the covariances / correlations between variables
# prcomp(): Singular value decomposition; examines the covariances / correlations between individuals (slightly better numerical accuracy)

trajectory.pca <- prcomp(subset(data.trajectory, select = -c(track_id, treatment, patient)), scale = TRUE) # remove factors from PCA
# visualize eigenvectors
summary(trajectory.pca)
ggplot2::autoplot(trajectory.pca, loadings = TRUE, loadings.labels = TRUE, loadings.label.size = 3, data = data.trajectory, colour = "treatment") #to add colour groups: insert "data = dataframe, colour = 'column'"
ggplot2::autoplot(trajectory.pca, loadings = TRUE, loadings.labels = TRUE, loadings.label.size = 3, data = data.trajectory, colour = "patient")



#------------------------------------------------------------------------------
#normality tests --- check box-cox method  https://stackoverflow.com/questions/33999512/how-to-use-the-box-cox-power-transformation-in-r
# transform dependent and independent variables by fitting boxcox(lm(variable ~ 1, data=df)) for each column of your matrix
# range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2]) to get confidence interval for lambda
# you can get a common lambda by simply doing boxcox(as.matrix(x[,-ncol(x)]) ~ x[,ncol(x)]), assuming x is your data frame and the last column is your phenotype
boxcox(lm(mean_speed~treatment,data=data.trajectory),lambda=seq(0,1,by=.1))
out<-boxcox(lm(mean_speed~1, data=data.trajectory))
# confidence intervals for lambda, constructed using the idea of a likelihood ratio test
range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])


bestNormalize(x, standardize = TRUE, allow_orderNorm = TRUE, allow_exp = TRUE,
              out_of_sample = TRUE, cluster = NULL, k = 10, r = 5,
              loo = FALSE)

# try to normalize
par(mfrow = c(2, 3))
qqPlot(data.trajectory$average_directness)  #car package
qqPlot(log2(data.trajectory$average_directness))
qqPlot(log10(data.trajectory$average_directness))
qqPlot(log(data.trajectory$average_directness))
qqPlot(exp(data.trajectory$average_directness))
qqPlot(sqrt(data.trajectory$average_directness))
par(mfrow = c(1, 1))

# wilcox-test

#linear model
##in case of multiple treatment: set PBS (or equivalent control) as baseline for the model to compare with
trajectories_releveled <-
  within(data.trajectory, treatment <-
           relevel(treatment, ref = "PBS"))




#MDS?

#probabilistic index models
pim_deltaz <- pim(formula=mean_speed ~ treatment * patient, data = data.trajectory)
summary(pim_deltaz)
coef(pim_deltaz)

#differential variability mood test

#omnibus tests



## Step-centric analysis

#features as functions in time
#splines and wavelet basis functions to smooth the temporal profiles

#basis function expansion





#FPCA + hierarchichal and model-based clustering
