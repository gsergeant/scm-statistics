# better tracked data
#samples MIA: 3=control, 4=fmlp, 5=cxcl8, 6=cxcl1
datalocations <-
  c("data/Leuko_Ghent/p2_pbs.txt", "data/Leuko_Ghent/p2_fmlp.txt", "data/Leuko_Ghent/p2_cxcl1.txt", "data/Leuko_Ghent/p2_cxcl8.txt",
    "data/Leuko_Ghent/p3_pbs.txt", "data/Leuko_Ghent/p3_fmlp.txt", "data/Leuko_Ghent/p3_cxcl1.txt", "data/Leuko_Ghent/p3_cxcl8.txt",
    "data/Leuko_Ghent/p4_pbs.txt", "data/Leuko_Ghent/p4_fmlp.txt", "data/Leuko_Ghent/p4_cxcl1.txt", "data/Leuko_Ghent/p4_cxcl8.txt",
    "data/Leuko_Ghent/p5_pbs.txt", "data/Leuko_Ghent/p5_fmlp.txt", "data/Leuko_Ghent/p5_cxcl1.txt", "data/Leuko_Ghent/p5_cxcl8.txt") #
data_original = lapply(datalocations,
                  FUN = read.table,
                  header = TRUE,
                  sep = "\t")

# calculate delta z from x and y + use sum to calculate rolling cumulative distance
for (i in 1:length(data_original)) {
  data_original[[i]]$delta_z = 0      # create placeholder delta z that is 0
  for (rownr in 1:length(data_original[[i]])) { # get full dataframe, go through each row
                      # remember x and y, calculate delta z and add
  }
}



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


# standardise column names of SC data, define factors
data.step <- standardise_names_cellmia(data.step)
data.step$treatment <- as.factor(data.step$treatment)
data.step$patient <- as.factor(data.step$patient)

# Create TC data
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
plot_turningangles(patient4, "treatment", "pbs")
plot_turningangles(patient4, "treatment", "fmlp")


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

#scatter plot matrix of all features, pairwise
car::scatterplotMatrix(quick_subset_classifier(patient4, "treatment", "fmlp")[3:8])

#Automatically detect highly skewed features using skewness()?
#go through all columns, remember feature name
#skewness(feature, na.rm = TRUE)
# if <-1 or >1 --> highly skewed left/right, show feature name
# others are moderately skewed to symmetrical

#--------------GG Statsplot ------------------
ggstatsplot::ggbetweenstats(
  data = data.trajectory,
  x = patient,
  y = mean_speed,
  title = "Distribution of mean speed across patients"
)

ggstatsplot::grouped_ggbetweenstats(
  data = dplyr::filter(
    .data = data.trajectory,
    treatment %in% c("pbs", "cxcl1", "cxcl8", "fmlp")
  ),
  x = patient,
  y = mean_speed,
  grouping.var = treatment, # grouping variable
  pairwise.comparisons = TRUE, # display significant pairwise comparisons
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni", # method for adjusting p-values for multiple comparisons
  # adding new components to `ggstatsplot` default
  ggplot.component = list(ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())),
  k = 3,
  title.prefix = "Treatment",
  palette = "default_jama",
  package = "ggsci",
  plotgrid.args = list(nrow = 2),
  title.text = "Differences in mean speed by patient for different treatments"
)

# now grouped the other way around
ggstatsplot::grouped_ggbetweenstats(
  data = dplyr::filter(
    .data = data.trajectory,
    patient %in% c("2", "3", "4", "5")
  ),
  x = treatment,
  y = mean_speed,
  grouping.var = patient, # grouping variable
  pairwise.comparisons = TRUE, # display significant pairwise comparisons
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni", # method for adjusting p-values for multiple comparisons
  # adding new components to `ggstatsplot` default
  ggplot.component = list(ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())),
  k = 3,
  title.prefix = "Patient",
  palette = "default_jama",
  package = "ggsci",
  plotgrid.args = list(nrow = 2),
  title.text = "Differences in mean speed by treatment for different patients"
)


# now for step data
ggstatsplot::grouped_ggbetweenstats(
  data = dplyr::filter(
    .data = data.step,
    patient %in% c("2", "3", "4", "5")
  ),
  x = treatment,
  y = speed,
  grouping.var = patient, # grouping variable
  pairwise.comparisons = TRUE, # display significant pairwise comparisons
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni", # method for adjusting p-values for multiple comparisons
  # adding new components to `ggstatsplot` default
  ggplot.component = list(ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())),
  k = 3,
  title.prefix = "Patient",
  palette = "default_jama",
  package = "ggsci",
  plotgrid.args = list(nrow = 2),
  title.text = "Differences in instantaneous speed by treatment for different patients"
)

#correlation matrix plot
ggstatsplot::ggcorrmat(
  data = data.trajectory,
  type = "robust", # correlation method
  p.adjust.method = "holm", # p-value adjustment method for multiple comparisons
  cor.vars = c(mean_speed:euclidian_distance), # a range of variables can be selected
  cor.vars.names = c(
    "Mean Speed", # variable names
    "Average Directness",
    "Cumulative Distance",
    "Euclidian Distance"
  ),
  matrix.type = "upper", # type of visualization matrix
  colors = c("#B2182B", "white", "#4D4D4D"),
  title = "Correlalogram for track TC features",
  subtitle = "Trajectory-centric migration features",
  caption = "Source: `ggstatsplot` R package"
)

#--------------------Exploration: PCA and MDS---------------------------------------
# princomp(): Spectral decomposition; examines the covariances / correlations between variables
# prcomp(): Singular value decomposition; examines the covariances / correlations between individuals (slightly better numerical accuracy)

trajectory.pca <- prcomp(subset(data.trajectory, select = -c(track_id, treatment, patient)), scale = TRUE) # remove factors from PCA
# visualize eigenvectors
summary(trajectory.pca)
trajectory.pca$rotation[,1:4]
#weird: in order for autoplot to work, first use ggfortify:: (gives error), then ggplot2:: works.
ggplot2::autoplot(trajectory.pca, loadings = TRUE, loadings.labels = TRUE, loadings.label.size = 3, data = data.trajectory, colour = "treatment") #to add colour groups: insert "data = dataframe, colour = 'column'"
ggplot2::autoplot(trajectory.pca, loadings = TRUE, loadings.labels = TRUE, loadings.label.size = 3, data = data.trajectory, colour = "patient")


#MDS? Probably remove
trajectory.dist <- dist(data.trajectory)

trajectory.mds <- cmdscale(trajectory.dist)
plot(trajectory.mds)


trajectory.isomds <- MASS::isoMDS(trajectory.dist, k=2)
x<-trajectory.isomds$points[,1]
y<-trajectory.isomds$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(data.trajectory), cex=.7)
autoplot(trajectory.isomds)

#-------------------------Normalization -----------------------------------
ms.bn <- bestNormalize::bestNormalize(data.trajectory$mean_speed)
ad.bn <- bestNormalize::bestNormalize(data.trajectory$average_directness)
MASS::truehist(data.trajectory$mean_speed)
MASS::truehist(ms.bn$x.t)
MASS::truehist(data.trajectory$average_directness)
MASS::truehist(ad.bn$x.t)

car::qqPlot(ms.bn$x.t) #perfectly straight QQ plot

tempdata.trajectory <- data.frame(ms.bn$x.t, ad.bn$x.t, data.trajectory$patient, data.trajectory$treatment)
linmod.t <- lm(ms.bn.x.t ~ data.trajectory.patient * data.trajectory.treatment, data = tempdata.trajectory)
summary(linmod.t)

#------------------- Wilcox-test -----------

#linear model
##in case of multiple treatment: set PBS (or equivalent control) as baseline for the model to compare with
trajectories_releveled <-
  within(data.trajectory, treatment <-
           relevel(treatment, ref = "pbs"))




# ---------------------------- Probabilistic index models--------------------------
# PI summarizes the covariate effects on the shape of the response distribution, while remaining a very informative interpretation of the covariate effect sizes.
# to add intercept, use +i in formula
# PIM with 2 groups == WMW test?

# double colon for pim function does not seem to work?
pim_speed <- pim(formula=mean_speed ~ treatment * patient +1, data = data.trajectory)
summary(pim_speed)
coef(pim_speed)

pim_directness <- pim(formula=average_directness ~ treatment * patient +1, data = data.trajectory)
summary(pim_directness)
coef(pim_directness)


#differential variability mood test

#omnibus tests


#----------------------------------------------------------------------------------------
## Step-centric analysis

#features as functions in time
patient4 <- quick_subset_classifier(data.step, "patient", "4")
#take sample of x tracks to plot
sampl <-sample(patient4$track_id, 1)
testp4 <- quick_subset_classifier(patient4, "track_id", sampl)
plot_speedProfile(testp4, "treatment", "fmlp")

plot_speedProfile(data.step, "patient", "4")

#---------------- Idea from paper ------------
# singular value decomposition to get eigenvectors
# use eigenvectors to choose dimensionality for modeling non-linear responses
# then regress eigenvectors into i-dimensional spline basis, knots evenly spaced at quantiles
# then cross validation to find optimal i across all features?
# using selected i, fit least-squares model to estimate population average curves and variance for all features under alternative and null hypothesis models
# --------------------------

# ----------- Try step-centric PIM ------------
#include frame data as well?
pim_speed <- pim(formula=speed ~ treatment * patient +1, data = data.step) #!!!! error: cannot allocate vector of size 169.3 Gb
# Try pim on pca components?

#splines and wavelet basis functions to smooth the temporal profiles
#basis function expansion,    fda package
#1.Construct the basis functions: create.bspline.basis    2.Smooth basis by smooth.basis    3.FPCA by pca.fd on train dataset

fdatryoutcondition <- quick_subset_classifier(quick_subset_classifier(data.step, "patient", "2"), "treatment", "pbs")
plot_speedProfile(quick_subset_classifier(data.step, "patient", "4"), "treatment", "pbs")
FPCAobj <- FPCA(Ly=fdatryoutcondition[["speed"]], Lt=fdatryoutcondition[["frame_id"]])
testi <- fdata(fdatryoutcondition)



# ------------- FPCA + hierarchichal and model-based clustering -------
# Normal pca
# SOME features are null at start/end of track, fix this first!! (best option most likely remove entire row)
# do pca with percentiles?? no problem with missing data this way
step.pca <- prcomp(subset(data.step, select = c(X, Y, speed, ta_deg, delta_ta_deg, norm_ta_deg)), scale = TRUE) # select which features to include in pca

