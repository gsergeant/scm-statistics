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


data_original[[1]]$treatment <- "pbs"
data_original[[2]]$treatment <- "fmlp"
data_original[[3]]$treatment <- "cxcl1"
data_original[[4]]$treatment <- "cxcl8"


# bind separate dataframes together by row, assuming the same header/columns for all files
data.step = do.call("rbind", data_original)


# ----------- using this function for standardising columns might be difficult as it would require all names
# ----------- (also ones that wouldn't need to be changed) AND in the right order
# dataframe <- lapply(dataframe, function(x){
#   colnames(x) <- c("NewName1", "NewName2")
#   x
# } )
# --------------------------------------------------------
standardise_names_cellmia <- function(dataframe) {
  # whenever I would want to generalize this, would need a gui where user selects which columns names conform to which standardised names, and work with the textfields
  names(dataframe)[names(dataframe) == 'ID.of.track'] <- 'track_id'
  names(dataframe)[names(dataframe) == 'time.index'] <- 'frame_id'
  names(dataframe)[names(dataframe) == 'cell.row'] <- 'Y'   #pixels
  names(dataframe)[names(dataframe) == 'cell.col'] <- 'X'   #pixels
  names(dataframe)[names(dataframe) == 'velocity..pixels.moved.between.timepoints...µm.'] <-
    'speed' # pixels/interval between frames
  names(dataframe)[names(dataframe) == 'Angle.of.movement'] <-
    'ta_deg'
  names(dataframe)[names(dataframe) == 'Delta.of.Angle.of.movement'] <-
    'delta_ta_deg'
  names(dataframe)[names(dataframe) == 'angle.of.movement.relative.to.center'] <-
    'norm_ta_deg'  # compared to the middle of the image frame
  names(dataframe)[names(dataframe) == 'length.of.track..total.number.of.timepoints.track.was.found.'] <-
    'track_length'

  # this is the evolving TRACK directionality, it is the ratio of current euclidian/current cumulative distance travelled
  names(dataframe)[names(dataframe) == 'Consistency.of.motion'] <-
    'average_directness'
  names(dataframe)[names(dataframe) == 'Cumulated.distance.travelled..µm.'] <-
    'cumulative_distance'
  names(dataframe)[names(dataframe) == 'Distance.between.start.and.endpoint..µm.'] <-
    'euclidian_distance'

  #track_id naar factor
  dataframe$track_id <- as.factor(dataframe$track_id)

  return(dataframe)
}

data.step <- standardise_names_cellmia(data.step)
data.step$treatment <- as.factor(data.step$treatment)

# data.step file is step centric datafile. Which columns are still missing?
# norm X and Y and delta X and Y, do we need these?

#make trajectory centric datafile
data.trajectory <- create_trajectory_table(data.step, "treatment")

# first: subset the big dataframe by treatment/patient/...

## Trajectory-centric analysis

#data exploration: plots


# !reminder: par(mfrow) werkt niet met ggplot!
plot_coordinates(data.step, "treatment", "pbs")
plot_coordinates(data.step, "treatment", "fmlp")
plot_turningangles(data.step, "treatment", "pbs")
plot_turningangles(data.step, "treatment", "fmlp")


plot(density(data.trajectory$mean_speed), main= "Mean speed, all treatments")
plot(density((quick_subset_treatment(data.trajectory, "pbs"))$mean_speed), main= "Mean speed, pbs")
plot(density((quick_subset_treatment(data.trajectory, "fmlp"))$mean_speed), main= "Mean speed, fmlp")

plot(density(data.trajectory$average_directness))
plot(density((quick_subset_treatment(data.trajectory, "pbs"))$average_directness), main= "Avg directness, pbs")
plot(density((quick_subset_treatment(data.trajectory, "fmlp"))$average_directness), main= "Avg directness, fmlp")
plot(data.trajectory$average_directness,
     data.trajectory$mean_speed, main="All conditions")
plot(quick_subset_treatment(data.trajectory, "pbs")$average_directness, quick_subset_treatment(data.trajectory, "pbs")$mean_speed, main = "PBS", xlab = "Avg directness", ylab = "Mean speed")
plot(quick_subset_treatment(data.trajectory, "fmlp")$average_directness, quick_subset_treatment(data.trajectory, "fmlp")$mean_speed, main = "FMLP", xlab = "Avg directness", ylab = "Mean speed")


#Automatically detect highly skewed features using skewness()?
#go through all columns, remember feature name
#skewness(feature, na.rm = TRUE)
# if <-1 or >1 --> highly skewed left/right, show feature name
# others are moderately skewed to symmetrical

#------------------------------------------------------------------------------
# Exploration: PCA and MDS

# princomp(): Spectral decomposition; examines the covariances / correlations between variables
# prcomp(): Singular value decomposition; examines the covariances / correlations between individuals (slightly better numerical accuracy)
trajectory.pca <- prcomp(subset(data.trajectory, select = -c(track_id, treatment)), scale = TRUE) # remove factors from PCA
# visualize eigenvectors
summary(trajectory.pca)
autoplot(trajectory.pca, loadings = TRUE, loadings.labels = TRUE, loadings.label.size = 3) #to add colour groups: insert "data = dataframe, colour = 'column'"




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
pim_deltaz <- pim(formula=mean_speed ~ treatment, data = data.trajectory)
summary(pim_deltaz)
coef(pim_deltaz)

#differential variability mood test

#omnibus tests



## Step-centric analysis

#features as functions in time
#splines and wavelet basis functions to smooth the temporal profiles

#basis function expansion





#FPCA + hierarchichal and model-based clustering
