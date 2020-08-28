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
#need: mean speed ///  average directness, cumulative distance and euclidian distance are already there

# first: subset the big dataframe by treatment/patient/...


split_by_factor <- function(large_dataframe, split_factor) {
  l <- vector("list", length(unique(large_dataframe[[split_factor]])))
  i <- 1
  for (factr in unique(large_dataframe[[split_factor]])) {
    sub.set <-
      large_dataframe[which(large_dataframe[[split_factor]] == factr), ]
    l[[i]] <- sub.set
    i <- i + 1
  }
  return (l)
}

create_trajectory_table <- function(large_step_table, split_factors = NULL) {
    # when the table does not need to be split, we can just create the trajectory data immediately
    if (is.null(split_factors)) {
      large_trajectory_table <-
        create_single_trajectory_table(large_step_table)
    } else {
      # first split the data then create trajectory table for each, then bind together
      # at this point having to split twice is still not possible!!!! (eg treatment AND patient)
      # should this become too difficult, we can overcome this by making all track id's unique in the large dataframe
      split_list <- split_by_factor(large_step_table, split_factors)
      traj.list <- lapply(split_list, create_single_trajectory_table, split_factors)
      large_trajectory_table <- do.call("rbind", traj.list)
    }

    return(large_trajectory_table)
  }

create_single_trajectory_table <- function(step_table, split_factors = NULL) {
  # data has to be in column vectors to make trajectory dataframe
  col1 <- unique(step_table[["track_id"]])
  col2 <- numeric(length(col1)) #mean displacement
  col3 <- numeric(length(col1)) #average directness   take second to last value: x[nrow(x)-1], length() returns amount of columns!
  col4 <- numeric(length(col1)) #cumulative distance
  col5 <- numeric(length(col1)) #euclidian distance
  col6 <- numeric(length(col1)) #placeholder for split factor
  for (track in 1:length(col1)) {
    onetrack <- subset(step_table, step_table$track_id == track)
    col2[track] <- mean(onetrack$speed, na.rm = TRUE)
    col3[track] <- onetrack[nrow(onetrack) - 1, "average_directness"]
    col4[track] <- onetrack[1, "cumulative_distance"]
    col5[track] <- onetrack[1, "euclidian_distance"]
    if (!is.null(split_factors)) {
      # without as.character, treatment is 1 or 2 (internal numeric)
      col6[track] <- as.character(onetrack[1, split_factors])
    }
  }
  trajectory_table <-
    data.frame(
      track_id = col1,
      mean_speed = col2,
      average_directness = col3,
      cumulative_distance = col4,
      euclidian_distance = col5
    )
  if (!is.null(split_factors)) {
    trajectory_table[[split_factors]] <- col6
  }
  return(trajectory_table)
}

quick_subset_treatment <- function(datatable, split_factor){
  sub.set <-
    datatable[which(datatable[["treatment"]] == split_factor),]
  return(sub.set)
}

data.trajectory <- create_trajectory_table(data.step, "treatment")

## Trajectory-centric analysis

#data exploration: plots
plot_coordinates <- function(dataset, subset_column, subset_factor) {
  b <- ggplot(data = dataset[which(dataset[[subset_column]] == subset_factor), ], aes(x = X, y = Y))
b + geom_point(shape = 20,
               aes(color = track_id),
               show.legend = FALSE)
}
plot_turningangles <- function(dataset, subset_column, subset_factor){
  ggplot(dataset[which(dataset[[subset_column]] == subset_factor), ], aes(x = ta_deg)) +
  geom_histogram(binwidth = 8) +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  coord_polar()
}

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
