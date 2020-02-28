# better tracked data
datalocations <-c("D:\\CellMIA\\p2_pbs.txt", "D:\\CellMIA\\p2_fmlp.txt") #"D:\\CellMIA\\p2_cxcl1.txt", "D:\\CellMIA\\p2_cxcl8.txt"
readtables <- function(data_){
  alltables <- list(length(datalocations))
  for(file_ in data_){
    i <- 1
    table_ <- read.table(file_, header = TRUE, sep="\t")
    alltables[i] <- table_
    i <- i+1
  }
  return (alltables)
}
alltables <- readtables(datalocations)


datalocation <- "D:\\CellMIA\\0001_mode1_z000_f000_tracking.txt"
coordinates <- read.table(datalocation, header=TRUE, sep="\t")
names(coordinates)[names(coordinates) == 'ID.of.track'] <- 'track_id'
#track_id naar factor
coordinates$track_id <- as.factor(coordinates$track_id)
names(coordinates)[names(coordinates) == 'time.index'] <- 'frame_id'
names(coordinates)[names(coordinates) == 'cell.row'] <- 'Y'
names(coordinates)[names(coordinates) == 'cell.col'] <- 'X'
names(coordinates)[names(coordinates) == 'velocity..pixels.moved.between.timepoints...µm.'] <- 'speed' # pixels/interval between frames
names(coordinates)[names(coordinates) == 'Angle.of.movement'] <- 'ta_deg'
names(coordinates)[names(coordinates) == 'Delta.of.Angle.of.movement'] <- 'delta_ta_deg'
names(coordinates)[names(coordinates) == 'angle.of.movement.relative.to.center'] <- 'norm_ta_deg'
names(coordinates)[names(coordinates) == 'length.of.track..total.number.of.timepoints.track.was.found.'] <- 'track_length'


# this is the evolving TRACK directionality, it is the ratio of current euclidian/current cumulative distance travelled
names(coordinates)[names(coordinates) == 'Consistency.of.motion'] <- 'average_directness'
names(coordinates)[names(coordinates) == 'Cumulated.distance.travelled..µm.'] <- 'cumulative_distance'
names(coordinates)[names(coordinates) == 'Distance.between.start.and.endpoint..µm.'] <- 'euclidian_distance'


#coordinates file is step centric datafile. Which columns are still missing?
# norm X and Y and delta X and Y, do we need these?

#make trajectory centric datafile
#need: mean speed ///  average directness, cumulative distance and euclidian distance are already there
#"conversion factor"= 1.55038 what is this?

# data moet in column vectoren zitten om trajectory dataframe te maken
col1 <- unique(coordinates[["track_id"]])
col2 <- numeric(length(col1)) #mean displacement
col3 <- numeric(length(col1)) #average directness   take second to last value: x[nrow(x)-1], length() returns amount of columns!
col4 <- numeric(length(col1)) #cumulative distance
col5 <- numeric(length(col1)) #euclidian distance
for (track in 1:length(col1)) {
  onetrack <- subset(coordinates, coordinates$track_id==track)
  col2[track] <- mean(onetrack$speed, na.rm = TRUE)
  col3[track] <- onetrack[nrow(onetrack)-1, "average_directness"]
  col4[track] <- onetrack[1,"cumulative_distance"]
  col5[track] <- onetrack[1,"euclidian_distance"]
}
trajectory_data <- data.frame(track_id = col1, mean_speed = col2, average_directness = col3, cumulative_distance = col4, euclidian_distance = col5)


## Trajectory-centric analysis

#data exploration: plots
b <- ggplot(data=coordinates, aes(x = X, y = Y))
b+ geom_point(shape = 20, aes(color = track_id), show.legend = FALSE)
ggplot(coordinates, aes(x = norm_ta_deg)) +
  geom_histogram(binwidth = 8) +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  coord_polar()
plot(density(subset(trajectory_data$mean_speed)))
hist(trajectory_data$average_directness)
plot(trajectory_data$average_directness, trajectory_data$mean_delta_z)


#normality tests
# try to normalize
par(mfrow=c(2,3))
qqPlot(trajectory_data$average_directness)  #car package
qqPlot(log2(trajectory_data$average_directness))
qqPlot(log10(trajectory_data$average_directness))
qqPlot(log(trajectory_data$average_directness))
qqPlot(exp(trajectory_data$average_directness))
qqPlot(sqrt(trajectory_data$average_directness))
par(mfrow=c(1,1))

# wilcox-test

#linear model
##in case of multiple treatment: set PBS (or equivalent control) as baseline for the model to compare with
trajectories_releveled <- within(trajectory_data, treatment <- relevel(treatment, ref = "PBS"))







#MDS?

#probabilistic index models
# pim_deltaz <- pim(formula=mean_delta_z ~ ... + ..., data = trajectory_data)
#summary(pim_deltaz)
#coef(pim_deltaz)

#differential variability mood test

#omnibus tests



## Step-centric analysis

#features as functions in time
#splines and wavelet basis functions to smooth the temporal profiles

#basis function expansion





#FPCA + hierarchichal and model-based clustering
