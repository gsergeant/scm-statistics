## Creates step-centric .. dataframes (?) and plots
# Setup ggplot
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

## first try with healthy patient control condition
step_data = read.csv("data/features_step_NAN.txt", header = TRUE)
str(step_data)
### placeholder for data getting in future
# step_data = loadData.getdata()

# neem een milde ziekte patient, treatment controle, een random tracknummer
# maak een aparte plot voor elke feature in functie van de tijd = 5(?) plots (delta x, -, -z, ta_deg én snelheid = delta_z/tijd)
get_random_trackno <- function(dataframe, amount){
  return(sample(1:max(dataframe$track_id), amount))
}

make_subset_table <- function(dataframe, trackno){
  # make 2-column subset with specific track and specific x and y (2 additional arguments needed)
  #subset_ <- data.frame(feature_x=dataframe[dataframe$track_id==trackno, feature_x], feature_y=dataframe[dataframe$track_id==trackno, feature_y])
  subset_ <- subset(dataframe, track_id == trackno)
  return(subset_)
}

# #this function is not really working yet, table and plot names weird
# plot_stepwise_feature <- function(table){
#   x_name <- colnames(table)[1]
#   y_name <- colnames(table)[2]
#   plot_ <- ggplot(data = table, aes(x=x_name, y=y_name))+geom_point(shape=1)
#   return(plot_)
# }


mildPBS <- subset(step_data, (patientnumber == 858&cellType_treatment == "PBS"))
track_one <- get_random_trackno(mildPBS, 1)

# Plot distribution of track lenghts
unique(mildPBS$track_id)
track_lenghts <- c()
for (track in unique(mildPBS$track_id)) {
  temptracktable <- subset(mildPBS, track_id == track)
  tracklength <- nrow(temptracktable)
  track_lenghts <- c(track_lenghts, tracklength)
}
plot(track_lenghts)
hist(track_lenghts)



# par(mfrow=c(1,1)) does not work with ggplot

track_table <- make_subset_table(mildPBS, track_one)
#I want to get the colour right. currently for more than 1 track, it's a gradient. I want discrete colours
ggplot(data = track_table, aes(x=t, y=delta_x, color=track_id))+geom_point(shape=16, size=2)
ggplot(data = track_table, aes(x=t, y=delta_z, color=track_id))+geom_point(shape=16, size=2)
ggplot(data = track_table, aes(x=t, y=ta_deg, color=track_id))+geom_point(shape=16, size=2)

# speed
df_speed <- data.frame(t=track_table$t, speed=track_table$delta_z/track_table$t)
ggplot(data = df_speed, aes(x=t, y=speed))+geom_point(shape=16)


# doe het opnieuw voor een tweede random tracknummer van dezelfde patient
track_two <- get_random_trackno(mildPBS, 1)
track_table <- make_subset_table(mildPBS, track_two)

ggplot(data = track_table, aes(x=t, y=delta_x, color=track_id))+geom_point(shape=16, size=2)
ggplot(data = track_table, aes(x=t, y=delta_z, color=track_id))+geom_point(shape=16, size=2)
ggplot(data = track_table, aes(x=t, y=ta_deg, color=track_id))+geom_point(shape=16, size=2)

# speed
df_speed <- data.frame(t=track_table$t, speed=track_table$delta_z/track_table$t)
ggplot(data = df_speed, aes(x=t, y=speed))+geom_point(shape=16)



# opnieuw voor een random track van severe patient
severePBS <- subset(step_data, (patientnumber == 722&cellType_treatment == "PBS"))
track_three <- get_random_trackno(severePBS, 1)

track_table <- make_subset_table(mildPBS, track_three)

ggplot(data = track_table, aes(x=t, y=delta_x, color=track_id))+geom_point(shape=16, size=2)
ggplot(data = track_table, aes(x=t, y=delta_z, color=track_id))+geom_point(shape=16, size=2)
ggplot(data = track_table, aes(x=t, y=ta_deg, color=track_id))+geom_point(shape=16, size=2)

# speed
df_speed <- data.frame(t=track_table$t, speed=track_table$delta_z/track_table$t)
ggplot(data = df_speed, aes(x=t, y=speed))+geom_point(shape=16)


