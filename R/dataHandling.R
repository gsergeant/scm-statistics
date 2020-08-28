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
