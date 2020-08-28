

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
