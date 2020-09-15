

plot_coordinates <- function(dataset, subset_column, subset_factor) {
  b <- ggplot2::ggplot(data = dataset[which(dataset[[subset_column]] == subset_factor), ], ggplot2::aes(x = X, y = Y))
  b + ggplot2::geom_point(shape = 20,
                 ggplot2::aes(color = track_id),
                 show.legend = FALSE)
}

plot_turningangles <- function(dataset, subset_column, subset_factor){
  ggplot2::ggplot(dataset[which(dataset[[subset_column]] == subset_factor), ], ggplot2::aes(x = ta_deg)) +
    ggplot2::geom_histogram(binwidth = 8) +
    ggplot2::scale_x_continuous(breaks = seq(0, 360, 60)) +
    ggplot2::coord_polar()
}


#test to plot step data features over time
plot_speedProfile <- function(dataset, subset_column, subset_factor) {
  b <- ggplot2::ggplot(data = dataset[which(dataset[[subset_column]] == subset_factor), ], ggplot2::aes(x = frame_id, y = speed))
  b + ggplot2::geom_point(size = 3, shape = 20,
                          ggplot2::aes(color = track_id),
                          show.legend = FALSE)+
    ggplot2::geom_line(col = 'gray')
}
