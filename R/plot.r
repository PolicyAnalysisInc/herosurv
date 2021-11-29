#' @export
plot.surv_dist <- function(x, max_time, steps = 1000, ...) {
    times <- seq(from = 0, to = max_time * 1.2, length.out = steps)
    df <- tibble(
        time = times,
        survival = surv_prob(x, times, ...)
    )

    df[!is.na(df$survival), ]
    ggplot(aes(x = time, y = survival), data = df) +
        geom_line() +
        coord_cartesian(xlim = c(0, max_time), ylim = c(0, 1)) +
        labs(x = 'Time', y = 'Survival')
}