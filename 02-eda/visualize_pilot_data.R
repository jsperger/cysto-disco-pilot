library(latex2exp)
#' Create a list of plots involving dwell time plots
#'
#' @param cysto A data frame containing the cystoscopy data
#' @return A list of plots created with ggplot
CreateDwellTimePlots <- function(cysto) {
  lido_minutes_by_condition <- ggplot2::ggplot(data = cysto,
                                               aes(x = LidoGroup, y = LidoDwellMinutes)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.25, height = 0, alpha = .5) +
    ggplot2::scale_x_discrete(labels = c(TeX("$\\leq 10$  mins"), TeX("$> 10$ mins"))) +
    ggplot2::labs(x = "Assigned Dwell Time", y = "Lidocaine Dwell Time (minutes)", title = "Lidocaine Actual Dwell Time by Assigned Group") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggplot2::facet_wrap(~ Gender)

  lido_minutes_vs_vas <- ggplot2::ggplot(data = cysto, aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score", title = "Lidocaine Dwell Time vs. VAS Worst Pain Score") +
    theme_bw()

  dwell_plot_list <- list(lido_minutes_by_condition, lido_minutes_vs_vas)

  return(dwell_plot_list)
}

