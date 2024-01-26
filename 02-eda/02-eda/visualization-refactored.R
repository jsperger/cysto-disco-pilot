library(latex2exp)

# Colorblind Friendly Palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#' Function to create a generic scatter plot overlaid with linear and loess fits
#'
#' @param plot.data A data frame containing the data to plot
#' @param x The name of the column to use for the x-axis
#' @param y The name of the column to use for the y-axis
#'
#' @return A ggplot object
CreateScatterPlot <- function(plot.data, x, y, title, colors = cbp1) {
  scatter_plot <-  ggplot(data = plot.data, aes_string(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = colors[[2]]) +
    geom_smooth(method = "loess", se = FALSE, color = colors[[3]]) +
    labs(x = x, y = y, title = title) +
    theme_bw()

  return(scatter_plot)
}

CreateVASScatterPlot <- function(plot.data, x, title, facet.formula = NULL, colors = cbp1) {
  vas_plot <- CreateScatterPlot(plot.data = plot.data,
                                  x = x,
                                  y ="VASWorstPainScore",
                                  title = title,
                                  colors = colors)

  if(!is.null(facet.formula)) {
    vas_plot <- vas_plot + facet_wrap(facet.formula)
  }

  return(vas_plot)
}

#' Function to filter data based on dwell time and add a subset label
#'
#' @param data A data frame containing the data to filter
#' @param max_time (Numeric) The maximum dwell time to include in the subset
#' @param label (Character) The label to assign to the subset
#'
#' @return A data frame containing the filtered data
filter_data_by_dwell_time <- function(data, max_time, label) {
  filtered_data <- data %>%
    filter(LidoDwellMinutes <= max_time) %>%
    mutate(Subset = label)

  return(filtered_data)
}

#' Create a barplot of dwell time by assigned group overlaid with jittered data
#'
#' @param cysto A data frame containing the cystoscopy data
#'
#' @return A ggplot object
CreateDwellTimeByAssignmentBarplot <- function(cysto){
  lido_minutes_by_condition <- ggplot(data = cysto, aes(x = LidoGroup, y = LidoDwellMinutes)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.25, height = 0, alpha = .5) +
    scale_x_discrete(labels = c(latex2exp::TeX("$\\leq 10$ mins"), latex2exp::TeX("$>10$ mins"))) +
    labs(x = "Assigned Dwell Time", y = "Lidocaine Dwell Time (minutes)",
         title = "Lidocaine Actual Dwell Time by Assigned Group") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(lido_minutes_by_condition)
}



# Create a list of plots for dwell time analysis
CreateDwellTimePlots <- function(cysto) {

  # Adding gender facet
  lido_minutes_by_condition_and_gender <- lido_minutes_by_condition +
    facet_wrap(~ Gender)

  # VAS score combined plot
  lido_minutes_vs_vas_combined <- create_scatter_plot(cysto, "LidoDwellMinutes",
                                                      "VASWorstPainScore",
                                                      "Lidocaine Dwell Time vs. VAS Worst Pain Score")

  # Filtered data subsets
  dwell_time_thresholds <- c(20, 25, 30)
  subset_labels <- c("Under 20 Minutes", "Under 25 Minutes", "Under 30 Minutes")
  filtered_data <- map2(dwell_time_thresholds, subset_labels,
                        ~filter_data_by_dwell_time(cysto, .x, .y))
  combined_data <- bind_rows(mutate(cysto, Subset = "Whole Sample"), filtered_data) %>%
    distinct(LidoDwellMinutes, VASWorstPainScore, Gender, Subset, .keep_all = TRUE) %>%
    mutate(Subset = factor(Subset, levels = c("Whole Sample", subset_labels)))

  # VAS score plots
  lido_minutes_vs_vas <- create_scatter_plot(combined_data, "LidoDwellMinutes",
                                             "VASWorstPainScore",
                                             "Lidocaine Dwell Time vs. VAS Worst Pain Score") +
    facet_wrap(~ Subset, ncol = 1)

  lido_minutes_vs_vas_gender <- lido_minutes_vs_vas +
    facet_grid(Subset ~ Gender, scales = "free_x", space = "free_x") +
    theme(strip.text.x = element_text(angle = 0))

  lido_minutes_vs_vas_gender_rmoutliers <- create_scatter_plot
