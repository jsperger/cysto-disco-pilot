library(latex2exp)

#' Create a barplot of dwell time by assigned group overlaid with jittered data
#'
#' @param cysto A data frame containing the cystoscopy data
#'
#' @return A ggplot object
CreateVASByFactorBarplot <- function(cysto, factor_var, factor_display_name, title){
  # Convert factor_var to a symbol and unquote it within mutate
  cysto_factored <- cysto %>%
    mutate(PlotVar = factor(!!rlang::sym(factor_var)))

  vas_by_factor <- ggplot(data = cysto_factored, aes(x = PlotVar, y = VASWorstPainScore)) +
   # geom_violin(draw_quantiles = TRUE) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.25, height = 0, alpha = .5) +
    labs(x = factor_display_name, y = "VAS Worst Pain Score",
         title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(vas_by_factor)
}

#' Create a list of plots involving dwell time plots
#'
#' @param cysto A data frame containing the cystoscopy data
#' @return A list of plots created with ggplot
CreateDwellTimePlots <- function(cysto) {
  lido_minutes_by_condition <- ggplot2::ggplot(data = cysto,
                                               aes(x = LidoGroup, y = LidoDwellMinutes)) +
  #  ggplot2::geom_violin(draw_quantiles = TRUE) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.25, height = 0, alpha = .5) +
    ggplot2::scale_x_discrete(
      labels = c(TeX("$\\leq 10$  mins"), TeX("$>10$ mins"))) +
    ggplot2::labs(x = "Assigned Dwell Time", y = "Lidocaine Dwell Time (minutes)",
                  title = "Lidocaine Actual Dwell Time by Assigned Group") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

  lido_minutes_by_condition_and_gender <- lido_minutes_by_condition +
    ggplot2::facet_wrap(~ Gender)

  lido_minutes_vs_vas_combined <- ggplot2::ggplot(data = cysto, aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score", title = "Lidocaine Dwell Time vs. VAS Worst Pain Score") +
    theme_bw()

  # Create a separate data frame for each group
  under_20 <- cysto %>% filter(LidoDwellMinutes <= 20) %>% mutate(Subset = "Under 20 Mins")
  under_25 <- cysto %>% filter(LidoDwellMinutes <= 25) %>% mutate(Subset = "Under 25 Mins")
  under_30 <- cysto %>% filter(LidoDwellMinutes <= 30) %>% mutate(Subset = "Under 30 Mins")
  whole_sample <- cysto %>% mutate(Subset = "Whole Sample")

  # Combine the data frames into one, stacking them on top of each other
  combined_data <- bind_rows(whole_sample, under_30, under_25, under_20) %>%
    # Remove duplicates so that each row is only represented once per subset
    distinct(LidoDwellMinutes, VASWorstPainScore, Gender, Subset, .keep_all = TRUE) %>%
    # Order the factor for the plot
    mutate(Subset = factor(Subset, levels = c("Whole Sample", "Under 30 Mins", "Under 25 Mins", "Under 20 Mins")))

  lido_minutes_vs_vas <- ggplot(data = combined_data, aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score", title = "VAS Score by (Analytic) Dwell Time Cutoff") +
    theme_bw() +
    facet_wrap(~ Subset, ncol = 1)

  # Now create the plot using this combined data
  lido_minutes_vs_vas_gender <- ggplot(data = combined_data, aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score",
         title = "VAS Score by Gender and (Analytic) Dwell Time Cutoff") +
    theme_bw() +
    facet_grid(Subset ~ Gender, scales = "free_x", space = "free_x") +
    theme(strip.text.x = element_text(angle = 0))

  # Now create the plot using the under 25 only data
  lido_minutes_vs_vas_gender_rmoutliers <- ggplot(data = under_25, aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score",
         title = "VAS Score by Gender with a 25-Minute Cutoff") +
    theme_bw() +
    facet_grid( ~ Gender, scales = "free_x", space = "free_x") +
    theme(strip.text.x = element_text(angle = 0))

  lido_minutes_vs_vas_site <- ggplot(data = combined_data, aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score",
title = "VAS Score by Site and (Analytic) Dwell Time Cutoff") +
    theme_bw() +
    facet_grid(Subset ~ Site, scales = "free_x", space = "free_x") +
    theme(strip.text.x = element_text(angle = 0))

  lido_minutes_vs_vas_site_rmoutliers <- ggplot(data = under_25, aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score",
"VAS Score by Site with 25-Minute Cutoff") +
    theme_bw() +
    facet_grid( ~ Site, scales = "free_x", space = "free_x") +
    theme(strip.text.x = element_text(angle = 0))

  lido_minutes_vs_vas_site_reason_rmoutliers <- ggplot(data = under_25,
                                                       aes(x = LidoDwellMinutes, y = VASWorstPainScore)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(x = "Lidocaine Dwell Time (minutes)", y = "VAS Worst Pain Score",
title = "VAS Score by Cysto Reason and Site with 25-Minute Cutoff") +
    theme_bw() +
    facet_grid(CystoReasonGeneral ~ Site, scales = "free_x", space = "free_x") +
    theme(strip.text.x = element_text(angle = 0))




  dwell_plot_list <- list("Condition" = lido_minutes_by_condition,
                          "ConditionAndGender" = lido_minutes_by_condition_and_gender,
                          "VAS" = lido_minutes_vs_vas,
                          "VASCombined" = lido_minutes_vs_vas_combined,
                          "VASAndGender" = lido_minutes_vs_vas_gender,
                          "VASAndGenderTrimmed" = lido_minutes_vs_vas_gender_rmoutliers,
                          "VASAndSite" = lido_minutes_vs_vas_site,
                          "VASAndSiteTrimmed" = lido_minutes_vs_vas_site_rmoutliers,
                          "VASAndSiteReasonTrimmed" = lido_minutes_vs_vas_site_reason_rmoutliers
  )

  return(dwell_plot_list)
}

