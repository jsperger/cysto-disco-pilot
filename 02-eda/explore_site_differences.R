
SummarizeVarsBySite <- function (cysto, summary_var_names){
  .SummarizeSingleVarBySite <- function (cysto_grouped, varname){
    var_site_summary <- cysto_grouped %>%
      dplyr::summarize(
        n = n(),
        min = min(!! rlang::sym(varname), na.rm = TRUE),
        q1 = quantile(!! rlang::sym(varname), 0.25, na.rm = TRUE),
        mean = mean(!! rlang::sym(varname), na.rm = TRUE),
        median = median(!! rlang::sym(varname), na.rm = TRUE),
        q3 = quantile(!! rlang::sym(varname), 0.75, na.rm = TRUE),
        max = max(!! rlang::sym(varname), na.rm = TRUE),
        sd = sd(!! rlang::sym(varname), na.rm = TRUE),
        se = sd / sqrt(n)
      ) %>%
      dplyr::mutate(
        Variable = varname
      ) %>%
      select(Variable, everything())

    return(var_site_summary)
  }
    grouped_cysto <- cysto %>%
        dplyr::group_by(Site)

    var_site_summaries <- purrr::map_dfr(.x = summary_var_names,
                                         .f = .SummarizeSingleVarBySite,
                                         cysto_grouped = grouped_cysto)

  return(var_site_summaries)

}