library(geepack)
library(lme4)

cysto_outlier <- cysto_cleaned[11,]
cysto_rm_outlier <- cysto_cleaned[-11,]

sorted_cysto <- cysto_cleaned %>%
  arrange(StudySite)

abstract_formula <- formula(VASWorstPainScore ~ PatientAge +
  CystoCountPrior + TrtViz + TrtMusic + LidoDwellMinutes)

abstract_formula <- formula(VASWorstPainScore ~ PatientAge +
  CystoCountPrior + TrtViz)

fixed_site_formula <- formula(VASWorstPainScore ~ (PatientAge +
  CystoCountPrior + TrtViz + LidoDwellMinutes)*StudySite)


abstract_vas_lm <- lm(formula = abstract_formula, data = cysto_cleaned)
outlier_vas_lm <- lm(formula = abstract_formula, data = cysto_rm_outlier)

fixed_vas_lm <- lm(formula = fixed_site_formula, data = cysto_cleaned)

gee_lm <- geese(formula = abstract_formula,
                id = StudySite,
                data = sorted_cysto,
                corstr = "exchangeable")

boot_lm <- boot::boot(data = cysto_cleaned,
                      statistic = function(data, indices) {
                        fit <- lm(formula = abstract_formula, data = data[indices,])
                        return(coef(fit))
                      },
                      R = 1000)

boot_coef_mat <- boot_lm$t
colnames(boot_coef_mat) <- c("Intercept", "PatientAge", "CystoCountPrior", "TrtViz", "TrtMusic", "LidoDwellMinutes")