library(tidyverse)
library(glmnet)
library(glmnetUtils)
library(MASS)


# Lidocaine as received
cysto %>%
  summarise(`Proportion lidocaine timing as assigned` = 100*mean(LidoAssignedReceivedAgree))

cysto %>%
  group_by(AssignedLidoLeq10) %>%
  summarise(`Lidocaine timing (min)` = mean(LidoDwellMinutes, na.rm = TRUE))

participation_model <- polr(InterestInPainReductionStudy ~
                              TrtMusic + TrtViz + TrtHydro + AssignedLidoLeq10 + PatientAge + I(log(1 + CystoCountPrior)) + PatientGender, data = cysto)

participation_lm <- lm(I(as.numeric(InterestInPainReductionStudy)) ~
  TrtMusic + TrtViz + TrtHydro + AssignedLidoLeq10 + PatientAge + I(log(1 + CystoCountPrior)) + PatientGender + FamilyPresent + PatientEthnicity + PatientRace + VASWorstPainScore, data = cysto)