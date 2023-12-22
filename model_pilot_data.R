library(tidyverse)
library(glmnet)
library(glmnetUtils)


# Bad John, bad!
# Wait until you do EDA and analyze interest in study, then do this
#
exlm <- lm(VASWorstPainScore ~ TrtMusic*TrtViz*TrtHydro +LidoNormed + StudySite + PatientAge + I(log(1 + CystoCountPrior)) + PatientGender +
  TrtMusic:PatientGender + TrtViz:PatientGender + TrtHydro:PatientGender, data = cysto)

twoway_int_lm <- lm(VASWorstPainScore ~ TrtMusic*TrtViz*TrtHydro - TrtMusic:TrtViz:TrtHydro +LidoNormed + StudySite + PatientAge + I(log(1 + CystoCountPrior)) + PatientGender +
  TrtMusic:PatientGender + TrtViz:PatientGender + TrtHydro:PatientGender, data = cysto)

noint_lm <- lm(VASWorstPainScore ~ TrtMusic + TrtViz + TrtHydro +LidoNormed, data = cysto)

twoway_int_lm_elastic_cv <- cv.glmnet(VASWorstPainScore ~
                             TrtMusic*TrtViz*TrtHydro -
                               TrtMusic:TrtViz:TrtHydro +
                               LidoNormed + StudySite + PatientAge + I(log(1 + CystoCountPrior)) + PatientGender +
                               TrtMusic:PatientGender + TrtViz:PatientGender + TrtHydro:PatientGender, data = cysto, alpha = 0.5)

twoway_int_lm_elastic <- glmnet(VASWorstPainScore ~
                                     TrtMusic*TrtViz*TrtHydro -
                                       TrtMusic:TrtViz:TrtHydro +
                                       LidoNormed, data = cysto, alpha = 0.5)

replication_lm <- lm(
  VASWorstPainScore ~ TrtMusic + TrtViz + LidoNormed +  CystoCountPrior + PatientAge + CystoReasonGeneral, data = cysto)


replication_lm_hydro <- lm(
  VASWorstPainScore ~ TrtMusic + TrtViz + TrtHydro + LidoNormed +  CystoCountPrior + PatientAge + CystoReasonGeneral, data = cysto)


rep_mod_lm <- lm(
  VASWorstPainScore ~ (TrtMusic + TrtViz + LidoNormed)*PatientGender +  I(log(1+CystoCountPrior)) + I(log(PatientAge)) + CystoReasonGeneral, data = cysto)