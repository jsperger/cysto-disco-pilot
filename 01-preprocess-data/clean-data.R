# Purpose: Clean and prepare data for analysis

CleanInputData <- function(cystodf){
    # Rename variables
    cysto_renamed <- .RenameVariables(cystodf)

    cysto_retyped <- .CleanTypes(cysto_renamed)

    # Add derived variables
    cysto_derived_covars <- .AddDerivedVariables(cysto_retyped)

    # Derive outcomes
    cysto_derived_outcomes <- .DeriveOutcomes(cysto_derived_covars)

    return(cysto_derived_outcomes)
}

# Renaming variables to more sensible names
.RenameVariables <- function(cystodf){
  data_renamed <- cystodf %>%
    dplyr::rename(
      Site = `Site`,
      AssignedIntervention = `Intervention assigned`,
      CompletedIntervention = `Intervention actually completed`,
      Age = `Age`,
      TimeLidocaineInstilled = `Time (lidocaine instilled) - use military time`,
      TimeCystoscopyStarted = `Time (cystoscopy started) -use military time`,
      ElapsedTimeCystoscopy = `Time elapsed (h:mm)`,
      TrtBlueLight = `Blue light used?`,
      TrtMusic = `Music?`,
      NoMusicReason = `If no music, why?`,
      MusicType = `If music, type?`,
      TrtViz = `Visualization?`,
      NoVisualReason = `If no visualization, why?`,
      FamilyPresent = `Family members/friends present in the room?`,
      TrtHydro = `Hydrostatic pressure applied?`,
      CystoResult = `Cystoscopy result`,
      CystoReasonSpecific = `Reason for cystoscopy - specific`,
      CystoReasonGeneral = `Reason for cystoscopy - general`,
      CystoCountPrior = `Number of prior cystoscopies`,
      MonthsSinceCysto = `Months since last cystoscopy`,
      IntravesicalTherapyRecent = `Intravesical therapy in last 3 months?`,
      MedicalStaffPresent = `Nurse/MA/Staff`,
      CystoChallenges = `Challenges encountered before and during cystoscopy (include specifics regarding intervention)`,
      Gender = `Gender`,
      Race = `Race`,
      Ethnicity = `Ethnicity`,
      AnxietyFearfulScore = `PROMIX Anxiety: I felt fearful (1-5)`,
      AnxietyFocusScore = `PROMIS Anxiety: I found it hard to focus on anything other than my anxiety (1-5)`,
      AnxietyOverwhelmScore = `PROMIS Anxiety: My worries overwhelmed me (1-5)`,
      AnxietyUneaseScore = `PROMIS Anxiety: I felt uneasy (1-5)`,
      PainWorstScore = `PROMIS Pain: Pain at its worst (1-5)`,
      PainAverageScore = `PROMIS Pain: Pain on average (1-5)`,
      PainCurrentScore = `PROMIS Pain: Current pain (1-5)`,
      VASWorstPainScore = `VAS Score: Pain at its worst (0-10)`,
      FuturePainMgmtPreference = `If you were to undergo cystoscopy again, would you use the same methods to manage pain or discomfort? (1-4)`,
      SatisfactionCystoPainMgmt = `In an overall, general sense, how satisfied are you with the way in which you received cystoscopy today with regard to pain or discomfort? (1-4)`,
      NeedsMetPainMgmt = `To what extent did the way in which cystoscopy was performed meet your needs for managing pain or discomfort? (1-4)`,
      InterestInPainReductionStudy = `If there was a study that tests ways to reduce pain or discomfort associated with cystoscopy, would you consider hearing more and participating if it did not require additional visits or procedure?`
    )

  return(data_renamed)
}

#' Clean recalcitrant types
#' Yes/No instead of TRUE FALSE, etc.
.CleanTypes <- function(cystodf){
  .RelabelYesNo <- function(in_vec){
    relabeled_vec <- dplyr::case_when(in_vec == "Yes" ~ TRUE,
                                      in_vec == "No" ~ FALSE,
                                      TRUE ~ NA)
    return(relabeled_vec)
  }
  cysto_retyped <- cystodf %>%
    dplyr::mutate(TrtViz = .RelabelYesNo(TrtViz),
                  TrtMusic = .RelabelYesNo(TrtMusic),
                  TrtBlueLight = .RelabelYesNo(TrtBlueLight),
                  TrtHydro = .RelabelYesNo(TrtHydro),
                  FamilyPresent = .RelabelYesNo(FamilyPresent),
                  IntravesicalTherapyRecent = .RelabelYesNo(IntravesicalTherapyRecent))


  return(cysto_retyped)
}

#' Transform base variables
#'
#' Operations: Make categorical variables factors, etc.
.TransformBaseVariables <- function(cystodf){

}

#' Derive new variables
#'
#'
.AddDerivedVariables <- function(cystodf) {
  cysto_derived <- .DeriveCovariates(cystodf) %>%
    .DeriveOutcomes(cystodf = .) %>%
    .TransformDerivedVariables(cystodf = .)

  return(cysto_derived)
}

.DeriveCovariates <- function(cystodf){
  cysto_add_covars <- cystodf %>%
    dplyr::mutate(
      LidoDwellMinutes = lubridate::minute(ElapsedTimeCystoscopy),
      AssignedLidoLeq10 = AssignedIntervention %in%
        c("Lidocaine ≤ 10 min + Vis.", "Lidocaine ≤ 10 min + Music"),
      ReceivedLidoLeq10 = LidoDwellMinutes <= 10,
      LidoAssignedReceivedAgree = AssignedLidoLeq10 == ReceivedLidoLeq10,
      LidoGroup = factor(AssignedLidoLeq10,
                         levels = c("TRUE", "FALSE"),
                         labels = c("Leq10Mins", "G10Mins"))) %>%
    dplyr::mutate(OfficeCondition = case_when(TrtMusic == FALSE & TrtViz == FALSE ~ "No Office Intervention",
                                              TrtMusic == TRUE & TrtViz == FALSE ~ "Music Only",
                                                TrtMusic == FALSE & TrtViz == TRUE ~ "Visualization Only",
                                                TrtMusic == TRUE & TrtViz == TRUE ~ "Music + Visualization"))

  return(cysto_add_covars)
}

.TransformDerivedVariables <- function (cystodf) {
  cysto_transformed_vars <- cystodf %>%
    dplyr::mutate(NormedLidoMinutes = (LidoDwellMinutes - mean(LidoDwellMinutes))/(2*sd(LidoDwellMinutes)),
                  NormedPainSumScore = (PainSumScore - mean(PainSumScore))/(2*sd(PainSumScore)),
                  NormedAnxietySumScore = (AnxietySumScore - mean(AnxietySumScore))/(2*sd(AnxietySumScore)),
    RootLidoMinutes = sqrt(LidoDwellMinutes),
    RootPainSumScore = sqrt(PainSumScore),
    RootAnxietySumScore = sqrt(AnxietySumScore),
    LogLidoMinutes = log(LidoDwellMinutes),
                  LogPainSumScore = log(PainSumScore),
                    LogAnxietySumScore = log(AnxietySumScore),
                  LogPlusOneVAS = log(VASWorstPainScore + 1)
    )

  return(cysto_transformed_vars)
}

#' Derived outcomes
.DeriveOutcomes <- function(cystodf){
  cysto_add_outcomes <- cystodf %>%
    dplyr::mutate(AnxietySumScore = AnxietyFearfulScore + AnxietyFocusScore +
      AnxietyOverwhelmScore + AnxietyUneaseScore,
      PainSumScore = PainWorstScore + PainAverageScore + PainCurrentScore
      )

  return(cysto_add_outcomes)
}