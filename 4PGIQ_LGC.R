# latent growth curve model 
# 24 Sept 2025
# Yujing Lin

library(lavaan)
library(stringr)
library(tibble)
library(semPlot)
library(tidyr) # export results 
library(dplyr) # export results 
library(openxlsx) # for Excel output

source("./PGIQ Codes/0PGIQ_VarList.R")
# source("/scratch/users/k21170717/polygenic_IQ_score/0PGIQ_VarList.R")

sourceFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Data220925/'
# sourceFileStem <- '/scratch/users/k21170717/polygenic_IQ_score/Data220925/'
dat_scaled <- read.csv(paste0(sourceFileStem, "PGIQ_scaled.csv"))

# =============================================================================
# Latent Growth Curve Model function ####
# =============================================================================
fit_lgcm <- function(data, var_list, trait_category) {
  
  # --- Extract relevant variables and ages from VarList ---
  # from VarList get the variables and the corresponding ages for a specific category 
  # var_list <- G_Composites_Varlist
  # trait_category <- "g"
  trait_specific_list <- Filter(function(x) x[5] == trait_category, var_list)
  
  # Stop with an error if no variables are found for that trait
  if (length(trait_specific_list) == 0) {
    stop(paste("No variables found for trait:", trait_category))
  }
  
  # Extract the base variable name (1st element)
  phenotype_vars_base <- sapply(trait_specific_list, `[`, 1)
  # Extract the age string (2nd element)
  age_strings <- sapply(trait_specific_list, `[`, 2)
  
  # parse the age strings into numbers
  # Use stringr::str_extract to pull out the numbers from strings like "25 yr"
  ages <- as.numeric(stringr::str_extract(age_strings, "\\d+"))
  # Handle special cases like "birth" by setting their age to 0
  ages[age_strings == "birth"] <- 0
  
  trait_info <- tibble::tibble(
    variable_name = paste0(phenotype_vars_base, "1"),
    age = ages
  )
  # print(trait_info)
  
  phenotype_vars <- trait_info$variable_name
  
  # --- Create the slope loadings based on real ages ---
  # This centers the time scores, so the intercept represents the first time point.
  slope_loadings <- trait_info$age - min(trait_info$age)
  
  # --- Build the model syntax ---
  # we first create different model syntax, then we will combine them to be fitted to the data  
  # Define latent growth factors
  growth_syntax <- paste0(
    "# Latent growth factors\n",
    "intercept =~ 1*", paste(phenotype_vars, collapse = " + 1*"), "\n",
    "slope =~ ", paste(slope_loadings, "*", phenotype_vars, collapse = " + "), "\n\n"
  )
  
  # autoregressive paths (the actual thing is more complicated but this is the idea)
  # ar_paths <- ""
  # for(i in 2:n_timepoints) {
  #   ar_paths <- paste0(ar_paths, phenotype_vars[i], " ~ ", phenotype_vars[i-1], "\n")
  # }
  
  # Add PGS and covariate effects
  pgs_effects <- paste0(
    "# PGS effects on growth factors\n",
    "intercept ~ PGIQ + sex1 \n",
    "slope ~ PGIQ + sex1 \n\n"
  ) # I could also add the chiptype and 10 PCs here, but they are not significant 
  
  # Add residual variances and covariances
  variance_syntax <- paste0(
    "# Residual variances & factor covariances\n",
    paste(phenotype_vars, "~~", phenotype_vars, collapse = "\n"), "\n",
    "intercept ~~ intercept\n", # estimate intercept variance 
    "slope ~~ slope\n", # estimate slope variance
    # "slope ~~ 0*slope\n", # for the maths model specifically, we fix the slope variance to be 0 to help convergence ####
    "intercept ~~ slope\n" # estimate covariance between intercept and slope
  )
  
  full_syntax <- paste(growth_syntax, pgs_effects, variance_syntax)
  
  # Fit model accounting for clustering by family ID
  fit <- growth(full_syntax, data = data, missing = "ML", cluster = "id_fam")
  
  return(list(model = fit, syntax = full_syntax))
}

# =============================================================================
# Fit the model to data ####
# =============================================================================
g_lgcm <- fit_lgcm(dat_scaled, G_Composites_Varlist, "g")
# print(summary(g_lgcm$model, fit.measures = TRUE))

vb_lgcm <- fit_lgcm(dat_scaled, G_Composites_Varlist, "verbal ability")
# print(summary(vb_lgcm$model, fit.measures = TRUE))

nv_lgcm <- fit_lgcm(dat_scaled, G_Composites_Varlist, "nonverbal ability")
# print(summary(nv_lgcm$model, fit.measures = TRUE))

eng_lgcm <- fit_lgcm(dat_scaled, Edu_Achieve_Attain_Varlist, "English")
# print(summary(eng_lgcm$model, fit.measures = TRUE))

maths_lgcm <- fit_lgcm(dat_scaled, Edu_Achieve_Attain_Varlist, "Maths Achievement")
print(summary(maths_lgcm$model, fit.measures = TRUE))

science_lgcm <- fit_lgcm(dat_scaled, Edu_Achieve_Attain_Varlist, "Science")
# print(summary(science_lgcm$model, fit.measures = TRUE))

edu_lgcm <- fit_lgcm(dat_scaled, Edu_Achieve_Attain_Varlist, "Core-Subject")
# print(summary(edu_lgcm$model, fit.measures = TRUE))

ht_lgcm <- fit_lgcm(dat_scaled, Anthro_Varlist, "Height")
# print(summary(ht_lgcm$model, fit.measures = TRUE))

BMI_lgcm <- fit_lgcm(dat_scaled, Anthro_Varlist, "BMI")
# print(summary(BMI_lgcm$model, fit.measures = TRUE))

shyness_lgcm <- fit_lgcm(dat_scaled, Anxiety_Varlist_oneRater, "Shyness")
# print(summary(shyness_lgcm$model, fit.measures = TRUE))

fear_lgcm <- fit_lgcm(dat_scaled, Anxiety_Varlist_oneRater, "Fear")
# print(summary(fear_lgcm$model, fit.measures = TRUE))

ocb_lgcm <- fit_lgcm(dat_scaled, Anxiety_Varlist_oneRater, "OCB")
# print(summary(ocb_lgcm$model, fit.measures = TRUE))

negaffect_lgcm <- fit_lgcm(dat_scaled, Anxiety_Varlist_oneRater, "Negative Affect")
# print(summary(negaffect_lgcm$model, fit.measures = TRUE))

negcog_lgcm <- fit_lgcm(dat_scaled, Anxiety_Varlist_oneRater, "Negative Cognition")
# print(summary(negcog_lgcm$model, fit.measures = TRUE))

anxiety_lgcm <- fit_lgcm(dat_scaled, Anxiety_Varlist_oneRater, "Anxiety")
# print(summary(anxiety_lgcm$model, fit.measures = TRUE))

inatt_lgcm <- fit_lgcm(dat_scaled, Conners_Varlist_oneRater, "Inattention")
# print(summary(inatt_lgcm$model, fit.measures = TRUE))

hyp_lgcm <- fit_lgcm(dat_scaled, Conners_Varlist_oneRater, "Hyper-Impuls")
# print(summary(hyp_lgcm$model, fit.measures = TRUE))

adhd_lgcm <- fit_lgcm(dat_scaled, Conners_Varlist_oneRater, "ADHD")
# print(summary(adhd_lgcm$model, fit.measures = TRUE))

conduct_lgcm <- fit_lgcm(dat_scaled, SDQ_Varlist_oneRater, "Conduct")
# print(summary(conduct_lgcm$model, fit.measures = TRUE))

emotion_lgcm <- fit_lgcm(dat_scaled, SDQ_Varlist_oneRater, "Emotion")
# print(summary(emotion_lgcm$model, fit.measures = TRUE))

hyperSDQ_lgcm <- fit_lgcm(dat_scaled, SDQ_Varlist_oneRater, "Hyperactivity")
# print(summary(hyperSDQ_lgcm$model, fit.measures = TRUE))

peer_lgcm <- fit_lgcm(dat_scaled, SDQ_Varlist_oneRater, "Peer Problems")
# print(summary(peer_lgcm$model, fit.measures = TRUE))

prosocial_lgcm <- fit_lgcm(dat_scaled, SDQ_Varlist_oneRater, "Prosocial")
# print(summary(prosocial_lgcm$model, fit.measures = TRUE))

sdq_lgcm <- fit_lgcm(dat_scaled, SDQ_Varlist_oneRater, "Total Problems")
# print(summary(sdq_lgcm$model, fit.measures = TRUE))

#semPaths(g_lgcm$model, what = "std", style = "lisrel", intercepts = FALSE, fade = FALSE, edge.label.cex = 0.8, nCharNodes = 0)

fit_list <- list(
  # Cognitive abilities
  "g" = g_lgcm$model,
  "Verbal" = vb_lgcm$model,
  "Nonverbal" = nv_lgcm$model,
  
  # Educational achievement
  "English" = eng_lgcm$model,
  "Maths" = maths_lgcm$model,
  "Science" = science_lgcm$model,
  "Core_Subject" = edu_lgcm$model,
  
  # Physical traits
  "Height" = ht_lgcm$model,
  "BMI" = BMI_lgcm$model,
  
  # Anxiety-related traits
  "Shyness" = shyness_lgcm$model,
  "Fear" = fear_lgcm$model,
  "OCB" = ocb_lgcm$model,
  "Negative_Affect" = negaffect_lgcm$model,
  "Negative_Cognition" = negcog_lgcm$model,
  "Anxiety" = anxiety_lgcm$model,
  
  # ADHD traits
  "Inattention" = inatt_lgcm$model,
  "Hyperactivity_Impulsivity" = hyp_lgcm$model,
  "ADHD" = adhd_lgcm$model,
  
  # SDQ traits
  "Conduct" = conduct_lgcm$model,
  "Emotion" = emotion_lgcm$model,
  "Hyperactivity_SDQ" = hyperSDQ_lgcm$model,
  "Peer_Problems" = peer_lgcm$model,
  "Prosocial" = prosocial_lgcm$model,
  "Total_Problems" = sdq_lgcm$model
)

# fit_list <- list(
#   "Maths" = maths_lgcm$model)
# =============================================================================
# Export whole sample results to single Excel file ####
# =============================================================================

create_whole_sample_output <- function(fit_list) {
  
  # Fit measures of interest
  fit_measures_of_interest <- c("chisq.scaled", "df", "pvalue.scaled", 
                                "cfi.scaled", "tli.scaled", "rmsea.scaled",
                                "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
                                "srmr")
  
  results_list <- lapply(names(fit_list), function(model_name) {
    fit <- fit_list[[model_name]]
    
    # Extract fit indices
    indices <- fitMeasures(fit, fit_measures_of_interest)
    
    # Extract standardized parameters
    std_params <- standardizedSolution(fit, se = TRUE, type = "std.all")
    
    # Get PGIQ effects on intercept and slope
    pgiq_effects <- std_params %>%
      filter(op == "~" & lhs %in% c("intercept", "slope") & rhs == "PGIQ") %>%
      mutate(
        Z_value = est.std / se,
        P_value = 2 * pnorm(-abs(Z_value))
      ) %>%
      select(lhs, Std_Beta = est.std, SE = se, Z = Z_value, P = P_value)
    
    # Get intercept-slope covariance (standardized)
    int_slope_cov <- std_params %>%
      filter(op == "~~" & lhs == "intercept" & rhs == "slope") %>%
      select(Std_Cov = est.std, Cov_SE = se)
    
    # Combine into one row
    result_row <- data.frame(
      Variable = model_name,
      Chi_Sq = indices["chisq.scaled"],
      df = indices["df"],
      P_Value = indices["pvalue.scaled"],
      CFI = indices["cfi.scaled"],
      TLI = indices["tli.scaled"],
      RMSEA = indices["rmsea.scaled"],
      RMSEA_Lower = indices["rmsea.ci.lower.scaled"],
      RMSEA_Upper = indices["rmsea.ci.upper.scaled"],
      SRMR = indices["srmr"]
    )
    
    # Add PGIQ effects
    intercept_row <- pgiq_effects %>% filter(lhs == "intercept")
    slope_row <- pgiq_effects %>% filter(lhs == "slope")
    
    if (nrow(intercept_row) > 0) {
      result_row$Intercept_Beta <- intercept_row$Std_Beta
      result_row$Intercept_SE <- intercept_row$SE
      result_row$Intercept_Z <- intercept_row$Z
      result_row$Intercept_P <- intercept_row$P
    }
    
    if (nrow(slope_row) > 0) {
      result_row$Slope_Beta <- slope_row$Std_Beta
      result_row$Slope_SE <- slope_row$SE
      result_row$Slope_Z <- slope_row$Z
      result_row$Slope_P <- slope_row$P
    }
    
    # Add intercept-slope covariance
    if (nrow(int_slope_cov) > 0) {
      result_row$IntSlope_Cov <- int_slope_cov$Std_Cov
      result_row$IntSlope_Cov_SE <- int_slope_cov$Cov_SE
    }
    
    return(result_row)
  })
  
  # Combine all results
  final_table <- bind_rows(results_list)
  
  # Create RMSEA CI column
  final_table <- final_table %>%
    mutate(
      RMSEA_CI = paste0(RMSEA, " [", 
                        RMSEA_Lower, ", ", 
                        RMSEA_Upper, "]")
    ) %>%
    select(Variable, Chi_Sq, df, P_Value, CFI, TLI, RMSEA_CI, SRMR,
           Intercept_Beta, Intercept_SE, Intercept_Z, Intercept_P,
           Slope_Beta, Slope_SE, Slope_Z, Slope_P,
           IntSlope_Cov, IntSlope_Cov_SE)
  
  return(final_table)
}

# Generate whole sample output
whole_sample_results <- create_whole_sample_output(fit_list)

# Save to Excel file
write.xlsx(whole_sample_results, 
           "LGCM_WholeSample_Results.xlsx",
           rowNames = FALSE)

cat("\n=== WHOLE SAMPLE RESULTS ===\n")
print(whole_sample_results)
cat("\n=== Whole sample results saved to: LGCM_WholeSample_Results.xlsx ===\n")

# ==============================================================================
# Multi-group model by sex ####
# ==============================================================================

dat_scaled <- read.csv(paste0(sourceFileStem, "PGIQ_scaled.csv"))
dat_scaled$sex1 <- factor(dat_scaled$sex1, levels = c(0, 1), labels = c("females", "males"))

fit_lgcm_multigroup_sex <- function(data, var_list, trait_category) {
  
  # --- Extract relevant variables and ages from VarList ---
  # from VarList get the variables and the corresponding ages for a specific category 
  # var_list <- G_Composites_Varlist
  # trait_category <- "g"
  trait_specific_list <- Filter(function(x) x[5] == trait_category, var_list)
  
  # Stop with an error if no variables are found for that trait
  if (length(trait_specific_list) == 0) {
    stop(paste("No variables found for trait:", trait_category))
  }
  
  # Extract the base variable name (1st element)
  phenotype_vars_base <- sapply(trait_specific_list, `[`, 1)
  # Extract the age string (2nd element)
  age_strings <- sapply(trait_specific_list, `[`, 2)
  
  # parse the age strings into numbers
  # Use stringr::str_extract to pull out the numbers from strings like "25 yr"
  ages <- as.numeric(stringr::str_extract(age_strings, "\\d+"))
  # Handle special cases like "birth" by setting their age to 0
  ages[age_strings == "birth"] <- 0
  
  trait_info <- tibble::tibble(
    variable_name = paste0(phenotype_vars_base, "1"),
    age = ages
  )
  # print(trait_info)
  
  phenotype_vars <- trait_info$variable_name
  
  # --- Create the slope loadings based on real ages ---
  # This centers the time scores, so the intercept represents the first time point.
  slope_loadings <- trait_info$age - min(trait_info$age)
  
  # --- Build the model syntax ---
  # we first create different model syntax, then we will combine them to be fitted to the data  
  # Define latent growth factors
  growth_syntax <- paste0(
    "# Latent growth factors\n",
    "intercept =~ 1*", paste(phenotype_vars, collapse = " + 1*"), "\n",
    "slope =~ ", paste(slope_loadings, "*", phenotype_vars, collapse = " + "), "\n\n"
  )
  
  # autoregressive paths (the actual thing is more complicated but this is the idea)
  # ar_paths <- ""
  # for(i in 2:n_timepoints) {
  #   ar_paths <- paste0(ar_paths, phenotype_vars[i], " ~ ", phenotype_vars[i-1], "\n")
  # }
  
  # Add PGS and covariate effects
  pgs_effects <- paste0(
    "# PGS effects on growth factors\n",
    "intercept ~ PGIQ \n",
    "slope ~ PGIQ \n\n"
  ) # I could also add the chiptype and 10 PCs here, but they are not significant 
  
  # Add residual variances and covariances
  variance_syntax <- paste0(
    "# Residual variances & factor covariances\n",
    paste(phenotype_vars, "~~", phenotype_vars, collapse = "\n"), "\n",
    "intercept ~~ intercept\n",
    "slope ~~ slope\n",
    # "slope ~~ 0*slope\n", # restrict the variance of slope to be 0 to help with convergence, but in this case, the intSlope_Cov will be NA ####
    "intercept ~~ slope\n"
  )
  
  full_syntax <- paste(growth_syntax, pgs_effects, variance_syntax)
  
  # Fit model accounting for clustering by family ID
  fit <- growth(full_syntax, data = data, missing = "ML", cluster = "id_fam", 
                group = "sex1"#,
                #group.equal = "means" # test for intercept/slope means equality across groups
  )
  
  return(list(model = fit, syntax = full_syntax))
}

g_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, G_Composites_Varlist, "g")
# print(summary(g_lgcm_sex$model, fit.measures = TRUE))

vb_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, G_Composites_Varlist, "verbal ability")
# print(summary(vb_lgcm_sex$model, fit.measures = TRUE))

nv_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, G_Composites_Varlist, "nonverbal ability")
# print(summary(nv_lgcm_sex$model, fit.measures = TRUE))

eng_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Edu_Achieve_Attain_Varlist, "English")
# print(summary(eng_lgcm_sex$model, fit.measures = TRUE))

maths_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Edu_Achieve_Attain_Varlist, "Maths")
# print(summary(maths_lgcm_sex$model, fit.measures = TRUE))

science_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Edu_Achieve_Attain_Varlist, "Science")
# print(summary(science_lgcm_sex$model, fit.measures = TRUE))

edu_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Edu_Achieve_Attain_Varlist, "Core-Subject")
# print(summary(edu_lgcm_sex$model, fit.measures = TRUE))

ht_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anthro_Varlist, "Height")
# print(summary(ht_lgcm_sex$model, fit.measures = TRUE))

BMI_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anthro_Varlist, "BMI")
# print(summary(BMI_lgcm_sex$model, fit.measures = TRUE))

shyness_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anxiety_Varlist_oneRater, "Shyness")
# print(summary(shyness_lgcm_sex$model, fit.measures = TRUE))

fear_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anxiety_Varlist_oneRater, "Fear")
# print(summary(fear_lgcm_sex$model, fit.measures = TRUE))

ocb_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anxiety_Varlist_oneRater, "OCB")
# print(summary(ocb_lgcm_sex$model, fit.measures = TRUE))

negaffect_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anxiety_Varlist_oneRater, "Negative Affect")
# print(summary(negaffect_lgcm_sex$model, fit.measures = TRUE))

negcog_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anxiety_Varlist_oneRater, "Negative Cognition")
# print(summary(negcog_lgcm_sex$model, fit.measures = TRUE))

anxiety_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Anxiety_Varlist_oneRater, "Anxiety")
# print(summary(anxiety_lgcm_sex$model, fit.measures = TRUE))

inatt_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Conners_Varlist_oneRater, "Inattention")
# print(summary(inatt_lgcm_sex$model, fit.measures = TRUE))

hyp_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Conners_Varlist_oneRater, "Hyper-Impuls")
# print(summary(hyp_lgcm_sex$model, fit.measures = TRUE))

adhd_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, Conners_Varlist_oneRater, "ADHD")
# print(summary(adhd_lgcm_sex$model, fit.measures = TRUE))

conduct_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, SDQ_Varlist_oneRater, "Conduct")
# print(summary(conduct_lgcm_sex$model, fit.measures = TRUE))

emotion_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, SDQ_Varlist_oneRater, "Emotion")
# print(summary(emotion_lgcm_sex$model, fit.measures = TRUE))

hyperSDQ_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, SDQ_Varlist_oneRater, "Hyperactivity")
# print(summary(hyperSDQ_lgcm_sex$model, fit.measures = TRUE))

peer_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, SDQ_Varlist_oneRater, "Peer Problems")
# print(summary(peer_lgcm_sex$model, fit.measures = TRUE))

prosocial_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, SDQ_Varlist_oneRater, "Prosocial")
# print(summary(prosocial_lgcm_sex$model, fit.measures = TRUE))

sdq_lgcm_sex <- fit_lgcm_multigroup_sex(dat_scaled, SDQ_Varlist_oneRater, "Total Problems")
# print(summary(sdq_lgcm_sex$model, fit.measures = TRUE))

fit_list_sex <- list(
  # Cognitive abilities
  "g" = g_lgcm_sex$model,
  "Verbal" = vb_lgcm_sex$model,
  "Nonverbal" = nv_lgcm_sex$model,
  
  # Educational achievement
  "English" = eng_lgcm_sex$model,
  "Maths" = maths_lgcm_sex$model,
  "Science" = science_lgcm_sex$model,
  "Core_Subject" = edu_lgcm_sex$model,
  
  # Physical traits
  "Height" = ht_lgcm_sex$model,
  "BMI" = BMI_lgcm_sex$model,
  
  # Anxiety-related traits
  "Shyness" = shyness_lgcm_sex$model,
  "Fear" = fear_lgcm_sex$model,
  "OCB" = ocb_lgcm_sex$model,
  "Negative_Affect" = negaffect_lgcm_sex$model,
  "Negative_Cognition" = negcog_lgcm_sex$model,
  "Anxiety" = anxiety_lgcm_sex$model,
  
  # ADHD traits
  "Inattention" = inatt_lgcm_sex$model,
  "Hyperactivity_Impulsivity" = hyp_lgcm_sex$model,
  "ADHD" = adhd_lgcm_sex$model,
  
  # SDQ traits
  "Conduct" = conduct_lgcm_sex$model,
  "Emotion" = emotion_lgcm_sex$model,
  "Hyperactivity_SDQ" = hyperSDQ_lgcm_sex$model,
  "Peer_Problems" = peer_lgcm_sex$model,
  "Prosocial" = prosocial_lgcm_sex$model,
  "Total_Problems" = sdq_lgcm_sex$model
)

# =============================================================================
# Export sex-stratified results to single Excel file ####
# =============================================================================

create_sex_stratified_output <- function(fit_list) {
  
  # Fit measures of interest
  fit_measures_of_interest <- c("chisq.scaled", "df", "pvalue.scaled", 
                                "cfi.scaled", "tli.scaled", "rmsea.scaled",
                                "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
                                "srmr")
  
  results_list <- lapply(names(fit_list), function(model_name) {
    fit <- fit_list[[model_name]]
    
    # Extract fit indices
    indices <- fitMeasures(fit, fit_measures_of_interest)
    
    # Extract standardized parameters
    std_params <- standardizedSolution(fit, se = TRUE, type = "std.all")
    
    # Get group labels
    group_labels <- lavInspect(fit, "group.label")
    
    # Get PGIQ effects by group
    pgiq_effects <- std_params %>%
      filter(op == "~" & lhs %in% c("intercept", "slope") & rhs == "PGIQ") %>%
      mutate(
        Group_Label = group_labels[group],
        Z_value = est.std / se,
        P_value = 2 * pnorm(-abs(Z_value))
      ) %>%
      select(Group_Label, lhs, Std_Beta = est.std, SE = se, Z = Z_value, P = P_value)
    
    # Combine into one row
    result_row <- data.frame(
      Variable = model_name,
      Chi_Sq = indices["chisq.scaled"],
      df = indices["df"],
      P_Value = indices["pvalue.scaled"],
      CFI = indices["cfi.scaled"],
      TLI = indices["tli.scaled"],
      RMSEA = indices["rmsea.scaled"],
      RMSEA_Lower = indices["rmsea.ci.lower.scaled"],
      RMSEA_Upper = indices["rmsea.ci.upper.scaled"],
      SRMR = indices["srmr"]
    )
    
    # Add effects for females
    female_intercept <- pgiq_effects %>% 
      filter(Group_Label == "females" & lhs == "intercept")
    female_slope <- pgiq_effects %>% 
      filter(Group_Label == "females" & lhs == "slope")
    
    if (nrow(female_intercept) > 0) {
      result_row$Female_Intercept_Beta <- female_intercept$Std_Beta
      result_row$Female_Intercept_SE <- female_intercept$SE
      result_row$Female_Intercept_Z <- female_intercept$Z
      result_row$Female_Intercept_P <- female_intercept$P
    }
    
    if (nrow(female_slope) > 0) {
      result_row$Female_Slope_Beta <- female_slope$Std_Beta
      result_row$Female_Slope_SE <- female_slope$SE
      result_row$Female_Slope_Z <- female_slope$Z
      result_row$Female_Slope_P <- female_slope$P
    }
    
    # Add effects for males
    male_intercept <- pgiq_effects %>% 
      filter(Group_Label == "males" & lhs == "intercept")
    male_slope <- pgiq_effects %>% 
      filter(Group_Label == "males" & lhs == "slope")
    
    if (nrow(male_intercept) > 0) {
      result_row$Male_Intercept_Beta <- male_intercept$Std_Beta
      result_row$Male_Intercept_SE <- male_intercept$SE
      result_row$Male_Intercept_Z <- male_intercept$Z
      result_row$Male_Intercept_P <- male_intercept$P
    }
    
    if (nrow(male_slope) > 0) {
      result_row$Male_Slope_Beta <- male_slope$Std_Beta
      result_row$Male_Slope_SE <- male_slope$SE
      result_row$Male_Slope_Z <- male_slope$Z
      result_row$Male_Slope_P <- male_slope$P
    }
    
    return(result_row)
  })
  
  # Combine all results
  final_table <- bind_rows(results_list)
  
  # Create RMSEA CI column
  final_table <- final_table %>%
    mutate(
      RMSEA_CI = paste0(RMSEA, " [", 
                        RMSEA_Lower, ", ", 
                        RMSEA_Upper, "]")
    ) %>%
    select(Variable, Chi_Sq, df, P_Value, CFI, TLI, RMSEA_CI, SRMR,
           Female_Intercept_Beta, Female_Intercept_SE, Female_Intercept_Z, Female_Intercept_P,
           Female_Slope_Beta, Female_Slope_SE, Female_Slope_Z, Female_Slope_P,
           Male_Intercept_Beta, Male_Intercept_SE, Male_Intercept_Z, Male_Intercept_P,
           Male_Slope_Beta, Male_Slope_SE, Male_Slope_Z, Male_Slope_P)
  
  return(final_table)
}

# Generate sex-stratified output
sex_stratified_results <- create_sex_stratified_output(fit_list_sex)

# Save to Excel files
write.xlsx(sex_stratified_results, 
           "LGCM_SexStratified_Results.xlsx",
           rowNames = FALSE)

cat("\n=== Results saved to Excel files ===\n")
cat("Whole sample: LGCM_WholeSample_Results.xlsx\n")
cat("Sex-stratified: LGCM_SexStratified_Results.xlsx\n")

# Preview the tables
cat("\n=== WHOLE SAMPLE RESULTS ===\n")
print(whole_sample_results)

cat("\n=== SEX-STRATIFIED RESULTS ===\n")
print(sex_stratified_results)















# rearrange the table to become wide format for the multi-group results ####
# Load the tidyverse library, which includes dplyr, tidyr, and readr
# If you don't have it installed, run this line first: install.packages("tidyverse")
library(tidyverse)

# --- 1. Read Data ---
# Read the comprehensive results to get the correct model order
comprehensive_data <- read_csv("/Users/yujinglin/Desktop/polygenic IQ score/Results220925/LGC_Multi-Group/LGCM_MultiGroup_Comprehensive_Results.csv")

# Read the detailed regression data that we need to transform
detailed_data <- read_csv("/Users/yujinglin/Desktop/polygenic IQ score/Results220925/LGC_Multi-Group/LGCM_MultiGroup_Detailed_Regressions.csv")

# --- 2. Get Model Order ---
# Extract the 'Model' column from the comprehensive table. This will be our sort key.
model_order <- comprehensive_data$Model

# --- 3. Reshape the Data ---
# We will pivot the detailed data from long to wide format.
# We'll also rename the columns to match your request (est, se, z, p).
detailed_wide <- detailed_data %>%
  
  # a. Select only the columns we need and rename them
  select(
    Model,
    Group_Label,
    Outcome,
    est = Estimate,
    se = SE,
    z = Z_value,
    p = P_value
  ) %>%
  
  # b. Create a new key column combining Group and Outcome (e.g., "females_intercept")
  mutate(group_outcome_key = paste(Group_Label, Outcome, sep = "_")) %>%
  
  # c. Pivot the data to a wide format
  pivot_wider(
    id_cols = Model, # Each row will be one Model
    names_from = group_outcome_key, # Column names will come from our new key
    values_from = c(est, se, z, p),  # The values to fill the new columns
    # This ensures names are like "females_intercept_est", "females_intercept_se", etc.
    names_glue = "{group_outcome_key}_{.value}" 
  )

# --- 4. Reorder the Data (Rows and Columns) ---
# Now we reorder the rows of our new wide table to match the comprehensive table
# and reorder the columns as requested.

# Define the desired column order
col_order <- c(
  "Model", # Keep the Model column first
  "females_slope_est", "females_slope_se", "females_slope_z", "females_slope_p",
  "females_intercept_est", "females_intercept_se", "females_intercept_z", "females_intercept_p",
  "males_slope_est", "males_slope_se", "males_slope_z", "males_slope_p",
  "males_intercept_est", "males_intercept_se", "males_intercept_z", "males_intercept_p"
)

detailed_wide_ordered <- detailed_wide %>%
  
  # a. Convert the 'Model' column to a factor, with levels set by our 'model_order'
  mutate(Model = factor(Model, levels = model_order)) %>%
  
  # b. Arrange the data frame by this new factor (row order)
  arrange(Model) %>%
  
  # c. Reorder the columns
  # We use any_of() in case some models don't have all combinations,
  # which would prevent select() from finding the column and causing an error.
  select(any_of(col_order))

# --- 5. Output the Table ---
# Write the final, transformed data frame to a new CSV file.
write_csv(detailed_wide_ordered, "LGCM_Detailed_Regressions_Wide_Ordered.csv")

# Print a message to the console
print("Successfully reshaped and reordered the data.")
print("Output saved to 'LGCM_Detailed_Regressions_Wide_Ordered.csv'")

# rearrange the table to become wide format for the whole sample results ####
comprehensive_data <- read_csv("/Users/yujinglin/Desktop/polygenic IQ score/Results220925/LGC_All/LGCM_Comprehensive_Results.csv")
detailed_data <- read_csv("/Users/yujinglin/Desktop/polygenic IQ score/Results220925/LGC_all/LGCM_Parameter_Summary.csv")
