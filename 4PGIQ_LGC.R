# Latent Growth Curve Model

library(lavaan)
library(stringr)
library(tibble)
library(semPlot)
library(tidyr) 
library(dplyr) 
library(openxlsx) 
library(readr)

# =============================================================================
# 1. Directory Setup 
# =============================================================================
base_dir <- "./"
code_dir <- file.path(base_dir, "PGIQ Codes")
data_dir <- file.path(base_dir, "Data")
results_dir <- file.path(base_dir, "Results/LGC/")

source(file.path(code_dir, "0PGIQ_VarList.R"))

dat_scaled <- read.csv(file.path(data_dir, "PGIQ_scaled.csv"))
dat_scaled$sex1 <- factor(dat_scaled$sex1, levels = c(0, 1), labels = c("females", "males"))

# =============================================================================
# 2. Universal Latent Growth Curve Model Function
# =============================================================================
fit_lgcm <- function(data, var_list, trait_category, group_var = NULL) {
  
  trait_specific_list <- Filter(function(x) x[5] == trait_category, var_list)
  
  if (length(trait_specific_list) == 0) {
    stop(paste("No variables found for trait:", trait_category))
  }
  
  phenotype_vars_base <- sapply(trait_specific_list, `[`, 1)
  age_strings <- sapply(trait_specific_list, `[`, 2)
  
  ages <- as.numeric(stringr::str_extract(age_strings, "\\d+"))
  ages[age_strings == "birth"] <- 0
  
  trait_info <- tibble::tibble(
    variable_name = paste0(phenotype_vars_base, "1"),
    age = ages
  )
  
  phenotype_vars <- trait_info$variable_name
  slope_loadings <- trait_info$age - min(trait_info$age)
  
  growth_syntax <- paste0(
    "# Latent growth factors\n",
    "intercept =~ 1*", paste(phenotype_vars, collapse = " + 1*"), "\n",
    "slope =~ ", paste(slope_loadings, "*", phenotype_vars, collapse = " + "), "\n\n"
  )
  
  pgs_effects <- paste0(
    "# PGS effects on growth factors\n",
    "intercept ~ PGgS", ifelse(is.null(group_var), " + sex1", ""), "\n",
    "slope ~ PGgS", ifelse(is.null(group_var), " + sex1", ""), "\n\n"
  ) 
  
  variance_syntax <- paste0(
    "# Residual variances & factor covariances\n",
    paste(phenotype_vars, "~~", phenotype_vars, collapse = "\n"), "\n",
    "intercept ~~ intercept\n", 
    "slope ~~ slope\n", 
    "intercept ~~ slope\n" 
  )
  
  full_syntax <- paste(growth_syntax, pgs_effects, variance_syntax)
  
  if (is.null(group_var)) {
    fit <- growth(full_syntax, data = data, missing = "ML", cluster = "id_fam")
  } else {
    fit <- growth(full_syntax, data = data, missing = "ML", cluster = "id_fam", group = group_var)
  }
  
  return(fit)
}

# =============================================================================
# 3. Model Configurations & Execution
# =============================================================================
model_configs <- list(
  "g" = list(G_Composites_Varlist, "g"),
  "Verbal" = list(G_Composites_Varlist, "verbal ability"),
  "Nonverbal" = list(G_Composites_Varlist, "nonverbal ability"),
  "English" = list(Edu_Achieve_Attain_Varlist, "English Achievement"),
  "Maths" = list(Edu_Achieve_Attain_Varlist, "Maths Achievement"),
  "Science" = list(Edu_Achieve_Attain_Varlist, "Science Achievement"),
  "Core_Subject" = list(Edu_Achieve_Attain_Varlist, "Core-Subject Achievement"),
  "Height" = list(Anthro_Varlist, "Height"),
  "BMI" = list(Anthro_Varlist, "BMI"),
  "Shyness" = list(Anxiety_Varlist_oneRater, "Shyness"),
  "Fear" = list(Anxiety_Varlist_oneRater, "Fear"),
  "OCB" = list(Anxiety_Varlist_oneRater, "OCB"),
  "Negative_Affect" = list(Anxiety_Varlist_oneRater, "Negative Affect"),
  "Negative_Cognition" = list(Anxiety_Varlist_oneRater, "Negative Cognition"),
  "Anxiety" = list(Anxiety_Varlist_oneRater, "Anxiety Total"),
  "Inattention" = list(Conners_Varlist_oneRater, "Inattention"),
  "Hyperactivity_Impulsivity" = list(Conners_Varlist_oneRater, "Hyper-Impuls"),
  "ADHD" = list(Conners_Varlist_oneRater, "ADHD Total"),
  "Conduct" = list(SDQ_Varlist_oneRater, "Conduct"),
  "Emotion" = list(SDQ_Varlist_oneRater, "Emotion"),
  "Hyperactivity_SDQ" = list(SDQ_Varlist_oneRater, "Hyperactivity"),
  "Peer_Problems" = list(SDQ_Varlist_oneRater, "Peer Problems"),
  "Prosocial" = list(SDQ_Varlist_oneRater, "Prosocial"),
  "Total_Problems" = list(SDQ_Varlist_oneRater, "Total Problems")
)

print("--- STARTING WHOLE SAMPLE MODELS ---")
fit_list <- lapply(names(model_configs), function(trait_name) {
  print(paste("==> Running Whole Sample Model for:", trait_name))
  cfg <- model_configs[[trait_name]]
  fit_lgcm(data = dat_scaled, var_list = cfg[[1]], trait_category = cfg[[2]], group_var = NULL)
})
names(fit_list) <- names(model_configs)

print("--- STARTING SEX-STRATIFIED MODELS ---")
fit_list_sex <- lapply(names(model_configs), function(trait_name) {
  print(paste("==> Running Sex-Stratified Model for:", trait_name))
  cfg <- model_configs[[trait_name]]
  fit_lgcm(data = dat_scaled, var_list = cfg[[1]], trait_category = cfg[[2]], group_var = "sex1")
})
names(fit_list_sex) <- names(model_configs)

# =============================================================================
# 4. Export Functions
# =============================================================================
create_whole_sample_output <- function(fit_list) {
  fit_measures_of_interest <- c("chisq.scaled", "df", "pvalue.scaled", 
                                "cfi.scaled", "tli.scaled", "rmsea.scaled",
                                "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")
  
  results_list <- lapply(names(fit_list), function(model_name) {
    fit <- fit_list[[model_name]]
    indices <- fitMeasures(fit, fit_measures_of_interest)
    params <- parameterEstimates(fit)
    
    pgiq_effects <- params %>%
      filter(op == "~" & lhs %in% c("intercept", "slope") & rhs == "PGgS") %>%
      select(lhs, Std_Beta = est, SE = se, Z = z, P = pvalue) 
    
    int_slope_cov <- params %>%
      filter(op == "~~" & lhs == "intercept" & rhs == "slope") %>%
      select(Std_Cov = est, Cov_SE = se)
    
    result_row <- data.frame(
      Variable = model_name,
      Chi_Sq = indices["chisq.scaled"], df = indices["df"], P_Value = indices["pvalue.scaled"],
      CFI = indices["cfi.scaled"], TLI = indices["tli.scaled"], RMSEA = indices["rmsea.scaled"],
      RMSEA_Lower = indices["rmsea.ci.lower.scaled"], RMSEA_Upper = indices["rmsea.ci.upper.scaled"],
      SRMR = indices["srmr"]
    )
    
    intercept_row <- pgiq_effects %>% filter(lhs == "intercept")
    slope_row <- pgiq_effects %>% filter(lhs == "slope")
    
    if (nrow(intercept_row) > 0) result_row[c("Intercept_Beta", "Intercept_SE", "Intercept_Z", "Intercept_P")] <- intercept_row[c("Std_Beta", "SE", "Z", "P")]
    if (nrow(slope_row) > 0) result_row[c("Slope_Beta", "Slope_SE", "Slope_Z", "Slope_P")] <- slope_row[c("Std_Beta", "SE", "Z", "P")]
    if (nrow(int_slope_cov) > 0) result_row[c("IntSlope_Cov", "IntSlope_Cov_SE")] <- int_slope_cov[c("Std_Cov", "Cov_SE")]
    
    return(result_row)
  })
  
  final_table <- bind_rows(results_list) %>%
    mutate(RMSEA_CI = paste0(sprintf("%.2f", RMSEA), " [", sprintf("%.2f", RMSEA_Lower), ", ", sprintf("%.2f", RMSEA_Upper), "]")) %>%
    select(Variable, Chi_Sq, df, P_Value, CFI, TLI, RMSEA_CI, SRMR, Intercept_Beta, Intercept_SE, Intercept_Z, Intercept_P, Slope_Beta, Slope_SE, Slope_Z, Slope_P, IntSlope_Cov, IntSlope_Cov_SE)
  
  return(final_table)
}

create_sex_stratified_output <- function(fit_list) {
  fit_measures_of_interest <- c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")
  
  results_list <- lapply(names(fit_list), function(model_name) {
    fit <- fit_list[[model_name]]
    indices <- fitMeasures(fit, fit_measures_of_interest)
    params <- parameterEstimates(fit)
    group_labels <- lavInspect(fit, "group.label")
    
    pgiq_effects <- params %>% filter(op == "~" & lhs %in% c("intercept", "slope") & rhs == "PGgS") %>%
      mutate(Group_Label = group_labels[group]) %>%
      select(Group_Label, lhs, Std_Beta = est, SE = se, Z = z, P = pvalue)
    
    result_row <- data.frame(
      Variable = model_name, Chi_Sq = indices["chisq.scaled"], df = indices["df"], P_Value = indices["pvalue.scaled"], CFI = indices["cfi.scaled"], TLI = indices["tli.scaled"], RMSEA = indices["rmsea.scaled"], RMSEA_Lower = indices["rmsea.ci.lower.scaled"], RMSEA_Upper = indices["rmsea.ci.upper.scaled"], SRMR = indices["srmr"]
    )
    
    f_int <- pgiq_effects %>% filter(Group_Label == "females" & lhs == "intercept")
    f_slp <- pgiq_effects %>% filter(Group_Label == "females" & lhs == "slope")
    m_int <- pgiq_effects %>% filter(Group_Label == "males" & lhs == "intercept")
    m_slp <- pgiq_effects %>% filter(Group_Label == "males" & lhs == "slope")
    
    if (nrow(f_int) > 0) result_row[c("Female_Intercept_Beta", "Female_Intercept_SE", "Female_Intercept_Z", "Female_Intercept_P")] <- f_int[c("Std_Beta", "SE", "Z", "P")]
    if (nrow(f_slp) > 0) result_row[c("Female_Slope_Beta", "Female_Slope_SE", "Female_Slope_Z", "Female_Slope_P")] <- f_slp[c("Std_Beta", "SE", "Z", "P")]
    if (nrow(m_int) > 0) result_row[c("Male_Intercept_Beta", "Male_Intercept_SE", "Male_Intercept_Z", "Male_Intercept_P")] <- m_int[c("Std_Beta", "SE", "Z", "P")]
    if (nrow(m_slp) > 0) result_row[c("Male_Slope_Beta", "Male_Slope_SE", "Male_Slope_Z", "Male_Slope_P")] <- m_slp[c("Std_Beta", "SE", "Z", "P")]
    
    return(result_row)
  })
  
  final_table <- bind_rows(results_list) %>%
    mutate(RMSEA_CI = paste0(sprintf("%.2f", RMSEA), " [", sprintf("%.2f", RMSEA_Lower), ", ", sprintf("%.2f", RMSEA_Upper), "]")) %>%
    select(Variable, Chi_Sq, df, P_Value, CFI, TLI, RMSEA_CI, SRMR, Female_Intercept_Beta, Female_Intercept_SE, Female_Intercept_Z, Female_Intercept_P, Female_Slope_Beta, Female_Slope_SE, Female_Slope_Z, Female_Slope_P, Male_Intercept_Beta, Male_Intercept_SE, Male_Intercept_Z, Male_Intercept_P, Male_Slope_Beta, Male_Slope_SE, Male_Slope_Z, Male_Slope_P)
  
  return(final_table)
}

# =============================================================================
# 5. Execute Exports & Save 
# =============================================================================
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

print("--- EXTRACTING RESULTS & SAVING FILES ---")
whole_sample_results <- create_whole_sample_output(fit_list)
sex_stratified_results <- create_sex_stratified_output(fit_list_sex)

write.xlsx(whole_sample_results, file.path(results_dir, "LGCM_WholeSample_Results.xlsx"), rowNames = FALSE)
write.xlsx(sex_stratified_results, file.path(results_dir, "LGCM_SexStratified_Results.xlsx"), rowNames = FALSE)
write.csv(whole_sample_results, file.path(results_dir, "LGCM_WholeSample_Results.csv"), row.names = FALSE)
write.csv(sex_stratified_results, file.path(results_dir, "LGCM_SexStratified_Results.csv"), row.names = FALSE)
