# polygenic IQ population prediction ####
# 2 June 2025
# Yujing Lin

# 1. population prediction w/wo covariates 
# 2. phenotypic prediction using IQ phenotype scores 
# 3. multi-level modelling to incorporate longitudinal data 
# 4. age interaction 
# 5. sex interaction 
# 6. latent growth curve modelling: with the highest and lowest PGIQ groups 

library(scales)
library(tidyr)
# library(Hmisc) #extract variable labels: label(data$variable)
# library(data.table)
library(boot)
# library(metafor)
library(openxlsx)
# library(MASS) # ordered logistic or probit regression
# library(pscl)
library(ggplot2)
# library(gridExtra)
# library(ggpubr)
# library(corrplot)  # for visualization
# library(moments)
library(dplyr) # I like to have dplyr at the end so select() won't be conflicted with MASS::select()
# library(foreign) # read.spss
library(tidyverse)
library(psych)

select <- dplyr::select

nboot <- 10 # change it to 1000 on HPC
ncpus <- 4 # change it to 32 on HPC

sourceFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Data220925/'
outFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Results220925/Reg_All/'
# sourceFileStem <- '/scratch/users/k21170717/polygenic_IQ_score/Data220925/'
# outFileStem <- '/scratch/users/k21170717/polygenic_IQ_score/Results220925/'

source("/Users/yujinglin/Desktop/PGIQ Codes/0PGIQ_VarList.R") # obtain the var lists
# source("/scratch/users/k21170717/polygenic_IQ_score/0PGIQ_VarList.R")

dat_scaled_selectunpaired <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired.csv"))
dat_scaled_selectunpaired_F <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_F.csv"))
dat_scaled_selectunpaired_M <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_M.csv"))

# add an age dummy for the cross-age latent factors equals to 1 for everyone 
dat_scaled_selectunpaired$CrossAgeDummy <- 1
dat_scaled_selectunpaired_F$CrossAgeDummy <- 1
dat_scaled_selectunpaired_M$CrossAgeDummy <- 1

# have a test list here 
CommonFactor_Score_Varlist <- list(
  # -- Cognitive Composites --
  c("g_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "General Cognitive Ability (g)"),
  c("vb_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Verbal Ability"),
  c("nv_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Nonverbal Ability"),
  
  # -- Anxiety Traits (ARBQ) --
  c("ARBQ_shy_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Shyness"),
  c("ARBQ_fear_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Fear"),
  c("ARBQ_ocb_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Obsessive-Compulsive"),
  c("ARBQ_naff_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Affect"),
  c("ARBQ_ncog_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Cognition"),
  c("Anxiety_total_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Anxiety Total"),
  
  # -- Conners Traits (ADHD) --
  c("Conners_inatt_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Conners Inattention"),
  c("Conners_inatt_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Conners Inattention"),
  c("Conners_hyper_impul_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Conners Hyperactivity-Impulsivity"),
  c("Conners_hyper_impul_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Conners Hyperactivity-Impulsivity"),
  c("Conners_Total_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Conners Total"),
  c("Conners_Total_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Conners Total"),
  
  # -- SDQ Conduct --
  c("SDQ_Conduct_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "SDQ Conduct"),
  c("SDQ_Conduct_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "SDQ Conduct"),
  c("SDQ_Conduct_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "SDQ Conduct"),
  c("SDQ_Conduct_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "SDQ Conduct"),
  
  # -- SDQ Emotion --
  c("SDQ_Emotion_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "SDQ Emotion"),
  c("SDQ_Emotion_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "SDQ Emotion"),
  c("SDQ_Emotion_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "SDQ Emotion"),
  c("SDQ_Emotion_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "SDQ Emotion"),
  
  # -- SDQ Hyperactivity --
  c("SDQ_Hyperactivity_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "SDQ Hyperactivity"),
  c("SDQ_Hyperactivity_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "SDQ Hyperactivity"),
  c("SDQ_Hyperactivity_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "SDQ Hyperactivity"),
  c("SDQ_Hyperactivity_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "SDQ Hyperactivity"),
  
  # -- SDQ Peer Problems --
  c("SDQ_Peer_Problems_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "SDQ Peer Problems"),
  c("SDQ_Peer_Problems_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "SDQ Peer Problems"),
  c("SDQ_Peer_Problems_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "SDQ Peer Problems"),
  c("SDQ_Peer_Problems_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "SDQ Peer Problems"),
  
  # -- SDQ Prosocial --
  c("SDQ_Prosocial_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "SDQ Prosocial"),
  c("SDQ_Prosocial_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "SDQ Prosocial"),
  c("SDQ_Prosocial_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "SDQ Prosocial"),
  c("SDQ_Prosocial_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "SDQ Prosocial"),
  
  # -- SDQ Total Problems --
  c("SDQ_Total_Problems_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "SDQ Total Problems"),
  c("SDQ_Total_Problems_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "SDQ Total Problems"),
  c("SDQ_Total_Problems_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "SDQ Total Problems"),
  c("SDQ_Total_Problems_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "SDQ Total Problems")
)



fit_linear_model_boot <- function(dat, indices, y, x, covar) {
  datx <- dat[indices, ] # Create the bootstrap sample
  
  # Define model formulas
  # The full model includes the main predictor 'x' and any covariates
  formula_full <- reformulate(c(x, covar), response = y)
  # The reduced model includes only the covariates (or is intercept-only if no covariates)
  formula_reduced <- if (!is.null(covar)) reformulate(covar, response = y) else reformulate("1", response = y)
  
  # Use tryCatch to handle any model fitting errors gracefully, which can occur
  # in bootstrap resamples (e.g., due to collinearity or lack of variance).
  tryCatch({
    # Fit the full and reduced models
    fit_model_full <- lm(formula_full, data = datx)
    fit_model_reduced <- lm(formula_reduced, data = datx)
    
    model_summary <- summary(fit_model_full)
    
    # Safely extract coefficient and p-value for the predictor 'x'
    if (x %in% rownames(model_summary$coefficients)) {
      beta <- model_summary$coefficients[x, "Estimate"]
      p_value <- model_summary$coefficients[x, "Pr(>|t|)"]
    } else {
      # This case handles rare instances where the predictor is dropped
      beta <- NA
      p_value <- NA
    }
    
    # Calculate R-squared values
    R.squared_full <- model_summary$r.squared
    R.squared_reduced <- summary(fit_model_reduced)$r.squared
    R.squared_predictor <- R.squared_full - R.squared_reduced
    
    return(c(beta, p_value, R.squared_full, R.squared_predictor))
    
  }, error = function(e) {
    # If an error occurs during fitting, return a vector of NAs
    return(c(NA, NA, NA, NA))
  })
}

run_linear_prediction_boot <- function(dat, var_list, predictor_var, include_covariates, sex_var, nboot, ncpus) {
  
  # --- 1. Main Loop: Iterate through each outcome variable and run bootstrapping ---
  boot_results_list <- lapply(var_list, function(var_item) {
    
    # Extract details from the var_list item
    outcome_stem <- var_item[1]
    age_variable <- var_item[3]
    
    # Construct the full outcome variable name by adding the "1" suffix
    y <- paste0(outcome_stem, "1")
    # y <- outcome_stem
    x <- predictor_var
    
    cat("Processing outcome:", y, "with predictor:", x, "\n")
    
    # Define the set of covariates
    covariates <- NULL
    if (include_covariates) {
      base_covariates <- c(age_variable, sex_var, "chiptype")
      pc_covariates <- paste0("PC", 1:10)
      covariates <- c(base_covariates, pc_covariates)
      covariates <- covariates[!sapply(covariates, is.null)]
      # Ensure covariates actually exist in the data to avoid errors
      covariates <- covariates[covariates %in% names(dat)]
      cat("Using covariates:", paste(covariates, collapse=", "), "\n")
    } else {
      cat("Running model without covariates.\n")
    }
    
    # Use tryCatch to prevent a single failure from stopping the entire analysis
    tryCatch({
      boot_results <- boot(
        data = dat,
        statistic = fit_linear_model_boot,
        R = nboot,
        parallel = "multicore",
        ncpus = ncpus,
        y = y,
        x = x,
        covar = covariates
      )
      return(boot_results)
    }, error = function(e) {
      cat("ERROR during bootstrapping for outcome '", y, "':", e$message, "\n")
      # Return NULL on failure to be handled in the next step
      return(NULL)
    })
  })
  
  # --- 2. Process and summarize the bootstrap results ---
  results_df <- do.call(rbind, lapply(1:length(boot_results_list), function(i) {
    
    boot_output <- boot_results_list[[i]]
    var_item <- var_list[[i]]
    
    # Define a default structure for failed runs
    na_result <- data.frame(
      Outcome = var_item[1],
      Trait_Name = var_item[2],
      Rater = var_item[4],
      Category = var_item[5],
      Predictor = predictor_var,
      Beta = NA,
      SE_boot = NA,
      CI_Lower = NA,
      CI_Upper = NA,
      P_value = NA,
      R2_Full_Model = NA,
      R2_Predictor = NA,
      Beta_SE_Formatted = "NA"
    )
    
    # Check for failure or insufficient valid bootstrap replicates
    if (is.null(boot_output) || sum(!is.na(boot_output$t[, 1])) < 10) {
      if(!is.null(boot_output)){
        cat("WARNING: Too few valid bootstraps for '", var_item[1], "'\n")
      }
      return(na_result)
    }
    
    # Proceed with valid results
    # Original estimates from the full dataset (t0)
    original_estimates <- boot_output$t0
    
    # Bootstrap replicates for the beta coefficient (first column of 't')
    boot_betas <- boot_output$t[, 1]
    boot_betas <- boot_betas[!is.na(boot_betas)] # Remove any NA replicates
    
    # Calculate bootstrap standard error and percentile confidence intervals
    se_boot <- sd(boot_betas)
    ci_boot <- quantile(boot_betas, probs = c(0.025, 0.975), na.rm = TRUE)
    
    # Compile the final results for this variable
    result <- data.frame(
      Outcome = var_item[1],
      Trait_Name = var_item[2],
      Rater = var_item[4],
      Category = var_item[5],
      Predictor = predictor_var,
      Beta = original_estimates[1],
      SE_boot = se_boot,
      CI_Lower = ci_boot[1],
      CI_Upper = ci_boot[2],
      P_value = original_estimates[2],
      R2_Full_Model = original_estimates[3],
      R2_Predictor = original_estimates[4]
    )
    
    # Add a nicely formatted column for reports
    result$Beta_SE_Formatted <- paste0(
      format(round(result$Beta, 3), nsmall = 3),
      " (",
      format(round(result$SE_boot, 3), nsmall = 3),
      ")"
    )
    
    return(result)
  }))
  
  return(results_df)
}



# ===================================================================
#          Run Bootstrapped Regressions for All Phenotypes ####
# ===================================================================
# --- CFA ---
cat("Running analysis for CFA...\n")
CFA_composites_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = CommonFactor_Score_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
CFA_composites_results$P_adj_value <- p.adjust(CFA_composites_results$P_value, method = "fdr")

# --- Cognitive Composites ---
cat("Running analysis for G Composites...\n")
G_composites_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = G_Composites_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
G_composites_results$P_adj_value <- p.adjust(G_composites_results$P_value, method = "fdr")

# --- Verbal Tests ---
cat("Running analysis for Verbal Tests...\n")
Verbal_tests_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = Verbal_Tests_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Verbal_tests_results$P_adj_value <- p.adjust(Verbal_tests_results$P_value, method = "fdr")

# --- Nonverbal Tests ---
cat("Running analysis for Nonverbal Tests...\n")
Nonverbal_tests_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = Nonverbal_Tests_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Nonverbal_tests_results$P_adj_value <- p.adjust(Nonverbal_tests_results$P_value, method = "fdr")

# --- Anthropometrics ---
cat("Running analysis for Anthropometrics...\n")
Anthro_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = Anthro_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Anthro_results$P_adj_value <- p.adjust(Anthro_results$P_value, method = "fdr")

# --- Educational Results ---
cat("Running analysis for Educational Variables...\n")
Education_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = Edu_Achieve_Attain_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Education_results$P_adj_value <- p.adjust(Education_results$P_value, method = "fdr")

# --- Anxiety Measures ---
cat("Running analysis for Anxiety...\n")
Anxiety_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = Anxiety_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Anxiety_results$P_adj_value <- p.adjust(Anxiety_results$P_value, method = "fdr")

# --- Conners' ADHD Measures ---
cat("Running analysis for Conners' (ADHD)...\n")
Conners_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = Conners_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Conners_results$P_adj_value <- p.adjust(Conners_results$P_value, method = "fdr")

# --- SDQ Measures ---
cat("Running analysis for SDQ...\n")
SDQ_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = SDQ_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
SDQ_results$P_adj_value <- p.adjust(SDQ_results$P_value, method = "fdr")

# --- Wellbeing Measures ---
cat("Running analysis for Wellbeing...\n")
Wellbeing_results <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired, var_list = Wellbeing_Varlist, predictor_var = "PGIQ", 
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Wellbeing_results$P_adj_value <- p.adjust(Wellbeing_results$P_value, method = "fdr")

cat("All analyses complete.\n")

# Combine all results into a single data frame for easier viewing
all_results_combined <- bind_rows(
  "CFA_Results" = CFA_composites_results,
  "G_Composites" = G_composites_results,
  "Verbal_Tests" = Verbal_tests_results,
  "Nonverbal_Tests" = Nonverbal_tests_results,
  "Anthro" = Anthro_results,
  "Education" = Education_results,
  "Anxiety" = Anxiety_results,
  "Conners_ADHD" = Conners_results,
  "SDQ" = SDQ_results,
  "Wellbeing" = Wellbeing_results,
  .id = "Domain"
)



p2asterisk<-function(df, target_p_column, new_labelled_column_name){
  labels<-rep(NA, nrow(df)) 
  df<-tibble::add_column(df, labels, .after=target_p_column)
  x<-df[[target_p_column]]
  x1<-ifelse(x>=0.05, "",
             ifelse(x<0.05 & x>=0.01, "*",
                    ifelse(x< 0.01 & x>=0.001, "**",
                           ifelse(x<0.001 & x>=0.0001, "***",
                                  ifelse(x<0.0001, "****", NA)))))
  colnames(df)[colnames(df)=="labels"]<-new_labelled_column_name
  df[[new_labelled_column_name]]<-x1
  return(df)
}

CFA_composites_results <- p2asterisk(CFA_composites_results, "P_adj_value", "sig_adj_p")
G_composites_results <- p2asterisk(G_composites_results, "P_adj_value", "sig_adj_p")
Verbal_tests_results <- p2asterisk(Verbal_tests_results, "P_adj_value", "sig_adj_p")
Nonverbal_tests_results <- p2asterisk(Nonverbal_tests_results, "P_adj_value", "sig_adj_p")
Anthro_results <- p2asterisk(Anthro_results, "P_adj_value", "sig_adj_p")
Education_results <- p2asterisk(Education_results, "P_adj_value", "sig_adj_p")
Anxiety_results <- p2asterisk(Anxiety_results, "P_adj_value", "sig_adj_p")
Conners_results <- p2asterisk(Conners_results, "P_adj_value", "sig_adj_p")
SDQ_results <- p2asterisk(SDQ_results, "P_adj_value", "sig_adj_p")
Wellbeing_results <- p2asterisk(Wellbeing_results, "P_adj_value", "sig_adj_p")
all_results_combined <- p2asterisk(all_results_combined, "P_adj_value", "sig_adj_p")


wb <- createWorkbook()
addWorksheet(wb, sheetName="CFA_composites")
addWorksheet(wb, sheetName="G_composites")
addWorksheet(wb, sheetName="Verbal_Tests")
addWorksheet(wb, sheetName="Nonverbal_Tests")
addWorksheet(wb, sheetName="Anthro")
addWorksheet(wb, sheetName="Education")
addWorksheet(wb, sheetName="Anxiety")
addWorksheet(wb, sheetName="Conners_ADHD")
addWorksheet(wb, sheetName="SDQ")
addWorksheet(wb, sheetName="Wellbeing")
writeData(wb, sheet="CFA_composites", x=CFA_composites_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="G_composites", x=G_composites_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="Verbal_Tests", x=Verbal_tests_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="Nonverbal_Tests", x=Nonverbal_tests_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="Anthro", x=Anthro_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="Education", x=Education_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="Anxiety", x=Anxiety_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="Conners_ADHD", x=Conners_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="SDQ", x=SDQ_results, colNames=TRUE, rowNames=FALSE)
writeData(wb, sheet="Wellbeing", x=Wellbeing_results, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, paste(outFileStem, "PGIQ_20boot_adjCovar_pop.xlsx", sep=""), overwrite = TRUE)

write.csv(all_results_combined, paste(outFileStem, "PGIQ_20boot_adjCovar_pop.csv", sep=""), row.names = FALSE)
write.csv(CFA_composites_results, paste(outFileStem, "CFA_composites_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(G_composites_results, paste(outFileStem, "G_composites_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(Verbal_tests_results, paste(outFileStem, "Verbal_tests_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(Nonverbal_tests_results, paste(outFileStem, "Nonverbal_tests_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(Anthro_results, paste(outFileStem, "Anthro_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(Education_results, paste(outFileStem, "Education_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(Anxiety_results, paste(outFileStem, "Anxiety_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(Conners_results, paste(outFileStem, "Conners_ADHD_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(SDQ_results, paste(outFileStem, "SDQ_adjCovar.csv", sep=""), row.names = FALSE)
write.csv(Wellbeing_results, paste(outFileStem, "Wellbeing_adjCovar.csv", sep=""), row.names = FALSE)










# ===================================================================
#     Run Bootstrapped Regressions for FEMALES ONLY ####
# ===================================================================

# --- CFA ---
cat("Running analysis for CFA (Female)...\n")
CFA_composites_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = CommonFactor_Score_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
CFA_composites_results_F$P_adj_value <- p.adjust(CFA_composites_results_F$P_value, method = "fdr")

# --- Cognitive Composites ---
cat("Running analysis for G Composites (Female)...\n")
G_composites_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = G_Composites_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
G_composites_results_F$P_adj_value <- p.adjust(G_composites_results_F$P_value, method = "fdr")

# --- Verbal Tests ---
cat("Running analysis for Verbal Tests (Female)...\n")
Verbal_tests_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = Verbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Verbal_tests_results_F$P_adj_value <- p.adjust(Verbal_tests_results_F$P_value, method = "fdr")

# --- Nonverbal Tests ---
cat("Running analysis for Nonverbal Tests (Female)...\n")
Nonverbal_tests_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = Nonverbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Nonverbal_tests_results_F$P_adj_value <- p.adjust(Nonverbal_tests_results_F$P_value, method = "fdr")

# --- Anthropometrics ---
cat("Running analysis for Anthropometrics (Female)...\n")
Anthro_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = Anthro_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Anthro_results_F$P_adj_value <- p.adjust(Anthro_results_F$P_value, method = "fdr")

# --- Educational Results ---
cat("Running analysis for Educational Variables (Female)...\n")
Education_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = Edu_Achieve_Attain_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Education_results_F$P_adj_value <- p.adjust(Education_results_F$P_value, method = "fdr")

# --- Anxiety Measures ---
cat("Running analysis for Anxiety (Female)...\n")
Anxiety_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = Anxiety_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Anxiety_results_F$P_adj_value <- p.adjust(Anxiety_results_F$P_value, method = "fdr")

# --- Conners' ADHD Measures ---
cat("Running analysis for Conners' (ADHD) (Female)...\n")
Conners_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = Conners_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Conners_results_F$P_adj_value <- p.adjust(Conners_results_F$P_value, method = "fdr")

# --- SDQ Measures ---
cat("Running analysis for SDQ (Female)...\n")
SDQ_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = SDQ_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
SDQ_results_F$P_adj_value <- p.adjust(SDQ_results_F$P_value, method = "fdr")

# --- Wellbeing Measures ---
cat("Running analysis for Wellbeing (Female)...\n")
Wellbeing_results_F <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_F, var_list = Wellbeing_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Wellbeing_results_F$P_adj_value <- p.adjust(Wellbeing_results_F$P_value, method = "fdr")

cat("Female-only analyses complete.\n")

# Combine and apply p-value asterisks
all_results_combined_F <- bind_rows(
  "CFA_Results" = CFA_composites_results_F,
  "G_Composites" = G_composites_results_F,
  "Verbal_Tests" = Verbal_tests_results_F,
  "Nonverbal_Tests" = Nonverbal_tests_results_F,
  "Anthro" = Anthro_results_F,
  "Education" = Education_results_F,
  "Anxiety" = Anxiety_results_F,
  "Conners_ADHD" = Conners_results_F,
  "SDQ" = SDQ_results_F,
  "Wellbeing" = Wellbeing_results_F,
  .id = "Domain"
)

CFA_composites_results_F <- p2asterisk(CFA_composites_results_F, "P_adj_value", "sig_adj_p")
G_composites_results_F <- p2asterisk(G_composites_results_F, "P_adj_value", "sig_adj_p")
Verbal_tests_results_F <- p2asterisk(Verbal_tests_results_F, "P_adj_value", "sig_adj_p")
Nonverbal_tests_results_F <- p2asterisk(Nonverbal_tests_results_F, "P_adj_value", "sig_adj_p")
Anthro_results_F <- p2asterisk(Anthro_results_F, "P_adj_value", "sig_adj_p")
Education_results_F <- p2asterisk(Education_results_F, "P_adj_value", "sig_adj_p")
Anxiety_results_F <- p2asterisk(Anxiety_results_F, "P_adj_value", "sig_adj_p")
Conners_results_F <- p2asterisk(Conners_results_F, "P_adj_value", "sig_adj_p")
SDQ_results_F <- p2asterisk(SDQ_results_F, "P_adj_value", "sig_adj_p")
Wellbeing_results_F <- p2asterisk(Wellbeing_results_F, "P_adj_value", "sig_adj_p")
all_results_combined_F <- p2asterisk(all_results_combined_F, "P_adj_value", "sig_adj_p")


# --- Save Female Results to Excel and CSV ---
wb_F <- createWorkbook()
addWorksheet(wb_F, sheetName="CFA_composites")
addWorksheet(wb_F, sheetName="G_composites")
addWorksheet(wb_F, sheetName="Verbal_Tests")
addWorksheet(wb_F, sheetName="Nonverbal_Tests")
addWorksheet(wb_F, sheetName="Anthro")
addWorksheet(wb_F, sheetName="Education")
addWorksheet(wb_F, sheetName="Anxiety")
addWorksheet(wb_F, sheetName="Conners_ADHD")
addWorksheet(wb_F, sheetName="SDQ")
addWorksheet(wb_F, sheetName="Wellbeing")

writeData(wb_F, sheet="CFA_composites", x=CFA_composites_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="G_composites", x=G_composites_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="Verbal_Tests", x=Verbal_tests_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="Nonverbal_Tests", x=Nonverbal_tests_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="Anthro", x=Anthro_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="Education", x=Education_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="Anxiety", x=Anxiety_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="Conners_ADHD", x=Conners_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="SDQ", x=SDQ_results_F, colNames=TRUE, rowNames=FALSE)
writeData(wb_F, sheet="Wellbeing", x=Wellbeing_results_F, colNames=TRUE, rowNames=FALSE)

saveWorkbook(wb_F, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_F.xlsx", sep=""), overwrite = TRUE)

write.csv(all_results_combined_F, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_F.csv", sep=""), row.names = FALSE)
write.csv(CFA_composites_results_F, paste(outFileStem, "CFA_composites_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(G_composites_results_F, paste(outFileStem, "G_composites_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(Verbal_tests_results_F, paste(outFileStem, "Verbal_tests_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(Nonverbal_tests_results_F, paste(outFileStem, "Nonverbal_tests_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(Anthro_results_F, paste(outFileStem, "Anthro_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(Education_results_F, paste(outFileStem, "Education_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(Anxiety_results_F, paste(outFileStem, "Anxiety_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(Conners_results_F, paste(outFileStem, "Conners_ADHD_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(SDQ_results_F, paste(outFileStem, "SDQ_adjCovar_F.csv", sep=""), row.names = FALSE)
write.csv(Wellbeing_results_F, paste(outFileStem, "Wellbeing_adjCovar_F.csv", sep=""), row.names = FALSE)









# ===================================================================
#     Run Bootstrapped Regressions for MALES ONLY ####
# ===================================================================

# --- CFA ---
cat("Running analysis for CFA (Male)...\n")
CFA_composites_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = CommonFactor_Score_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
CFA_composites_results_M$P_adj_value <- p.adjust(CFA_composites_results_M$P_value, method = "fdr")

# --- Cognitive Composites ---
cat("Running analysis for G Composites (Male)...\n")
G_composites_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = G_Composites_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
G_composites_results_M$P_adj_value <- p.adjust(G_composites_results_M$P_value, method = "fdr")

# --- Verbal Tests ---
cat("Running analysis for Verbal Tests (Male)...\n")
Verbal_tests_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = Verbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Verbal_tests_results_M$P_adj_value <- p.adjust(Verbal_tests_results_M$P_value, method = "fdr")

# --- Nonverbal Tests ---
cat("Running analysis for Nonverbal Tests (Male)...\n")
Nonverbal_tests_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = Nonverbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Nonverbal_tests_results_M$P_adj_value <- p.adjust(Nonverbal_tests_results_M$P_value, method = "fdr")

# --- Anthropometrics ---
cat("Running analysis for Anthropometrics (Male)...\n")
Anthro_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = Anthro_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Anthro_results_M$P_adj_value <- p.adjust(Anthro_results_M$P_value, method = "fdr")

# --- Educational Results ---
cat("Running analysis for Educational Variables (Male)...\n")
Education_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = Edu_Achieve_Attain_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Education_results_M$P_adj_value <- p.adjust(Education_results_M$P_value, method = "fdr")

# --- Anxiety Measures ---
cat("Running analysis for Anxiety (Male)...\n")
Anxiety_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = Anxiety_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Anxiety_results_M$P_adj_value <- p.adjust(Anxiety_results_M$P_value, method = "fdr")

# --- Conners' ADHD Measures ---
cat("Running analysis for Conners' (ADHD) (Male)...\n")
Conners_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = Conners_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Conners_results_M$P_adj_value <- p.adjust(Conners_results_M$P_value, method = "fdr")

# --- SDQ Measures ---
cat("Running analysis for SDQ (Male)...\n")
SDQ_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = SDQ_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
SDQ_results_M$P_adj_value <- p.adjust(SDQ_results_M$P_value, method = "fdr")

# --- Wellbeing Measures ---
cat("Running analysis for Wellbeing (Male)...\n")
Wellbeing_results_M <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_M, var_list = Wellbeing_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = NULL, nboot = nboot, ncpus = ncpus
)
Wellbeing_results_M$P_adj_value <- p.adjust(Wellbeing_results_M$P_value, method = "fdr")

cat("Male-only analyses complete.\n")

# Combine and apply p-value asterisks
all_results_combined_M <- bind_rows(
  "CFA_Results" = CFA_composites_results_M,
  "G_Composites" = G_composites_results_M,
  "Verbal_Tests" = Verbal_tests_results_M,
  "Nonverbal_Tests" = Nonverbal_tests_results_M,
  "Anthro" = Anthro_results_M,
  "Education" = Education_results_M,
  "Anxiety" = Anxiety_results_M,
  "Conners_ADHD" = Conners_results_M,
  "SDQ" = SDQ_results_M,
  "Wellbeing" = Wellbeing_results_M,
  .id = "Domain"
)

CFA_composites_results_M <- p2asterisk(CFA_composites_results_M, "P_adj_value", "sig_adj_p")
G_composites_results_M <- p2asterisk(G_composites_results_M, "P_adj_value", "sig_adj_p")
Verbal_tests_results_M <- p2asterisk(Verbal_tests_results_M, "P_adj_value", "sig_adj_p")
Nonverbal_tests_results_M <- p2asterisk(Nonverbal_tests_results_M, "P_adj_value", "sig_adj_p")
Anthro_results_M <- p2asterisk(Anthro_results_M, "P_adj_value", "sig_adj_p")
Education_results_M <- p2asterisk(Education_results_M, "P_adj_value", "sig_adj_p")
Anxiety_results_M <- p2asterisk(Anxiety_results_M, "P_adj_value", "sig_adj_p")
Conners_results_M <- p2asterisk(Conners_results_M, "P_adj_value", "sig_adj_p")
SDQ_results_M <- p2asterisk(SDQ_results_M, "P_adj_value", "sig_adj_p")
Wellbeing_results_M <- p2asterisk(Wellbeing_results_M, "P_adj_value", "sig_adj_p")
all_results_combined_M <- p2asterisk(all_results_combined_M, "P_adj_value", "sig_adj_p")


# --- Save Male Results to Excel and CSV ---
wb_M <- createWorkbook()
addWorksheet(wb_M, sheetName="CFA_composites")
addWorksheet(wb_M, sheetName="G_composites")
addWorksheet(wb_M, sheetName="Verbal_Tests")
addWorksheet(wb_M, sheetName="Nonverbal_Tests")
addWorksheet(wb_M, sheetName="Anthro")
addWorksheet(wb_M, sheetName="Education")
addWorksheet(wb_M, sheetName="Anxiety")
addWorksheet(wb_M, sheetName="Conners_ADHD")
addWorksheet(wb_M, sheetName="SDQ")
addWorksheet(wb_M, sheetName="Wellbeing")

writeData(wb_M, sheet="CFA_composites", x=CFA_composites_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="G_composites", x=G_composites_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="Verbal_Tests", x=Verbal_tests_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="Nonverbal_Tests", x=Nonverbal_tests_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="Anthro", x=Anthro_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="Education", x=Education_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="Anxiety", x=Anxiety_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="Conners_ADHD", x=Conners_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="SDQ", x=SDQ_results_M, colNames=TRUE, rowNames=FALSE)
writeData(wb_M, sheet="Wellbeing", x=Wellbeing_results_M, colNames=TRUE, rowNames=FALSE)

saveWorkbook(wb_M, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_M.xlsx", sep=""), overwrite = TRUE)

write.csv(all_results_combined_M, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_M.csv", sep=""), row.names = FALSE)
write.csv(CFA_composites_results_M, paste(outFileStem, "CFA_composites_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(G_composites_results_M, paste(outFileStem, "G_composites_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(Verbal_tests_results_M, paste(outFileStem, "Verbal_tests_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(Nonverbal_tests_results_M, paste(outFileStem, "Nonverbal_tests_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(Anthro_results_M, paste(outFileStem, "Anthro_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(Education_results_M, paste(outFileStem, "Education_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(Anxiety_results_M, paste(outFileStem, "Anxiety_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(Conners_results_M, paste(outFileStem, "Conners_ADHD_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(SDQ_results_M, paste(outFileStem, "SDQ_adjCovar_M.csv", sep=""), row.names = FALSE)
write.csv(Wellbeing_results_M, paste(outFileStem, "Wellbeing_adjCovar_M.csv", sep=""), row.names = FALSE)










# ===================================================================
#     Run Bootstrapped Regressions for MZ TWINS ONLY ####
# ===================================================================

# subset the data to MZ twins only
dat_scaled_selectunpaired_MZ <- dat_scaled_selectunpaired %>%
  filter(zygos == 1)
dim(dat_scaled_selectunpaired_MZ) # 2658 438

# --- CFA ---
cat("Running analysis for CFA (MZ twins)...\n")
CFA_composites_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = CommonFactor_Score_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
CFA_composites_results_MZ$P_adj_value <- p.adjust(CFA_composites_results_MZ$P_value, method = "fdr")

# --- Cognitive Composites ---
cat("Running analysis for G Composites (MZ twins)...\n")
G_composites_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = G_Composites_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
G_composites_results_MZ$P_adj_value <- p.adjust(G_composites_results_MZ$P_value, method = "fdr")

# --- Verbal Tests ---
cat("Running analysis for Verbal Tests (MZ twins)...\n")
Verbal_tests_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = Verbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Verbal_tests_results_MZ$P_adj_value <- p.adjust(Verbal_tests_results_MZ$P_value, method = "fdr")

# --- Nonverbal Tests ---
cat("Running analysis for Nonverbal Tests (MZ twins)...\n")
Nonverbal_tests_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = Nonverbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Nonverbal_tests_results_MZ$P_adj_value <- p.adjust(Nonverbal_tests_results_MZ$P_value, method = "fdr")

# --- Anthropometrics ---
cat("Running analysis for Anthropometrics (MZ twins)...\n")
Anthro_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = Anthro_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Anthro_results_MZ$P_adj_value <- p.adjust(Anthro_results_MZ$P_value, method = "fdr")

# --- Educational Results ---
cat("Running analysis for Educational Variables (MZ twins)...\n")
Education_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = Edu_Achieve_Attain_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Education_results_MZ$P_adj_value <- p.adjust(Education_results_MZ$P_value, method = "fdr")

# --- Anxiety Measures ---
cat("Running analysis for Anxiety (MZ twins)...\n")
Anxiety_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = Anxiety_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Anxiety_results_MZ$P_adj_value <- p.adjust(Anxiety_results_MZ$P_value, method = "fdr")

# --- Conners' ADHD Measures ---
cat("Running analysis for Conners' (ADHD) (MZ twins)...\n")
Conners_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = Conners_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Conners_results_MZ$P_adj_value <- p.adjust(Conners_results_MZ$P_value, method = "fdr")

# --- SDQ Measures ---
cat("Running analysis for SDQ (MZ twins)...\n")
SDQ_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = SDQ_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
SDQ_results_MZ$P_adj_value <- p.adjust(SDQ_results_MZ$P_value, method = "fdr")

# --- Wellbeing Measures ---
cat("Running analysis for Wellbeing (MZ twins)...\n")
Wellbeing_results_MZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_MZ, var_list = Wellbeing_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Wellbeing_results_MZ$P_adj_value <- p.adjust(Wellbeing_results_MZ$P_value, method = "fdr")

cat("MZ twins-only analyses complete.\n")

# Combine and apply p-value asterisks
all_results_combined_MZ <- bind_rows(
  "CFA_Results" = CFA_composites_results_MZ,
  "G_Composites" = G_composites_results_MZ,
  "Verbal_Tests" = Verbal_tests_results_MZ,
  "Nonverbal_Tests" = Nonverbal_tests_results_MZ,
  "Anthro" = Anthro_results_MZ,
  "Education" = Education_results_MZ,
  "Anxiety" = Anxiety_results_MZ,
  "Conners_ADHD" = Conners_results_MZ,
  "SDQ" = SDQ_results_MZ,
  "Wellbeing" = Wellbeing_results_MZ,
  .id = "Domain"
)

CFA_composites_results_MZ <- p2asterisk(CFA_composites_results_MZ, "P_adj_value", "sig_adj_p")
G_composites_results_MZ <- p2asterisk(G_composites_results_MZ, "P_adj_value", "sig_adj_p")
Verbal_tests_results_MZ <- p2asterisk(Verbal_tests_results_MZ, "P_adj_value", "sig_adj_p")
Nonverbal_tests_results_MZ <- p2asterisk(Nonverbal_tests_results_MZ, "P_adj_value", "sig_adj_p")
Anthro_results_MZ <- p2asterisk(Anthro_results_MZ, "P_adj_value", "sig_adj_p")
Education_results_MZ <- p2asterisk(Education_results_MZ, "P_adj_value", "sig_adj_p")
Anxiety_results_MZ <- p2asterisk(Anxiety_results_MZ, "P_adj_value", "sig_adj_p")
Conners_results_MZ <- p2asterisk(Conners_results_MZ, "P_adj_value", "sig_adj_p")
SDQ_results_MZ <- p2asterisk(SDQ_results_MZ, "P_adj_value", "sig_adj_p")
Wellbeing_results_MZ <- p2asterisk(Wellbeing_results_MZ, "P_adj_value", "sig_adj_p")
all_results_combined_MZ <- p2asterisk(all_results_combined_MZ, "P_adj_value", "sig_adj_p")


# --- Save MZ twins Results to Excel and CSV ---
wb_MZ <- createWorkbook()
addWorksheet(wb_MZ, sheetName="CFA_composites")
addWorksheet(wb_MZ, sheetName="G_composites")
addWorksheet(wb_MZ, sheetName="Verbal_Tests")
addWorksheet(wb_MZ, sheetName="Nonverbal_Tests")
addWorksheet(wb_MZ, sheetName="Anthro")
addWorksheet(wb_MZ, sheetName="Education")
addWorksheet(wb_MZ, sheetName="Anxiety")
addWorksheet(wb_MZ, sheetName="Conners_ADHD")
addWorksheet(wb_MZ, sheetName="SDQ")
addWorksheet(wb_MZ, sheetName="Wellbeing")

writeData(wb_MZ, sheet="CFA_composites", x=CFA_composites_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="G_composites", x=G_composites_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="Verbal_Tests", x=Verbal_tests_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="Nonverbal_Tests", x=Nonverbal_tests_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="Anthro", x=Anthro_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="Education", x=Education_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="Anxiety", x=Anxiety_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="Conners_ADHD", x=Conners_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="SDQ", x=SDQ_results_MZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_MZ, sheet="Wellbeing", x=Wellbeing_results_MZ, colNames=TRUE, rowNames=FALSE)

saveWorkbook(wb_MZ, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_MZ.xlsx", sep=""), overwrite = TRUE)

write.csv(all_results_combined_MZ, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_MZ.csv", sep=""), row.names = FALSE)
write.csv(CFA_composites_results_MZ, paste(outFileStem, "CFA_composites_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(G_composites_results_MZ, paste(outFileStem, "G_composites_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(Verbal_tests_results_MZ, paste(outFileStem, "Verbal_tests_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(Nonverbal_tests_results_MZ, paste(outFileStem, "Nonverbal_tests_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(Anthro_results_MZ, paste(outFileStem, "Anthro_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(Education_results_MZ, paste(outFileStem, "Education_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(Anxiety_results_MZ, paste(outFileStem, "Anxiety_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(Conners_results_MZ, paste(outFileStem, "Conners_ADHD_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(SDQ_results_MZ, paste(outFileStem, "SDQ_adjCovar_MZ.csv", sep=""), row.names = FALSE)
write.csv(Wellbeing_results_MZ, paste(outFileStem, "Wellbeing_adjCovar_MZ.csv", sep=""), row.names = FALSE)










# ===================================================================
#     Run Bootstrapped Regressions for DZ TWINS ONLY ####
# ===================================================================

# subset the data to DZ twins only
dat_scaled_selectunpaired_DZ <- dat_scaled_selectunpaired %>%
  filter(zygos == 2)

# --- CFA ---
cat("Running analysis for CFA (DZ twins)...\n")
CFA_composites_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = CommonFactor_Score_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
CFA_composites_results_DZ$P_adj_value <- p.adjust(CFA_composites_results_DZ$P_value, method = "fdr")

# --- Cognitive Composites ---
cat("Running analysis for G Composites (DZ twins)...\n")
G_composites_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = G_Composites_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
G_composites_results_DZ$P_adj_value <- p.adjust(G_composites_results_DZ$P_value, method = "fdr")

# --- Verbal Tests ---
cat("Running analysis for Verbal Tests (DZ twins)...\n")
Verbal_tests_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = Verbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Verbal_tests_results_DZ$P_adj_value <- p.adjust(Verbal_tests_results_DZ$P_value, method = "fdr")

# --- Nonverbal Tests ---
cat("Running analysis for Nonverbal Tests (DZ twins)...\n")
Nonverbal_tests_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = Nonverbal_Tests_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Nonverbal_tests_results_DZ$P_adj_value <- p.adjust(Nonverbal_tests_results_DZ$P_value, method = "fdr")

# --- Anthropometrics ---
cat("Running analysis for Anthropometrics (DZ twins)...\n")
Anthro_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = Anthro_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Anthro_results_DZ$P_adj_value <- p.adjust(Anthro_results_DZ$P_value, method = "fdr")

# --- Educational Results ---
cat("Running analysis for Educational Variables (DZ twins)...\n")
Education_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = Edu_Achieve_Attain_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Education_results_DZ$P_adj_value <- p.adjust(Education_results_DZ$P_value, method = "fdr")

# --- Anxiety Measures ---
cat("Running analysis for Anxiety (DZ twins)...\n")
Anxiety_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = Anxiety_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Anxiety_results_DZ$P_adj_value <- p.adjust(Anxiety_results_DZ$P_value, method = "fdr")

# --- Conners' ADHD Measures ---
cat("Running analysis for Conners' (ADHD) (DZ twins)...\n")
Conners_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = Conners_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Conners_results_DZ$P_adj_value <- p.adjust(Conners_results_DZ$P_value, method = "fdr")

# --- SDQ Measures ---
cat("Running analysis for SDQ (DZ twins)...\n")
SDQ_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = SDQ_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
SDQ_results_DZ$P_adj_value <- p.adjust(SDQ_results_DZ$P_value, method = "fdr")

# --- Wellbeing Measures ---
cat("Running analysis for Wellbeing (DZ twins)...\n")
Wellbeing_results_DZ <- run_linear_prediction_boot(
  dat = dat_scaled_selectunpaired_DZ, var_list = Wellbeing_Varlist, predictor_var = "PGIQ",
  include_covariates = TRUE, sex_var = "sex1", nboot = nboot, ncpus = ncpus
)
Wellbeing_results_DZ$P_adj_value <- p.adjust(Wellbeing_results_DZ$P_value, method = "fdr")

cat("DZ twins-only analyses complete.\n")

# Combine and apply p-value asterisks
all_results_combined_DZ <- bind_rows(
  "CFA_Results" = CFA_composites_results_DZ,
  "G_Composites" = G_composites_results_DZ,
  "Verbal_Tests" = Verbal_tests_results_DZ,
  "Nonverbal_Tests" = Nonverbal_tests_results_DZ,
  "Anthro" = Anthro_results_DZ,
  "Education" = Education_results_DZ,
  "Anxiety" = Anxiety_results_DZ,
  "Conners_ADHD" = Conners_results_DZ,
  "SDQ" = SDQ_results_DZ,
  "Wellbeing" = Wellbeing_results_DZ,
  .id = "Domain"
)

CFA_composites_results_DZ <- p2asterisk(CFA_composites_results_DZ, "P_adj_value", "sig_adj_p")
G_composites_results_DZ <- p2asterisk(G_composites_results_DZ, "P_adj_value", "sig_adj_p")
Verbal_tests_results_DZ <- p2asterisk(Verbal_tests_results_DZ, "P_adj_value", "sig_adj_p")
Nonverbal_tests_results_DZ <- p2asterisk(Nonverbal_tests_results_DZ, "P_adj_value", "sig_adj_p")
Anthro_results_DZ <- p2asterisk(Anthro_results_DZ, "P_adj_value", "sig_adj_p")
Education_results_DZ <- p2asterisk(Education_results_DZ, "P_adj_value", "sig_adj_p")
Anxiety_results_DZ <- p2asterisk(Anxiety_results_DZ, "P_adj_value", "sig_adj_p")
Conners_results_DZ <- p2asterisk(Conners_results_DZ, "P_adj_value", "sig_adj_p")
SDQ_results_DZ <- p2asterisk(SDQ_results_DZ, "P_adj_value", "sig_adj_p")
Wellbeing_results_DZ <- p2asterisk(Wellbeing_results_DZ, "P_adj_value", "sig_adj_p")
all_results_combined_DZ <- p2asterisk(all_results_combined_DZ, "P_adj_value", "sig_adj_p")


# --- Save DZ twins Results to Excel and CSV ---
wb_DZ <- createWorkbook()
addWorksheet(wb_DZ, sheetName="CFA_composites")
addWorksheet(wb_DZ, sheetName="G_composites")
addWorksheet(wb_DZ, sheetName="Verbal_Tests")
addWorksheet(wb_DZ, sheetName="Nonverbal_Tests")
addWorksheet(wb_DZ, sheetName="Anthro")
addWorksheet(wb_DZ, sheetName="Education")
addWorksheet(wb_DZ, sheetName="Anxiety")
addWorksheet(wb_DZ, sheetName="Conners_ADHD")
addWorksheet(wb_DZ, sheetName="SDQ")
addWorksheet(wb_DZ, sheetName="Wellbeing")

writeData(wb_DZ, sheet="CFA_composites", x=CFA_composites_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="G_composites", x=G_composites_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="Verbal_Tests", x=Verbal_tests_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="Nonverbal_Tests", x=Nonverbal_tests_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="Anthro", x=Anthro_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="Education", x=Education_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="Anxiety", x=Anxiety_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="Conners_ADHD", x=Conners_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="SDQ", x=SDQ_results_DZ, colNames=TRUE, rowNames=FALSE)
writeData(wb_DZ, sheet="Wellbeing", x=Wellbeing_results_DZ, colNames=TRUE, rowNames=FALSE)

saveWorkbook(wb_DZ, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_DZ.xlsx", sep=""), overwrite = TRUE)

write.csv(all_results_combined_DZ, paste(outFileStem, "PGIQ_20boot_adjCovar_pop_DZ.csv", sep=""), row.names = FALSE)
write.csv(CFA_composites_results_DZ, paste(outFileStem, "CFA_composites_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(G_composites_results_DZ, paste(outFileStem, "G_composites_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(Verbal_tests_results_DZ, paste(outFileStem, "Verbal_tests_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(Nonverbal_tests_results_DZ, paste(outFileStem, "Nonverbal_tests_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(Anthro_results_DZ, paste(outFileStem, "Anthro_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(Education_results_DZ, paste(outFileStem, "Education_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(Anxiety_results_DZ, paste(outFileStem, "Anxiety_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(Conners_results_DZ, paste(outFileStem, "Conners_ADHD_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(SDQ_results_DZ, paste(outFileStem, "SDQ_adjCovar_DZ.csv", sep=""), row.names = FALSE)
write.csv(Wellbeing_results_DZ, paste(outFileStem, "Wellbeing_adjCovar_DZ.csv", sep=""), row.names = FALSE)











# ===================================================================
#        Plotting Function for Standardized Beta Results ####
# ===================================================================
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)

CFA_composites_results <- read.csv(paste0(outFileStem, "CFA_composites_adjCovar.csv"))
G_composites_results <- read.csv(paste0(outFileStem, "G_composites_adjCovar.csv"))
Verbal_tests_results <- read.csv(paste0(outFileStem, "Verbal_tests_adjCovar.csv"))
Nonverbal_tests_results <- read.csv(paste0(outFileStem, "Nonverbal_tests_adjCovar.csv"))
Anthro_results <- read.csv(paste0(outFileStem, "Anthro_adjCovar.csv"))
Education_results <- read.csv(paste0(outFileStem, "Education_adjCovar.csv"))
Wellbeing_results <- read.csv(paste0(outFileStem, "Wellbeing_adjCovar.csv"))

G_composites_results$Category[G_composites_results$Category == "g"] <- "general cognitive ability"
Education_results$Category[Education_results$Category == "All-Subject"] <- "Core-Subject"
Education_results$Trait_Name[Education_results$Trait_Name == "21 yr"] <- "University Grades (21 yr)"

plot_beta_results <- function(results_df, main_title, sig_col_name = "sig_adj_p", facet_cols = 2, color_palette = NULL) {
  
  # Preserve original order of categories and traits
  results_df$Category <- factor(results_df$Category, levels = unique(results_df$Category))
  
  # Apply str_wrap to trait names and preserve the order
  wrapped_trait_names <- str_wrap(results_df$Trait_Name, width = 50)
  results_df$Trait_Name_Wrapped <- factor(wrapped_trait_names, levels = rev(unique(wrapped_trait_names)))
  
  dodge_width <- 0.7
  
  # Generate the plot
  p <- ggplot(
    results_df, 
    aes(x = Beta, y = Trait_Name_Wrapped, color = Category) # , shape = Rater
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
    
    geom_errorbarh(
      aes(xmin = CI_Lower, xmax = CI_Upper), 
      height = 0.2, 
      linewidth = 0.7,
      position = position_dodge(width = dodge_width)
    ) +
    
    geom_point(
      size = 3.2, 
      stroke = 0.8,
      position = position_dodge(width = dodge_width)
    ) +
    
    geom_text(
      data = filter(results_df, .data[[sig_col_name]] != ""),
      aes(x = Beta, y = Trait_Name_Wrapped, label = .data[[sig_col_name]]), 
      color = "black", 
      size = 4,
      vjust = -0.1,
      position = position_dodge(width = dodge_width),
      show.legend = FALSE,
      fontface = "bold"
    ) +
    
    # facet_wrap(~ Category, scales = "free_y", ncol = facet_cols) +
    
    labs(
      title = main_title,
      subtitle = "Standardised Beta (β) Coefficients with 95% Confidence Intervals",
      x = "Standardised Beta (β)",
      y = "Outcome Phenotype At Each Age",
      color = "Phenotypic Category"#,
      # shape = "Rater"
    ) +
    
    theme_bw(base_size = 14) + # base_size controls the base font size
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 16),
      strip.text = element_text(face = "bold", size = 16), # size for the facet labels
      axis.text.x = element_text(size = 14), # size for the x axis
      axis.text.y = element_text(size = 14), 
      strip.background = element_rect(fill = "grey90", color = "grey90"),
      panel.grid.major.y = element_line(linetype = "dotted", color = "grey85"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Good spacing for readability
      panel.spacing.x = unit(1.8, "lines"),
      panel.spacing.y = unit(1, "lines"),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 25),
      
      # Better axis text spacing
      #axis.text.y = element_text(size = 10, margin = margin(r = 8)),
      axis.title.y = element_text(margin = margin(r = 12)),
      
      # Prevent clipping
      strip.clip = "off"
    ) +
    
    #scale_shape_manual(values = c("Parent" = 16, "Child" = 17, "Teacher" = 15, "Parent/Child" = 18)) +
    
    # Prevent clipping and add some space
    coord_cartesian(clip = "off") +
    
    # Add space between y-axis labels
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) + 
    
    if (is.null(color_palette)) {
      # Default behavior if no palette is provided
      scale_color_brewer(palette = "Set1") 
    } else {
      # Use the custom palette if one is provided
      scale_color_manual(values = color_palette)
    }
  
  return(p)
}

# for the plot in the main text, I will customise the color to be different 
library(RColorBrewer)
darker_green_palette <- brewer.pal(n = 4, name = "Greens")[-1]
print(darker_green_palette)
G_composites_plot <- plot_beta_results(results_df = G_composites_results, main_title = "Polygenic Prediction of Cognitive Abilities", facet_cols = 3,
                                       color_palette = darker_green_palette)
print(G_composites_plot)
ggsave("G_composites_prediction_plot.png", plot = G_composites_plot, width = 14, height = 8, dpi = 300)

 # for this plot, comment out the facet_wrap line in the function, but we keep the facet_cols argument for the function to run
# if necessary, modify the width value in str_wrap to avoid excessive wrapping of trait names
darker_orange_palette <- brewer.pal(n = 5, name = "Oranges")[-1]
print(darker_orange_palette)
Verbal_plot <- plot_beta_results(results_df = Verbal_tests_results, main_title = "Polygenic Prediction of Verbal Test Scores", facet_cols = 2,
                                 color_palette = darker_orange_palette)
print(Verbal_plot)
ggsave("Verbal_prediction_plot.png", plot = Verbal_plot, width = 15, height = 12, dpi = 300)

# comment out the facet_wrap as well
darker_red_palette <- brewer.pal(n = 5, name = "Reds")[-1]
print(darker_red_palette)
Nonverbal_plot <- plot_beta_results(results_df = Nonverbal_tests_results, main_title = "Polygenic Prediction of Nonverbal Test Scores", facet_cols = 2,
                                    color_palette = darker_red_palette)
print(Nonverbal_plot)
ggsave("Nonverbal_prediction_plot.png", plot = Nonverbal_plot, width = 18, height = 18, dpi = 300)

# here we comment out the color = category related arguments as well
# Split the data by category
BMI_results <- Anthro_results %>%
  filter(Category == "BMI") %>%
  mutate(
    Trait_Name = case_when(
      Trait_Name == "birth (weight)" ~ "0 yr (weight)",
      TRUE ~ Trait_Name
    )
  )

Height_results <- Anthro_results %>%
  filter(Category == "Height") %>%
  mutate(
    Trait_Name = case_when(
      Trait_Name == "birth" ~ "0 yr",
      TRUE ~ Trait_Name
    )
  )

# Define desired orders
desired_order_BMI <- c(
  "0 yr (weight)", "3 yr", "4 yr", "7 yr", "12 yr", "14 yr", "16 yr", "21 yr", "26 yr")
desired_order_Height <- c(
  "0 yr", "7 yr", "12 yr", "14 yr", "16 yr", "21 yr", "26 yr")

# Set factor levels
BMI_results$Trait_Name <- factor(BMI_results$Trait_Name, levels = desired_order_BMI)
Height_results$Trait_Name <- factor(Height_results$Trait_Name, levels = desired_order_Height)

# Create separate plots
BMI_plot <- plot_beta_results(
  results_df = BMI_results, 
  main_title = "Polygenic Prediction of BMI"
)
print(BMI_plot)
ggsave("BMI_prediction_plot.png", plot = BMI_plot, width = 8, height = 6, dpi = 300)

Height_plot <- plot_beta_results(
  results_df = Height_results, 
  main_title = "Polygenic Prediction of Height"
)
print(Height_plot)
ggsave("Height_prediction_plot.png", plot = Height_plot, width = 8, height = 6, dpi = 300)

# before we put the facet back, run this one as well
wellbeing_plot <- plot_beta_results(results_df = Wellbeing_results, main_title = "Polygenic Prediction of Other Outcomes", facet_cols = 1)
print(wellbeing_plot)
ggsave("Wellbeing_prediction_plot.png", plot = wellbeing_plot, width = 14, height = 30, dpi = 300)



# remember to uncomment the facet_wrap & re-run the function 
# we have to use facet, b/c the y-axis variable names largely overlap between categories
# this one we change the str_wrap width to 18 so there isn't a big gap between the third and fourth plots
library(RColorBrewer)
darker_blue_palette <- brewer.pal(n = 5, name = "Blues")[-1]
print(darker_blue_palette)
Education_plot <- plot_beta_results(results_df = Education_results, main_title = "Polygenic Prediction of Educational Outcomes", facet_cols = 2,
                                    color_palette = darker_blue_palette)
print(Education_plot)
ggsave("Education_prediction_plot.png", plot = Education_plot, width = 16, height = 8, dpi = 300)

# we need rater for the following three plots 
# the plotting gets a bit funny when there are multiple raters but not all of them got the significance annotation
# we slightly change the function to add ns other than the asterisks
Anxiety_results <- read.csv(paste0(outFileStem, "Anxiety_adjCovar.csv"))
Conners_results <- read.csv(paste0(outFileStem, "Conners_ADHD_adjCovar.csv"))
SDQ_results <- read.csv(paste0(outFileStem, "SDQ_adjCovar.csv"))

p2asterisk_plot_multiple_rater<-function(df, target_p_column, new_labelled_column_name){
  labels<-rep(NA, nrow(df)) 
  df<-tibble::add_column(df, labels, .after=target_p_column)
  x<-df[[target_p_column]]
  x1<-ifelse(x>=0.05, "ns",
             ifelse(x<0.05 & x>=0.01, "*",
                    ifelse(x< 0.01 & x>=0.001, "**",
                           ifelse(x<0.001 & x>=0.0001, "***",
                                  ifelse(x<0.0001, "****", NA)))))
  colnames(df)[colnames(df)=="labels"]<-new_labelled_column_name
  df[[new_labelled_column_name]]<-x1
  return(df)
}

plot_beta_results_multiRater <- function(results_df, main_title, sig_col_name = "sig_adj_p", facet_cols = 2, color_palette = NULL) {
  
  # Preserve original order of categories and traits
  results_df$Category <- factor(results_df$Category, levels = unique(results_df$Category))
  
  # Apply str_wrap to trait names and preserve the order
  wrapped_trait_names <- str_wrap(results_df$Trait_Name, width = 50)
  results_df$Trait_Name_Wrapped <- factor(wrapped_trait_names, levels = rev(unique(wrapped_trait_names)))
  
  # Define the position dodge amount
  dodge_width <- 0.9
  
  # Generate the plot
  p <- ggplot(
    results_df,
    aes(x = Beta, y = Trait_Name_Wrapped, color = Category, shape = Rater)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
    
    geom_errorbarh(
      aes(xmin = CI_Lower, xmax = CI_Upper),
      height = 0.2,
      linewidth = 0.7,
      position = position_dodge(width = dodge_width)
    ) +
    
    geom_point(
      size = 3.2,
      stroke = 0.8,
      position = position_dodge(width = dodge_width)
    ) +
    
    # It's now the *only* geom_text for significance
    geom_text(
      data = results_df,
      aes(
        x = Beta,  # <-- CHANGED: Positioned at the point (Beta)
        y = Trait_Name_Wrapped,
        label = .data[[sig_col_name]],
        group = Rater,                 # <-- Correct grouping for dodge
        alpha = .data[[sig_col_name]]  # <-- Controls visibility
      ),
      color = "black",
      size = 4,
      vjust = -0.1, # <-- CHANGED: Nudges text *up* (negative vjust)
      position = position_dodge(width = dodge_width),
      show.legend = FALSE,
      fontface = "bold"
    ) +
    
    # Manually control the transparency
    # "ns" becomes 0 (invisible), everything else becomes 1 (visible)
    scale_alpha_manual(
      values = c("ns" = 0, "*" = 1, "**" = 1, "***" = 1, "****" = 1),
      na.value = 0 # Make any other values (like NA) also invisible
    ) +
    
    facet_wrap(~ Category, scales = "free_y", ncol = facet_cols) +
    
    labs(
      title = main_title,
      subtitle = "Standardised Beta (β) Coefficients with 95% Confidence Intervals",
      x = "Standardised Beta (β)",
      y = "Outcome Phenotype At Each Age",
      color = "Phenotypic Category",
      shape = "Rater"
    ) +
    
    theme_bw(base_size = 14) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 16),
      strip.text = element_text(face = "bold", size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      strip.background = element_rect(fill = "grey90", color = "grey90"),
      panel.grid.major.y = element_line(linetype = "dotted", color = "grey85"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing.x = unit(1.8, "lines"),
      panel.spacing.y = unit(1, "lines"),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 25),
      axis.title.y = element_text(margin = margin(r = 12)),
      strip.clip = "off"
    ) +
    
    guides(
      # color = guide_legend(reverse = TRUE),
      shape = guide_legend(reverse = TRUE)
    ) +
    
    # scale_color_brewer(palette = "Set2") +
    coord_cartesian(clip = "off") +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5)))+
    if (is.null(color_palette)) {
      # Default behavior if no palette is provided
      scale_color_brewer(palette = "Set1") # Pastel1 for anx, Pastel2 for conners
    } else {
      # Use the custom palette if one is provided
      scale_color_manual(values = color_palette)
    }
  
  return(p)
}

Anxiety_results <- p2asterisk_plot_multiple_rater(Anxiety_results, "P_adj_value", "sig_adj_p_withns")
Conners_results <- p2asterisk_plot_multiple_rater(Conners_results, "P_adj_value", "sig_adj_p_withns")
SDQ_results <- p2asterisk_plot_multiple_rater(SDQ_results, "P_adj_value", "sig_adj_p_withns")

# change dodge_width <- 0.7
Anxiety_results$Category[Anxiety_results$Category == "Anxiety"] <- "Anxiety Total"
anxiety_plot <- plot_beta_results_multiRater(results_df = Anxiety_results, sig_col_name = "sig_adj_p_withns", main_title = "Polygenic Prediction of Anxiety Scores", facet_cols = 3)
print(anxiety_plot)
ggsave("Anxiety_prediction_plot.png", plot = anxiety_plot, width = 14, height = 12, dpi = 300)

# change dodge_width <- 0.7
Conners_results$Category[Conners_results$Category == "ADHD"] <- "ADHD Total"
ADHD_plot <- plot_beta_results_multiRater(results_df = Conners_results, sig_col_name = "sig_adj_p_withns", main_title = "Polygenic Prediction of ADHD Scores", facet_cols = 3)
print(ADHD_plot)
ggsave("ADHD_prediction_plot.png", plot = ADHD_plot, width = 14, height = 12, dpi = 300)

# change dodge_width <- 0.9
library(RColorBrewer)
darker_purple_palette <- brewer.pal(n = 7, name = "Purples")[-1]
print(darker_purple_palette)
sdq_plot <- plot_beta_results_multiRater(results_df = SDQ_results, sig_col_name = "sig_adj_p_withns", main_title = "Polygenic Prediction of SDQ Scores", facet_cols = 3,
                                         color_palette = darker_purple_palette)
print(sdq_plot)
ggsave("SDQ_prediction_plot.png", plot = sdq_plot, width = 16, height = 18, dpi = 300)



plot_beta_results_multiRater <- function(results_df, main_title, sig_col_name = "sig_adj_p", facet_cols = 2) {
  
  # Preserve original order of categories and traits
  results_df$Category <- factor(results_df$Category, levels = unique(results_df$Category))
  
  # Apply str_wrap to trait names and preserve the order
  wrapped_trait_names <- str_wrap(results_df$Trait_Name, width = 50)
  results_df$Trait_Name_Wrapped <- factor(wrapped_trait_names, levels = rev(unique(wrapped_trait_names)))
  
  # Define the position dodge amount
  dodge_width <- 0.7
  
  # Separate significant and non-significant results
  sig_results <- filter(results_df, .data[[sig_col_name]] != "" & .data[[sig_col_name]] != "ns")
  ns_results <- filter(results_df, .data[[sig_col_name]] == "ns")
  
  # Generate the plot
  p <- ggplot(
    results_df, 
    aes(x = Beta, y = Trait_Name_Wrapped, color = Category, shape = Rater)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
    
    geom_errorbarh(
      aes(xmin = CI_Lower, xmax = CI_Upper), 
      height = 0.2, 
      linewidth = 0.7,
      position = position_dodge(width = dodge_width)
    ) +
    
    geom_point(
      size = 3.2, 
      stroke = 0.8,
      position = position_dodge(width = dodge_width)
    ) +
    
    geom_text(
      data = results_df, 
      aes(
        x = CI_Upper, 
        y = Trait_Name_Wrapped, 
        label = .data[[sig_col_name]], 
        group = Rater,             
        alpha = .data[[sig_col_name]]
      ), 
      color = "black",   
      size = 4,
      hjust = -0.3,
      position = position_dodge(width = dodge_width),
      show.legend = FALSE,
      fontface = "bold"
    ) +
    
    # Manually control the transparency
    # "ns" becomes 0 (invisible), everything else becomes 1 (visible)
    scale_alpha_manual(
      values = c("ns" = 0, "*" = 1, "**" = 1, "***" = 1, "****" = 1),
      na.value = 0 # Make any other values (like NA) also invisible
    ) +
    
    facet_wrap(~ Category, scales = "free_y", ncol = facet_cols) +
    
    labs(
      title = main_title,
      subtitle = "Standardised Beta (β) Coefficients with 95% Confidence Intervals",
      x = "Standardised Beta (β)",
      y = "Outcome Phenotype At Each Age",
      color = "Phenotypic Category",
      shape = "Rater"
    ) +
    
    theme_bw(base_size = 14) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 16),
      strip.text = element_text(face = "bold", size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14), 
      strip.background = element_rect(fill = "grey90", color = "grey90"),
      panel.grid.major.y = element_line(linetype = "dotted", color = "grey85"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing.x = unit(1.8, "lines"),
      panel.spacing.y = unit(1, "lines"),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 25),
      axis.title.y = element_text(margin = margin(r = 12)),
      strip.clip = "off"
    ) +
    
    scale_color_brewer(palette = "Set2") +
    coord_cartesian(clip = "off") +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5)))
  
  return(p)
}







# for CFA, we don't need facet, but b/c I set the category to be different for each composite, we write codes specifically for this one
CFA_composites_results <- read.csv(paste0(outFileStem, "CFA_composites_adjCovar.csv"))

CFA_composites_results <- p2asterisk_plot_multiple_rater(CFA_composites_results, "P_adj_value", "sig_adj_p_withns")

# We want the y-axis (Category) to appear in the same order as the file
# We use rev() because ggplot plots from bottom to top
CFA_composites_results$Category_Plot_Order <- factor(CFA_composites_results$Category, levels = rev(unique(CFA_composites_results$Category)))

# Define the position dodge amount
dodge_width <- 0.85

# --- 3. Create the Plot ---

cfa_plot <- ggplot(
  CFA_composites_results, 
  # Y-axis is Category_Plot_Order, X-axis is Beta
  # We dodge, color, and shape by Rater
  aes(x = Beta, y = Category_Plot_Order, shape = Rater)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  
  # Add the error bars
  geom_errorbarh(
    aes(xmin = CI_Lower, xmax = CI_Upper), 
    height = 0.2, 
    linewidth = 0.7,
    position = position_dodge(width = dodge_width),
    color = "steelblue"
  ) +
  
  # Add the points
  geom_point(
    size = 3.2, 
    stroke = 0.8,
    position = position_dodge(width = dodge_width),
    color = "steelblue"
  ) +
  
  # Add significance labels (asterisks-on-top version)
  # This uses the single-layer alpha trick
  geom_text(
    data = CFA_composites_results, 
    aes(
      x = Beta,  # Positioned at the point's Beta
      y = Category_Plot_Order, 
      label = sig_adj_p_withns, # The significance column
      group = Rater,     # This is the key for correct dodging
      alpha = sig_adj_p_withns  # This makes non-significant ones invisible
    ), 
    color = "black",   # All labels are black (some just become invisible)
    size = 4,
    vjust = -0.1, # Nudges text UP
    position = position_dodge(width = dodge_width),
    show.legend = FALSE,
    fontface = "bold"
  ) +
  
  # Manually control the transparency
  # "ns" and "" (empty string) become 0 (invisible)
  # Everything else becomes 1 (visible)
  scale_alpha_manual(
    values = c("ns" = 0, "*" = 1, "**" = 1, "***" = 1, "****" = 1),
    na.value = 0 # Make any other values (like NA) also invisible
  ) +
  
  # --- 4. Labels and Theme ---
  
  labs(
    title = "Polygenic Prediction of Common Latent Factor Across Time Composites",
    subtitle = "Standardised Beta (β) Coefficients with 95% Confidence Intervals",
    x = "Standardised Beta (β)",
    y = "Phenotypic Category", # Y-axis is now Category
    # color = "Rater",
    shape = "Rater"
  ) +
  
  # Use the same theme settings from your function
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 16), # Not used (no facets) but good to keep
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14), 
    strip.background = element_rect(fill = "grey90", color = "grey90"),
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey85"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(1.8, "lines"),
    panel.spacing.y = unit(1, "lines"),
    plot.margin = margin(t = 15, r = 15, b = 10, l = 25),
    axis.title.y = element_text(margin = margin(r = 12)),
    strip.clip = "off"
  ) +
  
  guides(
    #color = guide_legend(reverse = TRUE),
    shape = guide_legend(reverse = TRUE)
  ) +
  
  # Using Set2 palette from your function
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(clip = "off") +
  
  # Add space between y-axis labels
  scale_y_discrete(expand = expansion(add = c(0.5, 0.5)))

# --- 5. Save the Plot ---

# Print the plot to the viewer
print(cfa_plot)

ggsave("CFA_composites_prediction_plot.png", plot = cfa_plot, width = 12, height = 18, dpi = 300)
