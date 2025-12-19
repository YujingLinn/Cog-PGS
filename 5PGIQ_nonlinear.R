# polygenic IQ population prediction ####
# 10 Oct 2025
# Yujing Lin

# 1. nonlinearity test
# 2. t-test between top and lowest deciles 

library(boot)
library(dplyr)
library(openxlsx)

define_nboot <- 10  # Number of bootstrap iterations
define_ncpus <- 4     # Number of CPU cores for parallel processing

source("/Users/yujinglin/Desktop/PGIQ Codes/0PGIQ_VarList.R") # obtain the var lists
# source("/scratch/users/k21170717/polygenic_IQ_score/0PGIQ_VarList.R")
sourceFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Data220925/'
# sourceFileStem <- '/scratch/users/k21170717/polygenic_IQ_score/Data220925/'
outFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Results220925/Nonlinearity_All/'
outFileStem_F <- '/Users/yujinglin/Desktop/polygenic IQ score/Results220925/Nonlinearity_F/'
outFileStem_M <- '/Users/yujinglin/Desktop/polygenic IQ score/Results220925/Nonlinearity_M/'
# outFileStem <- '/scratch/users/k21170717/polygenic_IQ_score/Nonlinear_Results151025/All/'
# outFileStem_F <- '/scratch/users/k21170717/polygenic_IQ_score/Nonlinear_Results151025/Female/'
# outFileStem_M <- '/scratch/users/k21170717/polygenic_IQ_score/Nonlinear_Results151025/Male/'

dat_scaled_selectunpaired <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired.csv"))
dat_scaled_selectunpaired_F <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_F.csv"))
dat_scaled_selectunpaired_M <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_M.csv"))

# this list is not in Varlist.R, just define here also useful for testing
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

# ===================================================================
##          Helper Function: Convert p-values to asterisks      ####  
# ===================================================================

p2asterisk <- function(df, p_col, new_col) {
  df[[new_col]] <- cut(df[[p_col]], 
                       breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                       labels = c("***", "**", "*", "ns"))
  return(df)
}

# ===================================================================
##          Nonlinearity Tests with Quadratic Term              ####     
# ===================================================================

# Function to fit model with quadratic term and bootstrapping
fit_quadratic_model_boot <- function(dat, indices, y, x, covar) {
  datx <- dat[indices, ] # Create the bootstrap sample
  
  # Create quadratic term
  x_squared <- paste0(x, "_squared")
  datx[[x_squared]] <- datx[[x]]^2
  
  # Define model formulas
  # Full model with linear and quadratic terms
  formula_full <- reformulate(c(x, x_squared, covar), response = y)
  # Reduced model with only linear term and covariates
  formula_reduced <- reformulate(c(x, covar), response = y)
  
  tryCatch({
    # Fit the full and reduced models
    fit_model_full <- lm(formula_full, data = datx)
    fit_model_reduced <- lm(formula_reduced, data = datx)
    
    model_summary <- summary(fit_model_full)
    
    # Extract coefficient and p-value for linear term
    if (x %in% rownames(model_summary$coefficients)) {
      beta_linear <- model_summary$coefficients[x, "Estimate"]
      p_value_linear <- model_summary$coefficients[x, "Pr(>|t|)"]
    } else {
      beta_linear <- NA
      p_value_linear <- NA
    }
    
    # Extract coefficient and p-value for quadratic term
    if (x_squared %in% rownames(model_summary$coefficients)) {
      beta_quadratic <- model_summary$coefficients[x_squared, "Estimate"]
      p_value_quadratic <- model_summary$coefficients[x_squared, "Pr(>|t|)"]
    } else {
      beta_quadratic <- NA
      p_value_quadratic <- NA
    }
    
    # Calculate R-squared values
    R.squared_full <- model_summary$r.squared
    R.squared_reduced <- summary(fit_model_reduced)$r.squared
    R.squared_quadratic <- R.squared_full - R.squared_reduced
    
    return(c(beta_linear, p_value_linear, beta_quadratic, p_value_quadratic,
             R.squared_full, R.squared_quadratic))
    
  }, error = function(e) {
    return(c(NA, NA, NA, NA, NA, NA))
  })
}

# Function to run nonlinearity analysis with bootstrapping
run_nonlinearity_test_boot <- function(dat = dat_scaled_selectunpaired, var_list, predictor_var = "PGIQ", 
                                       include_covariates = TRUE, sex_var = "sex1", nboot = define_nboot, ncpus = define_ncpus) {
  
  # Main Loop: Iterate through each outcome variable and run bootstrapping
  boot_results_list <- lapply(var_list, function(var_item) {
    
    # Extract details from the var_list item
    outcome_stem <- var_item[1]
    age_variable <- var_item[3]
    
    # Construct the full outcome variable name
    y <- paste0(outcome_stem, "1")
    x <- predictor_var
    
    cat("Processing nonlinearity test for outcome:", y, "with predictor:", x, "\n")
    
    # Define the set of covariates
    covariates <- NULL
    if (include_covariates) {
      base_covariates <- c(age_variable, "chiptype")
      # conditionally include sex covariate only if it has variance
      if (sex_var %in% names(dat) && length(unique(dat[[sex_var]])) > 1) {
        base_covariates <- c(base_covariates, sex_var)
      }
      pc_covariates <- paste0("PC", 1:10)
      covariates <- c(base_covariates, pc_covariates)
      covariates <- covariates[!sapply(covariates, is.null)]
      covariates <- covariates[covariates %in% names(dat)]
    }
    
    tryCatch({
      boot_results <- boot(
        data = dat,
        statistic = fit_quadratic_model_boot,
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
      return(NULL)
    })
  })
  
  # Process and summarize the bootstrap results
  results_df <- do.call(rbind, lapply(1:length(boot_results_list), function(i) {
    
    boot_output <- boot_results_list[[i]]
    var_item <- var_list[[i]]
    
    # Default structure for failed runs
    na_result <- data.frame(
      Outcome = var_item[1],
      Trait_Name = var_item[2],
      Rater = var_item[4],
      Category = var_item[5],
      Predictor = predictor_var,
      Beta_Linear = NA,
      SE_Linear_boot = NA,
      CI_Linear_Lower = NA,
      CI_Linear_Upper = NA,
      P_value_Linear = NA,
      Beta_Quadratic = NA,
      SE_Quadratic_boot = NA,
      CI_Quadratic_Lower = NA,
      CI_Quadratic_Upper = NA,
      P_value_Quadratic = NA,
      R2_Full_Model = NA,
      R2_Quadratic = NA
    )
    
    # Check for failure or insufficient valid bootstrap replicates
    if (is.null(boot_output) || sum(!is.na(boot_output$t[, 1])) < 10) {
      if(!is.null(boot_output)){
        cat("WARNING: Too few valid bootstraps for '", var_item[1], "'\n")
      }
      return(na_result)
    }
    
    # Original estimates from the full dataset
    original_estimates <- boot_output$t0
    
    # Bootstrap replicates for linear term
    boot_betas_linear <- boot_output$t[, 1]
    boot_betas_linear <- boot_betas_linear[!is.na(boot_betas_linear)]
    
    # Bootstrap replicates for quadratic term
    boot_betas_quadratic <- boot_output$t[, 3]
    boot_betas_quadratic <- boot_betas_quadratic[!is.na(boot_betas_quadratic)]
    
    # Calculate bootstrap standard errors and confidence intervals
    se_linear_boot <- sd(boot_betas_linear)
    ci_linear_boot <- quantile(boot_betas_linear, probs = c(0.025, 0.975), na.rm = TRUE)
    
    se_quadratic_boot <- sd(boot_betas_quadratic)
    ci_quadratic_boot <- quantile(boot_betas_quadratic, probs = c(0.025, 0.975), na.rm = TRUE)
    
    # Compile the final results
    result <- data.frame(
      Outcome = var_item[1],
      Trait_Name = var_item[2],
      Rater = var_item[4],
      Category = var_item[5],
      Predictor = predictor_var,
      Beta_Linear = original_estimates[1],
      SE_Linear_boot = se_linear_boot,
      CI_Linear_Lower = ci_linear_boot[1],
      CI_Linear_Upper = ci_linear_boot[2],
      P_value_Linear = original_estimates[2],
      Beta_Quadratic = original_estimates[3],
      SE_Quadratic_boot = se_quadratic_boot,
      CI_Quadratic_Lower = ci_quadratic_boot[1],
      CI_Quadratic_Upper = ci_quadratic_boot[2],
      P_value_Quadratic = original_estimates[4],
      R2_Full_Model = original_estimates[5],
      R2_Quadratic = original_estimates[6]
    )
    
    return(result)
  }))
  
  return(results_df)
}

# ===================================================================
##          Decile Comparison (Top vs Bottom 10%)               ####     
# ===================================================================

# Function to perform t-tests comparing top and bottom deciles
run_decile_comparisons <- function(dat = dat_scaled_selectunpaired, var_list, predictor_var = "PGIQ") {
  # require ntile from dplyr
  if (!predictor_var %in% names(dat)) stop("predictor_var not found in dat")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install the 'dplyr' package.")
  dat$decile <- dplyr::ntile(dat[[predictor_var]], 10)
  
  dat_top <- dat[dat$decile == 10, ]
  dat_bottom <- dat[dat$decile == 1, ]
  
  decile_results <- do.call(
    rbind,
    lapply(var_list, function(var_item) {
      outcome_stem <- var_item[1]
      y <- paste0(outcome_stem, "1")
      
      cat("Running decile comparison for:", y, "\n")
      
      tryCatch({
        top_values <- dat_top[[y]]
        bottom_values <- dat_bottom[[y]]
        
        top_values <- top_values[!is.na(top_values)]
        bottom_values <- bottom_values[!is.na(bottom_values)]
        
        if (length(top_values) < 2 || length(bottom_values) < 2) {
          return(data.frame(
            Outcome = var_item[1],
            Trait_Name = ifelse(length(var_item) >= 2, var_item[2], NA),
            Rater = ifelse(length(var_item) >= 4, var_item[4], NA),
            Category = ifelse(length(var_item) >= 5, var_item[5], NA),
            Mean_Top_Decile = NA_real_,
            Mean_Bottom_Decile = NA_real_,
            Mean_Difference = NA_real_,
            T_Statistic = NA_real_,
            P_value = NA_real_,
            N_Top = length(top_values),
            N_Bottom = length(bottom_values),
            stringsAsFactors = FALSE
          ))
        }
        
        t_test_result <- t.test(top_values, bottom_values)
        
        data.frame(
          Outcome = var_item[1],
          Trait_Name = ifelse(length(var_item) >= 2, var_item[2], NA),
          Rater = ifelse(length(var_item) >= 4, var_item[4], NA),
          Category = ifelse(length(var_item) >= 5, var_item[5], NA),
          Mean_Top_Decile = mean(top_values, na.rm = TRUE),
          Mean_Bottom_Decile = mean(bottom_values, na.rm = TRUE),
          Mean_Difference = mean(top_values, na.rm = TRUE) - mean(bottom_values, na.rm = TRUE),
          T_Statistic = as.numeric(t_test_result$statistic),
          P_value = t_test_result$p.value,
          N_Top = length(top_values),
          N_Bottom = length(bottom_values),
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {
        cat("ERROR during t-test for outcome '", y, "':", e$message, "\n")
        data.frame(
          Outcome = var_item[1],
          Trait_Name = ifelse(length(var_item) >= 2, var_item[2], NA),
          Rater = ifelse(length(var_item) >= 4, var_item[4], NA),
          Category = ifelse(length(var_item) >= 5, var_item[5], NA),
          Mean_Top_Decile = NA_real_,
          Mean_Bottom_Decile = NA_real_,
          Mean_Difference = NA_real_,
          T_Statistic = NA_real_,
          P_value = NA_real_,
          N_Top = NA_integer_,
          N_Bottom = NA_integer_,
          stringsAsFactors = FALSE
        )
      }) # end tryCatch
    }) # end lapply
  ) # end do.call
  
  return(decile_results)
}


# ===================================================================
##          Run Nonlinearity Tests and Decile Comparisons        ####   
# ===================================================================

cat("\n=== Running analyses for CFA (Common Factor Analysis) ===\n")

# Run nonlinearity tests for CFA
cat("Running nonlinearity tests for CFA...\n")
CFA_nonlinearity_results <- run_nonlinearity_test_boot(var_list = CommonFactor_Score_Varlist)
CFA_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(CFA_nonlinearity_results$P_value_Quadratic, method = "fdr")
CFA_nonlinearity_results <- p2asterisk(CFA_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

# Run decile comparisons for CFA
cat("Running decile comparisons for CFA...\n")
CFA_decile_results <- run_decile_comparisons(var_list = CommonFactor_Score_Varlist)
CFA_decile_results$P_adj_value <- p.adjust(CFA_decile_results$P_value, method = "fdr")
CFA_decile_results <- p2asterisk(CFA_decile_results, "P_adj_value", "sig_adj_p")

# print results 
print(CFA_nonlinearity_results)
print(CFA_decile_results)

# Save results
write.csv(CFA_nonlinearity_results, paste0(outFileStem, "CFA_nonlinearity_results.csv"), row.names = FALSE)
write.csv(CFA_decile_results, paste0(outFileStem, "CFA_decile_comparison_results.csv"), row.names = FALSE)



# G Composites
cat("\n=== Running analyses for G Composites ===\n")
cat("Running nonlinearity tests for G Composites...\n")
G_Composites_nonlinearity_results <- run_nonlinearity_test_boot(var_list = G_Composites_Varlist)
G_Composites_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(G_Composites_nonlinearity_results$P_value_Quadratic, method = "fdr")
G_Composites_nonlinearity_results <- p2asterisk(G_Composites_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for G Composites...\n")
G_Composites_decile_results <- run_decile_comparisons(var_list = G_Composites_Varlist)
G_Composites_decile_results$P_adj_value <- p.adjust(G_Composites_decile_results$P_value, method = "fdr")
G_Composites_decile_results <- p2asterisk(G_Composites_decile_results, "P_adj_value", "sig_adj_p")

print(G_Composites_nonlinearity_results)
print(G_Composites_decile_results)

write.csv(G_Composites_nonlinearity_results, paste0(outFileStem, "G_Composites_nonlinearity_results.csv"), row.names = FALSE)
write.csv(G_Composites_decile_results, paste0(outFileStem, "G_Composites_decile_comparison_results.csv"), row.names = FALSE)



# Verbal Tests
cat("\n=== Running analyses for Verbal Tests ===\n")
cat("Running nonlinearity tests for Verbal Tests...\n")
Verbal_Tests_nonlinearity_results <- run_nonlinearity_test_boot(var_list = Verbal_Tests_Varlist)
Verbal_Tests_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(Verbal_Tests_nonlinearity_results$P_value_Quadratic, method = "fdr")
Verbal_Tests_nonlinearity_results <- p2asterisk(Verbal_Tests_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Verbal Tests...\n")
Verbal_Tests_decile_results <- run_decile_comparisons(var_list = Verbal_Tests_Varlist)
Verbal_Tests_decile_results$P_adj_value <- p.adjust(Verbal_Tests_decile_results$P_value, method = "fdr")
Verbal_Tests_decile_results <- p2asterisk(Verbal_Tests_decile_results, "P_adj_value", "sig_adj_p")

print(Verbal_Tests_nonlinearity_results)
print(Verbal_Tests_decile_results)

write.csv(Verbal_Tests_nonlinearity_results, paste0(outFileStem, "Verbal_Tests_nonlinearity_results.csv"), row.names = FALSE)
write.csv(Verbal_Tests_decile_results, paste0(outFileStem, "Verbal_Tests_decile_comparison_results.csv"), row.names = FALSE)



# Nonverbal Tests
cat("\n=== Running analyses for Nonverbal Tests ===\n")
cat("Running nonlinearity tests for Nonverbal Tests...\n")
Nonverbal_Tests_nonlinearity_results <- run_nonlinearity_test_boot(var_list = Nonverbal_Tests_Varlist)
Nonverbal_Tests_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(Nonverbal_Tests_nonlinearity_results$P_value_Quadratic, method = "fdr")
Nonverbal_Tests_nonlinearity_results <- p2asterisk(Nonverbal_Tests_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Nonverbal Tests...\n")
Nonverbal_Tests_decile_results <- run_decile_comparisons(var_list = Nonverbal_Tests_Varlist)
Nonverbal_Tests_decile_results$P_adj_value <- p.adjust(Nonverbal_Tests_decile_results$P_value, method = "fdr")
Nonverbal_Tests_decile_results <- p2asterisk(Nonverbal_Tests_decile_results, "P_adj_value", "sig_adj_p")

print(Nonverbal_Tests_nonlinearity_results)
print(Nonverbal_Tests_decile_results)

write.csv(Nonverbal_Tests_nonlinearity_results, paste0(outFileStem, "Nonverbal_Tests_nonlinearity_results.csv"), row.names = FALSE)
write.csv(Nonverbal_Tests_decile_results, paste0(outFileStem, "Nonverbal_Tests_decile_comparison_results.csv"), row.names = FALSE)



# Education Achievement Attainment
cat("\n=== Running analyses for Education Achievement Attainment ===\n")
cat("Running nonlinearity tests for Education Achievement Attainment...\n")
Edu_Achieve_Attain_nonlinearity_results <- run_nonlinearity_test_boot(var_list = Edu_Achieve_Attain_Varlist)
Edu_Achieve_Attain_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(Edu_Achieve_Attain_nonlinearity_results$P_value_Quadratic, method = "fdr")
Edu_Achieve_Attain_nonlinearity_results <- p2asterisk(Edu_Achieve_Attain_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Education Achievement Attainment...\n")
Edu_Achieve_Attain_decile_results <- run_decile_comparisons(var_list = Edu_Achieve_Attain_Varlist)
Edu_Achieve_Attain_decile_results$P_adj_value <- p.adjust(Edu_Achieve_Attain_decile_results$P_value, method = "fdr")
Edu_Achieve_Attain_decile_results <- p2asterisk(Edu_Achieve_Attain_decile_results, "P_adj_value", "sig_adj_p")

print(Edu_Achieve_Attain_nonlinearity_results)
print(Edu_Achieve_Attain_decile_results)

write.csv(Edu_Achieve_Attain_nonlinearity_results, paste0(outFileStem, "Edu_Achieve_Attain_nonlinearity_results.csv"), row.names = FALSE)
write.csv(Edu_Achieve_Attain_decile_results, paste0(outFileStem, "Edu_Achieve_Attain_decile_comparison_results.csv"), row.names = FALSE)



# Anxiety
cat("\n=== Running analyses for Anxiety ===\n")
cat("Running nonlinearity tests for Anxiety...\n")
Anxiety_nonlinearity_results <- run_nonlinearity_test_boot(var_list = Anxiety_Varlist)
Anxiety_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(Anxiety_nonlinearity_results$P_value_Quadratic, method = "fdr")
Anxiety_nonlinearity_results <- p2asterisk(Anxiety_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Anxiety...\n")
Anxiety_decile_results <- run_decile_comparisons(var_list = Anxiety_Varlist)
Anxiety_decile_results$P_adj_value <- p.adjust(Anxiety_decile_results$P_value, method = "fdr")
Anxiety_decile_results <- p2asterisk(Anxiety_decile_results, "P_adj_value", "sig_adj_p")

print(Anxiety_nonlinearity_results)
print(Anxiety_decile_results)

write.csv(Anxiety_nonlinearity_results, paste0(outFileStem, "Anxiety_nonlinearity_results.csv"), row.names = FALSE)
write.csv(Anxiety_decile_results, paste0(outFileStem, "Anxiety_decile_comparison_results.csv"), row.names = FALSE)



# Conners
cat("\n=== Running analyses for Conners ===\n")
cat("Running nonlinearity tests for Conners...\n")
Conners_nonlinearity_results <- run_nonlinearity_test_boot(var_list = Conners_Varlist)
Conners_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(Conners_nonlinearity_results$P_value_Quadratic, method = "fdr")
Conners_nonlinearity_results <- p2asterisk(Conners_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Conners...\n")
Conners_decile_results <- run_decile_comparisons(var_list = Conners_Varlist)
Conners_decile_results$P_adj_value <- p.adjust(Conners_decile_results$P_value, method = "fdr")
Conners_decile_results <- p2asterisk(Conners_decile_results, "P_adj_value", "sig_adj_p")

print(Conners_nonlinearity_results)
print(Conners_decile_results)

write.csv(Conners_nonlinearity_results, paste0(outFileStem, "Conners_nonlinearity_results.csv"), row.names = FALSE)
write.csv(Conners_decile_results, paste0(outFileStem, "Conners_decile_comparison_results.csv"), row.names = FALSE)



# SDQ
cat("\n=== Running analyses for SDQ ===\n")
cat("Running nonlinearity tests for SDQ...\n")
SDQ_nonlinearity_results <- run_nonlinearity_test_boot(var_list = SDQ_Varlist)
SDQ_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(SDQ_nonlinearity_results$P_value_Quadratic, method = "fdr")
SDQ_nonlinearity_results <- p2asterisk(SDQ_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for SDQ...\n")
SDQ_decile_results <- run_decile_comparisons(var_list = SDQ_Varlist)
SDQ_decile_results$P_adj_value <- p.adjust(SDQ_decile_results$P_value, method = "fdr")
SDQ_decile_results <- p2asterisk(SDQ_decile_results, "P_adj_value", "sig_adj_p")

print(SDQ_nonlinearity_results)
print(SDQ_decile_results)

write.csv(SDQ_nonlinearity_results, paste0(outFileStem, "SDQ_nonlinearity_results.csv"), row.names = FALSE)
write.csv(SDQ_decile_results, paste0(outFileStem, "SDQ_decile_comparison_results.csv"), row.names = FALSE)



# Anthropometric
cat("\n=== Running analyses for Anthropometric ===\n")
cat("Running nonlinearity tests for Anthropometric...\n")
Anthro_nonlinearity_results <- run_nonlinearity_test_boot(var_list = Anthro_Varlist)
Anthro_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(Anthro_nonlinearity_results$P_value_Quadratic, method = "fdr")
Anthro_nonlinearity_results <- p2asterisk(Anthro_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Anthropometric...\n")
Anthro_decile_results <- run_decile_comparisons(var_list = Anthro_Varlist)
Anthro_decile_results$P_adj_value <- p.adjust(Anthro_decile_results$P_value, method = "fdr")
Anthro_decile_results <- p2asterisk(Anthro_decile_results, "P_adj_value", "sig_adj_p")

print(Anthro_nonlinearity_results)
print(Anthro_decile_results)

write.csv(Anthro_nonlinearity_results, paste0(outFileStem, "Anthro_nonlinearity_results.csv"), row.names = FALSE)
write.csv(Anthro_decile_results, paste0(outFileStem, "Anthro_decile_comparison_results.csv"), row.names = FALSE)



# Wellbeing
cat("\n=== Running analyses for Wellbeing ===\n")
cat("Running nonlinearity tests for Wellbeing...\n")
Wellbeing_nonlinearity_results <- run_nonlinearity_test_boot(var_list = Wellbeing_Varlist)
Wellbeing_nonlinearity_results$P_adj_value_Quadratic <- p.adjust(Wellbeing_nonlinearity_results$P_value_Quadratic, method = "fdr")
Wellbeing_nonlinearity_results <- p2asterisk(Wellbeing_nonlinearity_results, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Wellbeing...\n")
Wellbeing_decile_results <- run_decile_comparisons(var_list = Wellbeing_Varlist)
Wellbeing_decile_results$P_adj_value <- p.adjust(Wellbeing_decile_results$P_value, method = "fdr")
Wellbeing_decile_results <- p2asterisk(Wellbeing_decile_results, "P_adj_value", "sig_adj_p")

print(Wellbeing_nonlinearity_results)
print(Wellbeing_decile_results)

write.csv(Wellbeing_nonlinearity_results, paste0(outFileStem, "Wellbeing_nonlinearity_results.csv"), row.names = FALSE)
write.csv(Wellbeing_decile_results, paste0(outFileStem, "Wellbeing_decile_comparison_results.csv"), row.names = FALSE)


wb_nonlinearity <- createWorkbook()
addWorksheet(wb_nonlinearity, "CFA")
writeData(wb_nonlinearity, "CFA", CFA_nonlinearity_results)
addWorksheet(wb_nonlinearity, "G_Composites")
writeData(wb_nonlinearity, "G_Composites", G_Composites_nonlinearity_results)
addWorksheet(wb_nonlinearity, "Verbal_Tests")
writeData(wb_nonlinearity, "Verbal_Tests", Verbal_Tests_nonlinearity_results)
addWorksheet(wb_nonlinearity, "Nonverbal_Tests")
writeData(wb_nonlinearity, "Nonverbal_Tests", Nonverbal_Tests_nonlinearity_results)
addWorksheet(wb_nonlinearity, "Edu_Achieve_Attain")
writeData(wb_nonlinearity, "Edu_Achieve_Attain", Edu_Achieve_Attain_nonlinearity_results)
addWorksheet(wb_nonlinearity, "Anxiety")
writeData(wb_nonlinearity, "Anxiety", Anxiety_nonlinearity_results)
addWorksheet(wb_nonlinearity, "Conners")
writeData(wb_nonlinearity, "Conners", Conners_nonlinearity_results)
addWorksheet(wb_nonlinearity, "SDQ")
writeData(wb_nonlinearity, "SDQ", SDQ_nonlinearity_results)
addWorksheet(wb_nonlinearity, "Anthro")
writeData(wb_nonlinearity, "Anthro", Anthro_nonlinearity_results)
addWorksheet(wb_nonlinearity, "Wellbeing")
writeData(wb_nonlinearity, "Wellbeing", Wellbeing_nonlinearity_results)
saveWorkbook(wb_nonlinearity, paste0(outFileStem, "All_Nonlinearity_Results.xlsx"), overwrite = TRUE)



wb_decile <- createWorkbook()
addWorksheet(wb_decile, "CFA")
writeData(wb_decile, "CFA", CFA_decile_results)
addWorksheet(wb_decile, "G_Composites")
writeData(wb_decile, "G_Composites", G_Composites_decile_results)
addWorksheet(wb_decile, "Verbal_Tests")
writeData(wb_decile, "Verbal_Tests", Verbal_Tests_decile_results)
addWorksheet(wb_decile, "Nonverbal_Tests")
writeData(wb_decile, "Nonverbal_Tests", Nonverbal_Tests_decile_results)
addWorksheet(wb_decile, "Edu_Achieve_Attain")
writeData(wb_decile, "Edu_Achieve_Attain", Edu_Achieve_Attain_decile_results)
addWorksheet(wb_decile, "Anxiety")
writeData(wb_decile, "Anxiety", Anxiety_decile_results)
addWorksheet(wb_decile, "Conners")
writeData(wb_decile, "Conners", Conners_decile_results)
addWorksheet(wb_decile, "SDQ")
writeData(wb_decile, "SDQ", SDQ_decile_results)
addWorksheet(wb_decile, "Anthro")
writeData(wb_decile, "Anthro", Anthro_decile_results)
addWorksheet(wb_decile, "Wellbeing")
writeData(wb_decile, "Wellbeing", Wellbeing_decile_results)
saveWorkbook(wb_decile, paste0(outFileStem, "All_Decile_Comparison_Results.xlsx"), overwrite = TRUE)

cat("\n=== All analyses complete! ===\n")
cat("Results saved to:\n")
cat(paste0(outFileStem, "All_Nonlinearity_Results.xlsx\n"))
cat(paste0(outFileStem, "All_Decile_Comparison_Results.xlsx\n"))

cat("\n=== Whole sample analysis complete! Results saved to:", outFileStem, "===\n")










# ===================================================================
##          FEMALE SUBPOPULATION ANALYSES                       ####
# ===================================================================

cat("\n=== Running analyses for CFA (Common Factor Analysis) among Females ===\n")
cat("Running nonlinearity tests for CFA...\n")
CFA_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = CommonFactor_Score_Varlist)
CFA_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(CFA_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
CFA_nonlinearity_results_F <- p2asterisk(CFA_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for CFA...\n")
CFA_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = CommonFactor_Score_Varlist)
CFA_decile_results_F$P_adj_value <- p.adjust(CFA_decile_results_F$P_value, method = "fdr")
CFA_decile_results_F <- p2asterisk(CFA_decile_results_F, "P_adj_value", "sig_adj_p")

print(CFA_nonlinearity_results_F)
print(CFA_decile_results_F)

write.csv(CFA_nonlinearity_results_F, paste0(outFileStem_F, "CFA_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(CFA_decile_results_F, paste0(outFileStem_F, "CFA_decile_comparison_results_F.csv"), row.names = FALSE)

# G Composites
cat("\n=== Running analyses for G Composites among Females ===\n")
cat("Running nonlinearity tests for G Composites...\n")
G_Composites_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = G_Composites_Varlist)
G_Composites_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(G_Composites_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
G_Composites_nonlinearity_results_F <- p2asterisk(G_Composites_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for G Composites...\n")
G_Composites_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = G_Composites_Varlist)
G_Composites_decile_results_F$P_adj_value <- p.adjust(G_Composites_decile_results_F$P_value, method = "fdr")
G_Composites_decile_results_F <- p2asterisk(G_Composites_decile_results_F, "P_adj_value", "sig_adj_p")

print(G_Composites_nonlinearity_results_F)
print(G_Composites_decile_results_F)

write.csv(G_Composites_nonlinearity_results_F, paste0(outFileStem_F, "G_Composites_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(G_Composites_decile_results_F, paste0(outFileStem_F, "G_Composites_decile_comparison_results_F.csv"), row.names = FALSE)

# Verbal Tests
cat("\n=== Running analyses for Verbal Tests among Females ===\n")
cat("Running nonlinearity tests for Verbal Tests...\n")
Verbal_Tests_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = Verbal_Tests_Varlist)
Verbal_Tests_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(Verbal_Tests_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
Verbal_Tests_nonlinearity_results_F <- p2asterisk(Verbal_Tests_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Verbal Tests...\n")
Verbal_Tests_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = Verbal_Tests_Varlist)
Verbal_Tests_decile_results_F$P_adj_value <- p.adjust(Verbal_Tests_decile_results_F$P_value, method = "fdr")
Verbal_Tests_decile_results_F <- p2asterisk(Verbal_Tests_decile_results_F, "P_adj_value", "sig_adj_p")

print(Verbal_Tests_nonlinearity_results_F)
print(Verbal_Tests_decile_results_F)

write.csv(Verbal_Tests_nonlinearity_results_F, paste0(outFileStem_F, "Verbal_Tests_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(Verbal_Tests_decile_results_F, paste0(outFileStem_F, "Verbal_Tests_decile_comparison_results_F.csv"), row.names = FALSE)

# Nonverbal Tests
cat("\n=== Running analyses for Nonverbal Tests among Females ===\n")
cat("Running nonlinearity tests for Nonverbal Tests...\n")
Nonverbal_Tests_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = Nonverbal_Tests_Varlist)
Nonverbal_Tests_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(Nonverbal_Tests_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
Nonverbal_Tests_nonlinearity_results_F <- p2asterisk(Nonverbal_Tests_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Nonverbal Tests...\n")
Nonverbal_Tests_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = Nonverbal_Tests_Varlist)
Nonverbal_Tests_decile_results_F$P_adj_value <- p.adjust(Nonverbal_Tests_decile_results_F$P_value, method = "fdr")
Nonverbal_Tests_decile_results_F <- p2asterisk(Nonverbal_Tests_decile_results_F, "P_adj_value", "sig_adj_p")

print(Nonverbal_Tests_nonlinearity_results_F)
print(Nonverbal_Tests_decile_results_F)

write.csv(Nonverbal_Tests_nonlinearity_results_F, paste0(outFileStem_F, "Nonverbal_Tests_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(Nonverbal_Tests_decile_results_F, paste0(outFileStem_F, "Nonverbal_Tests_decile_comparison_results_F.csv"), row.names = FALSE)

# Education Achievement Attainment
cat("\n=== Running analyses for Education Achievement Attainment among Females ===\n")
cat("Running nonlinearity tests for Education Achievement Attainment...\n")
Edu_Achieve_Attain_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = Edu_Achieve_Attain_Varlist)
Edu_Achieve_Attain_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(Edu_Achieve_Attain_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
Edu_Achieve_Attain_nonlinearity_results_F <- p2asterisk(Edu_Achieve_Attain_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Education Achievement Attainment...\n")
Edu_Achieve_Attain_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = Edu_Achieve_Attain_Varlist)
Edu_Achieve_Attain_decile_results_F$P_adj_value <- p.adjust(Edu_Achieve_Attain_decile_results_F$P_value, method = "fdr")
Edu_Achieve_Attain_decile_results_F <- p2asterisk(Edu_Achieve_Attain_decile_results_F, "P_adj_value", "sig_adj_p")

print(Edu_Achieve_Attain_nonlinearity_results_F)
print(Edu_Achieve_Attain_decile_results_F)

write.csv(Edu_Achieve_Attain_nonlinearity_results_F, paste0(outFileStem_F, "Edu_Achieve_Attain_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(Edu_Achieve_Attain_decile_results_F, paste0(outFileStem_F, "Edu_Achieve_Attain_decile_comparison_results_F.csv"), row.names = FALSE)

# Anxiety
cat("\n=== Running analyses for Anxiety among Females ===\n")
cat("Running nonlinearity tests for Anxiety...\n")
Anxiety_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = Anxiety_Varlist)
Anxiety_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(Anxiety_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
Anxiety_nonlinearity_results_F <- p2asterisk(Anxiety_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Anxiety...\n")
Anxiety_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = Anxiety_Varlist)
Anxiety_decile_results_F$P_adj_value <- p.adjust(Anxiety_decile_results_F$P_value, method = "fdr")
Anxiety_decile_results_F <- p2asterisk(Anxiety_decile_results_F, "P_adj_value", "sig_adj_p")

print(Anxiety_nonlinearity_results_F)
print(Anxiety_decile_results_F)

write.csv(Anxiety_nonlinearity_results_F, paste0(outFileStem_F, "Anxiety_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(Anxiety_decile_results_F, paste0(outFileStem_F, "Anxiety_decile_comparison_results_F.csv"), row.names = FALSE)

# Conners
cat("\n=== Running analyses for Conners among Females ===\n")
cat("Running nonlinearity tests for Conners...\n")
Conners_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = Conners_Varlist)
Conners_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(Conners_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
Conners_nonlinearity_results_F <- p2asterisk(Conners_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Conners...\n")
Conners_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = Conners_Varlist)
Conners_decile_results_F$P_adj_value <- p.adjust(Conners_decile_results_F$P_value, method = "fdr")
Conners_decile_results_F <- p2asterisk(Conners_decile_results_F, "P_adj_value", "sig_adj_p")

print(Conners_nonlinearity_results_F)
print(Conners_decile_results_F)

write.csv(Conners_nonlinearity_results_F, paste0(outFileStem_F, "Conners_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(Conners_decile_results_F, paste0(outFileStem_F, "Conners_decile_comparison_results_F.csv"), row.names = FALSE)

# SDQ
cat("\n=== Running analyses for SDQ among Females ===\n")
cat("Running nonlinearity tests for SDQ...\n")
SDQ_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = SDQ_Varlist)
SDQ_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(SDQ_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
SDQ_nonlinearity_results_F <- p2asterisk(SDQ_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for SDQ...\n")
SDQ_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = SDQ_Varlist)
SDQ_decile_results_F$P_adj_value <- p.adjust(SDQ_decile_results_F$P_value, method = "fdr")
SDQ_decile_results_F <- p2asterisk(SDQ_decile_results_F, "P_adj_value", "sig_adj_p")

print(SDQ_nonlinearity_results_F)
print(SDQ_decile_results_F)

write.csv(SDQ_nonlinearity_results_F, paste0(outFileStem_F, "SDQ_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(SDQ_decile_results_F, paste0(outFileStem_F, "SDQ_decile_comparison_results_F.csv"), row.names = FALSE)

# Anthropometric
cat("\n=== Running analyses for Anthropometric among Females ===\n")
cat("Running nonlinearity tests for Anthropometric...\n")
Anthro_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = Anthro_Varlist)
Anthro_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(Anthro_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
Anthro_nonlinearity_results_F <- p2asterisk(Anthro_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Anthropometric...\n")
Anthro_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = Anthro_Varlist)
Anthro_decile_results_F$P_adj_value <- p.adjust(Anthro_decile_results_F$P_value, method = "fdr")
Anthro_decile_results_F <- p2asterisk(Anthro_decile_results_F, "P_adj_value", "sig_adj_p")

print(Anthro_nonlinearity_results_F)
print(Anthro_decile_results_F)

write.csv(Anthro_nonlinearity_results_F, paste0(outFileStem_F, "Anthro_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(Anthro_decile_results_F, paste0(outFileStem_F, "Anthro_decile_comparison_results_F.csv"), row.names = FALSE)

# Wellbeing
cat("\n=== Running analyses for Wellbeing among Females ===\n")
cat("Running nonlinearity tests for Wellbeing...\n")
Wellbeing_nonlinearity_results_F <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_F, var_list = Wellbeing_Varlist)
Wellbeing_nonlinearity_results_F$P_adj_value_Quadratic <- p.adjust(Wellbeing_nonlinearity_results_F$P_value_Quadratic, method = "fdr")
Wellbeing_nonlinearity_results_F <- p2asterisk(Wellbeing_nonlinearity_results_F, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Wellbeing...\n")
Wellbeing_decile_results_F <- run_decile_comparisons(dat = dat_scaled_selectunpaired_F, var_list = Wellbeing_Varlist)
Wellbeing_decile_results_F$P_adj_value <- p.adjust(Wellbeing_decile_results_F$P_value, method = "fdr")
Wellbeing_decile_results_F <- p2asterisk(Wellbeing_decile_results_F, "P_adj_value", "sig_adj_p")

print(Wellbeing_nonlinearity_results_F)
print(Wellbeing_decile_results_F)

write.csv(Wellbeing_nonlinearity_results_F, paste0(outFileStem_F, "Wellbeing_nonlinearity_results_F.csv"), row.names = FALSE)
write.csv(Wellbeing_decile_results_F, paste0(outFileStem_F, "Wellbeing_decile_comparison_results_F.csv"), row.names = FALSE)

# Save all female results to Excel spreadsheets
# Create workbook for female nonlinearity results
wb_nonlinearity_F <- createWorkbook()
addWorksheet(wb_nonlinearity_F, "CFA")
writeData(wb_nonlinearity_F, "CFA", CFA_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "G_Composites")
writeData(wb_nonlinearity_F, "G_Composites", G_Composites_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "Verbal_Tests")
writeData(wb_nonlinearity_F, "Verbal_Tests", Verbal_Tests_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "Nonverbal_Tests")
writeData(wb_nonlinearity_F, "Nonverbal_Tests", Nonverbal_Tests_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "Edu_Achieve_Attain")
writeData(wb_nonlinearity_F, "Edu_Achieve_Attain", Edu_Achieve_Attain_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "Anxiety")
writeData(wb_nonlinearity_F, "Anxiety", Anxiety_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "Conners")
writeData(wb_nonlinearity_F, "Conners", Conners_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "SDQ")
writeData(wb_nonlinearity_F, "SDQ", SDQ_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "Anthro")
writeData(wb_nonlinearity_F, "Anthro", Anthro_nonlinearity_results_F)
addWorksheet(wb_nonlinearity_F, "Wellbeing")
writeData(wb_nonlinearity_F, "Wellbeing", Wellbeing_nonlinearity_results_F)
saveWorkbook(wb_nonlinearity_F, paste0(outFileStem_F, "All_Nonlinearity_Results_F.xlsx"), overwrite = TRUE)

# Create workbook for female decile results
wb_decile_F <- createWorkbook()
addWorksheet(wb_decile_F, "CFA")
writeData(wb_decile_F, "CFA", CFA_decile_results_F)
addWorksheet(wb_decile_F, "G_Composites")
writeData(wb_decile_F, "G_Composites", G_Composites_decile_results_F)
addWorksheet(wb_decile_F, "Verbal_Tests")
writeData(wb_decile_F, "Verbal_Tests", Verbal_Tests_decile_results_F)
addWorksheet(wb_decile_F, "Nonverbal_Tests")
writeData(wb_decile_F, "Nonverbal_Tests", Nonverbal_Tests_decile_results_F)
addWorksheet(wb_decile_F, "Edu_Achieve_Attain")
writeData(wb_decile_F, "Edu_Achieve_Attain", Edu_Achieve_Attain_decile_results_F)
addWorksheet(wb_decile_F, "Anxiety")
writeData(wb_decile_F, "Anxiety", Anxiety_decile_results_F)
addWorksheet(wb_decile_F, "Conners")
writeData(wb_decile_F, "Conners", Conners_decile_results_F)
addWorksheet(wb_decile_F, "SDQ")
writeData(wb_decile_F, "SDQ", SDQ_decile_results_F)
addWorksheet(wb_decile_F, "Anthro")
writeData(wb_decile_F, "Anthro", Anthro_decile_results_F)
addWorksheet(wb_decile_F, "Wellbeing")
writeData(wb_decile_F, "Wellbeing", Wellbeing_decile_results_F)
saveWorkbook(wb_decile_F, paste0(outFileStem_F, "All_Decile_Comparison_Results_F.xlsx"), overwrite = TRUE)

cat("\n=== Female subpopulation analysis complete! ===\n")
cat("Results saved to:\n")
cat(paste0(outFileStem_F, "All_Nonlinearity_Results_F.xlsx\n"))
cat(paste0(outFileStem_F, "All_Decile_Comparison_Results_F.xlsx\n"))









# ===================================================================
##          MALE SUBPOPULATION ANALYSES                         ####
# ===================================================================

cat("\n=== Running analyses for CFA (Common Factor Analysis) among Males ===\n")
cat("Running nonlinearity tests for CFA...\n")
CFA_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = CommonFactor_Score_Varlist)
CFA_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(CFA_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
CFA_nonlinearity_results_M <- p2asterisk(CFA_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for CFA...\n")
CFA_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = CommonFactor_Score_Varlist)
CFA_decile_results_M$P_adj_value <- p.adjust(CFA_decile_results_M$P_value, method = "fdr")
CFA_decile_results_M <- p2asterisk(CFA_decile_results_M, "P_adj_value", "sig_adj_p")

print(CFA_nonlinearity_results_M)
print(CFA_decile_results_M)

write.csv(CFA_nonlinearity_results_M, paste0(outFileStem_M, "CFA_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(CFA_decile_results_M, paste0(outFileStem_M, "CFA_decile_comparison_results_M.csv"), row.names = FALSE)

# G Composites
cat("\n=== Running analyses for G Composites among Males ===\n")
cat("Running nonlinearity tests for G Composites...\n")
G_Composites_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = G_Composites_Varlist)
G_Composites_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(G_Composites_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
G_Composites_nonlinearity_results_M <- p2asterisk(G_Composites_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for G Composites...\n")
G_Composites_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = G_Composites_Varlist)
G_Composites_decile_results_M$P_adj_value <- p.adjust(G_Composites_decile_results_M$P_value, method = "fdr")
G_Composites_decile_results_M <- p2asterisk(G_Composites_decile_results_M, "P_adj_value", "sig_adj_p")

print(G_Composites_nonlinearity_results_M)
print(G_Composites_decile_results_M)

write.csv(G_Composites_nonlinearity_results_M, paste0(outFileStem_M, "G_Composites_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(G_Composites_decile_results_M, paste0(outFileStem_M, "G_Composites_decile_comparison_results_M.csv"), row.names = FALSE)

# Verbal Tests
cat("\n=== Running analyses for Verbal Tests among Males ===\n")
cat("Running nonlinearity tests for Verbal Tests...\n")
Verbal_Tests_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = Verbal_Tests_Varlist)
Verbal_Tests_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(Verbal_Tests_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
Verbal_Tests_nonlinearity_results_M <- p2asterisk(Verbal_Tests_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Verbal Tests...\n")
Verbal_Tests_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = Verbal_Tests_Varlist)
Verbal_Tests_decile_results_M$P_adj_value <- p.adjust(Verbal_Tests_decile_results_M$P_value, method = "fdr")
Verbal_Tests_decile_results_M <- p2asterisk(Verbal_Tests_decile_results_M, "P_adj_value", "sig_adj_p")

print(Verbal_Tests_nonlinearity_results_M)
print(Verbal_Tests_decile_results_M)

write.csv(Verbal_Tests_nonlinearity_results_M, paste0(outFileStem_M, "Verbal_Tests_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(Verbal_Tests_decile_results_M, paste0(outFileStem_M, "Verbal_Tests_decile_comparison_results_M.csv"), row.names = FALSE)

# Nonverbal Tests
cat("\n=== Running analyses for Nonverbal Tests among Males ===\n")
cat("Running nonlinearity tests for Nonverbal Tests...\n")
Nonverbal_Tests_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = Nonverbal_Tests_Varlist)
Nonverbal_Tests_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(Nonverbal_Tests_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
Nonverbal_Tests_nonlinearity_results_M <- p2asterisk(Nonverbal_Tests_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Nonverbal Tests...\n")
Nonverbal_Tests_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = Nonverbal_Tests_Varlist)
Nonverbal_Tests_decile_results_M$P_adj_value <- p.adjust(Nonverbal_Tests_decile_results_M$P_value, method = "fdr")
Nonverbal_Tests_decile_results_M <- p2asterisk(Nonverbal_Tests_decile_results_M, "P_adj_value", "sig_adj_p")

print(Nonverbal_Tests_nonlinearity_results_M)
print(Nonverbal_Tests_decile_results_M)

write.csv(Nonverbal_Tests_nonlinearity_results_M, paste0(outFileStem_M, "Nonverbal_Tests_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(Nonverbal_Tests_decile_results_M, paste0(outFileStem_M, "Nonverbal_Tests_decile_comparison_results_M.csv"), row.names = FALSE)

# Education Achievement Attainment
cat("\n=== Running analyses for Education Achievement Attainment among Males ===\n")
cat("Running nonlinearity tests for Education Achievement Attainment...\n")
Edu_Achieve_Attain_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = Edu_Achieve_Attain_Varlist)
Edu_Achieve_Attain_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(Edu_Achieve_Attain_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
Edu_Achieve_Attain_nonlinearity_results_M <- p2asterisk(Edu_Achieve_Attain_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Education Achievement Attainment...\n")
Edu_Achieve_Attain_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = Edu_Achieve_Attain_Varlist)
Edu_Achieve_Attain_decile_results_M$P_adj_value <- p.adjust(Edu_Achieve_Attain_decile_results_M$P_value, method = "fdr")
Edu_Achieve_Attain_decile_results_M <- p2asterisk(Edu_Achieve_Attain_decile_results_M, "P_adj_value", "sig_adj_p")

print(Edu_Achieve_Attain_nonlinearity_results_M)
print(Edu_Achieve_Attain_decile_results_M)

write.csv(Edu_Achieve_Attain_nonlinearity_results_M, paste0(outFileStem_M, "Edu_Achieve_Attain_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(Edu_Achieve_Attain_decile_results_M, paste0(outFileStem_M, "Edu_Achieve_Attain_decile_comparison_results_M.csv"), row.names = FALSE)

# Anxiety
cat("\n=== Running analyses for Anxiety among Males ===\n")
cat("Running nonlinearity tests for Anxiety...\n")
Anxiety_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = Anxiety_Varlist)
Anxiety_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(Anxiety_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
Anxiety_nonlinearity_results_M <- p2asterisk(Anxiety_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Anxiety...\n")
Anxiety_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = Anxiety_Varlist)
Anxiety_decile_results_M$P_adj_value <- p.adjust(Anxiety_decile_results_M$P_value, method = "fdr")
Anxiety_decile_results_M <- p2asterisk(Anxiety_decile_results_M, "P_adj_value", "sig_adj_p")

print(Anxiety_nonlinearity_results_M)
print(Anxiety_decile_results_M)

write.csv(Anxiety_nonlinearity_results_M, paste0(outFileStem_M, "Anxiety_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(Anxiety_decile_results_M, paste0(outFileStem_M, "Anxiety_decile_comparison_results_M.csv"), row.names = FALSE)

# Conners
cat("\n=== Running analyses for Conners among Males ===\n")
cat("Running nonlinearity tests for Conners...\n")
Conners_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = Conners_Varlist)
Conners_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(Conners_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
Conners_nonlinearity_results_M <- p2asterisk(Conners_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Conners...\n")
Conners_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = Conners_Varlist)
Conners_decile_results_M$P_adj_value <- p.adjust(Conners_decile_results_M$P_value, method = "fdr")
Conners_decile_results_M <- p2asterisk(Conners_decile_results_M, "P_adj_value", "sig_adj_p")

print(Conners_nonlinearity_results_M)
print(Conners_decile_results_M)

write.csv(Conners_nonlinearity_results_M, paste0(outFileStem_M, "Conners_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(Conners_decile_results_M, paste0(outFileStem_M, "Conners_decile_comparison_results_M.csv"), row.names = FALSE)

# SDQ
cat("\n=== Running analyses for SDQ among Males ===\n")
cat("Running nonlinearity tests for SDQ...\n")
SDQ_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = SDQ_Varlist)
SDQ_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(SDQ_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
SDQ_nonlinearity_results_M <- p2asterisk(SDQ_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for SDQ...\n")
SDQ_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = SDQ_Varlist)
SDQ_decile_results_M$P_adj_value <- p.adjust(SDQ_decile_results_M$P_value, method = "fdr")
SDQ_decile_results_M <- p2asterisk(SDQ_decile_results_M, "P_adj_value", "sig_adj_p")

print(SDQ_nonlinearity_results_M)
print(SDQ_decile_results_M)

write.csv(SDQ_nonlinearity_results_M, paste0(outFileStem_M, "SDQ_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(SDQ_decile_results_M, paste0(outFileStem_M, "SDQ_decile_comparison_results_M.csv"), row.names = FALSE)

# Anthropometric
cat("\n=== Running analyses for Anthropometric among Males ===\n")
cat("Running nonlinearity tests for Anthropometric...\n")
Anthro_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = Anthro_Varlist)
Anthro_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(Anthro_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
Anthro_nonlinearity_results_M <- p2asterisk(Anthro_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Anthropometric...\n")
Anthro_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = Anthro_Varlist)
Anthro_decile_results_M$P_adj_value <- p.adjust(Anthro_decile_results_M$P_value, method = "fdr")
Anthro_decile_results_M <- p2asterisk(Anthro_decile_results_M, "P_adj_value", "sig_adj_p")

print(Anthro_nonlinearity_results_M)
print(Anthro_decile_results_M)

write.csv(Anthro_nonlinearity_results_M, paste0(outFileStem_M, "Anthro_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(Anthro_decile_results_M, paste0(outFileStem_M, "Anthro_decile_comparison_results_M.csv"), row.names = FALSE)

# Wellbeing
cat("\n=== Running analyses for Wellbeing among Males ===\n")
cat("Running nonlinearity tests for Wellbeing...\n")
Wellbeing_nonlinearity_results_M <- run_nonlinearity_test_boot(dat = dat_scaled_selectunpaired_M, var_list = Wellbeing_Varlist)
Wellbeing_nonlinearity_results_M$P_adj_value_Quadratic <- p.adjust(Wellbeing_nonlinearity_results_M$P_value_Quadratic, method = "fdr")
Wellbeing_nonlinearity_results_M <- p2asterisk(Wellbeing_nonlinearity_results_M, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")

cat("Running decile comparisons for Wellbeing...\n")
Wellbeing_decile_results_M <- run_decile_comparisons(dat = dat_scaled_selectunpaired_M, var_list = Wellbeing_Varlist)
Wellbeing_decile_results_M$P_adj_value <- p.adjust(Wellbeing_decile_results_M$P_value, method = "fdr")
Wellbeing_decile_results_M <- p2asterisk(Wellbeing_decile_results_M, "P_adj_value", "sig_adj_p")

print(Wellbeing_nonlinearity_results_M)
print(Wellbeing_decile_results_M)

write.csv(Wellbeing_nonlinearity_results_M, paste0(outFileStem_M, "Wellbeing_nonlinearity_results_M.csv"), row.names = FALSE)
write.csv(Wellbeing_decile_results_M, paste0(outFileStem_M, "Wellbeing_decile_comparison_results_M.csv"), row.names = FALSE)

# Save all male results to Excel spreadsheets
# Create workbook for male nonlinearity results
wb_nonlinearity_M <- createWorkbook()
addWorksheet(wb_nonlinearity_M, "CFA")
writeData(wb_nonlinearity_M, "CFA", CFA_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "G_Composites")
writeData(wb_nonlinearity_M, "G_Composites", G_Composites_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "Verbal_Tests")
writeData(wb_nonlinearity_M, "Verbal_Tests", Verbal_Tests_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "Nonverbal_Tests")
writeData(wb_nonlinearity_M, "Nonverbal_Tests", Nonverbal_Tests_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "Edu_Achieve_Attain")
writeData(wb_nonlinearity_M, "Edu_Achieve_Attain", Edu_Achieve_Attain_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "Anxiety")
writeData(wb_nonlinearity_M, "Anxiety", Anxiety_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "Conners")
writeData(wb_nonlinearity_M, "Conners", Conners_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "SDQ")
writeData(wb_nonlinearity_M, "SDQ", SDQ_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "Anthro")
writeData(wb_nonlinearity_M, "Anthro", Anthro_nonlinearity_results_M)
addWorksheet(wb_nonlinearity_M, "Wellbeing")
writeData(wb_nonlinearity_M, "Wellbeing", Wellbeing_nonlinearity_results_M)
saveWorkbook(wb_nonlinearity_M, paste0(outFileStem_M, "All_Nonlinearity_Results_M.xlsx"), overwrite = TRUE)

# Create workbook for male decile results
wb_decile_M <- createWorkbook()
addWorksheet(wb_decile_M, "CFA")
writeData(wb_decile_M, "CFA", CFA_decile_results_M)
addWorksheet(wb_decile_M, "G_Composites")
writeData(wb_decile_M, "G_Composites", G_Composites_decile_results_M)
addWorksheet(wb_decile_M, "Verbal_Tests")
writeData(wb_decile_M, "Verbal_Tests", Verbal_Tests_decile_results_M)
addWorksheet(wb_decile_M, "Nonverbal_Tests")
writeData(wb_decile_M, "Nonverbal_Tests", Nonverbal_Tests_decile_results_M)
addWorksheet(wb_decile_M, "Edu_Achieve_Attain")
writeData(wb_decile_M, "Edu_Achieve_Attain", Edu_Achieve_Attain_decile_results_M)
addWorksheet(wb_decile_M, "Anxiety")
writeData(wb_decile_M, "Anxiety", Anxiety_decile_results_M)
addWorksheet(wb_decile_M, "Conners")
writeData(wb_decile_M, "Conners", Conners_decile_results_M)
addWorksheet(wb_decile_M, "SDQ")
writeData(wb_decile_M, "SDQ", SDQ_decile_results_M)
addWorksheet(wb_decile_M, "Anthro")
writeData(wb_decile_M, "Anthro", Anthro_decile_results_M)
addWorksheet(wb_decile_M, "Wellbeing")
writeData(wb_decile_M, "Wellbeing", Wellbeing_decile_results_M)
saveWorkbook(wb_decile_M, paste0(outFileStem_M, "All_Decile_Comparison_Results_M.xlsx"), overwrite = TRUE)

cat("\n=== Male subpopulation analysis complete! ===\n")
cat("Results saved to:\n")
cat(paste0(outFileStem_M, "All_Nonlinearity_Results_M.xlsx\n"))
cat(paste0(outFileStem_M, "All_Decile_Comparison_Results_M.xlsx\n"))