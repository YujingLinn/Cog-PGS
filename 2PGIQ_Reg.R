# Polygenic IQ population prediction 

library(scales)
library(tidyr)
library(boot)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(psych)
select <- dplyr::select

nboot <- 1000 
ncpus <- 32 

sourceFileStem <- './Data/'
outFileStem <- './Results/'

source("./PGIQ Codes/0PGIQ_VarList.R") 
source("./PGIQ Codes/0PGIQ_CommFactorList.R")

dat_scaled_selectunpaired <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired.csv"))
dat_scaled_selectunpaired_F <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_F.csv"))
dat_scaled_selectunpaired_M <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_M.csv"))
dat_scaled_selectunpaired_MZ <- dat_scaled_selectunpaired %>% filter(zygos == 1)
dat_scaled_selectunpaired_DZ <- dat_scaled_selectunpaired %>% filter(zygos == 2)

dat_scaled_selectunpaired$CrossAgeDummy <- 1
dat_scaled_selectunpaired_F$CrossAgeDummy <- 1
dat_scaled_selectunpaired_M$CrossAgeDummy <- 1
dat_scaled_selectunpaired_MZ$CrossAgeDummy <- 1
dat_scaled_selectunpaired_DZ$CrossAgeDummy <- 1

domain_config <- list(
  "G_composites"    = G_Composites_Varlist,
  "Verbal_Tests"    = Verbal_Tests_Varlist,
  "Nonverbal_Tests" = Nonverbal_Tests_Varlist,
  "Education"       = Edu_Achieve_Attain_Varlist,
  "SDQ"             = SDQ_Varlist,
  "Anxiety"         = Anxiety_Varlist,
  "Conners_ADHD"    = Conners_Varlist,
  "Anthro"          = Anthro_Varlist,
  "Wellbeing"       = Wellbeing_Varlist,
  "CFA_composites"  = CommonFactor_Score_Varlist
)

# ===================================================================
# Boot function 
# ===================================================================
fit_linear_model_boot <- function(dat, indices, y, x, covar) {
  datx <- dat[indices, ] 
  formula_full <- reformulate(c(x, covar), response = y)
  formula_reduced <- if (!is.null(covar)) reformulate(covar, response = y) else reformulate("1", response = y)
  
  tryCatch({
    fit_model_full <- lm(formula_full, data = datx)
    fit_model_reduced <- lm(formula_reduced, data = datx)
    model_summary <- summary(fit_model_full)
    
    if (x %in% rownames(model_summary$coefficients)) {
      beta <- model_summary$coefficients[x, "Estimate"]
      p_value <- model_summary$coefficients[x, "Pr(>|t|)"]
    } else {
      beta <- NA; p_value <- NA
    }
    
    R.squared_full <- model_summary$r.squared
    R.squared_reduced <- summary(fit_model_reduced)$r.squared
    R.squared_predictor <- R.squared_full - R.squared_reduced
    
    return(c(beta, p_value, R.squared_full, R.squared_predictor))
  }, error = function(e) { return(c(NA, NA, NA, NA)) })
}

# ===================================================================
# Run regression with boot and covars function 
# ===================================================================
run_linear_prediction_boot <- function(dat, var_list, predictor_var, include_covariates, sex_var, nboot, ncpus) {
  boot_results_list <- lapply(var_list, function(var_item) {
    outcome_stem <- var_item[1]
    age_variable <- var_item[3]
    y <- paste0(outcome_stem, "1")
    x <- predictor_var
    
    covariates <- NULL
    if (include_covariates) {
      base_covariates <- c(age_variable, sex_var, "chiptype")
      pc_covariates <- paste0("PC", 1:10)
      covariates <- c(base_covariates, pc_covariates)
      covariates <- covariates[!sapply(covariates, is.null)]
      covariates <- covariates[covariates %in% names(dat)]
    }
    
    tryCatch({
      boot_results <- boot(data = dat, statistic = fit_linear_model_boot, R = nboot, parallel = "multicore", ncpus = ncpus, y = y, x = x, covar = covariates)
      return(boot_results)
    }, error = function(e) { return(NULL) })
  })
  
  results_df <- do.call(rbind, lapply(1:length(boot_results_list), function(i) {
    boot_output <- boot_results_list[[i]]
    var_item <- var_list[[i]]
    
    na_result <- data.frame(Outcome = var_item[1], Trait_Name = var_item[2], Rater = var_item[4], Category = var_item[5], Predictor = predictor_var, Beta = NA, SE_boot = NA, CI_Lower = NA, CI_Upper = NA, P_value = NA, R2_Full_Model = NA, R2_Predictor = NA, Beta_SE_Formatted = "NA")
    
    if (is.null(boot_output) || sum(!is.na(boot_output$t[, 1])) < 10) return(na_result)
    
    original_estimates <- boot_output$t0
    boot_betas <- boot_output$t[, 1][!is.na(boot_output$t[, 1])]
    
    se_boot <- sd(boot_betas)
    ci_boot <- quantile(boot_betas, probs = c(0.025, 0.975), na.rm = TRUE)
    
    result <- data.frame(Outcome = var_item[1], Trait_Name = var_item[2], Rater = var_item[4], Category = var_item[5], Predictor = predictor_var, Beta = original_estimates[1], SE_boot = se_boot, CI_Lower = ci_boot[1], CI_Upper = ci_boot[2], P_value = original_estimates[2], R2_Full_Model = original_estimates[3], R2_Predictor = original_estimates[4])
    result$Beta_SE_Formatted <- paste0(format(round(result$Beta, 3), nsmall = 3), " (", format(round(result$SE_boot, 3), nsmall = 3), ")")
    return(result)
  }))
  return(results_df)
}

# ===================================================================
# Asterisk function 
# ===================================================================
p2asterisk <- function(df, target_p_column, new_labelled_column_name){
  labels <- rep(NA, nrow(df)) 
  df <- tibble::add_column(df, labels, .after=target_p_column)
  x <- df[[target_p_column]]
  x1 <- ifelse(x>=0.05, "", ifelse(x<0.05 & x>=0.01, "*", ifelse(x< 0.01 & x>=0.001, "**", ifelse(x<0.001 & x>=0.0001, "***", ifelse(x<0.0001, "****", NA)))))
  colnames(df)[colnames(df)=="labels"] <- new_labelled_column_name
  df[[new_labelled_column_name]] <- x1
  return(df)
}

# ===================================================================
# Actual run function 
# ===================================================================
run_batch_analysis <- function(data, group_suffix, sex_covariate, domain_cfg, predictor="PGIQ", predictor_label="PGIQ", n_boot=20, n_cpus=4, out_stem) {
  target_folder <- switch(group_suffix, "pop" = "Reg_All", "F" = "Reg_F", "M" = "Reg_M", "MZ" = "Reg_MZ", "DZ" = "Reg_DZ")
  save_dir <- paste0(out_stem, target_folder, "/")
  
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  group_results_list <- list()
  for (domain_name in names(domain_cfg)) {
    current_var_list <- domain_cfg[[domain_name]]
    res <- run_linear_prediction_boot(dat = data, var_list = current_var_list, predictor_var = predictor, include_covariates = TRUE, sex_var = sex_covariate, nboot = n_boot, ncpus = n_cpus)
    
    if(!is.null(res) && nrow(res) > 0) {
      res$P_adj_value <- p.adjust(res$P_value, method = "fdr")
      res <- p2asterisk(res, "P_adj_value", "sig_adj_p")
      group_results_list[[domain_name]] <- res
    }
  }
  
  all_results_combined <- bind_rows(group_results_list, .id = "Domain")
  file_suffix <- ifelse(group_suffix == "pop", "", paste0("_", group_suffix))
  wb <- createWorkbook() 
  
  for (domain_name in names(group_results_list)) {
    df <- group_results_list[[domain_name]]
    sheet_name <- substr(domain_name, 1, 31) 
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df, colNames = TRUE, rowNames = FALSE)
    write.csv(df, paste0(save_dir, predictor_label, "_", domain_name, "_adjCovar", file_suffix, ".csv"), row.names = FALSE)
  }
  
  saveWorkbook(wb, paste0(save_dir, predictor_label, "_20boot_adjCovar", file_suffix, ".xlsx"), overwrite = TRUE)
  write.csv(all_results_combined, paste0(save_dir, predictor_label, "_20boot_adjCovar", file_suffix, ".csv"), row.names = FALSE)
}

# ===================================================================
# Execute Analysis 
# ===================================================================
predictors_to_run <- list("PGgS" = "PGgS", "Savage_g_PGS" = "Savage_g_PGS", "Okbay_EA_PGS" = "Okbay_EA_PGS")
groups_to_run <- list(
  "pop" = list(data = dat_scaled_selectunpaired, sex_covar = "sex1"),
  "F"   = list(data = dat_scaled_selectunpaired_F, sex_covar = NULL),
  "M"   = list(data = dat_scaled_selectunpaired_M, sex_covar = NULL),
  "MZ" = list(data = dat_scaled_selectunpaired_MZ, sex_covar = "sex1"),
  "DZ" = list(data = dat_scaled_selectunpaired_DZ, sex_covar = "sex1")
)

for (pred_col in names(predictors_to_run)) {
  pred_label <- predictors_to_run[[pred_col]]
  for (grp_suffix in names(groups_to_run)) {
    run_batch_analysis(data = groups_to_run[[grp_suffix]]$data, group_suffix = grp_suffix, sex_covariate = groups_to_run[[grp_suffix]]$sex_covar, domain_cfg = domain_config, predictor = pred_col, predictor_label = pred_label, n_boot = nboot, n_cpus = ncpus, out_stem = outFileStem)
  }
}

# ===================================================================
# 7PGIQ_FormatRegTables.R - Format Linear Regression Results
# ===================================================================
domains <- c("G_composites", "Verbal_Tests", "Nonverbal_Tests", "Education", "SDQ", "Anxiety", "Conners_ADHD", "Anthro", "Wellbeing", "CFA_composites")
predictors <- c("PGgS", "Savage_g_PGS", "Okbay_EA_PGS")
groups <- list(
  "pop" = list(folder = "Reg_All", suffix = ""), "F" = list(folder = "Reg_F", suffix = "_F"),
  "M" = list(folder = "Reg_M", suffix = "_M"), "MZ" = list(folder = "Reg_MZ", suffix = "_MZ"),
  "DZ" = list(folder = "Reg_DZ", suffix = "_DZ")
)

all_data_list <- list()
for (dom in domains) {
  for (grp_name in names(groups)) {
    for (pred in predictors) {
      file_path <- paste0(outFileStem, groups[[grp_name]]$folder, "/", pred, "_", dom, "_adjCovar", groups[[grp_name]]$suffix, ".csv")
      if (file.exists(file_path)) {
        temp_df <- read.csv(file_path, stringsAsFactors = FALSE)
        temp_df$Domain <- dom; temp_df$Group <- grp_name; temp_df$Predictor_Name <- pred 
        all_data_list[[length(all_data_list) + 1]] <- temp_df
      }
    }
  }
}

df_combined <- bind_rows(all_data_list)
df_manuscript <- df_combined %>%
  mutate(
    Category = factor(Category, levels = unique(Category)), Trait_Name = factor(Trait_Name, levels = unique(Trait_Name)),
    Rater = factor(Rater, levels = unique(Rater)), Domain = factor(Domain, levels = domains),
    Group = factor(Group, levels = c("pop", "F", "M", "MZ", "DZ"), labels = c("Whole Sample", "Female", "Male", "Monozygotic Twin", "Dizygotic Twin"))
  ) %>%
  select(Group, Domain, Category, Trait_Name, Rater, Predictor_Name, Beta, SE_boot, CI_Lower, CI_Upper, P_adj_value, sig_adj_p, R2_Full_Model, R2_Predictor) %>%
  pivot_wider(names_from = Predictor_Name, values_from = c(Beta, SE_boot, CI_Lower, CI_Upper, P_adj_value, sig_adj_p, R2_Full_Model, R2_Predictor), names_glue = "{Predictor_Name}_{.value}") %>%
  arrange(Group, Domain, Category, Trait_Name, Rater)

desired_cols <- c("Group", "Domain", "Category", "Trait_Name", "Rater")
for (p in predictors) {
  desired_cols <- c(desired_cols, paste0(p, "_Beta"), paste0(p, "_SE_boot"), paste0(p, "_CI_Lower"), paste0(p, "_CI_Upper"), paste0(p, "_R2_Full_Model"), paste0(p, "_R2_Predictor"), paste0(p, "_P_adj_value"), paste0(p, "_sig_adj_p"))
}
df_manuscript <- df_manuscript %>% select(any_of(desired_cols))

wb <- createWorkbook()
addWorksheet(wb, "Reg_Combined")
writeData(wb, "Reg_Combined", df_manuscript, colNames = TRUE)
freezePane(wb, "Reg_Combined", firstRow = TRUE) 
saveWorkbook(wb, paste0(outFileStem, "ManuscriptTable_B3_Regression_Combined.xlsx"), overwrite = TRUE)
