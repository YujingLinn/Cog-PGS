library(boot)
library(dplyr)
library(openxlsx)
library(tidyr)
library(tidyverse)
library(psych)

select <- dplyr::select

nboot <- 10 # change it to 1000 on HPC
ncpus <- 4 # change it to 32 on HPC

source("./0PGIQ_VarList.R") 
source("./0PGIQ_CommFactorList.R")

sourceFileStem <- './Data160226/'
outFileStem <- './Results160226/'

dat_scaled_selectunpaired <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired.csv"))
dat_scaled_selectunpaired_F <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_F.csv"))
dat_scaled_selectunpaired_M <- read.csv(paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_M.csv"))

dat_scaled_selectunpaired_MZ <- dat_scaled_selectunpaired %>% filter(zygos == 1)
dat_scaled_selectunpaired_DZ <- dat_scaled_selectunpaired %>% filter(zygos == 2)

# add an age dummy for the cross-age latent factors
dat_scaled_selectunpaired$CrossAgeDummy <- 1
dat_scaled_selectunpaired_F$CrossAgeDummy <- 1
dat_scaled_selectunpaired_M$CrossAgeDummy <- 1
dat_scaled_selectunpaired_MZ$CrossAgeDummy <- 1
dat_scaled_selectunpaired_DZ$CrossAgeDummy <- 1

# Define Domain Configurations
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
#          1. Boot Function for Quadratic Term ####
# ===================================================================
fit_quadratic_model_boot <- function(dat, indices, y, x, covar) {
  datx <- dat[indices, ] # Create the bootstrap sample
  
  # Create quadratic term
  x_squared <- paste0(x, "_squared")
  datx[[x_squared]] <- datx[[x]]^2
  
  # Formulas: Full (linear + quad + covar) and Reduced (linear + covar)
  formula_full <- reformulate(c(x, x_squared, covar), response = y)
  formula_reduced <- if (!is.null(covar)) reformulate(c(x, covar), response = y) else reformulate(x, response = y)
  
  tryCatch({
    fit_model_full <- lm(formula_full, data = datx)
    fit_model_reduced <- lm(formula_reduced, data = datx)
    model_summary <- summary(fit_model_full)
    
    # Extract linear stats safely
    if (x %in% rownames(model_summary$coefficients)) {
      beta_linear <- model_summary$coefficients[x, "Estimate"]
      p_value_linear <- model_summary$coefficients[x, "Pr(>|t|)"]
    } else { beta_linear <- NA; p_value_linear <- NA }
    
    # Extract quadratic stats safely
    if (x_squared %in% rownames(model_summary$coefficients)) {
      beta_quadratic <- model_summary$coefficients[x_squared, "Estimate"]
      p_value_quadratic <- model_summary$coefficients[x_squared, "Pr(>|t|)"]
    } else { beta_quadratic <- NA; p_value_quadratic <- NA }
    
    # R-squared values
    R.squared_full <- model_summary$r.squared
    R.squared_reduced <- summary(fit_model_reduced)$r.squared
    R.squared_quadratic <- R.squared_full - R.squared_reduced
    
    return(c(beta_linear, p_value_linear, beta_quadratic, p_value_quadratic, R.squared_full, R.squared_quadratic))
    
  }, error = function(e) {
    return(rep(NA, 6))
  })
}


# ===================================================================
#          2. Run Nonlinearity Test Function ####
# ===================================================================
run_nonlinearity_test_boot <- function(dat, var_list, predictor_var, include_covariates, sex_var, nboot, ncpus) {
  
  boot_results_list <- lapply(var_list, function(var_item) {
    outcome_stem <- var_item[1]
    age_variable <- var_item[3]
    y <- paste0(outcome_stem, "1")
    x <- predictor_var
    
    cat("  [Nonlinear] Outcome:", y, "| Predictor:", x, "\n")
    
    covariates <- NULL
    if (include_covariates) {
      base_covariates <- c(age_variable, sex_var, "chiptype")
      pc_covariates <- paste0("PC", 1:10)
      covariates <- c(base_covariates, pc_covariates)
      covariates <- covariates[!sapply(covariates, is.null)]
      covariates <- covariates[covariates %in% names(dat)]
    }
    
    tryCatch({
      boot_results <- boot(data = dat, statistic = fit_quadratic_model_boot, R = nboot, 
                           parallel = "multicore", ncpus = ncpus, y = y, x = x, covar = covariates)
      return(boot_results)
    }, error = function(e) { return(NULL) })
  })
  
  results_df <- do.call(rbind, lapply(1:length(boot_results_list), function(i) {
    boot_output <- boot_results_list[[i]]
    var_item <- var_list[[i]]
    
    na_result <- data.frame(
      Outcome = var_item[1], Trait_Name = var_item[2], Rater = var_item[4], Category = var_item[5], Predictor = predictor_var,
      Beta_Linear = NA, SE_Linear_boot = NA, CI_Linear_Lower = NA, CI_Linear_Upper = NA, P_value_Linear = NA,
      Beta_Quadratic = NA, SE_Quadratic_boot = NA, CI_Quadratic_Lower = NA, CI_Quadratic_Upper = NA, P_value_Quadratic = NA,
      R2_Full_Model = NA, R2_Quadratic = NA
    )
    
    if (is.null(boot_output) || sum(!is.na(boot_output$t[, 1])) < 10) return(na_result)
    
    orig <- boot_output$t0
    boot_b_lin <- boot_output$t[, 1][!is.na(boot_output$t[, 1])]
    boot_b_quad <- boot_output$t[, 3][!is.na(boot_output$t[, 3])]
    
    ci_lin <- quantile(boot_b_lin, probs = c(0.025, 0.975), na.rm = TRUE)
    ci_quad <- quantile(boot_b_quad, probs = c(0.025, 0.975), na.rm = TRUE)
    
    data.frame(
      Outcome = var_item[1], Trait_Name = var_item[2], Rater = var_item[4], Category = var_item[5], Predictor = predictor_var,
      Beta_Linear = orig[1], SE_Linear_boot = sd(boot_b_lin), CI_Linear_Lower = ci_lin[1], CI_Linear_Upper = ci_lin[2], P_value_Linear = orig[2],
      Beta_Quadratic = orig[3], SE_Quadratic_boot = sd(boot_b_quad), CI_Quadratic_Lower = ci_quad[1], CI_Quadratic_Upper = ci_quad[2], P_value_Quadratic = orig[4],
      R2_Full_Model = orig[5], R2_Quadratic = orig[6]
    )
  }))
  return(results_df)
}

# ===================================================================
#          3. Run Decile Comparison Function ####
# ===================================================================
run_decile_comparisons <- function(dat, var_list, predictor_var) {
  dat$decile <- dplyr::ntile(dat[[predictor_var]], 10)
  dat_top <- dat[dat$decile == 10, ]
  dat_bottom <- dat[dat$decile == 1, ]
  
  do.call(rbind, lapply(var_list, function(var_item) {
    y <- paste0(var_item[1], "1")
    cat("  [Decile] Outcome:", y, "\n")
    
    tryCatch({
      top_vals <- dat_top[[y]][!is.na(dat_top[[y]])]
      bot_vals <- dat_bottom[[y]][!is.na(dat_bottom[[y]])]
      
      if (length(top_vals) < 2 || length(bot_vals) < 2) stop("Not enough valid data")
      
      t_res <- t.test(top_vals, bot_vals)
      
      data.frame(
        Outcome = var_item[1], Trait_Name = var_item[2], Rater = var_item[4], Category = var_item[5],
        Mean_Top_Decile = mean(top_vals), Mean_Bottom_Decile = mean(bot_vals), Mean_Difference = mean(top_vals) - mean(bot_vals),
        T_Statistic = as.numeric(t_res$statistic), P_value = t_res$p.value, N_Top = length(top_vals), N_Bottom = length(bot_vals)
      )
    }, error = function(e) {
      data.frame(
        Outcome = var_item[1], Trait_Name = var_item[2], Rater = var_item[4], Category = var_item[5],
        Mean_Top_Decile = NA, Mean_Bottom_Decile = NA, Mean_Difference = NA,
        T_Statistic = NA, P_value = NA, N_Top = NA, N_Bottom = NA
      )
    })
  }))
}

# ===================================================================
#          4. Asterisk Helper ####
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
#          5. Master Batch Runner (Updated to return data) ####
# ===================================================================
run_batch_nonlinear_decile <- function(data, group_suffix, sex_covariate, domain_cfg, 
                                       predictor="PGIQ", predictor_label="PGIQ",
                                       n_boot=10, n_cpus=4, out_stem) {
  
  cat("\n============================================\n")
  cat(" STARTING NONLINEAR & DECILE ANALYSIS FOR: ", group_suffix, "\n")
  cat("============================================\n")
  
  # Optional: Keep the individual saving logic if you still want the subfolders
  target_folder <- switch(group_suffix, "pop" = "Nonlinear_All", "F" = "Nonlinear_F", "M" = "Nonlinear_M", "MZ" = "Nonlinear_MZ", "DZ" = "Nonlinear_DZ")
  save_dir <- paste0(out_stem, target_folder, "/")
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  group_results_nonlinear <- list()
  group_results_decile <- list()
  
  for (domain_name in names(domain_cfg)) {
    cat(">> Processing Domain:", domain_name, "\n")
    current_var_list <- domain_cfg[[domain_name]]
    
    # 1. Nonlinearity Test
    res_nonlin <- run_nonlinearity_test_boot(dat = data, var_list = current_var_list, predictor_var = predictor, include_covariates = TRUE, sex_var = sex_covariate, nboot = n_boot, ncpus = n_cpus)
    if(!is.null(res_nonlin) && nrow(res_nonlin) > 0) {
      res_nonlin$P_adj_value_Quadratic <- p.adjust(res_nonlin$P_value_Quadratic, method = "fdr")
      res_nonlin <- p2asterisk(res_nonlin, "P_adj_value_Quadratic", "sig_adj_p_Quadratic")
      group_results_nonlinear[[domain_name]] <- res_nonlin
    }
    
    # 2. Decile Comparison
    # Add a safety check for predictor to avoid "vctrs null" error
    if (!predictor %in% names(data)) stop(paste0("Predictor '", predictor, "' not found in dataset!"))
    
    res_decile <- run_decile_comparisons(dat = data, var_list = current_var_list, predictor_var = predictor)
    if(!is.null(res_decile) && nrow(res_decile) > 0) {
      res_decile$P_adj_value <- p.adjust(res_decile$P_value, method = "fdr")
      res_decile <- p2asterisk(res_decile, "P_adj_value", "sig_adj_p")
      group_results_decile[[domain_name]] <- res_decile
    }
  }
  
  # Combine domains into single dataframes for this specific group
  df_nonlin_combined <- bind_rows(group_results_nonlinear, .id = "Domain")
  df_decile_combined <- bind_rows(group_results_decile, .id = "Domain")
  
  cat(">> Completed Group:", group_suffix, "\n\n")
  
  # Return the combined dataframes so we can collect them in the main loop
  return(list(
    nonlinear = df_nonlin_combined,
    decile = df_decile_combined
  ))
}

# ===================================================================
#   6. Execute Analysis and 7. Format Manuscript Tables ####
# ===================================================================

predictors_to_run <- list(
  "PGgS" = "PGIQ" # Ensure left side is exact column name in dataset
)

groups_to_run <- list(
  "pop" = list(data = dat_scaled_selectunpaired,   sex_covar = "sex1"),
  "F"   = list(data = dat_scaled_selectunpaired_F, sex_covar = NULL),
  "M"   = list(data = dat_scaled_selectunpaired_M, sex_covar = NULL),
  "MZ"  = list(data = dat_scaled_selectunpaired_MZ, sex_covar = "sex1"),
  "DZ"  = list(data = dat_scaled_selectunpaired_DZ, sex_covar = "sex1")
)

# Create lists to hold ALL results across groups
master_nonlinear_list <- list()
master_decile_list <- list()

for (pred_col in names(predictors_to_run)) {
  pred_label <- predictors_to_run[[pred_col]]
  
  for (grp_suffix in names(groups_to_run)) {
    grp_data  <- groups_to_run[[grp_suffix]]$data
    grp_sex   <- groups_to_run[[grp_suffix]]$sex_covar
    
    # Run the analysis
    res <- run_batch_nonlinear_decile(
      data = grp_data,
      group_suffix = grp_suffix,
      sex_covariate = grp_sex,
      domain_cfg = domain_config,
      predictor = pred_col,
      predictor_label = pred_label,
      n_boot = nboot, 
      n_cpus = ncpus, 
      out_stem = outFileStem
    )
    
    # Tag the group name and store in master lists
    res$nonlinear$Group <- grp_suffix
    res$decile$Group <- grp_suffix
    
    master_nonlinear_list[[grp_suffix]] <- res$nonlinear
    master_decile_list[[grp_suffix]] <- res$decile
  }
  
  # -----------------------------------------------------------------
  # Formatting into Manuscript Table Layout (Original Columns)
  # -----------------------------------------------------------------
  cat("\nFormatting Manuscript Tables for predictor:", pred_label, "...\n")
  
  # 1. Format Nonlinearity Table
  df_all_nonlin <- bind_rows(master_nonlinear_list) %>%
    mutate(
      Category = factor(Category, levels = unique(Category)),
      Trait_Name = factor(Trait_Name, levels = unique(Trait_Name)),
      Rater = factor(Rater, levels = unique(Rater)),
      
      Group = factor(Group, levels = c("pop", "F", "M", "MZ", "DZ"), labels = c("Whole_Sample", "Female", "Male", "MZ", "DZ"))
    ) %>%
    # Select original raw numeric columns
    select(Domain, Category, Trait_Name, Rater, Predictor, Group, 
           Beta_Linear, SE_Linear_boot, P_value_Linear, 
           Beta_Quadratic, SE_Quadratic_boot, P_value_Quadratic) %>%
    # Pivot wider
    pivot_wider(
      names_from = Group,
      values_from = c(Beta_Linear, SE_Linear_boot, P_value_Linear, 
                      Beta_Quadratic, SE_Quadratic_boot, P_value_Quadratic),
      names_glue = "{Group}_{.value}"
    ) %>%
    arrange(Domain, Category, Trait_Name, Rater)
  
  # Reorder columns slightly to ensure Whole_Sample_Beta is next to Whole_Sample_SE, etc.
  desired_order_nl <- c("Domain", "Category", "Trait_Name", "Rater", "Predictor")
  for(g in c("Whole_Sample", "Female", "Male", "MZ", "DZ")) {
    desired_order_nl <- c(desired_order_nl, 
                          paste0(g, "_Beta_Linear"), 
                          paste0(g, "_SE_Linear_boot"), 
                          paste0(g, "_P_value_Linear"), 
                          paste0(g, "_Beta_Quadratic"), 
                          paste0(g, "_SE_Quadratic_boot"), 
                          paste0(g, "_P_value_Quadratic"))
  }
  df_manuscript_nonlin <- df_all_nonlin %>% select(any_of(desired_order_nl))
  
  
  # 2. Format Decile Table
  df_all_decile <- bind_rows(master_decile_list) %>%
    mutate(
      Category = factor(Category, levels = unique(Category)),
      Trait_Name = factor(Trait_Name, levels = unique(Trait_Name)),
      Rater = factor(Rater, levels = unique(Rater)),
      
      Group = factor(Group, levels = c("pop", "F", "M", "MZ", "DZ"), labels = c("Whole_Sample", "Female", "Male", "MZ", "DZ"))
    ) %>%
    select(Domain, Category, Trait_Name, Rater, Group, Mean_Difference, P_value) %>%
    pivot_wider(
      names_from = Group,
      values_from = c(Mean_Difference, P_value),
      names_glue = "{Group}_{.value}"
    ) %>%
    arrange(Domain, Category, Trait_Name, Rater)
  
  desired_order_dec <- c("Domain", "Category", "Trait_Name", "Rater")
  for(g in c("Whole_Sample", "Female", "Male", "MZ", "DZ")) {
    desired_order_dec <- c(desired_order_dec, paste0(g, "_Mean_Difference"), paste0(g, "_P_value"))
  }
  df_manuscript_decile <- df_all_decile %>% select(any_of(desired_order_dec))
  
  # -----------------------------------------------------------------
  # Save the finalized Manuscript Tables to a single Excel Workbook
  # -----------------------------------------------------------------
  wb_manuscript <- createWorkbook()
  
  addWorksheet(wb_manuscript, "B8_Nonlinear")
  writeData(wb_manuscript, "B8_Nonlinear", df_manuscript_nonlin, colNames = TRUE)
  freezePane(wb_manuscript, "B8_Nonlinear", firstRow = TRUE)
  
  addWorksheet(wb_manuscript, "B9_Decile")
  writeData(wb_manuscript, "B9_Decile", df_manuscript_decile, colNames = TRUE)
  freezePane(wb_manuscript, "B9_Decile", firstRow = TRUE)
  
  manuscript_file_path <- paste0(outFileStem, "ManuscriptTables_B8_B9_", pred_label, ".xlsx")
  saveWorkbook(wb_manuscript, manuscript_file_path, overwrite = TRUE)
  
  cat("Saved Manuscript Excel file to", manuscript_file_path, "\n")
}

cat("ALL NONLINEARITY & DECILE TESTS AND FORMATTING COMPLETED SUCCESSFULLY!\n")
