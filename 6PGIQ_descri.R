# title: "Descriptive Statistics and Sensitivity Analyses"
# author: "Yujing"
# date: "October 15, 2025"

# sensitivity analyses: age, sex, birth order, chiptype, PCs, zygosity
# whole sample descriptives of phenotypes: mean, SD, N, age(SD), min, max, skewness, kurtosis
# females' and males' descriptives: mean, SD, N, age(SD), min, max, skewness, kurtosis
# PGIQ_IQscale +3/-3SD descriptives: mean, SD, N, age(SD), min, max, skewness, kurtosis

library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(openxlsx)
library(moments)

# Source the variable lists
source("/Users/yujinglin/Desktop/PGIQ Codes/0PGIQ_VarList.R")

# Set file paths
sourceFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Data220925/'
outFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Results220925/'

# Load raw data
raw_data <- read.csv(paste0(sourceFileStem, "PGIQ_raw.csv"))

# select those with PGIQ available; otherwise, we may end up with more people than we have included in our analyses
dat_raw <- raw_data %>%
  filter(!is.na(PGIQ))

# ==============================================================================
# 1. DATA PREPARATION ####
# ==============================================================================
cat("\n=== PREPARING DATA ===\n\n")

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

# Define all variables for analysis
all_phenotype_vars <- c(
  sapply(G_Composites_Varlist, function(x) paste0(x[1], "1")),
  sapply(Verbal_Tests_Varlist, function(x) paste0(x[1], "1")),
  sapply(Nonverbal_Tests_Varlist, function(x) paste0(x[1], "1")),
  sapply(Edu_Achieve_Attain_Varlist, function(x) paste0(x[1], "1")),
  sapply(Anxiety_Varlist, function(x) paste0(x[1], "1")),
  sapply(Conners_Varlist, function(x) paste0(x[1], "1")),
  sapply(SDQ_Varlist, function(x) paste0(x[1], "1")),
  sapply(Anthro_Varlist, function(x) paste0(x[1], "1")),
  sapply(Wellbeing_Varlist, function(x) paste0(x[1], "1"))
)

common_factor_vars <- sapply(CommonFactor_Score_Varlist, function(x) paste0(x[1], "1"))

other_vars <- c("ases", "gses", "pses", "u1pses", "PGIQ_IQscale")

analysis_vars <- c(all_phenotype_vars, common_factor_vars, other_vars)
analysis_vars <- analysis_vars[analysis_vars %in% names(dat_raw)]
cat("Total variables in analysis:", length(analysis_vars), "\n\n")
# Total variables in analysis: 394 

missing_vars <- analysis_vars[!analysis_vars %in% names(dat_raw)]
cat("Variables missing:", length(missing_vars), "\n")



# ==============================================================================
# 2. CREATE METADATA LOOKUP TABLE ####
# ==============================================================================

master_varlist <- c(
  G_Composites_Varlist, Verbal_Tests_Varlist, Nonverbal_Tests_Varlist,
  Edu_Achieve_Attain_Varlist, Anxiety_Varlist, Conners_Varlist,
  SDQ_Varlist, Anthro_Varlist, Wellbeing_Varlist, CommonFactor_Score_Varlist
)

metadata_df <- purrr::map_dfr(master_varlist, ~tibble(
  Variable_Stem = .x[1],
  Trait_Name = .x[2],
  Age_Variable = .x[3],
  Rater = .x[4],
  Category = .x[5]
))

other_metadata <- tibble(
  Variable_Stem = c("ases", "gses", "pses", "u1pses", "PGIQ_IQscale"),
  Trait_Name = c("SES (Birth)", "SES (Age 7)", "SES (Age 16)", "SES (Age 21)", "Polygenic g Score"),
  Age_Variable = NA_character_,
  Rater = c("Parent", "Parent", "Parent", "Child", "N/A"),
  Category = c("Socioeconomic Status", "Socioeconomic Status", "Socioeconomic Status", "Socioeconomic Status", "Polygenic g Score")
)

master_metadata_df <- bind_rows(metadata_df, other_metadata) %>%
  mutate(Variable = if_else(
    Category %in% c("Polygenic g Score", "Socioeconomic Status"),
    Variable_Stem,
    paste0(Variable_Stem, "1")
  )) %>%
  select(Variable, Trait_Name, Age_Variable, Rater, Category)

# ==============================================================================
# 3. DESCRIPTIVE STATISTICS FUNCTION ####
# ==============================================================================

calc_descriptives <- function(x) {
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) == 0) {
    return(list(
      N = 0, Mean = NA, SD = NA, Min = NA, Max = NA,
      Skewness = NA, Kurtosis = NA
    ))
  }
  
  list(
    N = length(x_clean),
    Mean = mean(x_clean),
    SD = sd(x_clean),
    Min = min(x_clean),
    Max = max(x_clean),
    Skewness = if(length(x_clean) > 2) skewness(x_clean) else NA,
    Kurtosis = if(length(x_clean) > 3) kurtosis(x_clean) else NA
  )
}

# ==============================================================================
# 4. SENSITIVITY ANALYSES ####
# ==============================================================================

cat("\n=== RUNNING SENSITIVITY ANALYSES ===\n\n")

# Function to run linear regression
run_regression <- function(data, outcome_var, predictor_var) {
  if (!all(c(outcome_var, predictor_var) %in% names(data))) return(NULL)
  
  clean_data <- data[!is.na(data[[outcome_var]]) & !is.na(data[[predictor_var]]), ]
  
  if (nrow(clean_data) < 20) return(NULL)
  
  tryCatch({
    model <- lm(as.formula(paste(outcome_var, "~", predictor_var)), data = clean_data)
    model_summary <- summary(model)
    coef_table <- model_summary$coefficients
    
    if (nrow(coef_table) > 1) {
      coef_row <- coef_table[2, ]
      return(tibble(
        Variable = outcome_var,
        Predictor = predictor_var,
        Beta = coef_row[1],
        SE = coef_row[2],
        t_value = coef_row[3],
        P_Value = coef_row[4],
        R_squared = model_summary$r.squared,
        N = nrow(clean_data)
      ))
    }
  }, error = function(e) { return(NULL) })
}

# Define predictors
predictors_list <- c("sex1", "twin", "zygos", "chiptype", paste0("PC", 1:10))

# Run sensitivity analyses
sensitivity_results_list <- list()

for (pred in predictors_list) {
  if (!pred %in% names(dat_raw)) next
  
  cat("Testing predictor:", pred, "\n")
  
  pred_results <- purrr::map_dfr(
    analysis_vars,
    ~run_regression(dat_raw, .x, pred)
  )
  
  if (nrow(pred_results) > 0) {
    pred_results <- pred_results %>%
      mutate(P_adjusted_FDR = p.adjust(P_Value, method = "fdr")) %>%
      left_join(master_metadata_df, by = "Variable")
    
    sensitivity_results_list[[pred]] <- pred_results
  }
}

all_sensitivity_results <- bind_rows(sensitivity_results_list)

# Age sensitivity analysis
cat("Testing predictor: Age\n")

# we don't use the cross-age variables, PGIQ, or SES variables for age sensitivity analyses
age_sensitivity_list <- list()

for (i in seq_along(all_phenotype_vars)) {
  var <- all_phenotype_vars[i]
  age_var <- master_metadata_df %>%
    filter(Variable == var) %>%
    pull(Age_Variable) %>%
    first()
  
  if (!is.na(age_var) && age_var %in% names(dat_raw)) {
    result <- run_regression(dat_raw, var, age_var)
    if (!is.null(result)) {
      result <- result %>%
        mutate(
          Predictor = age_var,
          P_adjusted_FDR = p.adjust(P_Value, method = "fdr")
        ) %>%
        left_join(master_metadata_df, by = "Variable")
      age_sensitivity_list[[var]] <- result
    }
  }
}

age_sensitivity_results <- bind_rows(age_sensitivity_list)

cat("Sensitivity analyses completed.\n")

# ==============================================================================
# 5. WHOLE & SUB SAMPLE DESCRIPTIVES ####
# ==============================================================================

cat("\n=== GENERATING SAMPLE DESCRIPTIVES ===\n\n")

create_desc_table <- function(data, vars = analysis_vars, metadata = master_metadata_df) {
  
  # Check if the input data is empty
  if (nrow(data) == 0) {
    # Print a message and return an empty tibble to avoid errors
    message("Input data has 0 rows. Returning an empty table.")
    return(tibble())   
  }
  
  tibble(Variable = vars) %>%
    mutate(
      stats = map(Variable, ~calc_descriptives(data[[.x]]))
    ) %>%
    unnest_wider(stats) %>%
    filter(N > 0) %>%
    left_join(metadata, by = "Variable") %>%
    # Add Age statistics
    mutate(
      Age_Mean = map_dbl(Variable, ~{
        age_var <- metadata %>% filter(Variable == .x) %>% pull(Age_Variable) %>% first()
        if (!is.na(age_var) && age_var %in% names(data)) {
          age_data <- data[[age_var]][!is.na(data[[age_var]])]
          if (length(age_data) > 0) mean(age_data) else NA
        } else {
          NA
        }
      }),
      Age_SD = map_dbl(Variable, ~{
        age_var <- metadata %>% filter(Variable == .x) %>% pull(Age_Variable) %>% first()
        if (!is.na(age_var) && age_var %in% names(data)) {
          age_data <- data[[age_var]][!is.na(data[[age_var]])]
          if (length(age_data) > 1) sd(age_data) else NA
        } else {
          NA
        }
      })
    ) %>%
    mutate(
      across(
        # Apply formatting to all numeric columns except N
        c(Mean, SD, Min, Max, Skewness, Kurtosis, Age_Mean, Age_SD),
        ~ format(round(., 2), nsmall = 2)
      )
    ) %>%
    select(
      # Reorder columns for the final report
      Category, Trait_Name, Rater, N, Age_Mean, Age_SD, Mean, SD, Min, Max,
      Skewness, Kurtosis, Variable
    )
}

# --- For Whole Sample ---
whole_sample_desc <- create_desc_table(dat_raw)
# Warning message:
# In left_join(., master_metadata_df, by = "Variable") :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 18 of `x` matches multiple rows in `y`.
# ℹ Row 18 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.

# see this, don't panic; it is because I have variables are the composite as well as the test scores;
# i.e., only one test comprise the composite, like pcvctota1

# --- For Subgroups by Sex ---
female_desc <- create_desc_table(dat_raw %>% filter(sex1 == 0))
male_desc <- create_desc_table(dat_raw %>% filter(sex1 == 1))

# -- For Subgroups by zygosity --
mz_desc <- create_desc_table(dat_raw %>% filter(zygos == 1))
dz_desc <- create_desc_table(dat_raw %>% filter(zygos == 2))

# --- For Subgroups by PGIQ Score ---
high_pgiq <- dat_raw %>% filter(PGIQ_IQscale > 145)
low_pgiq  <- dat_raw %>% filter(PGIQ_IQscale < 55)

high_pgiq_desc <- create_desc_table(high_pgiq)
low_pgiq_desc <- create_desc_table(low_pgiq)



# ==============================================================================
# 6. T-TEST COMPARISON: HIGH vs. LOW PGIQ GROUPS ####
# ==============================================================================

cat("\n=== COMPARING HIGH vs. LOW PGIQ GROUPS ===\n\n")

# Function to perform a robust Welch's t-test for a single variable
# Welch's t-test is used because it doesn't assume equal variances, which is safer.
run_ttest <- function(high_data, low_data, outcome_var) {
  
  # Extract data vectors and remove NAs
  vec_high <- high_data[[outcome_var]][!is.na(high_data[[outcome_var]])]
  vec_low  <- low_data[[outcome_var]][!is.na(low_data[[outcome_var]])]
  
  # Ensure there is enough data AND variance in both groups to perform a test
  if (length(vec_high) < 2 || length(vec_low) < 2 || sd(vec_high) == 0 || sd(vec_low) == 0) {
    return(NULL) # Return nothing if conditions aren't met
  }
  
  # Use tryCatch to handle any other potential errors from t.test
  tryCatch({
    test_result <- t.test(vec_high, vec_low)
    
    tibble(
      Variable = outcome_var,
      t_statistic = test_result$statistic,
      p_value = test_result$p.value,
      df = test_result$parameter,
      mean_high_PGIQ = test_result$estimate[1],
      mean_low_PGIQ = test_result$estimate[2],
      N_high_PGIQ = length(vec_high),
      N_low_PGIQ = length(vec_low)
    )
  }, error = function(e) {
    # If t.test fails for any other reason, print a message and skip
    message(paste("Could not run t-test for:", outcome_var, "Reason:", e$message))
    return(NULL)
  })
}

# Apply the t-test function across all analysis variables
pgiq_comparison_results <- purrr::map_dfr(
  analysis_vars,
  ~run_ttest(high_pgiq, low_pgiq, .x)
)

# Process the final results table
if (nrow(pgiq_comparison_results) > 0) {
  pgiq_comparison_results <- pgiq_comparison_results %>%
    # Add FDR-corrected p-values for multiple testing
    mutate(p_adjusted_FDR = p.adjust(p_value, method = "fdr")) %>%
    # Join with metadata for descriptive names
    left_join(master_metadata_df, by = "Variable") %>%
    # Reorder columns for a clean report
    select(
      Category, Trait_Name, Rater,
      N_high_PGIQ, mean_high_PGIQ,
      N_low_PGIQ, mean_low_PGIQ,
      t_statistic, df, p_value, p_adjusted_FDR, Variable
    ) 
}

cat("T-test comparisons completed.\n")



# ==============================================================================
# 7. EXPORT ALL RESULTS TO EXCEL ####
# ==============================================================================

cat("\n=== EXPORTING RESULTS TO EXCEL ===\n\n")

# Create a list of all the data frames you want to save
results_list <- list(
  "Sensitivity_Age" = age_sensitivity_results,
  "Sensitivity_Other_Covariates" = all_sensitivity_results,
  "Whole_Sample_Descriptives" = whole_sample_desc,
  "Female_Descriptives" = female_desc,
  "Male_Descriptives" = male_desc,
  "MZ_Descriptives" = mz_desc,
  "DZ_Descriptives" = dz_desc,
  "High_PGIQ_Descriptives" = high_pgiq_desc,
  "Low_PGIQ_Descriptives" = low_pgiq_desc,
  "High_vs_Low_PGIQ_ttest" = pgiq_comparison_results
)

# Write the list to a single Excel file with multiple sheets
write.xlsx(results_list, file = paste0(outFileStem, "Descriptives.xlsx"), 
           colNames = TRUE, borders = "columns")

cat("All results have been saved to Descriptives.xlsx\n")
