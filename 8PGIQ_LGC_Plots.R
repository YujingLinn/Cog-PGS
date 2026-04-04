# Automated LGC Trajectory Plotting (Rank-Order/Modelled/Predicted Change / Scaled Data)

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(psych)

# =============================================================================
# 1. Directory Setup & Data Loading
# =============================================================================
base_dir <- "./"
code_dir <- file.path(base_dir, "PGIQ Codes")
results_dir <- file.path(base_dir, "Results/LGC")

source(file.path(code_dir, "0PGIQ_VarList.R"))

whole_sample <- read_csv(file.path(results_dir, "LGCM_WholeSample_Results.csv"))
sex_stratified <- read_csv(file.path(results_dir, "LGCM_SexStratified_Results.csv"))

desired_order <- c(
  "g", "Verbal", "Nonverbal", "English", "Maths", "Science", "Core_Subject",
  "Conduct", "Emotion", "Hyperactivity_SDQ", "Peer_Problems", "Prosocial", "Total_Problems",
  "Shyness", "Fear", "OCB", "Negative_Affect", "Negative_Cognition", "Anxiety",
  "Inattention", "Hyperactivity_Impulsivity", "ADHD",
  "Height", "BMI"
)

whole_sample <- whole_sample %>% mutate(Variable = factor(Variable, levels = desired_order)) %>% arrange(Variable)
sex_stratified <- sex_stratified %>% mutate(Variable = factor(Variable, levels = desired_order)) %>% arrange(Variable)

get_clean_title <- function(trait) {
  title_map <- c("g" = "General Cognitive Ability", "Verbal" = "Verbal Abilities", "Nonverbal" = "Nonverbal Abilities", "English" = "English Grades", "Maths" = "Maths Grades", "Science" = "Science Grades", "Core_Subject" = "Core Subject Grades", "Conduct" = "SDQ Conduct", "Emotion" = "SDQ Emotion", "Hyperactivity_SDQ" = "SDQ Hyperactivity_SDQ", "Peer_Problems" = "SDQ Peer Problems", "Prosocial" = "SDQ Prosocial", "Total_Problems" = "SDQ Total Problems", "Shyness" = "ARBQ Shyness", "Fear" = "ARBQ Fear", "OCB" = "ARBQ Obsessive-Compulsive Behaviours", "Negative_Affect" = "ARBQ Negative Affect", "Negative_Cognition" = "ARBQ Negative Cognition", "Anxiety" = "ARBQ Anxiety Total", "Inattention" = "Conners ADHD Inattention", "Hyperactivity_Impulsivity" = "Conners ADHD Hyperactivity_Impulsivity", "ADHD" = "Conners ADHD Total", "Height" = "Height", "BMI" = "BMI (as weight at birth)")
  if (trait %in% names(title_map)) { return(title_map[[trait]]) } else { return(trait) }
}
  
model_configs <- list(
  "g" = list(G_Composites_Varlist, "g"), "Verbal" = list(G_Composites_Varlist, "verbal ability"), "Nonverbal" = list(G_Composites_Varlist, "nonverbal ability"), "English" = list(Edu_Achieve_Attain_Varlist, "English Achievement"), "Maths" = list(Edu_Achieve_Attain_Varlist, "Maths Achievement"), "Science" = list(Edu_Achieve_Attain_Varlist, "Science Achievement"), "Core_Subject" = list(Edu_Achieve_Attain_Varlist, "Core-Subject Achievement"), "Conduct" = list(SDQ_Varlist_oneRater, "Conduct"), "Emotion" = list(SDQ_Varlist_oneRater, "Emotion"), "Hyperactivity_SDQ" = list(SDQ_Varlist_oneRater, "Hyperactivity"), "Peer_Problems" = list(SDQ_Varlist_oneRater, "Peer Problems"), "Prosocial" = list(SDQ_Varlist_oneRater, "Prosocial"), "Total_Problems" = list(SDQ_Varlist_oneRater, "Total Problems"), "Shyness" = list(Anxiety_Varlist_oneRater, "Shyness"), "Fear" = list(Anxiety_Varlist_oneRater, "Fear"), "OCB" = list(Anxiety_Varlist_oneRater, "OCB"), "Negative_Affect" = list(Anxiety_Varlist_oneRater, "Negative Affect"), "Negative_Cognition" = list(Anxiety_Varlist_oneRater, "Negative Cognition"), "Anxiety" = list(Anxiety_Varlist_oneRater, "Anxiety Total"), "Inattention" = list(Conners_Varlist_oneRater, "Inattention"), "Hyperactivity_Impulsivity" = list(Conners_Varlist_oneRater, "Hyper-Impuls"), "ADHD" = list(Conners_Varlist_oneRater, "ADHD Total"), "Height" = list(Anthro_Varlist, "Height"), "BMI" = list(Anthro_Varlist, "BMI")
)

# =============================================================================
# 2. Plotting Whole Sample Trajectories
# =============================================================================
pdf(file.path(results_dir, "LGCM_WholeSample_Plots.pdf"), width = 8, height = 6)
whole_counter <- 1

for (i in 1:nrow(whole_sample)) {
  trait <- whole_sample$Variable[i]
  if (is.na(whole_sample$Intercept_Beta[i])) next
  
  cfg <- model_configs[[trait]]
  trait_specific_list <- Filter(function(x) x[5] == cfg[[2]], cfg[[1]])
  age_strings <- sapply(trait_specific_list, `[`, 2)
  actual_ages <- as.numeric(str_extract(age_strings, "\\d+"))
  actual_ages[grepl("birth", age_strings, ignore.case = TRUE)] <- 0
  
  int_beta <- whole_sample$Intercept_Beta[i]
  slp_beta <- ifelse(is.na(whole_sample$Slope_Beta[i]), 0, whole_sample$Slope_Beta[i])
  
  plot_data <- expand.grid(Actual_Age = actual_ages, PGgS_Level = c("High PGgS (+1 SD)", "Average PGgS", "Low PGgS (-1 SD)")) %>%
    mutate(Time = Actual_Age - min(Actual_Age), PGS_Score = case_when(PGgS_Level == "High PGgS (+1 SD)" ~ 1, PGgS_Level == "Average PGgS" ~ 0, PGgS_Level == "Low PGgS (-1 SD)" ~ -1), Predicted = (int_beta * PGS_Score) + (slp_beta * PGS_Score) * Time)
  
  plot_title <- paste0("Figure S10_", whole_counter, " Predicted Trajectory: ", get_clean_title(trait))
  p <- ggplot(plot_data, aes(x = Actual_Age, y = Predicted, color = PGgS_Level, linetype = PGgS_Level)) + geom_line(linewidth = 1.2) + geom_point(size = 3) + geom_hline(yintercept = 0, color = "grey50", linetype = "dotted") + scale_x_continuous(breaks = actual_ages) + scale_color_manual(values = c("High PGgS (+1 SD)" = "#542788", "Average PGgS" = "#8073AC", "Low PGgS (-1 SD)" = "#C2A5CF")) + scale_linetype_manual(values = c("High PGgS (+1 SD)" = "solid", "Average PGgS" = "dashed", "Low PGgS (-1 SD)" = "solid")) + theme_classic() + labs(title = plot_title, x = "Age (Years)", y = "Standardised Outcome (SD Units)", color = "PGgS Levels", linetype = "PGgS Levels") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
  print(p)
  whole_counter <- whole_counter + 1
}
dev.off()

# =============================================================================
# 3. Plotting Sex-Stratified Trajectories (Faceted)
# =============================================================================
pdf(file.path(results_dir, "LGCM_SexStratified_Plots.pdf"), width = 10, height = 6)
sex_counter <- 1

for (i in 1:nrow(sex_stratified)) {
  trait <- sex_stratified$Variable[i]
  if (is.na(sex_stratified$Female_Intercept_Beta[i])) next
  
  cfg <- model_configs[[trait]]
  trait_specific_list <- Filter(function(x) x[5] == cfg[[2]], cfg[[1]])
  age_strings <- sapply(trait_specific_list, `[`, 2)
  actual_ages <- as.numeric(str_extract(age_strings, "\\d+"))
  actual_ages[grepl("birth", age_strings, ignore.case = TRUE)] <- 0
  
  f_int_beta <- sex_stratified$Female_Intercept_Beta[i]
  f_slp_beta <- ifelse(is.na(sex_stratified$Female_Slope_Beta[i]), 0, sex_stratified$Female_Slope_Beta[i])
  m_int_beta <- sex_stratified$Male_Intercept_Beta[i]
  m_slp_beta <- ifelse(is.na(sex_stratified$Male_Slope_Beta[i]), 0, sex_stratified$Male_Slope_Beta[i])
  
  f_data <- expand.grid(Actual_Age = actual_ages, PGgS_Level = c("High PGgS (+1 SD)", "Average PGgS", "Low PGgS (-1 SD)")) %>% mutate(Sex = "Females", Time = Actual_Age - min(Actual_Age), PGS_Score = case_when(grepl("High", PGgS_Level) ~ 1, grepl("Average", PGgS_Level) ~ 0, grepl("Low", PGgS_Level) ~ -1), Predicted = (f_int_beta * PGS_Score) + (f_slp_beta * PGS_Score) * Time)
  m_data <- expand.grid(Actual_Age = actual_ages, PGgS_Level = c("High PGgS (+1 SD)", "Average PGgS", "Low PGgS (-1 SD)")) %>% mutate(Sex = "Males", Time = Actual_Age - min(Actual_Age), PGS_Score = case_when(grepl("High", PGgS_Level) ~ 1, grepl("Average", PGgS_Level) ~ 0, grepl("Low", PGgS_Level) ~ -1), Predicted = (m_int_beta * PGS_Score) + (m_slp_beta * PGS_Score) * Time)
  plot_data <- bind_rows(f_data, m_data)
  
  plot_title <- paste0("Figure S12_", sex_counter, " Predicted Trajectory: ", get_clean_title(trait))
  p <- ggplot(plot_data, aes(x = Actual_Age, y = Predicted, color = PGgS_Level, linetype = PGgS_Level)) + geom_line(linewidth = 1.2) + geom_point(size = 3) + geom_hline(yintercept = 0, color = "grey50", linetype = "dotted") + facet_wrap(~ Sex) + scale_x_continuous(breaks = sort(unique(actual_ages))) + scale_color_manual(values = c("High PGgS (+1 SD)" = "#542788", "Average PGgS" = "#8073AC", "Low PGgS (-1 SD)" = "#C2A5CF")) + scale_linetype_manual(values = c("High PGgS (+1 SD)" = "solid", "Average PGgS" = "dashed", "Low PGgS (-1 SD)" = "solid")) + theme_bw() + labs(title = plot_title, x = "Age (Years)", y = "Standardised Outcome (SD Units)", color = "PGgS Levels", linetype = "PGgS Levels") + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), strip.text = element_text(size = 12, face = "bold"), legend.position = "bottom")
  print(p)
  sex_counter <- sex_counter + 1
}
dev.off()

# =============================================================================
# Empirical/Observed Growth Curve Plots
# =============================================================================
data_dir <- file.path(base_dir, "Data")
dat_scaled <- read_csv(file.path(data_dir, "PGIQ_scaled.csv")) %>% filter(!is.na(PGgS))

dat_scaled <- dat_scaled %>% mutate(PGgS_Group = case_when(PGgS >= 1 ~ "High PGgS (+1 SD)", PGgS <= -1 ~ "Low PGgS (-1 SD)", TRUE ~ "Average PGgS"), PGgS_Group = factor(PGgS_Group, levels = c("High PGgS (+1 SD)", "Average PGgS", "Low PGgS (-1 SD)")), Sex = case_when(sex1 == 0 ~ "Females", sex1 == 1 ~ "Males"))

pdf(file.path(results_dir, "Empirical_WholeSample_Plots.pdf"), width = 8, height = 6)
emp_whole_counter <- 1
for (trait in desired_order) {
  cfg <- model_configs[[trait]]
  trait_specific_list <- Filter(function(x) x[5] == cfg[[2]], cfg[[1]])
  var_names <- paste0(sapply(trait_specific_list, `[`, 1), "1")
  age_strings <- sapply(trait_specific_list, `[`, 2)
  actual_ages <- as.numeric(str_extract(age_strings, "\\d+"))
  actual_ages[grepl("birth", age_strings, ignore.case = TRUE)] <- 0
  age_map <- setNames(actual_ages, var_names)
  available_vars <- var_names[var_names %in% names(dat_scaled)]
  if (length(available_vars) == 0) next
  
  plot_data <- dat_scaled %>% select(id_twin, PGgS_Group, all_of(available_vars)) %>% pivot_longer(cols = all_of(available_vars), names_to = "Variable", values_to = "Raw_Score") %>% filter(!is.na(Raw_Score)) %>% mutate(Actual_Age = age_map[Variable]) %>% group_by(Actual_Age) %>% mutate(Standardized_Score = scale(Raw_Score)[, 1]) %>% group_by(PGgS_Group, Actual_Age) %>% summarise(Mean_Score = mean(Standardized_Score, na.rm = TRUE), SE = sd(Standardized_Score, na.rm = TRUE) / sqrt(n()), .groups = "drop")
  
  plot_title <- paste0("Figure S11_", emp_whole_counter, " Observed Trajectory: ", get_clean_title(trait))
  p <- ggplot(plot_data, aes(x = Actual_Age, y = Mean_Score, color = PGgS_Group, linetype = PGgS_Group)) + geom_line(linewidth = 1.2) + geom_point(size = 3) + geom_errorbar(aes(ymin = Mean_Score - SE, ymax = Mean_Score + SE), width = 0.5, alpha = 0.5) + geom_hline(yintercept = 0, color = "grey50", linetype = "dotted") + scale_x_continuous(breaks = sort(unique(actual_ages))) + scale_color_manual(values = c("High PGgS (+1 SD)" = "#1B7837", "Average PGgS" = "#5AAE61", "Low PGgS (-1 SD)" = "#A6DBA0")) + scale_linetype_manual(values = c("High PGgS (+1 SD)" = "solid", "Average PGgS" = "dashed", "Low PGgS (-1 SD)" = "solid")) + theme_classic() + labs(title = plot_title, x = "Age (Years)", y = "Observed Mean (Standardised Score)", color = "PGgS Levels", linetype = "PGgS Levels") + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12), legend.position = "bottom")
  print(p)
  emp_whole_counter <- emp_whole_counter + 1
}
dev.off()

pdf(file.path(results_dir, "Empirical_SexStratified_Plots.pdf"), width = 10, height = 6)
emp_sex_counter <- 1
for (trait in desired_order) {
  cfg <- model_configs[[trait]]
  trait_specific_list <- Filter(function(x) x[5] == cfg[[2]], cfg[[1]])
  var_names <- paste0(sapply(trait_specific_list, `[`, 1), "1")
  age_strings <- sapply(trait_specific_list, `[`, 2)
  actual_ages <- as.numeric(str_extract(age_strings, "\\d+"))
  actual_ages[grepl("birth", age_strings, ignore.case = TRUE)] <- 0
  age_map <- setNames(actual_ages, var_names)
  available_vars <- var_names[var_names %in% names(dat_scaled)]
  if (length(available_vars) == 0) next
  
  plot_data <- dat_scaled %>% filter(!is.na(Sex)) %>% select(id_twin, PGgS_Group, Sex, all_of(available_vars)) %>% pivot_longer(cols = all_of(available_vars), names_to = "Variable", values_to = "Raw_Score") %>% filter(!is.na(Raw_Score)) %>% mutate(Actual_Age = age_map[Variable]) %>% group_by(Actual_Age, Sex) %>% mutate(Standardized_Score = scale(Raw_Score)[, 1]) %>% group_by(PGgS_Group, Actual_Age, Sex) %>% summarise(Mean_Score = mean(Standardized_Score, na.rm = TRUE), .groups = "drop")
  
  plot_title <- paste0("Figure S13_", emp_sex_counter, " Observed Trajectory: ", get_clean_title(trait))
  p <- ggplot(plot_data, aes(x = Actual_Age, y = Mean_Score, color = PGgS_Group, linetype = PGgS_Group)) + geom_line(linewidth = 1.2) + geom_point(size = 3) + geom_hline(yintercept = 0, color = "grey50", linetype = "dotted") + facet_wrap(~ Sex) + scale_x_continuous(breaks = sort(unique(actual_ages))) + scale_color_manual(values = c("High PGgS (+1 SD)" = "#1B7837", "Average PGgS" = "#5AAE61", "Low PGgS (-1 SD)" = "#A6DBA0")) + scale_linetype_manual(values = c("High PGgS (+1 SD)" = "solid", "Average PGgS" = "dashed", "Low PGgS (-1 SD)" = "solid")) + theme_bw() + labs(title = plot_title, x = "Age (Years)", y = "Observed Mean (Standardised Score)", color = "PGgS Levels", linetype = "PGgS Levels") + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12), strip.text = element_text(size = 12, face = "bold"), legend.position = "bottom")
  print(p)
  emp_sex_counter <- emp_sex_counter + 1
}
dev.off()
