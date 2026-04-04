# Polygenic IQ score-individual profile plots

library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

source("./PGIQ Codes/0PGIQ_VarList.R")

Edu_Achieve_Attain_Varlist <- list(
  c("gteng", "7 yr", "gpbage", "Teacher", "English"),
  c("iteng", "9 yr", "itage1", "Teacher", "English"),
  c("jteng", "10 yr", "jtqage1", "Teacher", "English"),
  c("lteng", "12 yr", "ltqage1", "Teacher", "English"),
  c("pcexgcseenggrdm", "16 yr", "pcexgcseage1", "Child", "English"),
  c("rcqalsenggrdm", "18 yr", "rcqalage1", "Child", "English"),
  c("gtmat", "7 yr", "gpbage", "Teacher", "Maths"),
  c("itmat", "9 yr", "itage1", "Teacher", "Maths"),
  c("jtmat", "10 yr", "jtqage1", "Teacher", "Maths"),
  c("ltmat", "12 yr", "ltqage1", "Teacher", "Maths"),
  c("pcexgcsematgrdm", "16 yr", "pcexgcseage1", "Child", "Maths"),
  c("rcqalsmatgrdm", "18 yr", "rcqalage1", "Child", "Maths"),
  c("itsci", "9 yr", "itage1", "Teacher", "Science"),
  c("jtsci", "10 yr", "jtqage1", "Teacher", "Science"),
  c("ltsci", "12 yr", "ltqage1", "Teacher", "Science"),
  c("pcexgcsescigrdm", "16 yr", "pcexgcseage1", "Child", "Science"),
  c("rcqalsscigrdm", "18 yr", "rcqalage1", "Child", "Science"),
  c("gt2ac", "7 yr", "gpbage", "Teacher", "Core-Subject"),
  c("it3ac", "9 yr", "itage1", "Teacher", "Core-Subject"),
  c("jt3ac", "10 yr", "jtqage1", "Teacher", "Core-Subject"),
  c("lt3ac", "12 yr", "ltqage1", "Teacher", "Core-Subject"),
  c("npks3t3a", "14 yr", "ncqage1", "Parent", "Core-Subject"), 
  c("pcexgcsecoregrdm", "16 yr", "pcexgcseage1", "Child", "Core-Subject"),
  c("rcqalsgrdm", "18 yr", "rcqalage1", "Child", "Core-Subject"),
  c("u1cedat", "21 yr", "u1cage1", "Child", "Core-Subject"),
  c("zEA", "Years of Schooling (26 yr)", "u1cage1", "Child", "Core-Subject")
)

sourceFileStem <- './Data/'
outPlotsStem <- './Plots/Extreme/'

dat_scaled <- read.csv(paste0(sourceFileStem, "PGIQ_scaled.csv"))

dat_scaled$sex1 <- ifelse(dat_scaled$sex1 == 1, "Male", ifelse(dat_scaled$sex1 == 0, "Female", NA))

dat_scaled <- dat_scaled %>%
  mutate(ses = case_when(!is.na(ases) ~ ases, is.na(ases) & !is.na(gses) ~ gses, is.na(ases) & is.na(gses) & !is.na(pses) ~ pses, is.na(ases) & is.na(gses) & is.na(pses) & !is.na(u1pses) ~ u1pses, TRUE ~ NA_real_))

scale_to_IQ_fun <- function(df, varlist) {
  df_scaled <- df
  for (var_info in varlist) {
    var_name <- paste0(var_info[1], "1") 
    if (var_name %in% names(df_scaled)) {
      mean_val <- mean(df_scaled[[var_name]], na.rm = TRUE)
      sd_val <- sd(df_scaled[[var_name]], na.rm = TRUE)
      df_scaled[[var_name]] <- ((df_scaled[[var_name]] - mean_val) / sd_val) * 15 + 100
    }
  }
  return(df_scaled)
}

dat_scaled_IQscale4cog <- scale_to_IQ_fun(dat_scaled, G_Composites_Varlist)

# =============================================================================
# DATA PREPARATION FUNCTION
# =============================================================================
extract_age <- function(age_string) { as.numeric(gsub("^([0-9]+).*", "\\1", age_string)) }

prepare_long_data_fun <- function(data_subset, varlist, include_demographics = TRUE, scale_to_IQ = FALSE) {
  phenotype_df <- do.call(rbind, lapply(varlist, function(x) {
    data.frame(variable_root = x[1], variable = paste0(x[1], "1"), age_string = x[2], age = extract_age(x[2]), age_var = x[3], population = x[4], domain = x[5], stringsAsFactors = FALSE)
  }))
  
  existing_vars <- phenotype_df$variable[phenotype_df$variable %in% names(data_subset)]
  phenotype_df_filtered <- phenotype_df[phenotype_df$variable %in% existing_vars, ]
  
  if(length(existing_vars) == 0) stop("No matching variables found in dataset")
  
  id_cols <- c("id", "ID", "subject_id", "subjectid")
  fam_cols <- c("id_fam", "family_id", "fam_id")
  demo_cols <- c("ses", "sex1")
  
  id_col <- id_cols[id_cols %in% names(data_subset)]
  fam_col <- fam_cols[fam_cols %in% names(data_subset)]
  
  cols_to_select <- existing_vars
  if(length(id_col) > 0) cols_to_select <- c(id_col[1], cols_to_select)
  if(length(fam_col) > 0) cols_to_select <- c(fam_col[1], cols_to_select)
  if(include_demographics) { cols_to_select <- c(cols_to_select, demo_cols[demo_cols %in% names(data_subset)]) }
  
  if(length(fam_col) > 0) {
    family_mapping <- data_subset %>% select(!!sym(fam_col[1])) %>% distinct() %>% arrange(!!sym(fam_col[1])) %>% mutate(family_num = row_number())
    plot_data <- data_subset %>% select(all_of(cols_to_select)) %>% left_join(family_mapping, by = fam_col[1]) %>% arrange(family_num, !!sym(fam_col[1])) %>% group_by(!!sym(fam_col[1])) %>% mutate(family_size = n(), twin_letter = if_else(family_size > 1, LETTERS[row_number()], ""), subject_label = if_else(family_size > 1, paste0("Family ", family_num, twin_letter), paste0("Family ", family_num))) %>% ungroup() %>% mutate(subject_id = row_number()) %>% arrange(family_num, twin_letter)
  } else {
    plot_data <- data_subset %>% select(all_of(cols_to_select)) %>% mutate(subject_id = row_number(), subject_label = paste("Subject", sprintf("%02d", row_number())))
  }
  
  plot_data_long <- plot_data %>% pivot_longer(cols = all_of(existing_vars), names_to = "variable", values_to = "score") %>% left_join(phenotype_df_filtered, by = "variable") %>% filter(!is.na(score))
  
  if (scale_to_IQ) { plot_data_long <- plot_data_long %>% group_by(variable) %>% mutate(score = (score - mean(score, na.rm = TRUE)) / sd(score, na.rm = TRUE) * 15 + 100) %>% ungroup() }
  
  return(list(plot_data = plot_data, plot_data_long = plot_data_long, phenotype_df_filtered = phenotype_df_filtered))
}

# Create data subsets 
highIQ_subset <- dat_scaled_IQscale4cog %>% filter(!!sym("PGgEA_IQscale") > 145)
lowIQ_subset <- dat_scaled_IQscale4cog %>% filter(!!sym("PGgEA_IQscale") < 55)

highIQ_cog <- prepare_long_data_fun(highIQ_subset, G_Composites_Varlist, include_demographics = TRUE)
highIQ_edu <- prepare_long_data_fun(highIQ_subset, Edu_Achieve_Attain_Varlist, include_demographics = TRUE)
highIQ_SDQ <- prepare_long_data_fun(highIQ_subset, SDQ_Varlist_oneRater, include_demographics = TRUE)
highIQ_ADHD <- prepare_long_data_fun(highIQ_subset, Conners_Varlist_oneRater, include_demographics = TRUE)
highIQ_ARBQ <- prepare_long_data_fun(highIQ_subset, Anxiety_Varlist_oneRater, include_demographics = TRUE)
highIQ_anthro <- prepare_long_data_fun(highIQ_subset, Anthro_Varlist, include_demographics = TRUE)

all_cog <- prepare_long_data_fun(dat_scaled_IQscale4cog, G_Composites_Varlist, include_demographics = FALSE, scale_to_IQ = TRUE)
all_edu <- prepare_long_data_fun(dat_scaled_IQscale4cog, Edu_Achieve_Attain_Varlist, include_demographics = FALSE)
all_SDQ <- prepare_long_data_fun(dat_scaled_IQscale4cog, SDQ_Varlist_oneRater, include_demographics = FALSE)
all_ADHD <- prepare_long_data_fun(dat_scaled_IQscale4cog, Conners_Varlist_oneRater, include_demographics = FALSE)
all_ARBQ <- prepare_long_data_fun(dat_scaled_IQscale4cog, Anxiety_Varlist_oneRater, include_demographics = FALSE)
all_anthro <- prepare_long_data_fun(dat_scaled_IQscale4cog, Anthro_Varlist, include_demographics = FALSE)

lowIQ_cog <- prepare_long_data_fun(lowIQ_subset, G_Composites_Varlist, include_demographics = TRUE)
lowIQ_edu <- prepare_long_data_fun(lowIQ_subset, Edu_Achieve_Attain_Varlist, include_demographics = TRUE)
lowIQ_SDQ <- prepare_long_data_fun(lowIQ_subset, SDQ_Varlist_oneRater, include_demographics = TRUE)
lowIQ_ADHD <- prepare_long_data_fun(lowIQ_subset, Conners_Varlist_oneRater, include_demographics = TRUE)
lowIQ_ARBQ <- prepare_long_data_fun(lowIQ_subset, Anxiety_Varlist_oneRater, include_demographics = TRUE)
lowIQ_anthro <- prepare_long_data_fun(lowIQ_subset, Anthro_Varlist, include_demographics = TRUE)

# =============================================================================
# PLOT 1: Individual Profiles for the Target Group
# =============================================================================
plot_individual_profiles <- function(plot_data, plot_data_long, title = "Individual Profile Plot", subtitle = NULL, filename = "individual_profiles.png", include_demographics = TRUE, baseline = 0) {
  
  if(include_demographics && any(c("ses", "sex1") %in% names(plot_data))) {
    demo_labels <- plot_data %>% select(subject_label, any_of(c("ses", "sex1"))) %>% mutate(demo_text = paste(if("sex1" %in% names(plot_data)) paste("Sex:", sex1) else "", if("ses" %in% names(plot_data)) paste("SES:", round(ses, 2)) else "", sep = ifelse("sex1" %in% names(plot_data) & "ses" %in% names(plot_data), "\n", ""))) %>% select(subject_label, demo_text)
  } else { demo_labels <- plot_data %>% select(subject_label) %>% mutate(demo_text = "") }
  
  if(is.null(subtitle)) subtitle <- paste("N =", length(unique(plot_data_long$subject_id)), "individuals grouped by family | Black line = population average")
  
  p <- ggplot(plot_data_long, aes(x = age, y = score, color = domain)) + geom_hline(yintercept = baseline, color = "black", linetype = "solid", size = 0.5, alpha = 0.7) + geom_line(aes(group = interaction(subject_id, domain)), alpha = 0.8, size = 0.5) + geom_point(alpha = 0.8, size = 2) + facet_wrap(~factor(subject_label, levels = unique(plot_data$subject_label)), ncol = 5) + scale_color_brewer(type = "qual", palette = "Set2", name = "Cognitive Domain") + scale_x_continuous(breaks = sort(unique(plot_data_long$age))) + labs(title = title, subtitle = subtitle, x = "Age (years)", y = "Standardised Phenotypic Scores") + theme_bw() + theme(axis.text.x = element_text(hjust = 1, size = 9), strip.text = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title = element_text(size = 14), legend.text = element_text(size = 14), legend.position = "bottom")
  
  if(include_demographics && nrow(demo_labels) > 0 && any(demo_labels$demo_text != "")) p <- p + geom_text(data = demo_labels, aes(label = demo_text, x = Inf, y = Inf), hjust = 1.1, vjust = 1.1, size = 5, color = "black", inherit.aes = FALSE)
  print(p)
  ggsave(paste0(outPlotsStem, filename), p, width = 15, height = 12, dpi = 300)
  return(p)
}

p1 <- plot_individual_profiles(highIQ_cog$plot_data, highIQ_cog$plot_data_long, title = "Individual Cognitive Profiles: High Polygenic g Score (>145)", filename = "highIQprofile_cog.png", include_demographics = TRUE, baseline = 100)
p2 <- plot_individual_profiles(highIQ_edu$plot_data, highIQ_edu$plot_data_long, title = "Individual Educational Profiles: High Polygenic g Score (>145)", filename = "highIQprofile_edu.png", include_demographics = TRUE)
p3 <- plot_individual_profiles(highIQ_SDQ$plot_data, highIQ_SDQ$plot_data_long, title = "Individual SDQ Profiles: High Polygenic g Score (>145)", filename = "highIQprofile_SDQ.png", include_demographics = TRUE)
p4 <- plot_individual_profiles(highIQ_ADHD$plot_data, highIQ_ADHD$plot_data_long, title = "Individual ADHD Profiles: High Polygenic g Score (>145)", filename = "highIQprofile_ADHD.png", include_demographics = TRUE)
p5 <- plot_individual_profiles(highIQ_ARBQ$plot_data, highIQ_ARBQ$plot_data_long, title = "Individual Anxiety Profiles: High Polygenic g Score (>145)", filename = "highIQprofile_ARBQ.png", include_demographics = TRUE)
p6 <- plot_individual_profiles(highIQ_anthro$plot_data, highIQ_anthro$plot_data_long, title = "Individual Anthropometric Profiles: High Polygenic g Score (>145)", filename = "highIQprofile_anthro.png", include_demographics = TRUE)

q1 <- plot_individual_profiles(lowIQ_cog$plot_data, lowIQ_cog$plot_data_long, title = "Individual Cognitive Profiles: Low Polygenic g Score (<55)", filename = "lowIQprofile_cog.png", include_demographics = TRUE, baseline = 100)
q2 <- plot_individual_profiles(lowIQ_edu$plot_data, lowIQ_edu$plot_data_long, title = "Individual Educational Profiles: Low Polygenic g Score (<55)", filename = "lowIQprofile_edu.png", include_demographics = TRUE)
q3 <- plot_individual_profiles(lowIQ_SDQ$plot_data, lowIQ_SDQ$plot_data_long, title = "Individual SDQ Profiles: Low Polygenic g Score (<55)", filename = "lowIQprofile_SDQ.png", include_demographics = TRUE)
q4 <- plot_individual_profiles(lowIQ_ADHD$plot_data, lowIQ_ADHD$plot_data_long, title = "Individual ADHD Profiles: Low Polygenic g Score (<55)", filename = "lowIQprofile_ADHD.png", include_demographics = TRUE)
q5 <- plot_individual_profiles(lowIQ_ARBQ$plot_data, lowIQ_ARBQ$plot_data_long, title = "Individual Anxiety Profiles: Low Polygenic g Score (<55)", filename = "lowIQprofile_ARBQ.png", include_demographics = TRUE)
q6 <- plot_individual_profiles(lowIQ_anthro$plot_data, lowIQ_anthro$plot_data_long, title = "Individual Anthropometric Profiles: Low Polygenic g Score (<55)", filename = "lowIQprofile_anthro.png", include_demographics = TRUE)

# =============================================================================
# PLOT 2: Mean Trajectories for target groups
# =============================================================================
plot_mean_trajectories <- function(plot_data_long, group_name = "Group", title = NULL, subtitle = NULL, filename = "mean_trajectories.png", baseline = 0) {
  
  mean_data <- plot_data_long %>% group_by(age, domain, PGgS_Group) %>% summarise(mean_score = mean(score, na.rm = TRUE), se_score = sd(score, na.rm = TRUE) / sqrt(n()), n_obs = n(), .groups = 'drop')
  if(is.null(title)) title <- paste("Mean Cognitive Trajectories:", group_name)
  if(is.null(subtitle)) subtitle <- "Shadow represents 95% CIs"
  
  p <- ggplot(mean_data, aes(x = age, y = mean_score, color = domain, linetype = PGgS_Group, shape = PGgS_Group)) + geom_hline(yintercept = baseline, color = "black", size = 0.5, alpha = 0.7) + geom_ribbon(aes(ymin = mean_score - 1.96*se_score, ymax = mean_score + 1.96*se_score, fill = domain), alpha = 0.2, color = NA) + geom_line(size = 1.2) + geom_point(size = 3) + scale_color_brewer(type = "qual", palette = "Set2", name = "Trait") + scale_fill_brewer(type = "qual", palette = "Set2", name = "Trait") + scale_linetype_discrete(name = NULL, labels = c("PGgS > 145", "PGgS < 55")) + scale_shape_discrete(name = NULL, labels = c("PGgS > 145", "PGgS < 55")) + scale_x_continuous(breaks = sort(unique(plot_data_long$age))) + labs(title = title, subtitle = subtitle, x = "Age (years)", y = "Standardised Score") + theme_bw() + theme(axis.text.x = element_text(hjust = 1, size = 14), axis.text.y = element_text(size = 14), axis.title = element_text(size = 14), legend.text = element_text(size = 14), legend.position = "bottom")
  print(p)
  ggsave(paste0(outPlotsStem, filename), p, width = 10, height = 6, dpi = 300)
  return(p)
}

highIQ_cog$plot_data_long$PGgS_Group <- "High (>145)"
lowIQ_cog$plot_data_long$PGgS_Group <- "Low (<55)"
combined_cog_long <- rbind(highIQ_cog$plot_data_long, lowIQ_cog$plot_data_long)
p7 <- plot_mean_trajectories(combined_cog_long, title = "Cognitive Outcomes", filename = "combined_mean_cog.png", baseline = 100)

highIQ_edu$plot_data_long$PGgS_Group <- "High (>145)"
lowIQ_edu$plot_data_long$PGgS_Group <- "Low (<55)"
combined_edu_long <- rbind(highIQ_edu$plot_data_long, lowIQ_edu$plot_data_long)
p8 <- plot_mean_trajectories(combined_edu_long, title = "Educational Outcomes", filename = "combined_mean_edu.png")

highIQ_SDQ$plot_data_long$PGgS_Group <- "High (>145)"
lowIQ_SDQ$plot_data_long$PGgS_Group <- "Low (<55)"
combined_SDQ_long <- rbind(highIQ_SDQ$plot_data_long, lowIQ_SDQ$plot_data_long)
p9 <- plot_mean_trajectories(combined_SDQ_long, title = "Behavioural Outcomes (SDQ)", filename = "combined_mean_SDQ.png")

highIQ_ADHD$plot_data_long$PGgS_Group <- "High (>145)"
lowIQ_ADHD$plot_data_long$PGgS_Group <- "Low (<55)"
combined_ADHD_long <- rbind(highIQ_ADHD$plot_data_long, lowIQ_ADHD$plot_data_long)
p10 <- plot_mean_trajectories(combined_ADHD_long, title = "ADHD Symptoms", filename = "combined_mean_ADHD.png")

highIQ_ARBQ$plot_data_long$PGgS_Group <- "High (>145)"
lowIQ_ARBQ$plot_data_long$PGgS_Group <- "Low (<55)"
combined_ARBQ_long <- rbind(highIQ_ARBQ$plot_data_long, lowIQ_ARBQ$plot_data_long)
p11 <- plot_mean_trajectories(combined_ARBQ_long, title = "ARBQ Symptoms", filename = "combined_mean_ARBQ.png")

highIQ_anthro$plot_data_long$PGgS_Group <- "High (>145)"
lowIQ_anthro$plot_data_long$PGgS_Group <- "Low (<55)"
combined_anthro_long <- rbind(highIQ_anthro$plot_data_long, lowIQ_anthro$plot_data_long)
p12 <- plot_mean_trajectories(combined_anthro_long, title = "Height and BMI", filename = "combined_mean_anthro.png")

# =============================================================================
# PLOT 3: Heatmap for target Group
# =============================================================================
plot_heatmap <- function(plot_data, plot_data_long, title = "Heatmap of Cognitive Abilities by Individual and Age", subtitle = NULL, filename = "heatmap.png", baseline = 0) {
  heatmap_long <- plot_data_long %>% select(subject_id, subject_label, age, domain, score) %>% filter(!is.na(score))
  p <- ggplot(heatmap_long, aes(x = factor(age), y = factor(subject_label, levels = rev(unique(plot_data$subject_label))), fill = score)) + geom_tile(color = "white", size = 0.1) + facet_wrap(~domain, scales = "free_x") + scale_fill_gradient2(low = "#F8766D", mid = "#FFFACD", high = "#00BFC4", midpoint = baseline, name = "Score") + labs(title = title, subtitle = subtitle, x = "Age (years)", y = "Individual") + theme_bw() + theme(axis.text.x = element_text(hjust = 1), axis.text.y = element_text(size = 14), axis.title = element_text(size = 14), strip.text = element_text(size = 14), panel.grid = element_blank())
  print(p)
  ggsave(paste0(outPlotsStem, filename), p, width = 8, height = 8, dpi = 300)
  return(p)
}

p13 <- plot_heatmap(highIQ_cog$plot_data, highIQ_cog$plot_data_long, title = "Heatmap of Cognitive Abilities by Individual and Age", filename = "highIQheat_cog.png", baseline = 100)
p14 <- plot_heatmap(highIQ_edu$plot_data, highIQ_edu$plot_data_long, title = "Heatmap of Educational Outcomes by Individual and Age", filename = "highIQheat_edu.png")
p15 <- plot_heatmap(highIQ_SDQ$plot_data, highIQ_SDQ$plot_data_long, title = "Heatmap of SDQ Scores by Individual and Age", filename = "highIQheat_SDQ.png")
p16 <- plot_heatmap(highIQ_ADHD$plot_data, highIQ_ADHD$plot_data_long, title = "Heatmap of ADHD Scores by Individual and Age", filename = "highIQheat_ADHD.png")
p17 <- plot_heatmap(highIQ_ARBQ$plot_data, highIQ_ARBQ$plot_data_long, title = "Heatmap of Anxiety Scores by Individual and Age", filename = "highIQheat_ARBQ.png")
p18 <- plot_heatmap(highIQ_anthro$plot_data, highIQ_anthro$plot_data_long, title = "Heatmap of BMI and Height by Individual and Age", filename = "highIQheat_anthro.png")

q13 <- plot_heatmap(lowIQ_cog$plot_data, lowIQ_cog$plot_data_long, title = "Heatmap of Cognitive Abilities by Individual and Age", filename = "lowIQheat_cog.png", baseline = 100)
q14 <- plot_heatmap(lowIQ_edu$plot_data, lowIQ_edu$plot_data_long, title = "Heatmap of Educational Outcomes by Individual and Age", filename = "lowIQheat_edu.png")
q15 <- plot_heatmap(lowIQ_SDQ$plot_data, lowIQ_SDQ$plot_data_long, title = "Heatmap of SDQ Scores by Individual and Age", filename = "lowIQheat_SDQ.png")
q16 <- plot_heatmap(lowIQ_ADHD$plot_data, lowIQ_ADHD$plot_data_long, title = "Heatmap of ADHD Scores by Individual and Age", filename = "lowIQheat_ADHD.png")
q17 <- plot_heatmap(lowIQ_ARBQ$plot_data, lowIQ_ARBQ$plot_data_long, title = "Heatmap of Anxiety Scores by Individual and Age", filename = "lowIQheat_ARBQ.png")
q18 <- plot_heatmap(lowIQ_anthro$plot_data, lowIQ_anthro$plot_data_long, title = "Heatmap of BMI and Height by Individual and Age", filename = "lowIQheat_anthro.png")
