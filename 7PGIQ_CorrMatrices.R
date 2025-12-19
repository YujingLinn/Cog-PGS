# Title: Correlation Matrix per Varlist
# Author: Yujing Lin
# Date: 10 October, 2025

library(corrplot)
library(dplyr)
library(stringr)

source("/Users/yujinglin/Desktop/PGIQ Codes/0PGIQ_VarList.R")
sourceFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Data220925/'
dat_scaled <- read.csv(paste0(sourceFileStem, "PGIQ_scaled.csv"))

setwd("/Users/yujinglin/Desktop/polygenic IQ score/Plots220925/CorrMatrix/")

# should be 9 plots in total; plus 1 would be the common latent factor one 

# the custom label function ####
create_labels <- function(var_info_list) {
  ages_raw <- sapply(var_info_list, function(x) x[2])
  raters <- sapply(var_info_list, function(x) x[4])
  categories <- sapply(var_info_list, function(x) x[5])
  
  # Extract simplified ages for counting duplicates
  ages <- str_extract(ages_raw, "\\d+\\s*yr|birth")
  ages <- ifelse(is.na(ages), ages_raw, ages)
  
  age_counts <- table(ages)
  
  labels <- c()
  
  for (i in 1:length(var_info_list)) {
    current_age <- ages[i]
    current_age_raw <- ages_raw[i]
    current_rater <- raters[i]
    current_category <- categories[i]
    
    # Check if the raw age is just "X yr" or "birth" (simple format)
    is_simple_age <- grepl("^\\d+\\s*yr$|^birth$", trimws(current_age_raw))
    
    if (age_counts[current_age] == 1) {
      # Age is unique
      if (is_simple_age) {
        # Simple age format: include rater and category
        labels <- c(labels, paste0(current_age, " (", current_rater, ") - ", current_category))
      } else {
        # Complex age format: include rater only
        labels <- c(labels, paste0(current_age_raw, " (", current_rater, ")"))
      }
    } else {
      # Age is not unique, check rater
      age_rater_pairs <- paste(ages, raters, sep = "_")
      age_rater_counts <- table(age_rater_pairs)
      current_age_rater_pair <- paste(current_age, current_rater, sep = "_")
      
      if (age_rater_counts[current_age_rater_pair] == 1) {
        # Age-rater pair is unique
        if (is_simple_age) {
          labels <- c(labels, paste0(current_age, " (", current_rater, ")"))
        } else {
          labels <- c(labels, paste0(current_age_raw, " (", current_rater, ")"))
        }
      } else {
        # Age-rater is not unique, add category
        if (is_simple_age) {
          labels <- c(labels, paste0(current_age, " (", current_rater, ") - ", current_category))
        } else {
          labels <- c(labels, paste0(current_age_raw, " (", current_rater, ") - ", current_category))
        }
      }
    }
  }
  
  return(labels)
}

# the corrplot function with PGIQ variable and customizable text sizes ####
create_correlation_plot <- function(varlist, dat_scaled, plot_title = NULL, var_suffix = "", 
                                    include_pgiq = TRUE, rotation_angle = 45, label_size = 1, number_size = 1) {
  # Extract variable names and add suffix
  variables_to_correlate <- sapply(varlist, function(x) paste0(x[1], var_suffix))
  
  # Add PGIQ if requested
  if (include_pgiq) {
    variables_to_correlate <- c("PGIQ", variables_to_correlate)
  }
  
  # Check for missing variables
  missing_vars <- variables_to_correlate[!variables_to_correlate %in% names(dat_scaled)]
  
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in dataframe:", paste(missing_vars, collapse = ", ")))
  }
  
  # Subset the dataframe
  subset_df <- dat_scaled[, variables_to_correlate]
  
  # Calculate correlation matrix
  cor_matrix <- cor(subset_df, use = "pairwise.complete.obs")
  
  # Generate custom labels
  custom_labels <- create_labels(varlist)
  
  # Add PGIQ label if included
  if (include_pgiq) {
    custom_labels <- c("Polygenic g Score", custom_labels)
  }
  
  rownames(cor_matrix) <- custom_labels
  colnames(cor_matrix) <- custom_labels
  
  # Determine plot title if not provided
  if (is.null(plot_title)) {
    categories <- sapply(varlist, function(x) x[5])
    if (length(unique(categories)) == 1) {
      plot_title <- paste("Correlation Matrix for", unique(categories))
    } else {
      plot_title <- "Correlation Matrix"
    }
  }
  
  # Create the plot (returns a recordedplot object)
  corrplot.mixed(cor_matrix,
                 lower = "number", 
                 upper = "ellipse",
                 tl.col = "black", 
                 tl.pos = "lt",  # Labels on left and top
                 tl.srt = rotation_angle,    # Rotate labels 45 degrees
                 title = plot_title,
                 mar = c(0, 0, 1, 0),
                 cl.lim = c(-1, 1),
                 tl.cex = label_size,  # customizable label size
                 number.cex = number_size  # customizable number size
  )
  
  # Return the correlation matrix invisibly (useful for further analysis)
  invisible(cor_matrix)
}

# apply the function
# we may need to customize the label size and number size along the line (i.e., tl.cex & number.cex)
png("G_Composites_correlation.png", width = 16, height = 16, units = 'in', res = 300)
create_correlation_plot(G_Composites_Varlist, dat_scaled, plot_title = "Correlation Matrix for Cognitive Abilities", var_suffix = "1")
dev.off()

png("Verbal_Tests_correlation.png", width = 24, height = 18, units = 'in', res = 300)
create_correlation_plot(Verbal_Tests_Varlist, dat_scaled, plot_title = "Correlation Matrix for Verbal Tests", var_suffix = "1")
dev.off()

png("Nonverbal_Tests_correlation.png", width = 30, height = 24, units = 'in', res = 300)
create_correlation_plot(Nonverbal_Tests_Varlist, dat_scaled, plot_title = "Correlation Matrix for Nonverbal Tests", var_suffix = "1")
dev.off()

png("Edu_Achieve_Attain_correlation.png", width = 15, height = 15, units = 'in', res = 300)
create_correlation_plot(Edu_Achieve_Attain_Varlist, dat_scaled, plot_title = "Correlation Matrix for Educational Achievement and Attainment", var_suffix = "1")
dev.off()

png("Anxiety_correlation.png", width = 18, height = 18, units = 'in', res = 300)
create_correlation_plot(Anxiety_Varlist, dat_scaled, plot_title = "Correlation Matrix for Anxiety Measures", var_suffix = "1")
dev.off()

png("Conners_correlation.png", width = 15, height = 15, units = 'in', res = 300)
create_correlation_plot(Conners_Varlist, dat_scaled, plot_title = "Correlation Matrix for ADHD Measures", var_suffix = "1")
dev.off()

png("SDQ_correlation.png", width = 32, height = 32, units = 'in', res = 300)
create_correlation_plot(SDQ_Varlist, dat_scaled, plot_title = "Correlation Matrix for Strengths and Difficulties Questionnaire (SDQ)", var_suffix = "1",
                        label_size = 1, number_size = 0.8)
dev.off()

png("Wellbeing_correlation.png", width = 28, height = 28, units = 'in', res = 300)
create_correlation_plot(Wellbeing_Varlist, dat_scaled, plot_title = "Correlation Matrix for Other Measures", var_suffix = "1")
dev.off()

png("Anthro_correlation.png", width = 12, height = 10, units = 'in', res = 300)
create_correlation_plot(Anthro_Varlist, dat_scaled, plot_title = "Correlation Matrix for Anthropometric Measures", var_suffix = "1",
                        number_size = 1.2)
dev.off()


# To loop through multiple varlists: but this is going to loop for those like all_varlist and one_rater_varlist which I don't need
# list_names <- ls(pattern = "_Varlist$")
# for (list_name in list_names) {
#   current_varlist <- get(list_name)
# 
#   png(paste0(list_name, "_correlation_matrix.png"),
#       width = 12, height = 12, units = 'in', res = 300)
# 
#   tryCatch({
#     create_correlation_plot(current_varlist, dat_scaled, var_suffix = "1")
#     print(paste("Generated:", list_name))
#   }, error = function(e) {
#     print(paste("Error with", list_name, ":", e$message))
#   })
# 
#   dev.off()
# }

# add the common factors across ages as well
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

png("CFA_correlation.png", width = 22, height = 18, units = 'in', res = 300)
create_correlation_plot(CommonFactor_Score_Varlist, dat_scaled, plot_title = "Correlation Matrix for Common Latent Factors Across Ages (and Raters)", var_suffix = "1",
                        number_size = 0.8)
dev.off()
