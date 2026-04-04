library(corrplot)
library(dplyr)
library(stringr)

# ==============================================================================
# 0. Data Setup & Pre-processing
# ==============================================================================
source("./0PGIQ_VarList.R")
source("./0PGIQ_CommFactorList.R")
sourceFileStem <- './Data030326/'

dat_raw <- read.csv(paste0(sourceFileStem, "PGIQ_raw.csv"))
setwd("./CorrMatrix/")

dat_raw_unrelated <- subset(dat_raw, selectunpaired == 1)

CommonFactor_Score_Varlist_fixed <- lapply(CommonFactor_Score_Varlist, function(x) {
  if(x[5] == "ARBQ Obsessive-Compulsive") x[5] <- "ARBQ OCB"
  if(x[5] == "ARBQ Anxiety Total") x[5] <- "ARBQ Total Anxiety"
  return(x)
})


# ==============================================================================
# Helper Functions
# ==============================================================================

# Custom label function
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
      if (is_simple_age) {
        labels <- c(labels, paste0(current_age, " (", current_rater, ") - ", current_category))
      } else {
        labels <- c(labels, paste0(current_age_raw, " (", current_rater, ")"))
      }
    } else {
      age_rater_pairs <- paste(ages, raters, sep = "_")
      age_rater_counts <- table(age_rater_pairs)
      current_age_rater_pair <- paste(current_age, current_rater, sep = "_")
      
      if (age_rater_counts[current_age_rater_pair] == 1) {
        if (is_simple_age) {
          labels <- c(labels, paste0(current_age, " (", current_rater, ")"))
        } else {
          labels <- c(labels, paste0(current_age_raw, " (", current_rater, ")"))
        }
      } else {
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

# Corrplot function with PGIQ variables
create_correlation_plot <- function(varlist, dat_raw_unrelated, plot_title = NULL, var_suffix = "", 
                                    include_pgiq = TRUE, rotation_angle = 45, label_size = 1, number_size = 1,
                                    title_size = 1.5, legend_size = 1.2) {
  variables_to_correlate <- sapply(varlist, function(x) paste0(x[1], var_suffix))
  
  if (include_pgiq) {
    variables_to_correlate <- c("PGgS", "Okbay_EA_PGS", "Savage_g_PGS", variables_to_correlate)
  }
  
  missing_vars <- variables_to_correlate[!variables_to_correlate %in% names(dat_raw_unrelated)]
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in dataframe:", paste(missing_vars, collapse = ", ")))
  }
  
  subset_df <- dat_raw_unrelated[, variables_to_correlate]
  cor_matrix <- cor(subset_df, use = "pairwise.complete.obs")
  
  custom_labels <- create_labels(varlist)
  if (include_pgiq) {
    custom_labels <- c("Polygenic g Score", "Okbay EA PGS", "Savage g PGS", custom_labels)
  }
  
  rownames(cor_matrix) <- custom_labels
  colnames(cor_matrix) <- custom_labels
  
  if (is.null(plot_title)) {
    categories <- sapply(varlist, function(x) x[5])
    if (length(unique(categories)) == 1) {
      plot_title <- paste("Correlation Matrix for", unique(categories))
    } else {
      plot_title <- "Correlation Matrix"
    }
  }
  
  corrplot.mixed(cor_matrix,
                 lower = "number", 
                 upper = "ellipse",
                 tl.col = "black", 
                 tl.pos = "lt",
                 tl.srt = rotation_angle,
                 title = plot_title,
                 mar = c(0, 0, 2, 0),       # INCREASED top margin slightly from 1 to 2 to give the bigger title room
                 tl.cex = label_size,       # Variable label size
                 number.cex = number_size,  # Matrix number size
                 cl.cex = legend_size,      # NEW: Color legend label size
                 cex.main = title_size      # NEW: Plot title size
  )
  invisible(cor_matrix)
}

# Helper Function to extract and merge lists by a specific category
merge_by_category <- function(list1, list2, target_category) {
  filtered_list1 <- Filter(function(x) x[5] == target_category, list1)
  filtered_list2 <- Filter(function(x) x[5] == target_category, list2)
  return(c(filtered_list1, filtered_list2))
}

# Function to get all unique categories from a specific varlist
get_categories <- function(varlist) { 
  unique(sapply(varlist, function(x) x[5])) 
}


# ==============================================================================
# Plot Generation (Strictly matching screenshot numbering and filenames)
# ==============================================================================

# 1. G_Composites_correlation.png
png("1G_Composites_correlation.png", width = 16, height = 14, units = 'in', res = 300)
create_correlation_plot(G_Composites_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for Cognitive Ability Composites", 
                        var_suffix = "1")
dev.off()


# 2. Merged_g_correlation.png
Merged_g_Varlist <- merge_by_category(G_Composites_Varlist, CommonFactor_Score_Varlist_fixed, "g")

png("2Merged_g_correlation.png", width = 18, height = 16, units = 'in', res = 300)
create_correlation_plot(Merged_g_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for General Cognitive Ability (g)", 
                        var_suffix = "1", label_size = 1.2, number_size = 1.2)
dev.off()


# 3. Merged_Verbal_Tests_All_correlation.png
verbal_composites <- Filter(function(x) x[5] == "verbal ability", G_Composites_Varlist)
verbal_factors <- Filter(function(x) x[5] == "verbal ability", CommonFactor_Score_Varlist_fixed)
Merged_Verbal_Tests_Varlist <- c(Verbal_Tests_Varlist, verbal_composites, verbal_factors)

png("3Merged_Verbal_Tests_All_correlation.png", width = 23, height = 20, units = 'in', res = 300)
create_correlation_plot(Merged_Verbal_Tests_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for Verbal Tests, Composites, and Latent Factors", 
                        var_suffix = "1", label_size = 0.9, number_size = 0.7)
dev.off()


# 4. Merged_Nonverbal_Tests_All_correlation.png
nonverbal_composites <- Filter(function(x) x[5] == "nonverbal ability", G_Composites_Varlist)
nonverbal_factors <- Filter(function(x) x[5] == "nonverbal ability", CommonFactor_Score_Varlist_fixed)
Merged_Nonverbal_Tests_Varlist <- c(Nonverbal_Tests_Varlist, nonverbal_composites, nonverbal_factors)

png("4Merged_Nonverbal_Tests_All_correlation.png", width = 34, height = 28, units = 'in', res = 300)
create_correlation_plot(Merged_Nonverbal_Tests_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for Nonverbal Tests, Composites, and Latent Factors", 
                        var_suffix = "1", label_size = 0.9, number_size = 0.8)
dev.off()


# 5. Merged_Edu_Achieve_Attain_correlation.png
Edu_Latent_Factors <- list(
  c("Eng_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "English Achievement"),
  c("Mat_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "Maths Achievement"),
  c("Sci_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "Science Achievement"),
  c("Core_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "Core-Subject Achievement")
)
Merged_Edu_Varlist <- c(Edu_Achieve_Attain_Varlist, Edu_Latent_Factors)

png("5Merged_Edu_Achieve_Attain_correlation.png", width = 24, height = 18, units = 'in', res = 300)
create_correlation_plot(Merged_Edu_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for Educational Achievement and Attainment (with Latent Factors)", 
                        var_suffix = "1", label_size = 1.2)
dev.off()


# 6. Merged_SDQ_correlation.png
sdq_cats <- get_categories(SDQ_Varlist)
Merged_SDQ_Varlist <- unlist(lapply(sdq_cats, function(cat) {
  merge_by_category(SDQ_Varlist, CommonFactor_Score_Varlist_fixed, cat)
}), recursive = FALSE)

png("6Merged_SDQ_correlation.png", width = 55, height = 40, units = 'in', res = 300)
create_correlation_plot(Merged_SDQ_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for SDQ (with Latent Factors)", 
                        var_suffix = "1", label_size = 0.8, number_size = 0.6)
dev.off()


# 7. Anxiety_correlation.png
anxiety_cats <- get_categories(Anxiety_Varlist)
Merged_Anxiety_Varlist <- unlist(lapply(anxiety_cats, function(cat) {
  merge_by_category(Anxiety_Varlist, CommonFactor_Score_Varlist_fixed, cat)
}), recursive = FALSE)

png("7Anxiety_correlation.png", width = 30, height = 22, units = 'in', res = 300)
create_correlation_plot(Merged_Anxiety_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for Anxiety Measures (with Latent Factors)", 
                        var_suffix = "1")
dev.off()


# 8. Conners_correlation.png
conners_cats <- get_categories(Conners_Varlist)
Merged_Conners_Varlist <- unlist(lapply(conners_cats, function(cat) {
  merge_by_category(Conners_Varlist, CommonFactor_Score_Varlist_fixed, cat)
}), recursive = FALSE)

png("8Conners_correlation.png", width = 25, height = 18, units = 'in', res = 300)
create_correlation_plot(Merged_Conners_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for ADHD Measures (with Latent Factors)", 
                        var_suffix = "1")
dev.off()


# 9. Anthro_correlation.png
png("9Anthro_correlation.png", width = 12, height = 10, units = 'in', res = 300)
create_correlation_plot(Anthro_Varlist, dat_raw_unrelated, 
                        plot_title = "Correlation Matrix for Anthropometric Measures", 
                        var_suffix = "1", number_size = 1)
dev.off()




# ==============================================================================
# 10. Generate Combined PDF with all plots
# ==============================================================================

# Open a single PDF device with a large enough canvas for the biggest matrices
pdf("All_Correlation_Plots_Combined.pdf", width = 42, height = 40)

# 1. G_Composites
create_correlation_plot(G_Composites_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_1 Correlation Matrix for Cognitive Ability Composites", 
                        var_suffix = "1", label_size = 2, number_size = 2)

# 2. Merged g
create_correlation_plot(Merged_g_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_2 Correlation Matrix for General Cognitive Ability (g)", 
                        var_suffix = "1", label_size = 2, number_size = 2)

# 3. Merged Verbal Tests
create_correlation_plot(Merged_Verbal_Tests_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_3 Correlation Matrix for Verbal Tests, Composites, and Latent Factors", 
                        var_suffix = "1", label_size = 1.5, number_size = 1.2)

# 4. Merged Nonverbal Tests
create_correlation_plot(Merged_Nonverbal_Tests_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_4 Correlation Matrix for Nonverbal Tests, Composites, and Latent Factors", 
                        var_suffix = "1", label_size = 1.3, number_size = 1.1)

# 5. Merged Educational Achievement
create_correlation_plot(Merged_Edu_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_5 Correlation Matrix for Educational Achievement and Attainment (with Latent Factors)", 
                        var_suffix = "1", label_size = 2, number_size = 1.7)

# 6. Merged SDQ
create_correlation_plot(Merged_SDQ_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_6 Correlation Matrix for SDQ (with Latent Factors)", 
                        var_suffix = "1", label_size = 0.6, number_size = 0.4)

# 7. Merged Anxiety
create_correlation_plot(Merged_Anxiety_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_7 Correlation Matrix for Anxiety Measures (with Latent Factors)", 
                        var_suffix = "1", label_size = 1.4, number_size = 1.1)

# 8. Merged Conners
create_correlation_plot(Merged_Conners_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_8 Correlation Matrix for ADHD Measures (with Latent Factors)", 
                        var_suffix = "1", label_size = 1.4, number_size = 1.1)

# 9. Anthro
create_correlation_plot(Anthro_Varlist, dat_raw_unrelated, 
                        plot_title = "Figure S1_9 Correlation Matrix for Anthropometric Measures", 
                        var_suffix = "1", label_size = 2.5, number_size = 2.5)

# Close the PDF device to save the file
dev.off()







# ==============================================================================
# Helper Functions
# ==============================================================================

# Custom label function (Simplified to ALWAYS keep the category)
create_labels <- function(var_info_list) {
  ages_raw <- sapply(var_info_list, function(x) x[2])
  raters <- sapply(var_info_list, function(x) x[4])
  categories <- sapply(var_info_list, function(x) x[5])
  
  labels <- c()
  
  for (i in 1:length(var_info_list)) {
    current_age_raw <- ages_raw[i]
    current_rater <- raters[i]
    current_category <- categories[i]
    
    # Always concatenate: "Raw Age (Rater) - Category"
    labels <- c(labels, paste0(current_age_raw, " (", current_rater, ") - ", current_category))
  }
  
  return(labels)
}

# ==============================================================================
# 11. Comprehensive Cross-Domain Correlation Table (100% SAFE EXPORT)
# ==============================================================================

# 1. Simplified label generator specifically for a single item
get_single_label <- function(x) {
  # x[2] = age, x[4] = rater, x[5] = category
  paste0(x[2], " (", x[4], ") - ", x[5])
}

# 2. Combine the already-MERGED lists
# (If you want Edu Latent factors interspersed, you will need to reorder Merged_Edu_Varlist first)
Master_Varlist <- c(
  Merged_g_Varlist,
  Merged_Verbal_Tests_Varlist,
  Merged_Nonverbal_Tests_Varlist,
  Merged_Edu_Varlist,
  Merged_SDQ_Varlist,
  Merged_Anxiety_Varlist,
  Merged_Conners_Varlist,
  Anthro_Varlist
)

# Keep only unique variables to prevent duplicates
Master_Varlist <- unique(Master_Varlist)

# 3. CREATE A MAPPING DATAFRAME (This guarantees 100% safety against shifting)
mapping_df <- data.frame(
  VarName = sapply(Master_Varlist, function(x) paste0(x[1], "1")), # e.g., "brawg1"
  Label = sapply(Master_Varlist, get_single_label),                # e.g., "2 yr (Child) - g"
  stringsAsFactors = FALSE
)

# Prepend the PGS scores to the top of the mapping dataframe
pgs_df <- data.frame(
  VarName = c("PGgS", "Okbay_EA_PGS", "Savage_g_PGS"),
  Label = c("Polygenic g Score", "Okbay EA PGS", "Savage g PGS"),
  stringsAsFactors = FALSE
)
mapping_df <- rbind(pgs_df, mapping_df)

# Deduplicate based on VarName just in case
mapping_df <- mapping_df[!duplicated(mapping_df$VarName), ]

# 4. Filter the mapping dictionary by checking against the actual dataset
vars_in_data <- mapping_df$VarName %in% names(dat_raw_unrelated)

# Report missing variables so you know exactly what dropped out
if(sum(!vars_in_data) > 0) {
  cat("\n--- WARNING: Missing Variables Dropped ---\n")
  cat("The following variables were not found in dat_raw_unrelated and have been safely excluded to prevent label shifting:\n")
  print(mapping_df$VarName[!vars_in_data])
  cat("------------------------------------------\n\n")
}

# Keep ONLY the perfectly matched rows
mapping_df_safe <- mapping_df[vars_in_data, ]

# 5. Subset data and calculate the correlation matrix
subset_df <- dat_raw_unrelated[, mapping_df_safe$VarName] # 6973 446
master_cor_matrix <- cor(subset_df, use = "pairwise.complete.obs")

# Round to 3 decimal places for a clean table
# master_cor_matrix <- round(master_cor_matrix, 3)

# 6. Apply the absolutely safe labels from our perfectly matched dictionary
rownames(master_cor_matrix) <- mapping_df_safe$Label
colnames(master_cor_matrix) <- mapping_df_safe$Label

# 7. Export to CSV
setwd("/Users/yujinglin/Desktop/polygenic IQ score/Results030326/")

write.csv(master_cor_matrix, "CorrTable.csv", row.names = TRUE)

library(openxlsx)

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet
addWorksheet(wb, "CorrTable")

# Write the data (CRITICAL: set rowNames = TRUE so we don't lose the labels!)
writeData(wb, "CorrTable", master_cor_matrix, colNames = TRUE, rowNames = TRUE)

# Freeze the top row (headers) and first column (variable labels)
freezePane(wb, "CorrTable", firstActiveRow = 2, firstActiveCol = 2)

# Save the workbook
saveWorkbook(wb, file = "CorrTable.xlsx", overwrite = TRUE)

cat("Success! The matrix is securely exported to Excel with frozen panes.\n")
