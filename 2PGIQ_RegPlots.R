# ==============================================================================
# Standardized Beta Results Plotting
# ==============================================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(RColorBrewer)

# ==============================================================================
# 1. Setup Paths & Load Data
# ==============================================================================
setwd('./Plots/Reg/')
outFileStem <- './Results/Reg_All/'

CFA_composites_results  <- read.csv(paste0(outFileStem, "PGgS_CFA_composites_adjCovar.csv"))
G_composites_results    <- read.csv(paste0(outFileStem, "PGgS_G_composites_adjCovar.csv"))
Verbal_tests_results    <- read.csv(paste0(outFileStem, "PGgS_Verbal_Tests_adjCovar.csv"))
Nonverbal_tests_results <- read.csv(paste0(outFileStem, "PGgS_Nonverbal_Tests_adjCovar.csv"))
Anthro_results          <- read.csv(paste0(outFileStem, "PGgS_Anthro_adjCovar.csv"))
Education_results       <- read.csv(paste0(outFileStem, "PGgS_Education_adjCovar.csv"))
Wellbeing_results       <- read.csv(paste0(outFileStem, "PGgS_Wellbeing_adjCovar.csv"))
Anxiety_results         <- read.csv(paste0(outFileStem, "PGgS_Anxiety_adjCovar.csv"))
Conners_results         <- read.csv(paste0(outFileStem, "PGgS_Conners_ADHD_adjCovar.csv"))
SDQ_results             <- read.csv(paste0(outFileStem, "PGgS_SDQ_adjCovar.csv"))

# ==============================================================================
# 2. Pre-process Labels & Clean Trait Names
# ==============================================================================
G_composites_results$Category[G_composites_results$Category == "g"] <- "general cognitive ability"

Education_results$Category[Education_results$Category == "Educational Attainment"] <- "Core-Subject Achievement"
Education_results <- Education_results %>%
  mutate(Trait_Name = case_when(
    Trait_Name == "16 yr" ~ "GCSE Grades (16 yr)",
    Trait_Name == "18 yr" ~ "A-Level Grades (18 yr)",
    Trait_Name == "21 yr" ~ "University Grades (21 yr)",
    TRUE ~ Trait_Name
  ))

Anthro_results <- Anthro_results %>%
  mutate(Trait_Name = case_when(
    Category == "BMI" & Trait_Name == "birth (weight)" ~ "0 yr (weight)",
    Category == "Height" & Trait_Name == "birth" ~ "0 yr",
    TRUE ~ Trait_Name
  ))

Height_results <- Anthro_results %>% filter(Category == "Height")
BMI_results    <- Anthro_results %>% filter(Category == "BMI")

# ==============================================================================
# 3. Merge CFA Common Factors with Observed Variables
# ==============================================================================
CFA_composites_results <- CFA_composites_results %>%
  mutate(Category = case_when(
    Category == "ARBQ Obsessive-Compulsive" ~ "ARBQ OCB",
    Category == "ARBQ Anxiety Total" ~ "ARBQ Total Anxiety",
    TRUE ~ Category
  ))

CFA_G <- CFA_composites_results %>%
  filter(Category %in% c("g", "verbal ability", "nonverbal ability")) %>%
  mutate(Category = ifelse(Category == "g", "general cognitive ability", Category))
G_composites_combined <- bind_rows(G_composites_results, CFA_G)

Anthro_results_combined <- bind_rows(Height_results, BMI_results)

CFA_Edu <- CFA_composites_results %>%
  filter(Category %in% c("English Achievement", "Maths Achievement", "Science Achievement", "Core-Subject Achievement")) %>%
  mutate(Trait_Name = ifelse(Category == "Science Achievement", "Primary School (9-12 yr)", "Primary School (7-12 yr)"))
Education_results_combined <- bind_rows(Education_results, CFA_Edu)

Anxiety_combined <- bind_rows(Anxiety_results, CFA_composites_results %>% filter(Category %in% unique(Anxiety_results$Category)))
Conners_combined <- bind_rows(Conners_results, CFA_composites_results %>% filter(Category %in% unique(Conners_results$Category)))
SDQ_combined     <- bind_rows(SDQ_results, CFA_composites_results %>% filter(Category %in% unique(SDQ_results$Category)))

# ==============================================================================
# 4. Set Factor Levels
# ==============================================================================
desired_order_Anthro <- c("0 yr", "0 yr (weight)", "3 yr", "4 yr", "7 yr", "12 yr", "14 yr", "16 yr", "21 yr", "26 yr")
Anthro_results_combined$Trait_Name <- factor(Anthro_results_combined$Trait_Name, levels = desired_order_Anthro)

desired_order_Edu <- c("7 yr", "9 yr", "10 yr", "12 yr", "Primary School (7-12 yr)", "Primary School (9-12 yr)", "14 yr", "GCSE Grades (16 yr)", "A-Level Grades (18 yr)", "University Grades (21 yr)", "Years of Schooling (26 yr)")
Education_results_combined$Trait_Name <- factor(Education_results_combined$Trait_Name, levels = desired_order_Edu)

desired_order_G <- c("2 yr", "3 yr", "4 yr", "7 yr", "9 yr", "10 yr", "12 yr", "14 yr", "16 yr", "25 yr", "Cross Time", "by Stage Early", "by Stage Child", "by Stage Adol", "by Stage Latent", "by Method MCDI/PARCA", "by Method MCDI", "by Method PARCA", "by Method WISC/Raven", "by Method WISC", "by Method Raven", "by Method MillHill/Raven", "by Method MillHill", "by Method Latent")
G_composites_combined$Trait_Name <- factor(G_composites_combined$Trait_Name, levels = desired_order_G)

# ==============================================================================
# 5. Helper & Plotting Function
# ==============================================================================
get_sig_labels <- function(p_vec) {
  case_when(p_vec >= 0.05 ~ "ns", p_vec < 0.0001 ~ "****", p_vec < 0.001 ~ "***", p_vec < 0.01 ~ "**", p_vec < 0.05 ~ "*", TRUE ~ NA_character_)
}

plot_polygenic_results <- function(df, main_title, y_var = "Trait_Name", use_facet = FALSE, facet_cols = 2, color_palette = NULL, dodge_width = 0.8, asterisk_pos = "top", asterisk_offset = 0.02) {
  orig_levels <- if(is.factor(df[[y_var]])) levels(df[[y_var]]) else unique(df[[y_var]])
  wrapped_levels <- str_wrap(orig_levels, width = 45)
  df <- df %>% mutate(Category = factor(Category, levels = unique(Category)), sig_label = if("P_adj_value" %in% names(.)) get_sig_labels(P_adj_value) else get_sig_labels(sig_adj_p), Y_Axis_Plot = str_wrap(!!sym(y_var), width = 45), Y_Axis_Plot = factor(Y_Axis_Plot, levels = rev(wrapped_levels)))
  has_rater <- "Rater" %in% names(df) && length(unique(df$Rater)) > 1
  n_cats <- n_distinct(df$Category)
  
  p <- ggplot(df, aes(x = Beta, y = Y_Axis_Plot)) + geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8)
  if (n_cats > 1) { if (has_rater) { p <- p + aes(color = Category, shape = Rater, group = Rater) } else { p <- p + aes(color = Category) } } else { if (has_rater) p <- p + aes(shape = Rater, group = Rater); p <- p + guides(color = "none") }
  
  p <- p + geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, linewidth = 0.7, position = position_dodge(width = dodge_width)) + geom_point(size = 3.2, position = position_dodge(width = dodge_width))
  
  if (asterisk_pos == "right") { p <- p + geom_text(aes(x = CI_Upper + asterisk_offset, label = sig_label, alpha = sig_label, group = if(has_rater) Rater else NULL), color = "black", hjust = 0, vjust = 0.5, fontface = "bold", position = position_dodge(width = dodge_width), show.legend = FALSE) } else if (asterisk_pos == "left") { p <- p + geom_text(aes(x = CI_Lower - asterisk_offset, label = sig_label, alpha = sig_label, group = if(has_rater) Rater else NULL), color = "black", hjust = 1, vjust = 0.5, fontface = "bold", position = position_dodge(width = dodge_width), show.legend = FALSE) } else { p <- p + geom_text(aes(x = Beta, label = sig_label, alpha = sig_label, group = if(has_rater) Rater else NULL), color = "black", vjust = -0.3, hjust = 0.5, fontface = "bold", position = position_dodge(width = dodge_width), show.legend = FALSE) }
  p <- p + scale_alpha_manual(values = c("ns" = 0, "*" = 1, "**" = 1, "***" = 1, "****" = 1), na.value = 0)
  
  if (use_facet) p <- p + facet_wrap(~ Category, scales = "free_y", ncol = facet_cols) + guides(color = "none")
  if (n_cats > 1) { if (!is.null(color_palette)) { if(length(color_palette) < n_cats) color_palette <- colorRampPalette(color_palette)(n_cats); p <- p + scale_color_manual(values = color_palette) } else { p <- p + scale_color_brewer(palette = "Set1") } } else { if(!is.null(color_palette)) { p <- p + scale_color_manual(values = color_palette[1]) } }
  
  p <- p + theme_bw(base_size = 14) + labs(title = main_title, x = "Standardised Beta (β)", y = "Outcome Phenotype", color = "Phenotypic Category") + theme(legend.position = "top", plot.title = element_text(face = "bold", size = 16), strip.text = element_text(face = "bold", size = 16), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y = element_text(margin = margin(r = 12)), strip.background = element_rect(fill = "grey90", color = "grey90"), strip.clip = "off", panel.grid.major.y = element_line(linetype = "dotted", color = "grey85"), panel.spacing.x = unit(1.8, "lines"), panel.spacing.y = unit(1, "lines"), plot.margin = margin(t = 15, r = 15, b = 10, l = 25)) + coord_cartesian(clip = "off") + scale_y_discrete(expand = expansion(add = c(0.6, 0.6)))
  return(p)
}

# ==============================================================================
# 6. Generate and Save Plots
# ==============================================================================
pal_green  <- brewer.pal(4, "Greens")[-1]
pal_orange <- brewer.pal(5, "Oranges")[-1]
pal_red    <- brewer.pal(5, "Reds")[-1]
pal_blue   <- brewer.pal(5, "Blues")[-1]
pal_purple <- brewer.pal(7, "Purples")[-1]
pal_grey <- brewer.pal(3, "Greys")[-1]
pal_bupu <- brewer.pal(4, "BuPu")[-1]
pal_dark2 <- brewer.pal(8, "Dark2")
pal_set1 <- brewer.pal(7, "Set1")[-6]

plot_configs <- list(
  list(data = G_composites_combined, title = "Polygenic Prediction of Cognitive Abilities", facet = TRUE, cols = 3, pal = pal_green, file = "G_composites.png", w = 16, h = 10),
  list(data = Verbal_tests_results, title = "Polygenic Prediction of Verbal Test Scores", facet = FALSE, cols = 1, pal = pal_orange, file = "Verbal_tests.png", w = 15, h = 12),
  list(data = Nonverbal_tests_results, title = "Polygenic Prediction of Nonverbal Test Scores", facet = FALSE, cols = 1, pal = pal_red, file = "Nonverbal_tests.png", w = 18, h = 18),
  list(data = Anthro_results_combined, title = "Polygenic Prediction of Anthropometrics", facet = TRUE, cols = 2, pal = pal_grey, file = "Anthro_plot.png", w = 14, h = 8),
  list(data = Education_results_combined, title = "Polygenic Prediction of Educational Outcomes", facet = TRUE, cols = 2, pal = pal_blue, file = "Education_plot.png", w = 15, h = 10),
  list(data = Anxiety_combined, title = "Polygenic Prediction of Anxiety Scores", facet = TRUE, cols = 3, pal = pal_purple, file = "Anxiety_plot.png", w = 18, h = 14),
  list(data = Conners_combined, title = "Polygenic Prediction of ADHD Scores", facet = TRUE, cols = 3, pal = pal_bupu, file = "ADHD_plot.png", w = 18, h = 14),
  list(data = SDQ_combined, title = "Polygenic Prediction of SDQ Scores", facet = TRUE, cols = 3, pal = pal_set1, file = "SDQ_plot.png", w = 20, h = 28, dodge = 0.8, ast_pos = "left"),
  list(data = Wellbeing_results, title = "Polygenic Prediction of Other Outcomes", facet = FALSE, cols = 1, pal = pal_dark2, file = "Wellbeing_plot.png", w = 14, h = 30)
)

for (cfg in plot_configs) {
  y_mapping <- if (!is.null(cfg$y)) cfg$y else "Trait_Name"
  dodge_val <- if (!is.null(cfg$dodge)) cfg$dodge else 0.8
  ast_pos_val <- if (!is.null(cfg$ast_pos)) cfg$ast_pos else "top"
  p_out <- plot_polygenic_results(df = cfg$data, main_title = cfg$title, use_facet = cfg$facet, facet_cols = cfg$cols, color_palette = cfg$pal, y_var = y_mapping, dodge_width = dodge_val, asterisk_pos = ast_pos_val, asterisk_offset = 0.015)
  print(p_out)
  ggsave(cfg$file, plot = p_out, width = cfg$w, height = cfg$h, dpi = 300)
}

# Comparison Plot Generators
path_IQ3 <- "./Results/Reg_All/Savage_g_PGS_20boot_adjCovar.csv"
path_EA4 <- "./Results/Reg_All/Okbay_EA_PGS_20boot_adjCovar.csv"

results_IQ3_all <- read.csv(path_IQ3)
results_EA4_all <- read.csv(path_EA4)

prepare_comparison_df <- function(original_df, iq3_full, ea4_full) {
  original_df$Predictor <- "PGIQ"
  target_outcomes <- unique(original_df$Outcome)
  iq3_sub <- iq3_full %>% filter(Outcome %in% target_outcomes) %>% mutate(Predictor = "IQ3")
  ea4_sub <- ea4_full %>% filter(Outcome %in% target_outcomes) %>% mutate(Predictor = "EA4")
  combined <- bind_rows(original_df, iq3_sub, ea4_sub) %>% mutate(Predictor = factor(Predictor, levels = c("PGIQ", "IQ3", "EA4")))
  return(combined)
}

plot_comparison_with_shapes <- function(df, main_title, y_var = "Trait_Name", use_facet = FALSE, facet_cols = 2, color_palette = NULL, dodge_width = 0.8) {
  df <- df %>% mutate(Category = factor(Category, levels = unique(Category)), sig_label = if("P_adj_value" %in% names(.)) get_sig_labels(P_adj_value) else sig_adj_p, Y_Axis_Plot = str_wrap(!!sym(y_var), width = 45), Y_Axis_Plot = factor(Y_Axis_Plot, levels = rev(unique(Y_Axis_Plot))))
  p <- ggplot(df, aes(x = Beta, y = Y_Axis_Plot, color = Category, shape = Predictor)) + geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8)
  p <- p + geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, group = Predictor), height = 0.2, linewidth = 0.7, position = position_dodge(width = dodge_width)) + geom_point(size = 3.2, stroke = 0.8, position = position_dodge(width = dodge_width)) + geom_text(aes(label = sig_label, alpha = sig_label, group = Predictor), color = "black", vjust = -0.3, fontface = "bold", size = 4, position = position_dodge(width = dodge_width), show.legend = FALSE) + scale_alpha_manual(values = c("ns" = 0, "*" = 1, "**" = 1, "***" = 1, "****" = 1), na.value = 0) + scale_shape_manual(values = c("PGIQ" = 16, "IQ3" = 17, "EA4" = 15))
  if (use_facet) p <- p + facet_wrap(~ Category, scales = "free_y", ncol = facet_cols)
  n_cats <- length(unique(df$Category))
  if (n_cats > 1) { if (!is.null(color_palette)) { if(length(color_palette) < n_cats) color_palette <- colorRampPalette(color_palette)(n_cats); p <- p + scale_color_manual(values = color_palette) } else { p <- p + scale_color_brewer(palette = "Set1") } } else { if(!is.null(color_palette)) p <- p + scale_color_manual(values = color_palette[1]); p <- p + guides(color = "none") }
  p <- p + theme_bw(base_size = 14) + labs(title = main_title, subtitle = "Comparison: PGIQ (●), IQ3 (▲), EA4 (■)", x = "Standardised Beta (β)", y = "Outcome Phenotype At Each Age", color = "Phenotypic Category", shape = "Predictor") + theme(legend.position = "top", plot.title = element_text(face = "bold", size = 16), strip.text = element_text(face = "bold", size = 16), strip.background = element_rect(fill = "grey90", color = "grey90"), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y = element_text(margin = margin(r = 12)), panel.grid.major.y = element_line(linetype = "dotted", color = "grey85"), panel.spacing.x = unit(1.8, "lines"), panel.spacing.y = unit(1, "lines"), plot.margin = margin(t = 15, r = 15, b = 10, l = 25), strip.clip = "off") + coord_cartesian(clip = "off") + scale_y_discrete(expand = expansion(add = c(0.6, 0.6)))
  return(p)
}

comparison_configs <- list(
  list(data = G_composites_results, title = "Comparison: Cognitive Abilities (g)", facet = TRUE, cols = 3, pal = pal_green, file = "G_composites_Comparison.png", w = 14, h = 8),
  list(data = Verbal_tests_results, title = "Comparison: Verbal Test Scores", facet = FALSE, cols = 1, pal = pal_orange, file = "Verbal_tests_Comparison.png", w = 15, h = 12),
  list(data = Nonverbal_tests_results, title = "Comparison: Nonverbal Test Scores", facet = FALSE, cols = 1, pal = pal_red, file = "Nonverbal_tests_Comparison.png", w = 18, h = 18)
)

for (cfg in comparison_configs) {
  merged_df <- prepare_comparison_df(cfg$data, results_IQ3_all, results_EA4_all)
  p_out <- plot_comparison_with_shapes(df = merged_df, main_title = cfg$title, use_facet = cfg$facet, facet_cols = cfg$cols, color_palette = cfg$pal)
  print(p_out)
  ggsave(cfg$file, plot = p_out, width = cfg$w, height = cfg$h, dpi = 300)
}
