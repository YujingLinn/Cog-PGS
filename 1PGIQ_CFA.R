# Phenotypic CFA for cognitive and mental health underlying traits predicted by polygenic g score
# 19 Sept, 2025
# Yujing Lin

library(lavaan)
library(dplyr)
library(semPlot)
library(broom)
library(purrr)

sourceFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Data220925/'
# sourceFileStem <- '/scratch/users/k21170717/polygenic_IQ_score/Data220925/'
dat_scaled_for_CFA <- read.csv(paste0(sourceFileStem, "dat_scaled_for_CFA.csv"))
dat_scaled_for_CFA$PGgS <- dat_scaled_for_CFA$PGIQ

# plot function
# https://www.rdocumentation.org/packages/semPlot/versions/1.1.6/topics/semPaths
plot_sem_tree <- function(fit, labels_list = NULL) {
  semPaths(fit,
           what = "std",           # Show standardized parameter estimates
           layout = "tree",        # Top-down tree structure
           residuals = TRUE,       # Display residual variances
           edge.label.cex = 0.8,   # Font size for path labels
           sizeLat = 12,          # width of latent variable nodes
           sizeLat2 = 8,          # height of latent variable nodes
           label.cex = 1.2,        # Font size for node labels
           fade = FALSE,           # Show all paths regardless of significance
           style = "lisrel",       # Latents = ovals, observed = rectangles
           nCharNodes = 0,         # Don't abbreviate variable names
           labels = labels_list,   # Custom labels if provided
           mar = c(10, 5, 5, 5))
}

# --- Cognitive Composites ----
# Model for 'g'
dat_g_var <- subset(dat_scaled_for_CFA, select = c(brawg1, crawg1, drawg1, gcg1, icg1, jcg1, lcg1, ncg1, pcg1, ucgt1, PGgS, id_fam))

new_cog_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr10", "yr12", "yr14", "yr16", "yr25")
all_new_names <- c(new_cog_names, "PGgS", "id_fam")
names(dat_g_var) <- all_new_names

g_model <- '
  g =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr10 + yr12 + yr14 + yr16 + yr25
  g ~ PGgS
'
fit_g <- cfa(g_model, data = dat_g_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_g, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_g)

used_case_idx <- lavInspect(fit_g, "case.idx")
g_factor_scores <- lavPredict(fit_g, method = "regression")
dat_scaled_for_CFA$g_CTCR1 <- NA
dat_scaled_for_CFA$g_CTCR1[used_case_idx] <- g_factor_scores[,1]

cat("g model done\n")

# Model for 'verbal ability'
dat_vb_var <- subset(dat_scaled_for_CFA, select = c(bscv1, cscv1, dscv1, gcl1, icvb1, jcvb1, lverbal12T1, pcvctota1, ucgvbt1, PGgS, id_fam))
new_vb_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr10", "yr12", "yr16", "yr25")
all_new_vb_names <- c(new_vb_names, "PGgS", "id_fam")
names(dat_vb_var) <- all_new_vb_names

verbal_ability_model <- '
  Verbal_Ability =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr10 + yr12 + yr16 + yr25
  Verbal_Ability ~ PGgS
'
fit_vb <- cfa(verbal_ability_model, data = dat_vb_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_vb, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_vb)

used_data_vb <- lavInspect(fit_vb, "data")
used_case_idx_vb <- lavInspect(fit_vb, "case.idx")
vb_factor_scores <- lavPredict(fit_vb, method = "regression")
dat_scaled_for_CFA$vb_CTCR1 <- NA
dat_scaled_for_CFA$vb_CTCR1[used_case_idx_vb] <- vb_factor_scores[,1]

cat("verbal ability model done\n")

# Model for 'nonverbal ability'
dat_nv_var <- subset(dat_scaled_for_CFA, select = c(bscnv1, cscnv1, dscnv1, gcn1, icnv1, jcnv1, lnonverbal12T1, pcrvtota1, ucgnvt1, PGgS, id_fam))
new_nv_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr10", "yr12", "yr16", "yr25")
all_new_nv_names <- c(new_nv_names, "PGgS", "id_fam")
names(dat_nv_var) <- all_new_nv_names

nonverbal_ability_model <- '
  Nonverbal_Ability =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr10 + yr12 + yr16 + yr25
  Nonverbal_Ability ~ PGgS
'
fit_nv <- cfa(nonverbal_ability_model, data = dat_nv_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_nv, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_nv)

used_data_nv <- lavInspect(fit_nv, "data")
used_case_idx_nv <- lavInspect(fit_nv, "case.idx")
nv_factor_scores <- lavPredict(fit_nv, method = "regression")
dat_scaled_for_CFA$nv_CTCR1 <- NA
dat_scaled_for_CFA$nv_CTCR1[used_case_idx_nv] <- nv_factor_scores[,1]

cat("nonverbal ability model done\n")



# --- Anxiety Traits (ARBQ) ----
# Model for 'ARBQ Shyness'
dat_shy_var <- subset(dat_scaled_for_CFA, select = c(canxshyt1, danxshyt1, gpanxshyt1, ipanxshyt1, ppbhanxshyt1, gtanxshyt1, PGgS, id_fam))

new_shy_names <- c("yr3P", "yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
all_new_shy_names <- c(new_shy_names, "PGgS", "id_fam")
names(dat_shy_var) <- all_new_shy_names

ARBQ_Shyness_model <- '
  ARBQ_Shyness =~ yr3P + yr4P + yr7P + yr9P + yr16P + yr7T
  ARBQ_Shyness ~ PGgS
'
fit_shy <- cfa(ARBQ_Shyness_model, data = dat_shy_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_shy, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_shy)

used_data_shy <- lavInspect(fit_shy, "data")
used_case_idx_shy <- lavInspect(fit_shy, "case.idx")
shy_factor_scores <- lavPredict(fit_shy, method = "regression")
dat_scaled_for_CFA$ARBQ_shy_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_shy_CTCR1[used_case_idx_shy] <- shy_factor_scores[,1]

# Model for 'ARBQ_Fear'
dat_fear_var <- subset(dat_scaled_for_CFA, select = c(canxfeart1, danxfeart1, gpanxfeart1, ipanxfeart1, ppbhanxfeart1, gtanxfeart1, PGgS, id_fam))

new_fear_names <- c("yr3P", "yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
all_new_fear_names <- c(new_fear_names, "PGgS", "id_fam")
names(dat_fear_var) <- all_new_fear_names

ARBQ_Fear_model <- '
  ARBQ_Fear =~ yr3P + yr4P + yr7P + yr9P + yr16P + yr7T
  ARBQ_Fear ~ PGgS
'
fit_fear <- cfa(ARBQ_Fear_model, data = dat_fear_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_fear, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_fear)

used_data_fear <- lavInspect(fit_fear, "data")
used_case_idx_fear <- lavInspect(fit_fear, "case.idx")
fear_factor_scores <- lavPredict(fit_fear, method = "regression")
dat_scaled_for_CFA$ARBQ_fear_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_fear_CTCR1[used_case_idx_fear] <- fear_factor_scores[,1]

# Model for 'ARBQ_Obsessive-Compulsive Behaviour'
dat_ocb_var <- subset(dat_scaled_for_CFA, select = c(danxocbt1, gpanxocbt1, ipanxocbt1, ppbhanxocbt1, gtanxocbt1, PGgS, id_fam))

new_ocb_names <- c("yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
all_new_ocb_names <- c(new_ocb_names, "PGgS", "id_fam")
names(dat_ocb_var) <- all_new_ocb_names

ARBQ_Obsessive_Compulsive_Behaviour_model <- '
  ARBQ_OCB =~ yr4P + yr7P + yr9P + yr16P + yr7T
  ARBQ_OCB ~ PGgS
'
fit_ocb <- cfa(ARBQ_Obsessive_Compulsive_Behaviour_model, data = dat_ocb_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_ocb, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_ocb)

used_data_ocb <- lavInspect(fit_ocb, "data")
used_case_idx_ocb <- lavInspect(fit_ocb, "case.idx")
ocb_factor_scores <- lavPredict(fit_ocb, method = "regression")
dat_scaled_for_CFA$ARBQ_ocb_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_ocb_CTCR1[used_case_idx_ocb] <- ocb_factor_scores[,1]

# Model for 'ARBQ_Negative Affect'
dat_naff_var <- subset(dat_scaled_for_CFA, select = c(danxnafft1, gpanxnafft1, ipanxnafft1, ppbhanxnafft1, gtanxnafft1, PGgS, id_fam))

new_naff_names <- c("yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
all_new_naff_names <- c(new_naff_names, "PGgS", "id_fam")
names(dat_naff_var) <- all_new_naff_names

ARBQ_Negative_Affect_model <- '
  ARBQ_NegAff =~ yr4P + yr7P + yr9P + yr16P + yr7T
  ARBQ_NegAff ~ PGgS
'
fit_naff <- cfa(ARBQ_Negative_Affect_model, data = dat_naff_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_naff, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_naff)

used_data_naff <- lavInspect(fit_naff, "data")
used_case_idx_naff <- lavInspect(fit_naff, "case.idx")
naff_factor_scores <- lavPredict(fit_naff, method = "regression")
dat_scaled_for_CFA$ARBQ_naff_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_naff_CTCR1[used_case_idx_naff] <- naff_factor_scores[,1]

# Model for 'ARBQ_Negative Cognition'
dat_ncog_var <- subset(dat_scaled_for_CFA, select = c(danxncogt1, gpanxncogt1, ipanxncogt1, ppbhanxncogt1, gtanxncogt1, PGgS, id_fam))

new_ncog_names <- c("yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
all_new_ncog_names <- c(new_ncog_names, "PGgS", "id_fam")
names(dat_ncog_var) <- all_new_ncog_names

ARBQ_Negative_Cognition_model <- '
  ARBQ_NegCog =~ yr4P + yr7P + yr9P + yr16P + yr7T
  ARBQ_NegCog ~ PGgS
'
fit_ncog <- cfa(ARBQ_Negative_Cognition_model, data = dat_ncog_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_ncog, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_ncog)

used_data_ncog <- lavInspect(fit_ncog, "data")
used_case_idx_ncog <- lavInspect(fit_ncog, "case.idx")
ncog_factor_scores <- lavPredict(fit_ncog, method = "regression")
dat_scaled_for_CFA$ARBQ_ncog_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_ncog_CTCR1[used_case_idx_ncog] <- ncog_factor_scores[,1]

# Model for 'Anxiety Total'
dat_ARBQ_total_var <- subset(dat_scaled_for_CFA, select = c(canxt1, danxt1, gpanxt1, ipanxt1, ppbhanxt1, gtanxt1, PGgS, id_fam))

new_total_names <- c("yr3P", "yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
all_new_total_names <- c(new_total_names, "PGgS", "id_fam")
names(dat_ARBQ_total_var) <- all_new_total_names

Anxiety_Total_model <- '
  Anxiety_Total =~ yr3P + yr4P + yr7P + yr9P + yr16P + yr7T
  Anxiety_Total ~ PGgS
'
fit_ARBQ_total <- cfa(Anxiety_Total_model, data = dat_ARBQ_total_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_ARBQ_total, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_ARBQ_total)

used_data_ARBQ_total <- lavInspect(fit_ARBQ_total, "data")
used_case_idx_ARBQ_total <- lavInspect(fit_ARBQ_total, "case.idx")
ARBQ_total_factor_scores <- lavPredict(fit_ARBQ_total, method = "regression")
dat_scaled_for_CFA$Anxiety_total_CTCR1 <- NA
dat_scaled_for_CFA$Anxiety_total_CTCR1[used_case_idx_ARBQ_total] <- ARBQ_total_factor_scores[,1]

cat("ARBQ model done\n")



# --- Conners Traits ----
# Model for 'Conners Inattention'
dat_inatt_ctcr_var <- subset(dat_scaled_for_CFA, select = c(hconint1, lpconint1, npconint1, ppbhconninat1, u1pconinat1, ntconint1, ncconint1, u2cconninat1, zmhconnt1, PGgS, id_fam))

new_inatt_ctcr_names <- c("yr8P", "yr12P", "yr14P", "yr16P", "yr21P", "yr14T", "yr14C", "yr21C", "yr26C")
all_new_inatt_ctcr_names <- c(new_inatt_ctcr_names, "PGgS", "id_fam")
names(dat_inatt_ctcr_var) <- all_new_inatt_ctcr_names

Conners_Inattention_model <- '
  Inattention =~ yr8P + yr12P + yr14P + yr16P + yr21P + yr14T + yr14C + yr21C + yr26C
  Inattention ~ PGgS
'
# here, we got cross-time & cross-rater latent factor
# I tried to drop the teacher- and child-rating; the model fit has increased but the prediction doesn't differ much
# so we just well keep them 
fit_inatt <- cfa(Conners_Inattention_model, data = dat_inatt_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_inatt, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_inatt)

used_data_inatt <- lavInspect(fit_inatt, "data")
used_case_idx_inatt <- lavInspect(fit_inatt, "case.idx")
inatt_factor_scores <- lavPredict(fit_inatt, method = "regression")
dat_scaled_for_CFA$Conners_inatt_CTCR1 <- NA
dat_scaled_for_CFA$Conners_inatt_CTCR1[used_case_idx_inatt] <- inatt_factor_scores[,1]

# parent-only
dat_inatt_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(hconint1, lpconint1, npconint1, ppbhconninat1, u1pconinat1, PGgS, id_fam))

new_inatt_ctwrp_names <- c("yr8", "yr12", "yr14", "yr16", "yr21")
all_new_inatt_ctwrp_names <- c(new_inatt_ctwrp_names, "PGgS", "id_fam")
names(dat_inatt_ctwrp_var) <- all_new_inatt_ctwrp_names

Conners_Inattention_model_P <- '
  Inattention_P =~ yr8 + yr12 + yr14 + yr16 + yr21
  Inattention_P ~ PGgS
'
fit_inatt_P <- cfa(Conners_Inattention_model_P, data = dat_inatt_ctwrp_var, missing = "fiml", cluster = "id_fam")
# we got 5 timepoints for parents, 3 for child for inattention but only 2 for the rest from child; only 1 from teacher 
# so we only do a separate CFA for parent
print(summary(fit_inatt_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_inatt_P)

used_data_inatt_P <- lavInspect(fit_inatt_P, "data")
used_case_idx_inatt_P <- lavInspect(fit_inatt_P, "case.idx")
inatt_factor_scores_P <- lavPredict(fit_inatt_P, method = "regression")
dat_scaled_for_CFA$Conners_inatt_CTWR_P1 <- NA
dat_scaled_for_CFA$Conners_inatt_CTWR_P1[used_case_idx_inatt_P] <- inatt_factor_scores_P[,1]

# Model for 'Conners Hyperactivity-Impulsivity'
dat_hi_ctcr_var <- subset(dat_scaled_for_CFA, select = c(hconhit1, lpconhit1, npconhit1, ppbhconnimpt1, u1pconimpt1, ntconhit1, ncconhit1, u2cconnhypt1, PGgS, id_fam))

new_hi_ctcr_names <- c("yr8P", "yr12P", "yr14P", "yr16P", "yr21P", "yr14T", "yr14C", "yr21C")
all_new_hi_ctcr_names <- c(new_hi_ctcr_names, "PGgS", "id_fam")
names(dat_hi_ctcr_var) <- all_new_hi_ctcr_names

Conners_Hyperactivity_Impulsivity_model <- '
  Hyper_Impuls =~ yr8P + yr12P + yr14P + yr16P + yr21P + yr14T + yr14C + yr21C
  Hyper_Impuls ~ PGgS
'
fit_hyper_impul <- cfa(Conners_Hyperactivity_Impulsivity_model, data = dat_hi_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_hyper_impul, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_hyper_impul)

used_data_hyper_impul <- lavInspect(fit_hyper_impul, "data")
used_case_idx_hyper_impul <- lavInspect(fit_hyper_impul, "case.idx")
hyper_impul_factor_scores <- lavPredict(fit_hyper_impul, method = "regression")
dat_scaled_for_CFA$Conners_hyper_impul_CTCR1 <- NA
dat_scaled_for_CFA$Conners_hyper_impul_CTCR1[used_case_idx_hyper_impul] <- hyper_impul_factor_scores[,1]

# parent-only
dat_hi_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(hconhit1, lpconhit1, npconhit1, ppbhconnimpt1, u1pconimpt1, PGgS, id_fam))

new_hi_ctwrp_names <- c("yr8", "yr12", "yr14", "yr16", "yr21")
all_new_hi_ctwrp_names <- c(new_hi_ctwrp_names, "PGgS", "id_fam")
names(dat_hi_ctwrp_var) <- all_new_hi_ctwrp_names

Conners_Hyperactivity_Impulsivity_model_P <- '
  Hyper_Impuls_P =~ yr8 + yr12 + yr14 + yr16 + yr21
  Hyper_Impuls_P ~ PGgS
'
fit_hyper_impul_P <- cfa(Conners_Hyperactivity_Impulsivity_model_P, data = dat_hi_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_hyper_impul_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_hyper_impul_P)

used_data_hyper_impul_P <- lavInspect(fit_hyper_impul_P, "data")
used_case_idx_hyper_impul_P <- lavInspect(fit_hyper_impul_P, "case.idx")
hyper_impul_factor_scores_P <- lavPredict(fit_hyper_impul_P, method = "regression")
dat_scaled_for_CFA$Conners_hyper_impul_CTWR_P1 <- NA
dat_scaled_for_CFA$Conners_hyper_impul_CTWR_P1[used_case_idx_hyper_impul_P] <- hyper_impul_factor_scores_P[,1]

# Model for 'Conners Total'
dat_total_ctcr_var <- subset(dat_scaled_for_CFA, select = c(hconnt1, lpconnt1, npconnt1, ppbhconnt1, u1pcont1, ntconnt1, ncconnt1, u2cconnt1, PGgS, id_fam))

new_total_ctcr_names <- c("yr8P", "yr12P", "yr14P", "yr16P", "yr21P", "yr14T", "yr14C", "yr21C")
all_new_total_ctcr_names <- c(new_total_ctcr_names, "PGgS", "id_fam")
names(dat_total_ctcr_var) <- all_new_total_ctcr_names

Conners_Total_model <- '
  ADHD_Total =~ yr8P + yr12P + yr14P + yr16P + yr21P + yr14T + yr14C + yr21C
  ADHD_Total ~ PGgS
'
fit_ADHD <- cfa(Conners_Total_model, data = dat_total_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_ADHD, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_ADHD)

used_data_ADHD <- lavInspect(fit_ADHD, "data")
used_case_idx_ADHD <- lavInspect(fit_ADHD, "case.idx")
ADHD_factor_scores <- lavPredict(fit_ADHD, method = "regression")
dat_scaled_for_CFA$Conners_Total_CTCR1 <- NA
dat_scaled_for_CFA$Conners_Total_CTCR1[used_case_idx_ADHD] <- ADHD_factor_scores[,1]

# parent-only
dat_total_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(hconnt1, lpconnt1, npconnt1, ppbhconnt1, u1pcont1, PGgS, id_fam))

new_total_ctwrp_names <- c("yr8", "yr12", "yr14", "yr16", "yr21")
all_new_total_ctwrp_names <- c(new_total_ctwrp_names, "PGgS", "id_fam")
names(dat_total_ctwrp_var) <- all_new_total_ctwrp_names

Conners_Total_model_P <- '
  ADHD_Total_P =~ yr8 + yr12 + yr14 + yr16 + yr21
  ADHD_Total_P ~ PGgS
'
fit_ADHD_P <- cfa(Conners_Total_model_P, data = dat_total_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_ADHD_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_ADHD_P)

used_data_ADHD_P <- lavInspect(fit_ADHD_P, "data")
used_case_idx_ADHD_P <- lavInspect(fit_ADHD_P, "case.idx")
ADHD_factor_scores_P <- lavPredict(fit_ADHD_P, method = "regression")
dat_scaled_for_CFA$Conners_Total_CTWR_P1 <- NA
dat_scaled_for_CFA$Conners_Total_CTWR_P1[used_case_idx_ADHD_P] <- ADHD_factor_scores_P[,1]

cat("Conners model done\n")



# --- SDQ Traits ----
# Model for 'SDQ Conduct'
dat_conduct_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqccont1, csdqccont1, dsdqcont1, gpsdqcont1, ipsdqcont1, lpsdqcont1, ppbhsdqcont1, u1psdqcont1, gtsdqcont1, itsdqcont1, ltsdqcont1, lcsdqcont1, pcbhsdqcont1, u1csdqcont1, zmhsdqcont1, PGgS, id_fam))
new_conduct_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr16P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
all_new_conduct_ctcr_names <- c(new_conduct_ctcr_names, "PGgS", "id_fam")
names(dat_conduct_ctcr_var) <- all_new_conduct_ctcr_names

SDQ_Conduct_model <- '
  SDQ_Conduct =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
  SDQ_Conduct ~ PGgS
'
fit_SDQconduct <- cfa(SDQ_Conduct_model, data = dat_conduct_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQconduct, standardized = TRUE, fit.measures = TRUE)) # 0.19
# plot_sem_tree(fit_SDQconduct)

used_data_SDQconduct <- lavInspect(fit_SDQconduct, "data")
used_case_idx_SDQconduct <- lavInspect(fit_SDQconduct, "case.idx")
SDQconduct_factor_scores <- lavPredict(fit_SDQconduct, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTCR1[used_case_idx_SDQconduct] <- SDQconduct_factor_scores[,1]

# parent-only
dat_conduct_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqccont1, csdqccont1, dsdqcont1, gpsdqcont1, ipsdqcont1, lpsdqcont1, ppbhsdqcont1, u1psdqcont1, PGgS, id_fam))
new_conduct_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr16", "yr21")
all_new_conduct_ctwrp_names <- c(new_conduct_ctwrp_names, "PGgS", "id_fam")
names(dat_conduct_ctwrp_var) <- all_new_conduct_ctwrp_names

SDQ_Conduct_model_P <- '
  SDQ_Conduct_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21
  SDQ_Conduct_P ~ PGgS
'
fit_SDQconduct_P <- cfa(SDQ_Conduct_model_P, data = dat_conduct_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQconduct_P, standardized = TRUE, fit.measures = TRUE)) # 0.18
# plot_sem_tree(fit_SDQconduct_P)

used_data_SDQconduct_P <- lavInspect(fit_SDQconduct_P, "data")
used_case_idx_SDQconduct_P <- lavInspect(fit_SDQconduct_P, "case.idx")
SDQconduct_factor_scores_P <- lavPredict(fit_SDQconduct_P, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTWR_P1[used_case_idx_SDQconduct_P] <- SDQconduct_factor_scores_P[,1]

# teacher only
dat_conduct_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqcont1, itsdqcont1, ltsdqcont1, PGgS, id_fam))
new_conduct_ctwrt_names <- c("yr7", "yr9", "yr12")
all_new_conduct_ctwrt_names <- c(new_conduct_ctwrt_names, "PGgS", "id_fam")
names(dat_conduct_ctwrt_var) <- all_new_conduct_ctwrt_names

SDQ_Conduct_model_T <- '
  SDQ_Conduct_T =~ yr7 + yr9 + yr12
  SDQ_Conduct_T ~ PGgS
'
fit_SDQconduct_T <- cfa(SDQ_Conduct_model_T, data = dat_conduct_ctwrt_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQconduct_T, standardized = TRUE, fit.measures = TRUE)) # 0.13
# plot_sem_tree(fit_SDQconduct_T)

used_data_SDQconduct_T <- lavInspect(fit_SDQconduct_T, "data")
used_case_idx_SDQconduct_T <- lavInspect(fit_SDQconduct_T, "case.idx")
SDQconduct_factor_scores_T <- lavPredict(fit_SDQconduct_T, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTWR_T1[used_case_idx_SDQconduct_T] <- SDQconduct_factor_scores_T[,1]

# child only
dat_conduct_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqcont1, pcbhsdqcont1, u1csdqcont1, zmhsdqcont1, PGgS, id_fam))
new_conduct_ctwrc_names <- c("yr12", "yr16", "yr21", "yr26")
all_new_conduct_ctwrc_names <- c(new_conduct_ctwrc_names, "PGgS", "id_fam")
names(dat_conduct_ctwrc_var) <- all_new_conduct_ctwrc_names

SDQ_Conduct_model_C <- '
  SDQ_Conduct_C =~ yr12 + yr16 + yr21 + yr26
  SDQ_Conduct_C ~ PGgS
'
fit_SDQconduct_C <- cfa(SDQ_Conduct_model_C, data = dat_conduct_ctwrc_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQconduct_C, standardized = TRUE, fit.measures = TRUE)) # 0.19
# plot_sem_tree(fit_SDQconduct_C)

used_data_SDQconduct_C <- lavInspect(fit_SDQconduct_C, "data")
used_case_idx_SDQconduct_C <- lavInspect(fit_SDQconduct_C, "case.idx")
SDQconduct_factor_scores_C <- lavPredict(fit_SDQconduct_C, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTWR_C1[used_case_idx_SDQconduct_C] <- SDQconduct_factor_scores_C[,1]

cat("SDQ Conduct done\n")

# Model for 'SDQ Emotion'
dat_emot_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcemot1, csdqcemot1, dsdqemot1, gpsdqemot1, ipsdqemot1, lpsdqemot1, u1psdqemot1, gtsdqemot1, itsdqemot1, ltsdqemot1, lcsdqemot1, pcbhsdqemot1, u1csdqemot1, zmhsdqemot1, PGgS, id_fam))
new_emot_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
all_new_emot_ctcr_names <- c(new_emot_ctcr_names, "PGgS", "id_fam")
names(dat_emot_ctcr_var) <- all_new_emot_ctcr_names

SDQ_Emotion_model <- '
  SDQ_Emotion =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
  SDQ_Emotion ~ PGgS
'
fit_SDQemot <- cfa(SDQ_Emotion_model, data = dat_emot_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQemot, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQemot)

used_data_SDQemot <- lavInspect(fit_SDQemot, "data")
used_case_idx_SDQemot <- lavInspect(fit_SDQemot, "case.idx")
SDQemot_factor_scores <- lavPredict(fit_SDQemot, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTCR1[used_case_idx_SDQemot] <- SDQemot_factor_scores[,1]

# parent-only
dat_emot_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcemot1, csdqcemot1, dsdqemot1, gpsdqemot1, ipsdqemot1, lpsdqemot1, u1psdqemot1, PGgS, id_fam))
new_emot_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr21")
all_new_emot_ctwrp_names <- c(new_emot_ctwrp_names, "PGgS", "id_fam")
names(dat_emot_ctwrp_var) <- all_new_emot_ctwrp_names

SDQ_Emotion_model_P <- '
  SDQ_Emotion_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr21
  SDQ_Emotion_P ~ PGgS
'
fit_SDQemot_P <- cfa(SDQ_Emotion_model_P, data = dat_emot_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQemot_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQemot_P)

used_data_SDQemot_P <- lavInspect(fit_SDQemot_P, "data")
used_case_idx_SDQemot_P <- lavInspect(fit_SDQemot_P, "case.idx")
SDQemot_factor_scores_P <- lavPredict(fit_SDQemot_P, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTWR_P1[used_case_idx_SDQemot_P] <- SDQemot_factor_scores_P[,1]

# teacher only
dat_emot_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqemot1, itsdqemot1, ltsdqemot1, PGgS, id_fam))
new_emot_ctwrt_names <- c("yr7", "yr9", "yr12")
all_new_emot_ctwrt_names <- c(new_emot_ctwrt_names, "PGgS", "id_fam")
names(dat_emot_ctwrt_var) <- all_new_emot_ctwrt_names

SDQ_Emotion_model_T <- '
  SDQ_Emotion_T =~ yr7 + yr9 + yr12
  SDQ_Emotion_T ~ PGgS
'
fit_SDQemot_T <- cfa(SDQ_Emotion_model_T, data = dat_emot_ctwrt_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQemot_T, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQemot_T)

used_data_SDQemot_T <- lavInspect(fit_SDQemot_T, "data")
used_case_idx_SDQemot_T <- lavInspect(fit_SDQemot_T, "case.idx")
SDQemot_factor_scores_T <- lavPredict(fit_SDQemot_T, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTWR_T1[used_case_idx_SDQemot_T] <- SDQemot_factor_scores_T[,1]

# child only
dat_emot_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqemot1, pcbhsdqemot1, u1csdqemot1, zmhsdqemot1, PGgS, id_fam))
new_emot_ctwrc_names <- c("yr12", "yr16", "yr21", "yr26")
all_new_emot_ctwrc_names <- c(new_emot_ctwrc_names, "PGgS", "id_fam")
names(dat_emot_ctwrc_var) <- all_new_emot_ctwrc_names

SDQ_Emotion_model_C <- '
  SDQ_Emotion_C =~ yr12 + yr16 + yr21 + yr26
  SDQ_Emotion_C ~ PGgS
'
fit_SDQemot_C <- cfa(SDQ_Emotion_model_C, data = dat_emot_ctwrc_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQemot_C, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQemot_C)

used_data_SDQemot_C <- lavInspect(fit_SDQemot_C, "data")
used_case_idx_SDQemot_C <- lavInspect(fit_SDQemot_C, "case.idx")
SDQemot_factor_scores_C <- lavPredict(fit_SDQemot_C, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTWR_C1[used_case_idx_SDQemot_C] <- SDQemot_factor_scores_C[,1]

cat("SDQ Emotion done\n")

# Model for 'SDQ Hyperactivity'
dat_hyper_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqchypt1, csdqchypt1, dsdqhypt1, gpsdqhypt1, ipsdqhypt1, lpsdqhypt1, ppbhsdqhypt1, u1psdqhypt1, gtsdqhypt1, itsdqhypt1, ltsdqhypt1, lcsdqhypt1, pcbhsdqhypt1, u1csdqhypt1, zmhsdqhypt1, PGgS, id_fam))
new_hyper_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr16P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
all_new_hyper_ctcr_names <- c(new_hyper_ctcr_names, "PGgS", "id_fam")
names(dat_hyper_ctcr_var) <- all_new_hyper_ctcr_names

SDQ_Hyperactivity_model <- '
  SDQ_Hyperactivity =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
  SDQ_Hyperactivity ~ PGgS
'
fit_SDQhyper <- cfa(SDQ_Hyperactivity_model, data = dat_hyper_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQhyper, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQhyper)

used_data_SDQhyper <- lavInspect(fit_SDQhyper, "data")
used_case_idx_SDQhyper <- lavInspect(fit_SDQhyper, "case.idx")
SDQhyper_factor_scores <- lavPredict(fit_SDQhyper, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTCR1[used_case_idx_SDQhyper] <- SDQhyper_factor_scores[,1]

# parent-only
dat_hyper_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqchypt1, csdqchypt1, dsdqhypt1, gpsdqhypt1, ipsdqhypt1, lpsdqhypt1, ppbhsdqhypt1, u1psdqhypt1, PGgS, id_fam))
new_hyper_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr16", "yr21")
all_new_hyper_ctwrp_names <- c(new_hyper_ctwrp_names, "PGgS", "id_fam")
names(dat_hyper_ctwrp_var) <- all_new_hyper_ctwrp_names

SDQ_Hyperactivity_model_P <- '
  SDQ_Hyperactivity_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21
  SDQ_Hyperactivity_P ~ PGgS
'
fit_SDQhyper_P <- cfa(SDQ_Hyperactivity_model_P, data = dat_hyper_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQhyper_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQhyper_P)

used_data_SDQhyper_P <- lavInspect(fit_SDQhyper_P, "data")
used_case_idx_SDQhyper_P <- lavInspect(fit_SDQhyper_P, "case.idx")
SDQhyper_factor_scores_P <- lavPredict(fit_SDQhyper_P, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_P1[used_case_idx_SDQhyper_P] <- SDQhyper_factor_scores_P[,1]

# teacher only
dat_hyper_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqhypt1, itsdqhypt1, ltsdqhypt1, PGgS, id_fam))
new_hyper_ctwrt_names <- c("yr7", "yr9", "yr12")
all_new_hyper_ctwrt_names <- c(new_hyper_ctwrt_names, "PGgS", "id_fam")
names(dat_hyper_ctwrt_var) <- all_new_hyper_ctwrt_names

SDQ_Hyperactivity_model_T <- '
  SDQ_Hyperactivity_T =~ yr7 + yr9 + yr12
  SDQ_Hyperactivity_T ~ PGgS
'
fit_SDQhyper_T <- cfa(SDQ_Hyperactivity_model_T, data = dat_hyper_ctwrt_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQhyper_T, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQhyper_T)

used_data_SDQhyper_T <- lavInspect(fit_SDQhyper_T, "data")
used_case_idx_SDQhyper_T <- lavInspect(fit_SDQhyper_T, "case.idx")
SDQhyper_factor_scores_T <- lavPredict(fit_SDQhyper_T, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_T1[used_case_idx_SDQhyper_T] <- SDQhyper_factor_scores_T[,1]

# child only
dat_hyper_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqhypt1, pcbhsdqhypt1, u1csdqhypt1, zmhsdqhypt1, PGgS, id_fam))
new_hyper_ctwrc_names <- c("yr12", "yr16", "yr21", "yr26")
all_new_hyper_ctwrc_names <- c(new_hyper_ctwrc_names, "PGgS", "id_fam")
names(dat_hyper_ctwrc_var) <- all_new_hyper_ctwrc_names

SDQ_Hyperactivity_model_C <- '
  SDQ_Hyperactivity_C =~ yr12 + yr16 + yr21 + yr26
  SDQ_Hyperactivity_C ~ PGgS
'
fit_SDQhyper_C <- cfa(SDQ_Hyperactivity_model_C, data = dat_hyper_ctwrc_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQhyper_C, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQhyper_C)

used_data_SDQhyper_C <- lavInspect(fit_SDQhyper_C, "data")
used_case_idx_SDQhyper_C <- lavInspect(fit_SDQhyper_C, "case.idx")
SDQhyper_factor_scores_C <- lavPredict(fit_SDQhyper_C, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_C1[used_case_idx_SDQhyper_C] <- SDQhyper_factor_scores_C[,1]

cat("SDQ Hyperactivity done\n")

# Model for 'SDQ Peer Problems'
dat_peer_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcpert1, csdqcpert1, dsdqpert1, gpsdqpert1, ipsdqpert1, lpsdqpert1, u1psdqpert1, gtsdqpert1, itsdqpert1, ltsdqpert1, lcsdqpert1, pcbhsdqpert1, u1csdqpert1, zmhsdqpert1, PGgS, id_fam))
new_peer_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
all_new_peer_ctcr_names <- c(new_peer_ctcr_names, "PGgS", "id_fam")
names(dat_peer_ctcr_var) <- all_new_peer_ctcr_names

SDQ_Peer_Problems_model <- '
  SDQ_Peer_Problems =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
  SDQ_Peer_Problems ~ PGgS
'
fit_SDQpeer <- cfa(SDQ_Peer_Problems_model, data = dat_peer_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQpeer, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQpeer)

used_data_SDQpeer <- lavInspect(fit_SDQpeer, "data")
used_case_idx_SDQpeer <- lavInspect(fit_SDQpeer, "case.idx")
SDQpeer_factor_scores <- lavPredict(fit_SDQpeer, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTCR1[used_case_idx_SDQpeer] <- SDQpeer_factor_scores[,1]

# parent-only
dat_peer_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcpert1, csdqcpert1, dsdqpert1, gpsdqpert1, ipsdqpert1, lpsdqpert1, u1psdqpert1, PGgS, id_fam))
new_peer_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr21")
all_new_peer_ctwrp_names <- c(new_peer_ctwrp_names, "PGgS", "id_fam")
names(dat_peer_ctwrp_var) <- all_new_peer_ctwrp_names

SDQ_Peer_Problems_model_P <- '
  SDQ_Peer_Problems_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr21
  SDQ_Peer_Problems_P ~ PGgS
'
fit_SDQpeer_P <- cfa(SDQ_Peer_Problems_model_P, data = dat_peer_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQpeer_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQpeer_P)

used_data_SDQpeer_P <- lavInspect(fit_SDQpeer_P, "data")
used_case_idx_SDQpeer_P <- lavInspect(fit_SDQpeer_P, "case.idx")
SDQpeer_factor_scores_P <- lavPredict(fit_SDQpeer_P, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_P1[used_case_idx_SDQpeer_P] <- SDQpeer_factor_scores_P[,1]

# teacher only
dat_peer_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqpert1, itsdqpert1, ltsdqpert1, PGgS, id_fam))
new_peer_ctwrt_names <- c("yr7", "yr9", "yr12")
all_new_peer_ctwrt_names <- c(new_peer_ctwrt_names, "PGgS", "id_fam")
names(dat_peer_ctwrt_var) <- all_new_peer_ctwrt_names

SDQ_Peer_Problems_model_T <- '
  SDQ_Peer_Problems_T =~ yr7 + yr9 + yr12
  SDQ_Peer_Problems_T ~ PGgS
'
fit_SDQpeer_T <- cfa(SDQ_Peer_Problems_model_T, data = dat_peer_ctwrt_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQpeer_T, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQpeer_T)

used_data_SDQpeer_T <- lavInspect(fit_SDQpeer_T, "data")
used_case_idx_SDQpeer_T <- lavInspect(fit_SDQpeer_T, "case.idx")
SDQpeer_factor_scores_T <- lavPredict(fit_SDQpeer_T, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_T1[used_case_idx_SDQpeer_T] <- SDQpeer_factor_scores_T[,1]

# child only
dat_peer_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqpert1, pcbhsdqpert1, u1csdqpert1, zmhsdqpert1, PGgS, id_fam))
new_peer_ctwrc_names <- c("yr12", "yr16", "yr21", "yr26")
all_new_peer_ctwrc_names <- c(new_peer_ctwrc_names, "PGgS", "id_fam")
names(dat_peer_ctwrc_var) <- all_new_peer_ctwrc_names

SDQ_Peer_Problems_model_C <- '
  SDQ_Peer_Problems_C =~ yr12 + yr16 + yr21 + yr26
  SDQ_Peer_Problems_C ~ PGgS
'
fit_SDQpeer_C <- cfa(SDQ_Peer_Problems_model_C, data = dat_peer_ctwrc_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQpeer_C, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQpeer_C)

used_data_SDQpeer_C <- lavInspect(fit_SDQpeer_C, "data")
used_case_idx_SDQpeer_C <- lavInspect(fit_SDQpeer_C, "case.idx")
SDQpeer_factor_scores_C <- lavPredict(fit_SDQpeer_C, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_C1[used_case_idx_SDQpeer_C] <- SDQpeer_factor_scores_C[,1]

cat("SDQ Peer Problem done\n")


# Model for 'SDQ Prosocial'
dat_prosocial_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcprot1, csdqcprot1, dsdqprot1, gpsdqprot1, ipsdqprot1, lpsdqprot1, ppbhsdqprot1, u1psdqprot1, gtsdqprot1, itsdqprot1, ltsdqprot1, lcsdqprot1, pcbhsdqprot1, u1csdqprot1, zmhsdqprot1, PGgS, id_fam))
new_prosocial_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr16P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
all_new_prosocial_ctcr_names <- c(new_prosocial_ctcr_names, "PGgS", "id_fam")
names(dat_prosocial_ctcr_var) <- all_new_prosocial_ctcr_names

SDQ_Prosocial_model <- '
  SDQ_Prosocial =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
  SDQ_Prosocial ~ PGgS
'
fit_SDQprosocial <- cfa(SDQ_Prosocial_model, data = dat_prosocial_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQprosocial, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQprosocial)

used_data_SDQprosocial <- lavInspect(fit_SDQprosocial, "data")
used_case_idx_SDQprosocial <- lavInspect(fit_SDQprosocial, "case.idx")
SDQprosocial_factor_scores <- lavPredict(fit_SDQprosocial, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTCR1[used_case_idx_SDQprosocial] <- SDQprosocial_factor_scores[,1]

# parent-only
dat_prosocial_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcprot1, csdqcprot1, dsdqprot1, gpsdqprot1, ipsdqprot1, lpsdqprot1, ppbhsdqprot1, u1psdqprot1, PGgS, id_fam))
new_prosocial_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr16", "yr21")
all_new_prosocial_ctwrp_names <- c(new_prosocial_ctwrp_names, "PGgS", "id_fam")
names(dat_prosocial_ctwrp_var) <- all_new_prosocial_ctwrp_names

SDQ_Prosocial_model_P <- '
  SDQ_Prosocial_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21
  SDQ_Prosocial_P ~ PGgS
'
fit_SDQprosocial_P <- cfa(SDQ_Prosocial_model_P, data = dat_prosocial_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQprosocial_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQprosocial_P)

used_data_SDQprosocial_P <- lavInspect(fit_SDQprosocial_P, "data")
used_case_idx_SDQprosocial_P <- lavInspect(fit_SDQprosocial_P, "case.idx")
SDQprosocial_factor_scores_P <- lavPredict(fit_SDQprosocial_P, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_P1[used_case_idx_SDQprosocial_P] <- SDQprosocial_factor_scores_P[,1]

# teacher only
dat_prosocial_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqprot1, itsdqprot1, ltsdqprot1, PGgS, id_fam))
new_prosocial_ctwrt_names <- c("yr7", "yr9", "yr12")
all_new_prosocial_ctwrt_names <- c(new_prosocial_ctwrt_names, "PGgS", "id_fam")
names(dat_prosocial_ctwrt_var) <- all_new_prosocial_ctwrt_names

SDQ_Prosocial_model_T <- '
  SDQ_Prosocial_T =~ yr7 + yr9 + yr12
  SDQ_Prosocial_T ~ PGgS
'
fit_SDQprosocial_T <- cfa(SDQ_Prosocial_model_T, data = dat_prosocial_ctwrt_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQprosocial_T, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQprosocial_T)

used_data_SDQprosocial_T <- lavInspect(fit_SDQprosocial_T, "data")
used_case_idx_SDQprosocial_T <- lavInspect(fit_SDQprosocial_T, "case.idx")
SDQprosocial_factor_scores_T <- lavPredict(fit_SDQprosocial_T, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_T1[used_case_idx_SDQprosocial_T] <- SDQprosocial_factor_scores_T[,1]

# child only
dat_prosocial_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqprot1, pcbhsdqprot1, u1csdqprot1, zmhsdqprot1, PGgS, id_fam))
new_prosocial_ctwrc_names <- c("yr12", "yr16", "yr21", "yr26")
all_new_prosocial_ctwrc_names <- c(new_prosocial_ctwrc_names, "PGgS", "id_fam")
names(dat_prosocial_ctwrc_var) <- all_new_prosocial_ctwrc_names

SDQ_Prosocial_model_C <- '
  SDQ_Prosocial_C =~ yr12 + yr16 + yr21 + yr26
  SDQ_Prosocial_C ~ PGgS
'
fit_SDQprosocial_C <- cfa(SDQ_Prosocial_model_C, data = dat_prosocial_ctwrc_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQprosocial_C, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQprosocial_C)

used_data_SDQprosocial_C <- lavInspect(fit_SDQprosocial_C, "data")
used_case_idx_SDQprosocial_C <- lavInspect(fit_SDQprosocial_C, "case.idx")
SDQprosocial_factor_scores_C <- lavPredict(fit_SDQprosocial_C, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_C1[used_case_idx_SDQprosocial_C] <- SDQprosocial_factor_scores_C[,1]

cat("SDQ Prosocial done\n")

# Model for 'SDQ Total Problems'
dat_total_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcbeht1, csdqcbeht1, dsdqbeht1, gpsdqbeht1, ipsdqbeht1, lpsdqbeht1, ppbhsdqbeht1, u1psdqbeht1, gtsdqbeht1, itsdqbeht1, ltsdqbeht1, lcsdqbeht1, pcbhsdqbeht1, u1csdqbeht1, zmhsdqbeht1, PGgS, id_fam))
new_total_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr16P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
all_new_total_ctcr_names <- c(new_total_ctcr_names, "PGgS", "id_fam")
names(dat_total_ctcr_var) <- all_new_total_ctcr_names

SDQ_Total_Problems_model <- '
  SDQ_Total_Problems =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
  SDQ_Total_Problems ~ PGgS
'
fit_SDQtotal <- cfa(SDQ_Total_Problems_model, data = dat_total_ctcr_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQtotal, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQtotal)

used_data_SDQtotal <- lavInspect(fit_SDQtotal, "data")
used_case_idx_SDQtotal <- lavInspect(fit_SDQtotal, "case.idx")
SDQtotal_factor_scores <- lavPredict(fit_SDQtotal, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTCR1[used_case_idx_SDQtotal] <- SDQtotal_factor_scores[,1]

# parent-only
dat_total_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcbeht1, csdqcbeht1, dsdqbeht1, gpsdqbeht1, ipsdqbeht1, lpsdqbeht1, ppbhsdqbeht1, u1psdqbeht1, PGgS, id_fam))
new_total_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr16", "yr21")
all_new_total_ctwrp_names <- c(new_total_ctwrp_names, "PGgS", "id_fam")
names(dat_total_ctwrp_var) <- all_new_total_ctwrp_names

SDQ_Total_Problems_model_P <- '
  SDQ_Total_Problems_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21
  SDQ_Total_Problems_P ~ PGgS
'
fit_SDQtotal_P <- cfa(SDQ_Total_Problems_model_P, data = dat_total_ctwrp_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQtotal_P, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQtotal_P)

used_data_SDQtotal_P <- lavInspect(fit_SDQtotal_P, "data")
used_case_idx_SDQtotal_P <- lavInspect(fit_SDQtotal_P, "case.idx")
SDQtotal_factor_scores_P <- lavPredict(fit_SDQtotal_P, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_P1[used_case_idx_SDQtotal_P] <- SDQtotal_factor_scores_P[,1]

# teacher only
dat_total_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqbeht1, itsdqbeht1, ltsdqbeht1, PGgS, id_fam))
new_total_ctwrt_names <- c("yr7", "yr9", "yr12")
all_new_total_ctwrt_names <- c(new_total_ctwrt_names, "PGgS", "id_fam")
names(dat_total_ctwrt_var) <- all_new_total_ctwrt_names

SDQ_Total_Problems_model_T <- '
  SDQ_Total_Problems_T =~ yr7 + yr9 + yr12
  SDQ_Total_Problems_T ~ PGgS
'
fit_SDQtotal_T <- cfa(SDQ_Total_Problems_model_T, data = dat_total_ctwrt_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQtotal_T, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQtotal_T)

used_data_SDQtotal_T <- lavInspect(fit_SDQtotal_T, "data")
used_case_idx_SDQtotal_T <- lavInspect(fit_SDQtotal_T, "case.idx")
SDQtotal_factor_scores_T <- lavPredict(fit_SDQtotal_T, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_T1[used_case_idx_SDQtotal_T] <- SDQtotal_factor_scores_T[,1]

# child only
dat_total_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqbeht1, pcbhsdqbeht1, u1csdqbeht1, zmhsdqbeht1, PGgS, id_fam))
new_total_ctwrc_names <- c("yr12", "yr16", "yr21", "yr26")
all_new_total_ctwrc_names <- c(new_total_ctwrc_names, "PGgS", "id_fam")
names(dat_total_ctwrc_var) <- all_new_total_ctwrc_names

SDQ_Total_Problems_model_C <- '
  SDQ_Total_Problems_C =~ yr12 + yr16 + yr21 + yr26
  SDQ_Total_Problems_C ~ PGgS
'
fit_SDQtotal_C <- cfa(SDQ_Total_Problems_model_C, data = dat_total_ctwrc_var, missing = "fiml", cluster = "id_fam")
print(summary(fit_SDQtotal_C, standardized = TRUE, fit.measures = TRUE))
# plot_sem_tree(fit_SDQtotal_C)

used_data_SDQtotal_C <- lavInspect(fit_SDQtotal_C, "data")
used_case_idx_SDQtotal_C <- lavInspect(fit_SDQtotal_C, "case.idx")
SDQtotal_factor_scores_C <- lavPredict(fit_SDQtotal_C, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_C1[used_case_idx_SDQtotal_C] <- SDQtotal_factor_scores_C[,1]

cat("SDQ Total Problem done\n")



# Save all plots ####
fit_list <- list(
  # Cognitive Composites
  "General Cognitive Ability (g) - Cross Rater" = fit_g,
  "Verbal Ability - Cross Rater" = fit_vb,
  "Nonverbal Ability - Cross Rater" = fit_nv,
  
  # Anxiety Traits (ARBQ)
  "ARBQ Shyness - Cross Rater" = fit_shy,
  "ARBQ Fear - Cross Rater" = fit_fear,
  
  "ARBQ Obsessive-Compulsive - Cross Rater" = fit_ocb,
  "ARBQ Negative Affect - Cross Rater" = fit_naff,
  "ARBQ Negative Cognition - Cross Rater" = fit_ncog,
  "ARBQ Anxiety Total - Cross Rater" = fit_ARBQ_total,
  
  # Conners Traits (ADHD)
  "Conners Inattention - Cross Rater" = fit_inatt,
  "Conners Inattention - Parent Only" = fit_inatt_P,
  "Conners Hyperactivity-Impulsivity - Cross Rater" = fit_hyper_impul,
  "Conners Hyperactivity-Impulsivity - Parent Only" = fit_hyper_impul_P,
  "Conners Total - Cross Rater" = fit_ADHD,
  "Conners Total - Parent Only" = fit_ADHD_P,
  
  # SDQ Traits
  "SDQ Conduct - Cross Rater" = fit_SDQconduct,
  "SDQ Conduct - Parent Only" = fit_SDQconduct_P,
  "SDQ Conduct - Teacher Only" = fit_SDQconduct_T,
  "SDQ Conduct - Child Only" = fit_SDQconduct_C,
  "SDQ Emotion - Cross Rater" = fit_SDQemot,
  "SDQ Emotion - Parent Only" = fit_SDQemot_P,
  "SDQ Emotion - Teacher Only" = fit_SDQemot_T,
  "SDQ Emotion - Child Only" = fit_SDQemot_C,
  "SDQ Hyperactivity - Cross Rater" = fit_SDQhyper,
  "SDQ Hyperactivity - Parent Only" = fit_SDQhyper_P,
  "SDQ Hyperactivity - Teacher Only" = fit_SDQhyper_T,
  "SDQ Hyperactivity - Child Only" = fit_SDQhyper_C,
  "SDQ Peer Problems - Cross Rater" = fit_SDQpeer,
  "SDQ Peer Problems - Parent Only" = fit_SDQpeer_P,
  "SDQ Peer Problems - Teacher Only" = fit_SDQpeer_T,
  "SDQ Peer Problems - Child Only" = fit_SDQpeer_C,
  "SDQ Prosocial - Cross Rater" = fit_SDQprosocial,
  "SDQ Prosocial - Parent Only" = fit_SDQprosocial_P,
  "SDQ Prosocial - Teacher Only" = fit_SDQprosocial_T,
  "SDQ Prosocial - Child Only" = fit_SDQprosocial_C,
  "SDQ Total Problems - Cross Rater" = fit_SDQtotal,
  "SDQ Total Problems - Parent Only" = fit_SDQtotal_P,
  "SDQ Total Problems - Teacher Only" = fit_SDQtotal_T,
  "SDQ Total Problems - Child Only" = fit_SDQtotal_C
)

pdf("SEM_Factor_Models_All.pdf", width = 20, height = 15)

for(i in seq_along(fit_list)) {
  cat("Plotting:", names(fit_list)[i], "\n")  
  plot_sem_tree(fit_list[[i]])
  title(main = names(fit_list)[i], outer = TRUE, line = -1)
}

dev.off()

cat("All plots saved to 'SEM_Factor_Models_All.pdf'\n")



# Export results ####
# Create lists to store the results
all_fit_indices <- list()
all_loadings <- list()
all_predictions <- list()

# Define the set of scaled fit measures to extract
fit_measures_of_interest_scaled <- c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "rmsea.scaled", "srmr")

# Loop through each model in your fit_list
for (model_name in names(fit_list)) {
  # Get the current model fit object
  fit <- fit_list[[model_name]]
  
  # --- Extract Scaled Fit Indices ---
  all_fit_indices[[model_name]] <- fitMeasures(fit, fit_measures_of_interest_scaled)
  
  # --- Extract Standardized Parameters (Loadings & Predictions) ---
  params <- standardizedSolution(fit)
  
  # --- Isolate Factor Loadings ---
  loadings <- params %>%
    filter(op == "=~") %>%
    select(Latent_Factor = lhs, Indicator = rhs, Loading = est.std, SE = se, P_value = pvalue)
  all_loadings[[model_name]] <- loadings
  
  # --- Isolate PGS Prediction ---
  prediction <- params %>%
    filter(op == "~") %>%
    select(Outcome = lhs, Predictor = rhs, Beta = est.std, SE = se, P_value = pvalue)
  all_predictions[[model_name]] <- prediction
}

# --- Combine Fit Indices (MODIFIED) ----
fit_summary_list <- lapply(names(all_fit_indices), function(model_name) {
  # Extract the named vector of fit indices
  indices <- all_fit_indices[[model_name]]
  # Explicitly create a one-row data frame, which handles NAs gracefully
  data.frame(
    Model        = model_name,
    ChiSq_Scaled = indices["chisq.scaled"],
    df           = indices["df"],
    P_Scaled     = indices["pvalue.scaled"],
    CFI_Scaled   = indices["cfi.scaled"],
    RMSEA_Scaled = indices["rmsea.scaled"],
    SRMR         = indices["srmr"] # SRMR is typically unscaled
  )
})
fit_summary <- dplyr::bind_rows(fit_summary_list)

cat("--- Model Fit Summary (Scaled Results) ---\n")
print(fit_summary)
write.csv(fit_summary, "SEM_Fit_Summary_Scaled.csv", row.names = FALSE)

# --- Combine Prediction Results (UNCHANGED) ----
prediction_summary_list <- lapply(names(all_predictions), function(model_name) {
  df <- all_predictions[[model_name]]
  df$Model <- model_name
  df[, c("Model", setdiff(names(df), "Model"))]
})
prediction_summary <- dplyr::bind_rows(prediction_summary_list)

cat("\n\n--- Polygenic Score Prediction Summary ---\n")
print(prediction_summary)
write.csv(prediction_summary, "SEM_PGS_Prediction_Summary.csv", row.names = FALSE)

# --- Combine Loadings (UNCHANGED) ----
loadings_summary_list <- lapply(names(all_loadings), function(model_name) {
  df <- all_loadings[[model_name]]
  df$Model <- model_name
  df[, c("Model", setdiff(names(df), "Model"))]
})
loadings_summary <- dplyr::bind_rows(loadings_summary_list)

cat("\n\n--- Factor Loadings Summary ---\n")
print(head(loadings_summary, 15))
write.csv(loadings_summary, "SEM_Factor_Loadings_Summary.csv", row.names = FALSE)
