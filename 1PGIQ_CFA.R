# Phenotypic CFA for cognitive and mental health underlying traits predicted by polygenic g score

library(lavaan)
library(dplyr)
library(semPlot)
library(broom)
library(purrr)

sourceFileStem <- './Data/'
dat_scaled_for_CFA <- read.csv(paste0(sourceFileStem, "dat_scaled_for_CFA.csv"))
dat_scaled_for_CFA$PGgS <- dat_scaled_for_CFA$PGg_EA

# Plot function
plot_sem_tree <- function(fit, labels_list = NULL) {
  semPaths(fit,
           what = "std",           
           layout = "tree2",       
           rotation = 2,           
           residuals = FALSE,      
           edge.label.cex = 0.8,   
           edge.label.position = 0.55, 
           sizeLat = 10,           
           sizeLat2 = 6,           
           sizeMan = 7,            
           sizeMan2 = 5,           
           label.cex = 1.0,        
           fade = FALSE,           
           style = "lisrel",       
           nCharNodes = 0,         
           labels = labels_list,   
           mar = c(2, 2, 2, 2),    
           curvePivot = TRUE)      
}

# --- Cognitive Composites ----
# Model for 'g'
dat_g_var <- subset(dat_scaled_for_CFA, select = c(brawg1, crawg1, drawg1, gcg1, icg1, jcg1, lcg1, ncg1, pcg1, ucgt1, id_fam))

new_cog_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr10", "yr12", "yr14", "yr16", "yr25")
all_new_names <- c(new_cog_names, "id_fam")
names(dat_g_var) <- all_new_names

g_model <- '
  g =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr10 + yr12 + yr14 + yr16 + yr25
'
fit_g <- cfa(g_model, data = dat_g_var, missing = "fiml", cluster = "id_fam")
used_case_idx <- lavInspect(fit_g, "case.idx")
g_factor_scores <- lavPredict(fit_g, method = "regression")
dat_scaled_for_CFA$g_CTCR1 <- NA
dat_scaled_for_CFA$g_CTCR1[used_case_idx] <- g_factor_scores[,1]

# Model for 'g' by developmental stage 
g_stage_model <- '
  Early_g  =~ yr2 + yr3 + yr4
  Child_g =~ yr7 + yr9 + yr10
  Adol_g   =~ yr12 + yr14 + yr16
  Latent_Stage_g =~ Early_g + Child_g + Adol_g + yr25
'
fit_g_stage <- cfa(g_stage_model, data = dat_g_var, missing = "fiml", cluster = "id_fam")
used_case_idx_stage <- lavInspect(fit_g_stage, "case.idx")
g_stage_factor_scores <- lavPredict(fit_g_stage, method = "regression")

dat_scaled_for_CFA$g_stage_Early1 <- NA
dat_scaled_for_CFA$g_stage_Early1[used_case_idx_stage] <- g_stage_factor_scores[,"Early_g"]
dat_scaled_for_CFA$g_stage_Child1 <- NA
dat_scaled_for_CFA$g_stage_Child1[used_case_idx_stage] <- g_stage_factor_scores[,"Child_g"]
dat_scaled_for_CFA$g_stage_Adol1 <- NA
dat_scaled_for_CFA$g_stage_Adol1[used_case_idx_stage] <- g_stage_factor_scores[,"Adol_g"]
dat_scaled_for_CFA$g_stage_Latent1 <- NA
dat_scaled_for_CFA$g_stage_Latent1[used_case_idx_stage] <- g_stage_factor_scores[,"Latent_Stage_g"]

# Model for 'g' by methods
g_method_model <- '
  Early_MCDI_PARCA =~ yr2 + yr3 + yr4
  Middle_WISC_Raven =~ yr7 + yr9 + yr10 + yr12 + yr14
  Later_MillHill_Raven =~ yr16 + yr25
  Latent_Measure_g =~ Early_MCDI_PARCA + Middle_WISC_Raven + Later_MillHill_Raven
'
fit_g_method <- cfa(g_method_model, data = dat_g_var, missing = "fiml", cluster = "id_fam")
used_case_idx_method <- lavInspect(fit_g_method, "case.idx")
g_method_factor_scores <- lavPredict(fit_g_method, method = "regression")

dat_scaled_for_CFA$g_method_Early_MCDI_PARCA1 <- NA
dat_scaled_for_CFA$g_method_Early_MCDI_PARCA1[used_case_idx_method] <- g_method_factor_scores[,"Early_MCDI_PARCA"]
dat_scaled_for_CFA$g_method_Middle_WISC_Raven1 <- NA
dat_scaled_for_CFA$g_method_Middle_WISC_Raven1[used_case_idx_method] <- g_method_factor_scores[,"Middle_WISC_Raven"]
dat_scaled_for_CFA$g_method_Later_MillHill_Raven1 <- NA
dat_scaled_for_CFA$g_method_Later_MillHill_Raven1[used_case_idx_method] <- g_method_factor_scores[,"Later_MillHill_Raven"]
dat_scaled_for_CFA$g_method_Latent1 <- NA
dat_scaled_for_CFA$g_method_Latent1[used_case_idx_method] <- g_method_factor_scores[,"Latent_Measure_g"]

# Model for 'verbal ability'
dat_vb_var <- subset(dat_scaled_for_CFA, select = c(bscv1, cscv1, dscv1, gcl1, icvb1, jcvb1, lverbal12T1, pcvctota1, ucgvbt1, id_fam))
new_vb_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr10", "yr12", "yr16", "yr25")
all_new_vb_names <- c(new_vb_names, "id_fam")
names(dat_vb_var) <- all_new_vb_names

verbal_ability_model <- '
  Verbal_Ability =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr10 + yr12 + yr16 + yr25
'
fit_vb <- cfa(verbal_ability_model, data = dat_vb_var, missing = "fiml", cluster = "id_fam")
used_case_idx_vb <- lavInspect(fit_vb, "case.idx")
vb_factor_scores <- lavPredict(fit_vb, method = "regression")
dat_scaled_for_CFA$vb_CTCR1 <- NA
dat_scaled_for_CFA$vb_CTCR1[used_case_idx_vb] <- vb_factor_scores[,1]

# Model for 'verbal ability' by developmental stage
verbal_stage_model <- '
  Early_VB  =~ yr2 + yr3 + yr4
  Child_VB =~ yr7 + yr9 + yr10
  Adol_VB   =~ yr12 + yr16 
  Latent_Stage_VB =~ Early_VB + Child_VB + Adol_VB + yr25
'
fit_vb_stage <- cfa(verbal_stage_model, data = dat_vb_var, missing = "fiml", cluster = "id_fam")
used_case_idx_vb_stage <- lavInspect(fit_vb_stage, "case.idx")
vb_stage_factor_scores <- lavPredict(fit_vb_stage, method = "regression")

dat_scaled_for_CFA$vb_stage_Early1 <- NA
dat_scaled_for_CFA$vb_stage_Early1[used_case_idx_vb_stage] <- vb_stage_factor_scores[,"Early_VB"]
dat_scaled_for_CFA$vb_stage_Child1 <- NA
dat_scaled_for_CFA$vb_stage_Child1[used_case_idx_vb_stage] <- vb_stage_factor_scores[,"Child_VB"]
dat_scaled_for_CFA$vb_stage_Adol1 <- NA
dat_scaled_for_CFA$vb_stage_Adol1[used_case_idx_vb_stage] <- vb_stage_factor_scores[,"Adol_VB"]
dat_scaled_for_CFA$vb_stage_Latent1 <- NA
dat_scaled_for_CFA$vb_stage_Latent1[used_case_idx_vb_stage] <- vb_stage_factor_scores[,"Latent_Stage_VB"]

# Model for 'verbal ability' by methods
verbal_method_model <- '
  Early_MCDI =~ yr2 + yr3 + yr4
  Middle_WISC =~ yr7 + yr9 + yr10 + yr12
  Later_MillHill =~ yr16 + yr25
  Latent_Measure_VB =~ Early_MCDI + Middle_WISC + Later_MillHill
'
fit_vb_method <- cfa(verbal_method_model, data = dat_vb_var, missing = "fiml", cluster = "id_fam")
used_case_idx_vb_method <- lavInspect(fit_vb_method, "case.idx")
vb_method_factor_scores <- lavPredict(fit_vb_method, method = "regression")

dat_scaled_for_CFA$vb_method_Early_MCDI1 <- NA
dat_scaled_for_CFA$vb_method_Early_MCDI1[used_case_idx_vb_method] <- vb_method_factor_scores[,"Early_MCDI"]
dat_scaled_for_CFA$vb_method_Middle_WISC1 <- NA
dat_scaled_for_CFA$vb_method_Middle_WISC1[used_case_idx_vb_method] <- vb_method_factor_scores[,"Middle_WISC"]
dat_scaled_for_CFA$vb_method_Later_MillHill1 <- NA
dat_scaled_for_CFA$vb_method_Later_MillHill1[used_case_idx_vb_method] <- vb_method_factor_scores[,"Later_MillHill"]
dat_scaled_for_CFA$vb_method_Latent1 <- NA
dat_scaled_for_CFA$vb_method_Latent1[used_case_idx_vb_method] <- vb_method_factor_scores[,"Latent_Measure_VB"]

# Model for 'nonverbal ability'
dat_nv_var <- subset(dat_scaled_for_CFA, select = c(bscnv1, cscnv1, dscnv1, gcn1, icnv1, jcnv1, lnonverbal12T1, pcrvtota1, ucgnvt1, id_fam))
new_nv_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr10", "yr12", "yr16", "yr25")
all_new_nv_names <- c(new_nv_names, "id_fam")
names(dat_nv_var) <- all_new_nv_names

nonverbal_ability_model <- '
  Nonverbal_Ability =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr10 + yr12 + yr16 + yr25
'
fit_nv <- cfa(nonverbal_ability_model, data = dat_nv_var, missing = "fiml", cluster = "id_fam")
used_case_idx_nv <- lavInspect(fit_nv, "case.idx")
nv_factor_scores <- lavPredict(fit_nv, method = "regression")
dat_scaled_for_CFA$nv_CTCR1 <- NA
dat_scaled_for_CFA$nv_CTCR1[used_case_idx_nv] <- nv_factor_scores[,1]

# Model for 'nonverbal ability' by developmental stage
nv_stage_model <- '
  Early_NV  =~ yr2 + yr3 + yr4
  Child_NV =~ yr7 + yr9 + yr10
  Adol_NV   =~ yr12 + yr16 
  Latent_Stage_NV =~ Early_NV + Child_NV + Adol_NV + yr25
'
fit_nv_stage <- cfa(nv_stage_model, data = dat_nv_var, missing = "fiml", cluster = "id_fam")
used_case_idx_nv_stage <- lavInspect(fit_nv_stage, "case.idx")
nv_stage_factor_scores <- lavPredict(fit_nv_stage, method = "regression")

dat_scaled_for_CFA$nv_stage_Early1 <- NA
dat_scaled_for_CFA$nv_stage_Early1[used_case_idx_nv_stage] <- nv_stage_factor_scores[,"Early_NV"]
dat_scaled_for_CFA$nv_stage_Child1 <- NA
dat_scaled_for_CFA$nv_stage_Child1[used_case_idx_nv_stage] <- nv_stage_factor_scores[,"Child_NV"]
dat_scaled_for_CFA$nv_stage_Adol1 <- NA
dat_scaled_for_CFA$nv_stage_Adol1[used_case_idx_nv_stage] <- nv_stage_factor_scores[,"Adol_NV"]
dat_scaled_for_CFA$nv_stage_Latent1 <- NA
dat_scaled_for_CFA$nv_stage_Latent1[used_case_idx_nv_stage] <- nv_stage_factor_scores[,"Latent_Stage_NV"]

# Model for 'nonverbal ability' by methods
nv_method_model <- '
  Early_PARCA =~ yr2 + yr3 + yr4
  MiddleLate_Raven =~ yr9 + yr10 + yr12 + yr16 + yr25
  Latent_Measure_NV =~ Early_PARCA + yr7 + MiddleLate_Raven
'
fit_nv_method <- cfa(nv_method_model, data = dat_nv_var, missing = "fiml", cluster = "id_fam")
used_case_idx_nv_method <- lavInspect(fit_nv_method, "case.idx")
nv_method_factor_scores <- lavPredict(fit_nv_method, method = "regression")

dat_scaled_for_CFA$nv_method_Early_PARCA1 <- NA
dat_scaled_for_CFA$nv_method_Early_PARCA1[used_case_idx_nv_method] <- nv_method_factor_scores[, "Early_PARCA"]
dat_scaled_for_CFA$nv_method_MiddleLate_Raven1 <- NA
dat_scaled_for_CFA$nv_method_MiddleLate_Raven1[used_case_idx_nv_method] <- nv_method_factor_scores[, "MiddleLate_Raven"]
dat_scaled_for_CFA$nv_method_Latent1 <- NA
dat_scaled_for_CFA$nv_method_Latent1[used_case_idx_nv_method] <- nv_method_factor_scores[, "Latent_Measure_NV"]


# --- Anxiety Traits (ARBQ) ----
# Model for 'ARBQ Shyness'
dat_shy_var <- subset(dat_scaled_for_CFA, select = c(canxshyt1, danxshyt1, gpanxshyt1, ipanxshyt1, ppbhanxshyt1, gtanxshyt1, id_fam))
new_shy_names <- c("yr3P", "yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
names(dat_shy_var) <- c(new_shy_names, "id_fam")

ARBQ_Shyness_model <- 'ARBQ_Shyness =~ yr3P + yr4P + yr7P + yr9P + yr16P + yr7T'
fit_shy <- cfa(ARBQ_Shyness_model, data = dat_shy_var, missing = "fiml", cluster = "id_fam")
used_case_idx_shy <- lavInspect(fit_shy, "case.idx")
shy_factor_scores <- lavPredict(fit_shy, method = "regression")
dat_scaled_for_CFA$ARBQ_shy_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_shy_CTCR1[used_case_idx_shy] <- shy_factor_scores[,1]

# Model for 'ARBQ_Fear'
dat_fear_var <- subset(dat_scaled_for_CFA, select = c(canxfeart1, danxfeart1, gpanxfeart1, ipanxfeart1, ppbhanxfeart1, gtanxfeart1, id_fam))
names(dat_fear_var) <- c(new_shy_names, "id_fam")

ARBQ_Fear_model <- 'ARBQ_Fear =~ yr3P + yr4P + yr7P + yr9P + yr16P + yr7T'
fit_fear <- cfa(ARBQ_Fear_model, data = dat_fear_var, missing = "fiml", cluster = "id_fam")
used_case_idx_fear <- lavInspect(fit_fear, "case.idx")
fear_factor_scores <- lavPredict(fit_fear, method = "regression")
dat_scaled_for_CFA$ARBQ_fear_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_fear_CTCR1[used_case_idx_fear] <- fear_factor_scores[,1]

# Model for 'ARBQ_Obsessive-Compulsive Behaviour'
dat_ocb_var <- subset(dat_scaled_for_CFA, select = c(danxocbt1, gpanxocbt1, ipanxocbt1, ppbhanxocbt1, gtanxocbt1, id_fam))
new_ocb_names <- c("yr4P", "yr7P", "yr9P", "yr16P", "yr7T")
names(dat_ocb_var) <- c(new_ocb_names, "id_fam")

ARBQ_Obsessive_Compulsive_Behaviour_model <- 'ARBQ_OCB =~ yr4P + yr7P + yr9P + yr16P + yr7T'
fit_ocb <- cfa(ARBQ_Obsessive_Compulsive_Behaviour_model, data = dat_ocb_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ocb <- lavInspect(fit_ocb, "case.idx")
ocb_factor_scores <- lavPredict(fit_ocb, method = "regression")
dat_scaled_for_CFA$ARBQ_ocb_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_ocb_CTCR1[used_case_idx_ocb] <- ocb_factor_scores[,1]

# Model for 'ARBQ_Negative Affect'
dat_naff_var <- subset(dat_scaled_for_CFA, select = c(danxnafft1, gpanxnafft1, ipanxnafft1, ppbhanxnafft1, gtanxnafft1, id_fam))
names(dat_naff_var) <- c(new_ocb_names, "id_fam")

ARBQ_Negative_Affect_model <- 'ARBQ_NegAff =~ yr4P + yr7P + yr9P + yr16P + yr7T'
fit_naff <- cfa(ARBQ_Negative_Affect_model, data = dat_naff_var, missing = "fiml", cluster = "id_fam")
used_case_idx_naff <- lavInspect(fit_naff, "case.idx")
naff_factor_scores <- lavPredict(fit_naff, method = "regression")
dat_scaled_for_CFA$ARBQ_naff_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_naff_CTCR1[used_case_idx_naff] <- naff_factor_scores[,1]

# Model for 'ARBQ_Negative Cognition'
dat_ncog_var <- subset(dat_scaled_for_CFA, select = c(danxncogt1, gpanxncogt1, ipanxncogt1, ppbhanxncogt1, gtanxncogt1, id_fam))
names(dat_ncog_var) <- c(new_ocb_names, "id_fam")

ARBQ_Negative_Cognition_model <- 'ARBQ_NegCog =~ yr4P + yr7P + yr9P + yr16P + yr7T'
fit_ncog <- cfa(ARBQ_Negative_Cognition_model, data = dat_ncog_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ncog <- lavInspect(fit_ncog, "case.idx")
ncog_factor_scores <- lavPredict(fit_ncog, method = "regression")
dat_scaled_for_CFA$ARBQ_ncog_CTCR1 <- NA
dat_scaled_for_CFA$ARBQ_ncog_CTCR1[used_case_idx_ncog] <- ncog_factor_scores[,1]

# Model for 'Anxiety Total'
dat_ARBQ_total_var <- subset(dat_scaled_for_CFA, select = c(canxt1, danxt1, gpanxt1, ipanxt1, ppbhanxt1, gtanxt1, id_fam))
names(dat_ARBQ_total_var) <- c(new_shy_names, "id_fam")

Anxiety_Total_model <- 'Anxiety_Total =~ yr3P + yr4P + yr7P + yr9P + yr16P + yr7T'
fit_ARBQ_total <- cfa(Anxiety_Total_model, data = dat_ARBQ_total_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ARBQ_total <- lavInspect(fit_ARBQ_total, "case.idx")
ARBQ_total_factor_scores <- lavPredict(fit_ARBQ_total, method = "regression")
dat_scaled_for_CFA$Anxiety_total_CTCR1 <- NA
dat_scaled_for_CFA$Anxiety_total_CTCR1[used_case_idx_ARBQ_total] <- ARBQ_total_factor_scores[,1]

# --- Anxiety Traits (ARBQ) by Developmental Stage ----

ARBQ_Shyness_stage_model <- '
  Early_Shy =~ yr3P + yr4P
  Child_Shy =~ yr7P + yr9P + yr7T
  Latent_Stage_Shy =~ Early_Shy + Child_Shy + yr16P
'
fit_shy_stage <- cfa(ARBQ_Shyness_stage_model, data = dat_shy_var, missing = "fiml", cluster = "id_fam")
used_case_idx_shy_stage <- lavInspect(fit_shy_stage, "case.idx")
shy_stage_factor_scores <- lavPredict(fit_shy_stage, method = "regression")
dat_scaled_for_CFA$ARBQ_shy_stage_Early1 <- NA
dat_scaled_for_CFA$ARBQ_shy_stage_Early1[used_case_idx_shy_stage] <- shy_stage_factor_scores[, "Early_Shy"]
dat_scaled_for_CFA$ARBQ_shy_stage_Child1 <- NA
dat_scaled_for_CFA$ARBQ_shy_stage_Child1[used_case_idx_shy_stage] <- shy_stage_factor_scores[, "Child_Shy"]
dat_scaled_for_CFA$ARBQ_shy_stage_Latent1 <- NA
dat_scaled_for_CFA$ARBQ_shy_stage_Latent1[used_case_idx_shy_stage] <- shy_stage_factor_scores[, "Latent_Stage_Shy"]

ARBQ_Fear_stage_model <- '
  Early_Fear =~ yr3P + yr4P
  Child_Fear =~ yr7P + yr9P + yr7T
  Latent_Stage_Fear =~ Early_Fear + Child_Fear + yr16P
'
fit_fear_stage <- cfa(ARBQ_Fear_stage_model, data = dat_fear_var, missing = "fiml", cluster = "id_fam")
used_case_idx_fear_stage <- lavInspect(fit_fear_stage, "case.idx")
fear_stage_factor_scores <- lavPredict(fit_fear_stage, method = "regression")
dat_scaled_for_CFA$ARBQ_fear_stage_Early1 <- NA
dat_scaled_for_CFA$ARBQ_fear_stage_Early1[used_case_idx_fear_stage] <- fear_stage_factor_scores[, "Early_Fear"]
dat_scaled_for_CFA$ARBQ_fear_stage_Child1 <- NA
dat_scaled_for_CFA$ARBQ_fear_stage_Child1[used_case_idx_fear_stage] <- fear_stage_factor_scores[, "Child_Fear"]
dat_scaled_for_CFA$ARBQ_fear_stage_Latent1 <- NA
dat_scaled_for_CFA$ARBQ_fear_stage_Latent1[used_case_idx_fear_stage] <- fear_stage_factor_scores[, "Latent_Stage_Fear"]

ARBQ_OCB_stage_model <- '
  Child_OCB =~ yr7P + yr9P + yr7T
  Latent_Stage_OCB =~ yr4P + Child_OCB + yr16P
'
fit_ocb_stage <- cfa(ARBQ_OCB_stage_model, data = dat_ocb_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ocb_stage <- lavInspect(fit_ocb_stage, "case.idx")
ocb_stage_factor_scores <- lavPredict(fit_ocb_stage, method = "regression")
dat_scaled_for_CFA$ARBQ_ocb_stage_Child1 <- NA
dat_scaled_for_CFA$ARBQ_ocb_stage_Child1[used_case_idx_ocb_stage] <- ocb_stage_factor_scores[, "Child_OCB"]
dat_scaled_for_CFA$ARBQ_ocb_stage_Latent1 <- NA
dat_scaled_for_CFA$ARBQ_ocb_stage_Latent1[used_case_idx_ocb_stage] <- ocb_stage_factor_scores[, "Latent_Stage_OCB"]

ARBQ_NegAff_stage_model <- '
  Child_NegAff =~ yr7P + yr9P + yr7T
  Latent_Stage_NegAff =~ yr4P + Child_NegAff + yr16P
'
fit_naff_stage <- cfa(ARBQ_NegAff_stage_model, data = dat_naff_var, missing = "fiml", cluster = "id_fam")
used_case_idx_naff_stage <- lavInspect(fit_naff_stage, "case.idx")
naff_stage_factor_scores <- lavPredict(fit_naff_stage, method = "regression")
dat_scaled_for_CFA$ARBQ_naff_stage_Child1 <- NA
dat_scaled_for_CFA$ARBQ_naff_stage_Child1[used_case_idx_naff_stage] <- naff_stage_factor_scores[, "Child_NegAff"]
dat_scaled_for_CFA$ARBQ_naff_stage_Latent1 <- NA
dat_scaled_for_CFA$ARBQ_naff_stage_Latent1[used_case_idx_naff_stage] <- naff_stage_factor_scores[, "Latent_Stage_NegAff"]

ARBQ_NegCog_stage_model <- '
  Child_NegCog =~ yr7P + yr9P + yr7T
  Latent_Stage_NegCog =~ yr4P + Child_NegCog + yr16P
'
fit_ncog_stage <- cfa(ARBQ_NegCog_stage_model, data = dat_ncog_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ncog_stage <- lavInspect(fit_ncog_stage, "case.idx")
ncog_stage_factor_scores <- lavPredict(fit_ncog_stage, method = "regression")
dat_scaled_for_CFA$ARBQ_ncog_stage_Child1 <- NA
dat_scaled_for_CFA$ARBQ_ncog_stage_Child1[used_case_idx_ncog_stage] <- ncog_stage_factor_scores[, "Child_NegCog"]
dat_scaled_for_CFA$ARBQ_ncog_stage_Latent1 <- NA
dat_scaled_for_CFA$ARBQ_ncog_stage_Latent1[used_case_idx_ncog_stage] <- ncog_stage_factor_scores[, "Latent_Stage_NegCog"]

ARBQ_Total_stage_model <- '
  Early_Total =~ yr3P + yr4P
  Child_Total =~ yr7P + yr9P + yr7T
  Latent_Stage_Total =~ Early_Total + Child_Total + yr16P
'
fit_ARBQ_total_stage <- cfa(ARBQ_Total_stage_model, data = dat_ARBQ_total_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ARBQ_total_stage <- lavInspect(fit_ARBQ_total_stage, "case.idx")
ARBQ_total_stage_factor_scores <- lavPredict(fit_ARBQ_total_stage, method = "regression")
dat_scaled_for_CFA$Anxiety_total_stage_Early1 <- NA
dat_scaled_for_CFA$Anxiety_total_stage_Early1[used_case_idx_ARBQ_total_stage] <- ARBQ_total_stage_factor_scores[, "Early_Total"]
dat_scaled_for_CFA$Anxiety_total_stage_Child1 <- NA
dat_scaled_for_CFA$Anxiety_total_stage_Child1[used_case_idx_ARBQ_total_stage] <- ARBQ_total_stage_factor_scores[, "Child_Total"]
dat_scaled_for_CFA$Anxiety_total_stage_Latent1 <- NA
dat_scaled_for_CFA$Anxiety_total_stage_Latent1[used_case_idx_ARBQ_total_stage] <- ARBQ_total_stage_factor_scores[, "Latent_Stage_Total"]


# --- Conners Traits ----
# Model for 'Conners Inattention'
dat_inatt_ctcr_var <- subset(dat_scaled_for_CFA, select = c(hconint1, lpconint1, npconint1, ppbhconninat1, u1pconinat1, ntconint1, ncconint1, u2cconninat1, zmhconnt1, id_fam))
new_inatt_ctcr_names <- c("yr8P", "yr12P", "yr14P", "yr16P", "yr21P", "yr14T", "yr14C", "yr21C", "yr26C")
names(dat_inatt_ctcr_var) <- c(new_inatt_ctcr_names, "id_fam")

Conners_Inattention_model <- '
  Inattention =~ yr8P + yr12P + yr14P + yr16P + yr21P + yr14T + yr14C + yr21C + yr26C
'
fit_inatt <- cfa(Conners_Inattention_model, data = dat_inatt_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_inatt <- lavInspect(fit_inatt, "case.idx")
inatt_factor_scores <- lavPredict(fit_inatt, method = "regression")
dat_scaled_for_CFA$Conners_inatt_CTCR1 <- NA
dat_scaled_for_CFA$Conners_inatt_CTCR1[used_case_idx_inatt] <- inatt_factor_scores[,1]

# parent-only
dat_inatt_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(hconint1, lpconint1, npconint1, ppbhconninat1, u1pconinat1, id_fam))
new_inatt_ctwrp_names <- c("yr8", "yr12", "yr14", "yr16", "yr21")
names(dat_inatt_ctwrp_var) <- c(new_inatt_ctwrp_names, "id_fam")

Conners_Inattention_model_P <- 'Inattention_P =~ yr8 + yr12 + yr14 + yr16 + yr21'
fit_inatt_P <- cfa(Conners_Inattention_model_P, data = dat_inatt_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_inatt_P <- lavInspect(fit_inatt_P, "case.idx")
inatt_factor_scores_P <- lavPredict(fit_inatt_P, method = "regression")
dat_scaled_for_CFA$Conners_inatt_CTWR_P1 <- NA
dat_scaled_for_CFA$Conners_inatt_CTWR_P1[used_case_idx_inatt_P] <- inatt_factor_scores_P[,1]

# Model for 'Conners Hyperactivity-Impulsivity'
dat_hi_ctcr_var <- subset(dat_scaled_for_CFA, select = c(hconhit1, lpconhit1, npconhit1, ppbhconnimpt1, u1pconimpt1, ntconhit1, ncconhit1, u2cconnhypt1, id_fam))
new_hi_ctcr_names <- c("yr8P", "yr12P", "yr14P", "yr16P", "yr21P", "yr14T", "yr14C", "yr21C")
names(dat_hi_ctcr_var) <- c(new_hi_ctcr_names, "id_fam")

Conners_Hyperactivity_Impulsivity_model <- '
  Hyper_Impuls =~ yr8P + yr12P + yr14P + yr16P + yr21P + yr14T + yr14C + yr21C
'
fit_hyper_impul <- cfa(Conners_Hyperactivity_Impulsivity_model, data = dat_hi_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_hyper_impul <- lavInspect(fit_hyper_impul, "case.idx")
hyper_impul_factor_scores <- lavPredict(fit_hyper_impul, method = "regression")
dat_scaled_for_CFA$Conners_hyper_impul_CTCR1 <- NA
dat_scaled_for_CFA$Conners_hyper_impul_CTCR1[used_case_idx_hyper_impul] <- hyper_impul_factor_scores[,1]

# parent-only
dat_hi_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(hconhit1, lpconhit1, npconhit1, ppbhconnimpt1, u1pconimpt1, id_fam))
names(dat_hi_ctwrp_var) <- c(new_inatt_ctwrp_names, "id_fam")

Conners_Hyperactivity_Impulsivity_model_P <- 'Hyper_Impuls_P =~ yr8 + yr12 + yr14 + yr16 + yr21'
fit_hyper_impul_P <- cfa(Conners_Hyperactivity_Impulsivity_model_P, data = dat_hi_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_hyper_impul_P <- lavInspect(fit_hyper_impul_P, "case.idx")
hyper_impul_factor_scores_P <- lavPredict(fit_hyper_impul_P, method = "regression")
dat_scaled_for_CFA$Conners_hyper_impul_CTWR_P1 <- NA
dat_scaled_for_CFA$Conners_hyper_impul_CTWR_P1[used_case_idx_hyper_impul_P] <- hyper_impul_factor_scores_P[,1]

# Model for 'Conners Total'
dat_total_ctcr_var <- subset(dat_scaled_for_CFA, select = c(hconnt1, lpconnt1, npconnt1, ppbhconnt1, u1pcont1, ntconnt1, ncconnt1, u2cconnt1, id_fam))
names(dat_total_ctcr_var) <- c(new_hi_ctcr_names, "id_fam")

Conners_Total_model <- '
  ADHD_Total =~ yr8P + yr12P + yr14P + yr16P + yr21P + yr14T + yr14C + yr21C
'
fit_ADHD <- cfa(Conners_Total_model, data = dat_total_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ADHD <- lavInspect(fit_ADHD, "case.idx")
ADHD_factor_scores <- lavPredict(fit_ADHD, method = "regression")
dat_scaled_for_CFA$Conners_Total_CTCR1 <- NA
dat_scaled_for_CFA$Conners_Total_CTCR1[used_case_idx_ADHD] <- ADHD_factor_scores[,1]

# parent-only
dat_total_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(hconnt1, lpconnt1, npconnt1, ppbhconnt1, u1pcont1, id_fam))
names(dat_total_ctwrp_var) <- c(new_inatt_ctwrp_names, "id_fam")

Conners_Total_model_P <- 'ADHD_Total_P =~ yr8 + yr12 + yr14 + yr16 + yr21'
fit_ADHD_P <- cfa(Conners_Total_model_P, data = dat_total_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ADHD_P <- lavInspect(fit_ADHD_P, "case.idx")
ADHD_factor_scores_P <- lavPredict(fit_ADHD_P, method = "regression")
dat_scaled_for_CFA$Conners_Total_CTWR_P1 <- NA
dat_scaled_for_CFA$Conners_Total_CTWR_P1[used_case_idx_ADHD_P] <- ADHD_factor_scores_P[,1]

# --- Conners Traits by Developmental Stage ----

Conners_Inatt_stage_model <- '
  Adol_Inatt =~ yr12P + yr14P + yr16P + yr14T + yr14C
  Adult_Inatt =~ yr21P + yr21C + yr26C
  Latent_Stage_Inatt =~ yr8P + Adol_Inatt + Adult_Inatt
'
fit_inatt_stage <- cfa(Conners_Inatt_stage_model, data = dat_inatt_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_inatt_stage <- lavInspect(fit_inatt_stage, "case.idx")
inatt_stage_scores <- lavPredict(fit_inatt_stage, method = "regression")
dat_scaled_for_CFA$Conners_inatt_stage_Adol1 <- NA
dat_scaled_for_CFA$Conners_inatt_stage_Adol1[used_case_idx_inatt_stage] <- inatt_stage_scores[, "Adol_Inatt"]
dat_scaled_for_CFA$Conners_inatt_stage_Adult1 <- NA
dat_scaled_for_CFA$Conners_inatt_stage_Adult1[used_case_idx_inatt_stage] <- inatt_stage_scores[, "Adult_Inatt"]
dat_scaled_for_CFA$Conners_inatt_stage_Latent1 <- NA
dat_scaled_for_CFA$Conners_inatt_stage_Latent1[used_case_idx_inatt_stage] <- inatt_stage_scores[, "Latent_Stage_Inatt"]

Conners_Inatt_P_stage_model <- '
  Adol_Inatt_P =~ yr12 + yr14 + yr16
  Latent_Stage_Inatt_P =~ yr8 + Adol_Inatt_P + yr21
'
fit_inatt_P_stage <- cfa(Conners_Inatt_P_stage_model, data = dat_inatt_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_inatt_P_stage <- lavInspect(fit_inatt_P_stage, "case.idx")
inatt_P_stage_scores <- lavPredict(fit_inatt_P_stage, method = "regression")
dat_scaled_for_CFA$Conners_inatt_P_stage_Adol1 <- NA
dat_scaled_for_CFA$Conners_inatt_P_stage_Adol1[used_case_idx_inatt_P_stage] <- inatt_P_stage_scores[, "Adol_Inatt_P"]
dat_scaled_for_CFA$Conners_inatt_P_stage_Latent1 <- NA
dat_scaled_for_CFA$Conners_inatt_P_stage_Latent1[used_case_idx_inatt_P_stage] <- inatt_P_stage_scores[, "Latent_Stage_Inatt_P"]

Conners_Hyper_stage_model <- '
  Adol_Hyper =~ yr12P + yr14P + yr16P + yr14T + yr14C
  Adult_Hyper =~ yr21P + yr21C
  Latent_Stage_Hyper =~ yr8P + Adol_Hyper + Adult_Hyper
'
fit_hyper_stage <- cfa(Conners_Hyper_stage_model, data = dat_hi_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_hyper_stage <- lavInspect(fit_hyper_stage, "case.idx")
hyper_stage_scores <- lavPredict(fit_hyper_stage, method = "regression")
dat_scaled_for_CFA$Conners_hyper_stage_Adol1 <- NA
dat_scaled_for_CFA$Conners_hyper_stage_Adol1[used_case_idx_hyper_stage] <- hyper_stage_scores[, "Adol_Hyper"]
dat_scaled_for_CFA$Conners_hyper_stage_Adult1 <- NA
dat_scaled_for_CFA$Conners_hyper_stage_Adult1[used_case_idx_hyper_stage] <- hyper_stage_scores[, "Adult_Hyper"]
dat_scaled_for_CFA$Conners_hyper_stage_Latent1 <- NA
dat_scaled_for_CFA$Conners_hyper_stage_Latent1[used_case_idx_hyper_stage] <- hyper_stage_scores[, "Latent_Stage_Hyper"]

Conners_Hyper_P_stage_model <- '
  Adol_Hyper_P =~ yr12 + yr14 + yr16
  Latent_Stage_Hyper_P =~ yr8 + Adol_Hyper_P + yr21
'
fit_hyper_P_stage <- cfa(Conners_Hyper_P_stage_model, data = dat_hi_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_hyper_P_stage <- lavInspect(fit_hyper_P_stage, "case.idx")
hyper_P_stage_scores <- lavPredict(fit_hyper_P_stage, method = "regression")
dat_scaled_for_CFA$Conners_hyper_P_stage_Adol1 <- NA
dat_scaled_for_CFA$Conners_hyper_P_stage_Adol1[used_case_idx_hyper_P_stage] <- hyper_P_stage_scores[, "Adol_Hyper_P"]
dat_scaled_for_CFA$Conners_hyper_P_stage_Latent1 <- NA
dat_scaled_for_CFA$Conners_hyper_P_stage_Latent1[used_case_idx_hyper_P_stage] <- hyper_P_stage_scores[, "Latent_Stage_Hyper_P"]

Conners_Total_stage_model <- '
  Adol_Total =~ yr12P + yr14P + yr16P + yr14T + yr14C
  Adult_Total =~ yr21P + yr21C
  Latent_Stage_Total =~ yr8P + Adol_Total + Adult_Total
'
fit_ADHD_stage <- cfa(Conners_Total_stage_model, data = dat_total_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ADHD_stage <- lavInspect(fit_ADHD_stage, "case.idx")
ADHD_stage_scores <- lavPredict(fit_ADHD_stage, method = "regression")
dat_scaled_for_CFA$Conners_Total_stage_Adol1 <- NA
dat_scaled_for_CFA$Conners_Total_stage_Adol1[used_case_idx_ADHD_stage] <- ADHD_stage_scores[, "Adol_Total"]
dat_scaled_for_CFA$Conners_Total_stage_Adult1 <- NA
dat_scaled_for_CFA$Conners_Total_stage_Adult1[used_case_idx_ADHD_stage] <- ADHD_stage_scores[, "Adult_Total"]
dat_scaled_for_CFA$Conners_Total_stage_Latent1 <- NA
dat_scaled_for_CFA$Conners_Total_stage_Latent1[used_case_idx_ADHD_stage] <- ADHD_stage_scores[, "Latent_Stage_Total"]

Conners_Total_P_stage_model <- '
  Adol_Total_P =~ yr12 + yr14 + yr16
  Latent_Stage_Total_P =~ yr8 + Adol_Total_P + yr21
'
fit_ADHD_P_stage <- cfa(Conners_Total_P_stage_model, data = dat_total_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_ADHD_P_stage <- lavInspect(fit_ADHD_P_stage, "case.idx")
ADHD_P_stage_scores <- lavPredict(fit_ADHD_P_stage, method = "regression")
dat_scaled_for_CFA$Conners_Total_P_stage_Adol1 <- NA
dat_scaled_for_CFA$Conners_Total_P_stage_Adol1[used_case_idx_ADHD_P_stage] <- ADHD_P_stage_scores[, "Adol_Total_P"]
dat_scaled_for_CFA$Conners_Total_P_stage_Latent1 <- NA
dat_scaled_for_CFA$Conners_Total_P_stage_Latent1[used_case_idx_ADHD_P_stage] <- ADHD_P_stage_scores[, "Latent_Stage_Total_P"]

# --- SDQ Traits ----
# Model for 'SDQ Conduct'
dat_conduct_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqccont1, csdqccont1, dsdqcont1, gpsdqcont1, ipsdqcont1, lpsdqcont1, ppbhsdqcont1, u1psdqcont1, gtsdqcont1, itsdqcont1, ltsdqcont1, lcsdqcont1, pcbhsdqcont1, u1csdqcont1, zmhsdqcont1, id_fam))
new_conduct_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr16P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
names(dat_conduct_ctcr_var) <- c(new_conduct_ctcr_names, "id_fam")

SDQ_Conduct_model <- '
  SDQ_Conduct =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
'
fit_SDQconduct <- cfa(SDQ_Conduct_model, data = dat_conduct_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQconduct <- lavInspect(fit_SDQconduct, "case.idx")
SDQconduct_factor_scores <- lavPredict(fit_SDQconduct, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTCR1[used_case_idx_SDQconduct] <- SDQconduct_factor_scores[,1]

# Parent-only
dat_conduct_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqccont1, csdqccont1, dsdqcont1, gpsdqcont1, ipsdqcont1, lpsdqcont1, ppbhsdqcont1, u1psdqcont1, id_fam))
new_conduct_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr16", "yr21")
names(dat_conduct_ctwrp_var) <- c(new_conduct_ctwrp_names, "id_fam")

SDQ_Conduct_model_P <- 'SDQ_Conduct_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21'
fit_SDQconduct_P <- cfa(SDQ_Conduct_model_P, data = dat_conduct_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQconduct_P <- lavInspect(fit_SDQconduct_P, "case.idx")
SDQconduct_factor_scores_P <- lavPredict(fit_SDQconduct_P, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTWR_P1[used_case_idx_SDQconduct_P] <- SDQconduct_factor_scores_P[,1]

# Teacher only
dat_conduct_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqcont1, itsdqcont1, ltsdqcont1, id_fam))
new_conduct_ctwrt_names <- c("yr7", "yr9", "yr12")
names(dat_conduct_ctwrt_var) <- c(new_conduct_ctwrt_names, "id_fam")

SDQ_Conduct_model_T <- 'SDQ_Conduct_T =~ yr7 + yr9 + yr12'
fit_SDQconduct_T <- cfa(SDQ_Conduct_model_T, data = dat_conduct_ctwrt_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQconduct_T <- lavInspect(fit_SDQconduct_T, "case.idx")
SDQconduct_factor_scores_T <- lavPredict(fit_SDQconduct_T, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTWR_T1[used_case_idx_SDQconduct_T] <- SDQconduct_factor_scores_T[,1]

# Child only
dat_conduct_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqcont1, pcbhsdqcont1, u1csdqcont1, zmhsdqcont1, id_fam))
new_conduct_ctwrc_names <- c("yr12", "yr16", "yr21", "yr26")
names(dat_conduct_ctwrc_var) <- c(new_conduct_ctwrc_names, "id_fam")

SDQ_Conduct_model_C <- 'SDQ_Conduct_C =~ yr12 + yr16 + yr21 + yr26'
fit_SDQconduct_C <- cfa(SDQ_Conduct_model_C, data = dat_conduct_ctwrc_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQconduct_C <- lavInspect(fit_SDQconduct_C, "case.idx")
SDQconduct_factor_scores_C <- lavPredict(fit_SDQconduct_C, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_CTWR_C1[used_case_idx_SDQconduct_C] <- SDQconduct_factor_scores_C[,1]

# Model for 'SDQ Emotion'
dat_emot_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcemot1, csdqcemot1, dsdqemot1, gpsdqemot1, ipsdqemot1, lpsdqemot1, u1psdqemot1, gtsdqemot1, itsdqemot1, ltsdqemot1, lcsdqemot1, pcbhsdqemot1, u1csdqemot1, zmhsdqemot1, id_fam))
new_emot_ctcr_names <- c("yr2P", "yr3P", "yr4P", "yr7P", "yr9P", "yr12P", "yr21P", "yr7T", "yr9T", "yr12T", "yr12C", "yr16C", "yr21C", "yr26C")
names(dat_emot_ctcr_var) <- c(new_emot_ctcr_names, "id_fam")

SDQ_Emotion_model <- '
  SDQ_Emotion =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
'
fit_SDQemot <- cfa(SDQ_Emotion_model, data = dat_emot_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQemot <- lavInspect(fit_SDQemot, "case.idx")
SDQemot_factor_scores <- lavPredict(fit_SDQemot, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTCR1[used_case_idx_SDQemot] <- SDQemot_factor_scores[,1]

# Parent-only
dat_emot_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcemot1, csdqcemot1, dsdqemot1, gpsdqemot1, ipsdqemot1, lpsdqemot1, u1psdqemot1, id_fam))
new_emot_ctwrp_names <- c("yr2", "yr3", "yr4", "yr7", "yr9", "yr12", "yr21")
names(dat_emot_ctwrp_var) <- c(new_emot_ctwrp_names, "id_fam")

SDQ_Emotion_model_P <- 'SDQ_Emotion_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr21'
fit_SDQemot_P <- cfa(SDQ_Emotion_model_P, data = dat_emot_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQemot_P <- lavInspect(fit_SDQemot_P, "case.idx")
SDQemot_factor_scores_P <- lavPredict(fit_SDQemot_P, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTWR_P1[used_case_idx_SDQemot_P] <- SDQemot_factor_scores_P[,1]

# Teacher only
dat_emot_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqemot1, itsdqemot1, ltsdqemot1, id_fam))
names(dat_emot_ctwrt_var) <- c(new_conduct_ctwrt_names, "id_fam")

SDQ_Emotion_model_T <- 'SDQ_Emotion_T =~ yr7 + yr9 + yr12'
fit_SDQemot_T <- cfa(SDQ_Emotion_model_T, data = dat_emot_ctwrt_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQemot_T <- lavInspect(fit_SDQemot_T, "case.idx")
SDQemot_factor_scores_T <- lavPredict(fit_SDQemot_T, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTWR_T1[used_case_idx_SDQemot_T] <- SDQemot_factor_scores_T[,1]

# Child only
dat_emot_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqemot1, pcbhsdqemot1, u1csdqemot1, zmhsdqemot1, id_fam))
names(dat_emot_ctwrc_var) <- c(new_conduct_ctwrc_names, "id_fam")

SDQ_Emotion_model_C <- 'SDQ_Emotion_C =~ yr12 + yr16 + yr21 + yr26'
fit_SDQemot_C <- cfa(SDQ_Emotion_model_C, data = dat_emot_ctwrc_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQemot_C <- lavInspect(fit_SDQemot_C, "case.idx")
SDQemot_factor_scores_C <- lavPredict(fit_SDQemot_C, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_CTWR_C1[used_case_idx_SDQemot_C] <- SDQemot_factor_scores_C[,1]

# Model for 'SDQ Hyperactivity'
dat_hyper_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqchypt1, csdqchypt1, dsdqhypt1, gpsdqhypt1, ipsdqhypt1, lpsdqhypt1, ppbhsdqhypt1, u1psdqhypt1, gtsdqhypt1, itsdqhypt1, ltsdqhypt1, lcsdqhypt1, pcbhsdqhypt1, u1csdqhypt1, zmhsdqhypt1, id_fam))
names(dat_hyper_ctcr_var) <- c(new_conduct_ctcr_names, "id_fam")

SDQ_Hyperactivity_model <- '
  SDQ_Hyperactivity =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
'
fit_SDQhyper <- cfa(SDQ_Hyperactivity_model, data = dat_hyper_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQhyper <- lavInspect(fit_SDQhyper, "case.idx")
SDQhyper_factor_scores <- lavPredict(fit_SDQhyper, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTCR1[used_case_idx_SDQhyper] <- SDQhyper_factor_scores[,1]

# Parent-only
dat_hyper_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqchypt1, csdqchypt1, dsdqhypt1, gpsdqhypt1, ipsdqhypt1, lpsdqhypt1, ppbhsdqhypt1, u1psdqhypt1, id_fam))
names(dat_hyper_ctwrp_var) <- c(new_conduct_ctwrp_names, "id_fam")

SDQ_Hyperactivity_model_P <- 'SDQ_Hyperactivity_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21'
fit_SDQhyper_P <- cfa(SDQ_Hyperactivity_model_P, data = dat_hyper_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQhyper_P <- lavInspect(fit_SDQhyper_P, "case.idx")
SDQhyper_factor_scores_P <- lavPredict(fit_SDQhyper_P, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_P1[used_case_idx_SDQhyper_P] <- SDQhyper_factor_scores_P[,1]

# Teacher only
dat_hyper_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqhypt1, itsdqhypt1, ltsdqhypt1, id_fam))
names(dat_hyper_ctwrt_var) <- c(new_conduct_ctwrt_names, "id_fam")

SDQ_Hyperactivity_model_T <- 'SDQ_Hyperactivity_T =~ yr7 + yr9 + yr12'
fit_SDQhyper_T <- cfa(SDQ_Hyperactivity_model_T, data = dat_hyper_ctwrt_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQhyper_T <- lavInspect(fit_SDQhyper_T, "case.idx")
SDQhyper_factor_scores_T <- lavPredict(fit_SDQhyper_T, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_T1[used_case_idx_SDQhyper_T] <- SDQhyper_factor_scores_T[,1]

# Child only
dat_hyper_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqhypt1, pcbhsdqhypt1, u1csdqhypt1, zmhsdqhypt1, id_fam))
names(dat_hyper_ctwrc_var) <- c(new_conduct_ctwrc_names, "id_fam")

SDQ_Hyperactivity_model_C <- 'SDQ_Hyperactivity_C =~ yr12 + yr16 + yr21 + yr26'
fit_SDQhyper_C <- cfa(SDQ_Hyperactivity_model_C, data = dat_hyper_ctwrc_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQhyper_C <- lavInspect(fit_SDQhyper_C, "case.idx")
SDQhyper_factor_scores_C <- lavPredict(fit_SDQhyper_C, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_CTWR_C1[used_case_idx_SDQhyper_C] <- SDQhyper_factor_scores_C[,1]

# Model for 'SDQ Peer Problems'
dat_peer_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcpert1, csdqcpert1, dsdqpert1, gpsdqpert1, ipsdqpert1, lpsdqpert1, u1psdqpert1, gtsdqpert1, itsdqpert1, ltsdqpert1, lcsdqpert1, pcbhsdqpert1, u1csdqpert1, zmhsdqpert1, id_fam))
names(dat_peer_ctcr_var) <- c(new_emot_ctcr_names, "id_fam")

SDQ_Peer_Problems_model <- '
  SDQ_Peer_Problems =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
'
fit_SDQpeer <- cfa(SDQ_Peer_Problems_model, data = dat_peer_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQpeer <- lavInspect(fit_SDQpeer, "case.idx")
SDQpeer_factor_scores <- lavPredict(fit_SDQpeer, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTCR1[used_case_idx_SDQpeer] <- SDQpeer_factor_scores[,1]

# Parent-only
dat_peer_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcpert1, csdqcpert1, dsdqpert1, gpsdqpert1, ipsdqpert1, lpsdqpert1, u1psdqpert1, id_fam))
names(dat_peer_ctwrp_var) <- c(new_emot_ctwrp_names, "id_fam")

SDQ_Peer_Problems_model_P <- 'SDQ_Peer_Problems_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr21'
fit_SDQpeer_P <- cfa(SDQ_Peer_Problems_model_P, data = dat_peer_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQpeer_P <- lavInspect(fit_SDQpeer_P, "case.idx")
SDQpeer_factor_scores_P <- lavPredict(fit_SDQpeer_P, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_P1[used_case_idx_SDQpeer_P] <- SDQpeer_factor_scores_P[,1]

# Teacher only
dat_peer_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqpert1, itsdqpert1, ltsdqpert1, id_fam))
names(dat_peer_ctwrt_var) <- c(new_conduct_ctwrt_names, "id_fam")

SDQ_Peer_Problems_model_T <- 'SDQ_Peer_Problems_T =~ yr7 + yr9 + yr12'
fit_SDQpeer_T <- cfa(SDQ_Peer_Problems_model_T, data = dat_peer_ctwrt_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQpeer_T <- lavInspect(fit_SDQpeer_T, "case.idx")
SDQpeer_factor_scores_T <- lavPredict(fit_SDQpeer_T, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_T1[used_case_idx_SDQpeer_T] <- SDQpeer_factor_scores_T[,1]

# Child only
dat_peer_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqpert1, pcbhsdqpert1, u1csdqpert1, zmhsdqpert1, id_fam))
names(dat_peer_ctwrc_var) <- c(new_conduct_ctwrc_names, "id_fam")

SDQ_Peer_Problems_model_C <- 'SDQ_Peer_Problems_C =~ yr12 + yr16 + yr21 + yr26'
fit_SDQpeer_C <- cfa(SDQ_Peer_Problems_model_C, data = dat_peer_ctwrc_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQpeer_C <- lavInspect(fit_SDQpeer_C, "case.idx")
SDQpeer_factor_scores_C <- lavPredict(fit_SDQpeer_C, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Peer_Problems_CTWR_C1[used_case_idx_SDQpeer_C] <- SDQpeer_factor_scores_C[,1]

# Model for 'SDQ Prosocial'
dat_prosocial_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcprot1, csdqcprot1, dsdqprot1, gpsdqprot1, ipsdqprot1, lpsdqprot1, ppbhsdqprot1, u1psdqprot1, gtsdqprot1, itsdqprot1, ltsdqprot1, lcsdqprot1, pcbhsdqprot1, u1csdqprot1, zmhsdqprot1, id_fam))
names(dat_prosocial_ctcr_var) <- c(new_conduct_ctcr_names, "id_fam")

SDQ_Prosocial_model <- '
  SDQ_Prosocial =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
'
fit_SDQprosocial <- cfa(SDQ_Prosocial_model, data = dat_prosocial_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQprosocial <- lavInspect(fit_SDQprosocial, "case.idx")
SDQprosocial_factor_scores <- lavPredict(fit_SDQprosocial, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTCR1[used_case_idx_SDQprosocial] <- SDQprosocial_factor_scores[,1]

# Parent-only
dat_prosocial_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcprot1, csdqcprot1, dsdqprot1, gpsdqprot1, ipsdqprot1, lpsdqprot1, ppbhsdqprot1, u1psdqprot1, id_fam))
names(dat_prosocial_ctwrp_var) <- c(new_conduct_ctwrp_names, "id_fam")

SDQ_Prosocial_model_P <- 'SDQ_Prosocial_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21'
fit_SDQprosocial_P <- cfa(SDQ_Prosocial_model_P, data = dat_prosocial_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQprosocial_P <- lavInspect(fit_SDQprosocial_P, "case.idx")
SDQprosocial_factor_scores_P <- lavPredict(fit_SDQprosocial_P, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_P1[used_case_idx_SDQprosocial_P] <- SDQprosocial_factor_scores_P[,1]

# Teacher only
dat_prosocial_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqprot1, itsdqprot1, ltsdqprot1, id_fam))
names(dat_prosocial_ctwrt_var) <- c(new_conduct_ctwrt_names, "id_fam")

SDQ_Prosocial_model_T <- 'SDQ_Prosocial_T =~ yr7 + yr9 + yr12'
fit_SDQprosocial_T <- cfa(SDQ_Prosocial_model_T, data = dat_prosocial_ctwrt_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQprosocial_T <- lavInspect(fit_SDQprosocial_T, "case.idx")
SDQprosocial_factor_scores_T <- lavPredict(fit_SDQprosocial_T, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_T1[used_case_idx_SDQprosocial_T] <- SDQprosocial_factor_scores_T[,1]

# Child only
dat_prosocial_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqprot1, pcbhsdqprot1, u1csdqprot1, zmhsdqprot1, id_fam))
names(dat_prosocial_ctwrc_var) <- c(new_conduct_ctwrc_names, "id_fam")

SDQ_Prosocial_model_C <- 'SDQ_Prosocial_C =~ yr12 + yr16 + yr21 + yr26'
fit_SDQprosocial_C <- cfa(SDQ_Prosocial_model_C, data = dat_prosocial_ctwrc_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQprosocial_C <- lavInspect(fit_SDQprosocial_C, "case.idx")
SDQprosocial_factor_scores_C <- lavPredict(fit_SDQprosocial_C, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_CTWR_C1[used_case_idx_SDQprosocial_C] <- SDQprosocial_factor_scores_C[,1]

# Model for 'SDQ Total Problems'
dat_total_ctcr_var <- subset(dat_scaled_for_CFA, select = c(bsdqcbeht1, csdqcbeht1, dsdqbeht1, gpsdqbeht1, ipsdqbeht1, lpsdqbeht1, ppbhsdqbeht1, u1psdqbeht1, gtsdqbeht1, itsdqbeht1, ltsdqbeht1, lcsdqbeht1, pcbhsdqbeht1, u1csdqbeht1, zmhsdqbeht1, id_fam))
names(dat_total_ctcr_var) <- c(new_conduct_ctcr_names, "id_fam")

SDQ_Total_Problems_model <- '
  SDQ_Total_Problems =~ yr2P + yr3P + yr4P + yr7P + yr9P + yr12P + yr16P + yr21P + yr7T + yr9T + yr12T + yr12C + yr16C + yr21C + yr26C
'
fit_SDQtotal <- cfa(SDQ_Total_Problems_model, data = dat_total_ctcr_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQtotal <- lavInspect(fit_SDQtotal, "case.idx")
SDQtotal_factor_scores <- lavPredict(fit_SDQtotal, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTCR1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTCR1[used_case_idx_SDQtotal] <- SDQtotal_factor_scores[,1]

# Parent-only
dat_total_ctwrp_var <- subset(dat_scaled_for_CFA, select = c(bsdqcbeht1, csdqcbeht1, dsdqbeht1, gpsdqbeht1, ipsdqbeht1, lpsdqbeht1, ppbhsdqbeht1, u1psdqbeht1, id_fam))
names(dat_total_ctwrp_var) <- c(new_conduct_ctwrp_names, "id_fam")

SDQ_Total_Problems_model_P <- 'SDQ_Total_Problems_P =~ yr2 + yr3 + yr4 + yr7 + yr9 + yr12 + yr16 + yr21'
fit_SDQtotal_P <- cfa(SDQ_Total_Problems_model_P, data = dat_total_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQtotal_P <- lavInspect(fit_SDQtotal_P, "case.idx")
SDQtotal_factor_scores_P <- lavPredict(fit_SDQtotal_P, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_P1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_P1[used_case_idx_SDQtotal_P] <- SDQtotal_factor_scores_P[,1]

# Teacher only
dat_total_ctwrt_var <- subset(dat_scaled_for_CFA, select = c(gtsdqbeht1, itsdqbeht1, ltsdqbeht1, id_fam))
names(dat_total_ctwrt_var) <- c(new_conduct_ctwrt_names, "id_fam")

SDQ_Total_Problems_model_T <- 'SDQ_Total_Problems_T =~ yr7 + yr9 + yr12'
fit_SDQtotal_T <- cfa(SDQ_Total_Problems_model_T, data = dat_total_ctwrt_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQtotal_T <- lavInspect(fit_SDQtotal_T, "case.idx")
SDQtotal_factor_scores_T <- lavPredict(fit_SDQtotal_T, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_T1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_T1[used_case_idx_SDQtotal_T] <- SDQtotal_factor_scores_T[,1]

# Child only
dat_total_ctwrc_var <- subset(dat_scaled_for_CFA, select = c(lcsdqbeht1, pcbhsdqbeht1, u1csdqbeht1, zmhsdqbeht1, id_fam))
names(dat_total_ctwrc_var) <- c(new_conduct_ctwrc_names, "id_fam")

SDQ_Total_Problems_model_C <- 'SDQ_Total_Problems_C =~ yr12 + yr16 + yr21 + yr26'
fit_SDQtotal_C <- cfa(SDQ_Total_Problems_model_C, data = dat_total_ctwrc_var, missing = "fiml", cluster = "id_fam")
used_case_idx_SDQtotal_C <- lavInspect(fit_SDQtotal_C, "case.idx")
SDQtotal_factor_scores_C <- lavPredict(fit_SDQtotal_C, method = "regression")
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_C1 <- NA
dat_scaled_for_CFA$SDQ_Total_Problems_CTWR_C1[used_case_idx_SDQtotal_C] <- SDQtotal_factor_scores_C[,1]


# ==========================================
# --- SDQ Traits by Developmental Stage ----
# ==========================================

# 1. SDQ Conduct
SDQ_Conduct_stage_model <- '
  Early_Cond =~ yr2P + yr3P + yr4P
  Child_Cond =~ yr7P + yr9P + yr7T + yr9T
  Adol_Cond =~ yr12P + yr16P + yr12T + yr12C + yr16C
  Adult_Cond =~ yr21P + yr21C + yr26C
  Latent_Stage_Cond =~ Early_Cond + Child_Cond + Adol_Cond + Adult_Cond
'
fit_SDQconduct_stage <- cfa(SDQ_Conduct_stage_model, data = dat_conduct_ctcr_var, missing = "fiml", cluster = "id_fam")
used_idx_cond <- lavInspect(fit_SDQconduct_stage, "case.idx")
cond_scores <- lavPredict(fit_SDQconduct_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_stage_Early1[used_idx_cond] <- cond_scores[, "Early_Cond"]
dat_scaled_for_CFA$SDQ_Conduct_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_stage_Child1[used_idx_cond] <- cond_scores[, "Child_Cond"]
dat_scaled_for_CFA$SDQ_Conduct_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_stage_Adol1[used_idx_cond] <- cond_scores[, "Adol_Cond"]
dat_scaled_for_CFA$SDQ_Conduct_stage_Adult1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_stage_Adult1[used_idx_cond] <- cond_scores[, "Adult_Cond"]
dat_scaled_for_CFA$SDQ_Conduct_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_stage_Latent1[used_idx_cond] <- cond_scores[, "Latent_Stage_Cond"]

SDQ_Conduct_P_stage_model <- '
  Early_Cond_P =~ yr2 + yr3 + yr4
  Child_Cond_P =~ yr7 + yr9
  Adol_Cond_P =~ yr12 + yr16
  Latent_Stage_Cond_P =~ Early_Cond_P + Child_Cond_P + Adol_Cond_P + yr21
'
fit_SDQconduct_P_stage <- cfa(SDQ_Conduct_P_stage_model, data = dat_conduct_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_idx_cond_P <- lavInspect(fit_SDQconduct_P_stage, "case.idx")
cond_P_scores <- lavPredict(fit_SDQconduct_P_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Early1[used_idx_cond_P] <- cond_P_scores[, "Early_Cond_P"]
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Child1[used_idx_cond_P] <- cond_P_scores[, "Child_Cond_P"]
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Adol1[used_idx_cond_P] <- cond_P_scores[, "Adol_Cond_P"]
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Conduct_P_stage_Latent1[used_idx_cond_P] <- cond_P_scores[, "Latent_Stage_Cond_P"]

# 2. SDQ Emotion
SDQ_Emotion_stage_model <- '
  Early_Emot =~ yr2P + yr3P + yr4P
  Child_Emot =~ yr7P + yr9P + yr7T + yr9T
  Adol_Emot =~ yr12P + yr12T + yr12C + yr16C
  Adult_Emot =~ yr21P + yr21C + yr26C
  Latent_Stage_Emot =~ Early_Emot + Child_Emot + Adol_Emot + Adult_Emot
'
fit_SDQemot_stage <- cfa(SDQ_Emotion_stage_model, data = dat_emot_ctcr_var, missing = "fiml", cluster = "id_fam")
used_idx_emot <- lavInspect(fit_SDQemot_stage, "case.idx")
emot_scores <- lavPredict(fit_SDQemot_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_stage_Early1[used_idx_emot] <- emot_scores[, "Early_Emot"]
dat_scaled_for_CFA$SDQ_Emotion_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_stage_Child1[used_idx_emot] <- emot_scores[, "Child_Emot"]
dat_scaled_for_CFA$SDQ_Emotion_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_stage_Adol1[used_idx_emot] <- emot_scores[, "Adol_Emot"]
dat_scaled_for_CFA$SDQ_Emotion_stage_Adult1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_stage_Adult1[used_idx_emot] <- emot_scores[, "Adult_Emot"]
dat_scaled_for_CFA$SDQ_Emotion_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_stage_Latent1[used_idx_emot] <- emot_scores[, "Latent_Stage_Emot"]

SDQ_Emotion_P_stage_model <- '
  Early_Emot_P =~ yr2 + yr3 + yr4
  Child_Emot_P =~ yr7 + yr9
  Latent_Stage_Emot_P =~ Early_Emot_P + Child_Emot_P + yr12 + yr21
'
fit_SDQemot_P_stage <- cfa(SDQ_Emotion_P_stage_model, data = dat_emot_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_idx_emot_P <- lavInspect(fit_SDQemot_P_stage, "case.idx")
emot_P_scores <- lavPredict(fit_SDQemot_P_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Emotion_P_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_P_stage_Early1[used_idx_emot_P] <- emot_P_scores[, "Early_Emot_P"]
dat_scaled_for_CFA$SDQ_Emotion_P_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_P_stage_Child1[used_idx_emot_P] <- emot_P_scores[, "Child_Emot_P"]
dat_scaled_for_CFA$SDQ_Emotion_P_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Emotion_P_stage_Latent1[used_idx_emot_P] <- emot_P_scores[, "Latent_Stage_Emot_P"]


# 3. SDQ Hyperactivity
SDQ_Hyper_stage_model <- '
  Early_Hyper =~ yr2P + yr3P + yr4P
  Child_Hyper =~ yr7P + yr9P + yr7T + yr9T
  Adol_Hyper =~ yr12P + yr16P + yr12T + yr12C + yr16C
  Adult_Hyper =~ yr21P + yr21C + yr26C
  Latent_Stage_Hyper =~ Early_Hyper + Child_Hyper + Adol_Hyper + Adult_Hyper
'
fit_SDQhyper_stage <- cfa(SDQ_Hyper_stage_model, data = dat_hyper_ctcr_var, missing = "fiml", cluster = "id_fam")
used_idx_hyper <- lavInspect(fit_SDQhyper_stage, "case.idx")
hyper_scores <- lavPredict(fit_SDQhyper_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Early1[used_idx_hyper] <- hyper_scores[, "Early_Hyper"]
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Child1[used_idx_hyper] <- hyper_scores[, "Child_Hyper"]
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Adol1[used_idx_hyper] <- hyper_scores[, "Adol_Hyper"]
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Adult1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Adult1[used_idx_hyper] <- hyper_scores[, "Adult_Hyper"]
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_stage_Latent1[used_idx_hyper] <- hyper_scores[, "Latent_Stage_Hyper"]

SDQ_Hyper_P_stage_model <- '
  Early_Hyper_P =~ yr2 + yr3 + yr4
  Child_Hyper_P =~ yr7 + yr9
  Adol_Hyper_P =~ yr12 + yr16
  Latent_Stage_Hyper_P =~ Early_Hyper_P + Child_Hyper_P + Adol_Hyper_P + yr21
'
fit_SDQhyper_P_stage <- cfa(SDQ_Hyper_P_stage_model, data = dat_hyper_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_idx_hyper_P <- lavInspect(fit_SDQhyper_P_stage, "case.idx")
hyper_P_scores <- lavPredict(fit_SDQhyper_P_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Early1[used_idx_hyper_P] <- hyper_P_scores[, "Early_Hyper_P"]
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Child1[used_idx_hyper_P] <- hyper_P_scores[, "Child_Hyper_P"]
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Adol1[used_idx_hyper_P] <- hyper_P_scores[, "Adol_Hyper_P"]
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Hyperactivity_P_stage_Latent1[used_idx_hyper_P] <- hyper_P_scores[, "Latent_Stage_Hyper_P"]


# 4. SDQ Peer Problems
SDQ_Peer_stage_model <- '
  Early_Peer =~ yr2P + yr3P + yr4P
  Child_Peer =~ yr7P + yr9P + yr7T + yr9T
  Adol_Peer =~ yr12P + yr12T + yr12C + yr16C
  Adult_Peer =~ yr21P + yr21C + yr26C
  Latent_Stage_Peer =~ Early_Peer + Child_Peer + Adol_Peer + Adult_Peer
'
fit_SDQpeer_stage <- cfa(SDQ_Peer_stage_model, data = dat_peer_ctcr_var, missing = "fiml", cluster = "id_fam")
used_idx_peer <- lavInspect(fit_SDQpeer_stage, "case.idx")
peer_scores <- lavPredict(fit_SDQpeer_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Peer_stage_Early1[used_idx_peer] <- peer_scores[, "Early_Peer"]
dat_scaled_for_CFA$SDQ_Peer_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Peer_stage_Child1[used_idx_peer] <- peer_scores[, "Child_Peer"]
dat_scaled_for_CFA$SDQ_Peer_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Peer_stage_Adol1[used_idx_peer] <- peer_scores[, "Adol_Peer"]
dat_scaled_for_CFA$SDQ_Peer_stage_Adult1 <- NA
dat_scaled_for_CFA$SDQ_Peer_stage_Adult1[used_idx_peer] <- peer_scores[, "Adult_Peer"]
dat_scaled_for_CFA$SDQ_Peer_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Peer_stage_Latent1[used_idx_peer] <- peer_scores[, "Latent_Stage_Peer"]

SDQ_Peer_P_stage_model <- '
  Early_Peer_P =~ yr2 + yr3 + yr4
  Child_Peer_P =~ yr7 + yr9
  Latent_Stage_Peer_P =~ Early_Peer_P + Child_Peer_P + yr12 + yr21
'
fit_SDQpeer_P_stage <- cfa(SDQ_Peer_P_stage_model, data = dat_peer_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_idx_peer_P <- lavInspect(fit_SDQpeer_P_stage, "case.idx")
peer_P_scores <- lavPredict(fit_SDQpeer_P_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Peer_P_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Peer_P_stage_Early1[used_idx_peer_P] <- peer_P_scores[, "Early_Peer_P"]
dat_scaled_for_CFA$SDQ_Peer_P_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Peer_P_stage_Child1[used_idx_peer_P] <- peer_P_scores[, "Child_Peer_P"]
dat_scaled_for_CFA$SDQ_Peer_P_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Peer_P_stage_Latent1[used_idx_peer_P] <- peer_P_scores[, "Latent_Stage_Peer_P"]


# 5. SDQ Prosocial
SDQ_Prosocial_stage_model <- '
  Early_Pro =~ yr2P + yr3P + yr4P
  Child_Pro =~ yr7P + yr9P + yr7T + yr9T
  Adol_Pro =~ yr12P + yr16P + yr12T + yr12C + yr16C
  Adult_Pro =~ yr21P + yr21C + yr26C
  Latent_Stage_Pro =~ Early_Pro + Child_Pro + Adol_Pro + Adult_Pro
'
fit_SDQprosocial_stage <- cfa(SDQ_Prosocial_stage_model, data = dat_prosocial_ctcr_var, missing = "fiml", cluster = "id_fam")
used_idx_pro <- lavInspect(fit_SDQprosocial_stage, "case.idx")
pro_scores <- lavPredict(fit_SDQprosocial_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_stage_Early1[used_idx_pro] <- pro_scores[, "Early_Pro"]
dat_scaled_for_CFA$SDQ_Prosocial_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_stage_Child1[used_idx_pro] <- pro_scores[, "Child_Pro"]
dat_scaled_for_CFA$SDQ_Prosocial_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_stage_Adol1[used_idx_pro] <- pro_scores[, "Adol_Pro"]
dat_scaled_for_CFA$SDQ_Prosocial_stage_Adult1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_stage_Adult1[used_idx_pro] <- pro_scores[, "Adult_Pro"]
dat_scaled_for_CFA$SDQ_Prosocial_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_stage_Latent1[used_idx_pro] <- pro_scores[, "Latent_Stage_Pro"]

SDQ_Prosocial_P_stage_model <- '
  Early_Pro_P =~ yr2 + yr3 + yr4
  Child_Pro_P =~ yr7 + yr9
  Adol_Pro_P =~ yr12 + yr16
  Latent_Stage_Pro_P =~ Early_Pro_P + Child_Pro_P + Adol_Pro_P + yr21
'
fit_SDQprosocial_P_stage <- cfa(SDQ_Prosocial_P_stage_model, data = dat_prosocial_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_idx_pro_P <- lavInspect(fit_SDQprosocial_P_stage, "case.idx")
pro_P_scores <- lavPredict(fit_SDQprosocial_P_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Early1[used_idx_pro_P] <- pro_P_scores[, "Early_Pro_P"]
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Child1[used_idx_pro_P] <- pro_P_scores[, "Child_Pro_P"]
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Adol1[used_idx_pro_P] <- pro_P_scores[, "Adol_Pro_P"]
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Prosocial_P_stage_Latent1[used_idx_pro_P] <- pro_P_scores[, "Latent_Stage_Pro_P"]


# 6. SDQ Total Problems
SDQ_Total_stage_model <- '
  Early_Tot =~ yr2P + yr3P + yr4P
  Child_Tot =~ yr7P + yr9P + yr7T + yr9T
  Adol_Tot =~ yr12P + yr16P + yr12T + yr12C + yr16C
  Adult_Tot =~ yr21P + yr21C + yr26C
  Latent_Stage_Tot =~ Early_Tot + Child_Tot + Adol_Tot + Adult_Tot
'
fit_SDQtotal_stage <- cfa(SDQ_Total_stage_model, data = dat_total_ctcr_var, missing = "fiml", cluster = "id_fam")
used_idx_tot <- lavInspect(fit_SDQtotal_stage, "case.idx")
tot_scores <- lavPredict(fit_SDQtotal_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Total_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Total_stage_Early1[used_idx_tot] <- tot_scores[, "Early_Tot"]
dat_scaled_for_CFA$SDQ_Total_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Total_stage_Child1[used_idx_tot] <- tot_scores[, "Child_Tot"]
dat_scaled_for_CFA$SDQ_Total_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Total_stage_Adol1[used_idx_tot] <- tot_scores[, "Adol_Tot"]
dat_scaled_for_CFA$SDQ_Total_stage_Adult1 <- NA
dat_scaled_for_CFA$SDQ_Total_stage_Adult1[used_idx_tot] <- tot_scores[, "Adult_Tot"]
dat_scaled_for_CFA$SDQ_Total_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Total_stage_Latent1[used_idx_tot] <- tot_scores[, "Latent_Stage_Tot"]

SDQ_Total_P_stage_model <- '
  Early_Tot_P =~ yr2 + yr3 + yr4
  Child_Tot_P =~ yr7 + yr9
  Adol_Tot_P =~ yr12 + yr16
  Latent_Stage_Tot_P =~ Early_Tot_P + Child_Tot_P + Adol_Tot_P + yr21
'
fit_SDQtotal_P_stage <- cfa(SDQ_Total_P_stage_model, data = dat_total_ctwrp_var, missing = "fiml", cluster = "id_fam")
used_idx_tot_P <- lavInspect(fit_SDQtotal_P_stage, "case.idx")
tot_P_scores <- lavPredict(fit_SDQtotal_P_stage, method = "regression")
dat_scaled_for_CFA$SDQ_Total_P_stage_Early1 <- NA
dat_scaled_for_CFA$SDQ_Total_P_stage_Early1[used_idx_tot_P] <- tot_P_scores[, "Early_Tot_P"]
dat_scaled_for_CFA$SDQ_Total_P_stage_Child1 <- NA
dat_scaled_for_CFA$SDQ_Total_P_stage_Child1[used_idx_tot_P] <- tot_P_scores[, "Child_Tot_P"]
dat_scaled_for_CFA$SDQ_Total_P_stage_Adol1 <- NA
dat_scaled_for_CFA$SDQ_Total_P_stage_Adol1[used_idx_tot_P] <- tot_P_scores[, "Adol_Tot_P"]
dat_scaled_for_CFA$SDQ_Total_P_stage_Latent1 <- NA
dat_scaled_for_CFA$SDQ_Total_P_stage_Latent1[used_idx_tot_P] <- tot_P_scores[, "Latent_Stage_Tot_P"]


# ==========================================
# --- Educational Achievement Latent Models (Ages 7-12) ----
# ==========================================

# 1. English Achievement (Ages 7, 9, 10, 12)
dat_eng_var <- subset(dat_scaled_for_CFA, select = c(gteng1, iteng1, jteng1, lteng1, id_fam))
names(dat_eng_var) <- c("yr7", "yr9", "yr10", "yr12", "id_fam")

Eng_Achieve_model <- 'Eng_Achieve =~ yr7 + yr9 + yr10 + yr12'
fit_eng <- cfa(Eng_Achieve_model, data = dat_eng_var, missing = "fiml", cluster = "id_fam")
used_idx_eng <- lavInspect(fit_eng, "case.idx")
eng_scores <- lavPredict(fit_eng, method = "regression")
dat_scaled_for_CFA$Eng_Achieve_Latent1 <- NA
dat_scaled_for_CFA$Eng_Achieve_Latent1[used_idx_eng] <- eng_scores[,1]

# 2. Maths Achievement (Ages 7, 9, 10, 12)
dat_mat_var <- subset(dat_scaled_for_CFA, select = c(gtmat1, itmat1, jtmat1, ltmat1, id_fam))
names(dat_mat_var) <- c("yr7", "yr9", "yr10", "yr12", "id_fam")

Mat_Achieve_model <- 'Mat_Achieve =~ yr7 + yr9 + yr10 + yr12'
fit_mat <- cfa(Mat_Achieve_model, data = dat_mat_var, missing = "fiml", cluster = "id_fam")
used_idx_mat <- lavInspect(fit_mat, "case.idx")
mat_scores <- lavPredict(fit_mat, method = "regression")
dat_scaled_for_CFA$Mat_Achieve_Latent1 <- NA
dat_scaled_for_CFA$Mat_Achieve_Latent1[used_idx_mat] <- mat_scores[,1]

# 3. Science Achievement (Ages 9, 10, 12)
dat_sci_var <- subset(dat_scaled_for_CFA, select = c(itsci1, jtsci1, ltsci1, id_fam))
names(dat_sci_var) <- c("yr9", "yr10", "yr12", "id_fam")

Sci_Achieve_model <- 'Sci_Achieve =~ yr9 + yr10 + yr12'
fit_sci <- cfa(Sci_Achieve_model, data = dat_sci_var, missing = "fiml", cluster = "id_fam")
used_idx_sci <- lavInspect(fit_sci, "case.idx")
sci_scores <- lavPredict(fit_sci, method = "regression")
dat_scaled_for_CFA$Sci_Achieve_Latent1 <- NA
dat_scaled_for_CFA$Sci_Achieve_Latent1[used_idx_sci] <- sci_scores[,1]

# 4. Core-Subject Achievement (Ages 7, 9, 10, 12)
dat_core_var <- subset(dat_scaled_for_CFA, select = c(gt2ac1, it3ac1, jt3ac1, lt3ac1, id_fam))
names(dat_core_var) <- c("yr7", "yr9", "yr10", "yr12", "id_fam")

Core_Achieve_model <- 'Core_Achieve =~ yr7 + yr9 + yr10 + yr12'
fit_core <- cfa(Core_Achieve_model, data = dat_core_var, missing = "fiml", cluster = "id_fam")
used_idx_core <- lavInspect(fit_core, "case.idx")
core_scores <- lavPredict(fit_core, method = "regression")
dat_scaled_for_CFA$Core_Achieve_Latent1 <- NA
dat_scaled_for_CFA$Core_Achieve_Latent1[used_idx_core] <- core_scores[,1]


# ==========================================
# --- Master Fit List & Exports ----
# ==========================================

fit_list <- list(
  "General Cognitive Ability (g) - Overall" = fit_g,
  "General Cognitive Ability (g) - Stage" = fit_g_stage,
  "General Cognitive Ability (g) - Method" = fit_g_method,
  "Verbal Ability - Overall" = fit_vb,
  "Verbal Ability - Stage" = fit_vb_stage,
  "Verbal Ability - Method" = fit_vb_method,
  "Nonverbal Ability - Overall" = fit_nv,
  "Nonverbal Ability - Stage" = fit_nv_stage,
  "Nonverbal Ability - Method" = fit_nv_method,
  "English Achievement Latent (Teacher 7-12)" = fit_eng,
  "Maths Achievement Latent (Teacher 7-12)" = fit_mat,
  "Science Achievement Latent (Teacher 9-12)" = fit_sci,
  "Core-Subject Achievement Latent (Teacher 7-12)" = fit_core,
  "ARBQ Shyness - CTCR Overall" = fit_shy,
  "ARBQ Shyness - CTCR Stage" = fit_shy_stage,
  "ARBQ Fear - CTCR Overall" = fit_fear,
  "ARBQ Fear - CTCR Stage" = fit_fear_stage,
  "ARBQ Obsessive-Compulsive - CTCR Overall" = fit_ocb,
  "ARBQ Obsessive-Compulsive - CTCR Stage" = fit_ocb_stage,
  "ARBQ Negative Affect - CTCR Overall" = fit_naff,
  "ARBQ Negative Affect - CTCR Stage" = fit_naff_stage,
  "ARBQ Negative Cognition - CTCR Overall" = fit_ncog,
  "ARBQ Negative Cognition - CTCR Stage" = fit_ncog_stage,
  "ARBQ Anxiety Total - CTCR Overall" = fit_ARBQ_total,
  "ARBQ Anxiety Total - CTCR Stage" = fit_ARBQ_total_stage,
  "Conners Inattention - CTCR Overall" = fit_inatt,
  "Conners Inattention - CTCR Stage" = fit_inatt_stage,
  "Conners Inattention - Parent Overall" = fit_inatt_P,
  "Conners Inattention - Parent Stage" = fit_inatt_P_stage,
  "Conners Hyperactivity-Impulsivity - CTCR Overall" = fit_hyper_impul,
  "Conners Hyperactivity-Impulsivity - CTCR Stage" = fit_hyper_stage,
  "Conners Hyperactivity-Impulsivity - Parent Overall" = fit_hyper_impul_P,
  "Conners Hyperactivity-Impulsivity - Parent Stage" = fit_hyper_P_stage,
  "Conners Total - CTCR Overall" = fit_ADHD,
  "Conners Total - CTCR Stage" = fit_ADHD_stage,
  "Conners Total - Parent Overall" = fit_ADHD_P,
  "Conners Total - Parent Stage" = fit_ADHD_P_stage,
  "SDQ Conduct - CTCR Overall" = fit_SDQconduct,
  "SDQ Conduct - CTCR Stage" = fit_SDQconduct_stage,
  "SDQ Conduct - Parent Overall" = fit_SDQconduct_P,
  "SDQ Conduct - Parent Stage" = fit_SDQconduct_P_stage,
  "SDQ Conduct - Teacher Overall" = fit_SDQconduct_T,
  "SDQ Conduct - Child Overall" = fit_SDQconduct_C,
  "SDQ Emotion - CTCR Overall" = fit_SDQemot,
  "SDQ Emotion - CTCR Stage" = fit_SDQemot_stage,
  "SDQ Emotion - Parent Overall" = fit_SDQemot_P,
  "SDQ Emotion - Parent Stage" = fit_SDQemot_P_stage,
  "SDQ Emotion - Teacher Overall" = fit_SDQemot_T,
  "SDQ Emotion - Child Overall" = fit_SDQemot_C,
  "SDQ Hyperactivity - CTCR Overall" = fit_SDQhyper,
  "SDQ Hyperactivity - CTCR Stage" = fit_SDQhyper_stage,
  "SDQ Hyperactivity - Parent Overall" = fit_SDQhyper_P,
  "SDQ Hyperactivity - Parent Stage" = fit_SDQhyper_P_stage,
  "SDQ Hyperactivity - Teacher Overall" = fit_SDQhyper_T,
  "SDQ Hyperactivity - Child Overall" = fit_SDQhyper_C,
  "SDQ Peer Problems - CTCR Overall" = fit_SDQpeer,
  "SDQ Peer Problems - CTCR Stage" = fit_SDQpeer_stage,
  "SDQ Peer Problems - Parent Overall" = fit_SDQpeer_P,
  "SDQ Peer Problems - Parent Stage" = fit_SDQpeer_P_stage,
  "SDQ Peer Problems - Teacher Overall" = fit_SDQpeer_T,
  "SDQ Peer Problems - Child Overall" = fit_SDQpeer_C,
  "SDQ Prosocial - CTCR Overall" = fit_SDQprosocial,
  "SDQ Prosocial - CTCR Stage" = fit_SDQprosocial_stage,
  "SDQ Prosocial - Parent Overall" = fit_SDQprosocial_P,
  "SDQ Prosocial - Parent Stage" = fit_SDQprosocial_P_stage,
  "SDQ Prosocial - Teacher Overall" = fit_SDQprosocial_T,
  "SDQ Prosocial - Child Overall" = fit_SDQprosocial_C,
  "SDQ Total Problems - CTCR Overall" = fit_SDQtotal,
  "SDQ Total Problems - CTCR Stage" = fit_SDQtotal_stage,
  "SDQ Total Problems - Parent Overall" = fit_SDQtotal_P,
  "SDQ Total Problems - Parent Stage" = fit_SDQtotal_P_stage,
  "SDQ Total Problems - Teacher Overall" = fit_SDQtotal_T,
  "SDQ Total Problems - Child Overall" = fit_SDQtotal_C
)

# ==========================================
# --- Generate Combined PDF of All Plots ----
# ==========================================
pdf("CFA_Factor_Models_All_wPGS.pdf", width = 40, height = 30)

for(i in seq_along(fit_list)) {
  plot_title <- paste0("Figure S9_", i, ": ", names(fit_list)[i])
  plot_sem_tree(fit_list[[i]])
  title(main = plot_title, outer = TRUE, line = -2, cex.main = 3.0)
}
dev.off() 

# ==========================================
# --- Export Results ----
# ==========================================
all_fit_indices <- list()
all_loadings <- list()
fit_measures_of_interest_scaled <- c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "rmsea.scaled", "srmr")

for (model_name in names(fit_list)) {
  fit <- fit_list[[model_name]]
  
  if (lavInspect(fit, "converged")) {
    all_fit_indices[[model_name]] <- fitMeasures(fit, fit_measures_of_interest_scaled)
    params <- standardizedSolution(fit)
    
    loadings <- params %>%
      filter(op == "=~") %>%
      select(Latent_Factor = lhs, Indicator = rhs, Loading = est.std, SE = se, P_value = pvalue)
    all_loadings[[model_name]] <- loadings
    
  } else {
    na_fit <- rep(NA, length(fit_measures_of_interest_scaled))
    names(na_fit) <- fit_measures_of_interest_scaled
    all_fit_indices[[model_name]] <- na_fit
    all_loadings[[model_name]] <- data.frame(Latent_Factor=NA, Indicator=NA, Loading=NA, SE=NA, P_value=NA)
  }
}

fit_summary_list <- lapply(names(all_fit_indices), function(model_name) {
  indices <- all_fit_indices[[model_name]]
  data.frame(
    Model        = model_name,
    ChiSq_Scaled = indices["chisq.scaled"],
    df           = indices["df"],
    P_Scaled     = indices["pvalue.scaled"],
    CFI_Scaled   = indices["cfi.scaled"],
    RMSEA_Scaled = indices["rmsea.scaled"],
    SRMR         = indices["srmr"] 
  )
})
fit_summary <- dplyr::bind_rows(fit_summary_list)
write.csv(fit_summary, "CFA_Fit_Summary_Scaled_wPGS.csv", row.names = FALSE)

loadings_summary_list <- lapply(names(all_loadings), function(model_name) {
  df <- all_loadings[[model_name]]
  df$Model <- model_name
  df[, c("Model", setdiff(names(df), "Model"))]
})
loadings_summary <- dplyr::bind_rows(loadings_summary_list)
write.csv(loadings_summary, "CFA_Factor_Loadings_Summary_wPGS.csv", row.names = FALSE)
write.csv(dat_scaled_for_CFA, paste0(sourceFileStem, "dat_scaled_with_CFA_scores_wPGS.csv"), row.names = FALSE)
