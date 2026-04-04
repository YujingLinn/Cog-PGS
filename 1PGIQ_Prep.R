# Data preparation for polygenic g score project

library(tibble)
library(Hmisc) 
library(dplyr)
library(stringr)
library(foreign) 
library(corrplot)
library(purrr)

dat_path <- "./Data/743 YL FINAL (August 2025).sav"
PGIQ_path <- "./Data/PGIQ_auto_BLUP_useThis.score"
pca_file_path <- "./Data/teds_trio_projected_pca.sscore.txt"
sourceFileStem <- './Data/'
VarList_path <- "./PGIQ Codes/0PGIQ_VarList.R"
CFA_path <- "./PGIQ Codes/1PGIQ_CFA.R"
CFA_VarList_path <- "./PGIQ Codes/0PGIQ_CommFactorList.R"

dat <- read.spss(dat_path, to.data.frame = TRUE, use.value.labels = FALSE)

# =============================================================================
# 0. VarLists
# =============================================================================
df_verbal12T1 <- dplyr::select(dat, id_twin, ltotota1, lfltot1, lintot1, lpitota1, lgotota1, lyntot1, ltowt1)
df_verbal12T2 <- dplyr::select(dat, id_twin, ltotota2, lfltot2, lintot2, lpitota2, lgotota2, lyntot2, ltowt2)
df_nonverbal12T1 <- dplyr::select(dat, id_twin, lma1tot1, lma2tot1, lma3tot1)
df_nonverbal12T2 <- dplyr::select(dat, id_twin, lma1tot2, lma2tot2, lma3tot2)

df_list_cognitive12 <- list(df_verbal12T1, df_verbal12T2, df_nonverbal12T1, df_nonverbal12T2)

score_list_cognitive12 <- map(df_list_cognitive12, ~ {
  n_missing <- apply(.[-1], 1, function(x) sum(is.na(x))) > ((ncol(.)-1)/2)
  row_sums <- apply(.[!n_missing, -1], 1, mean, na.rm=TRUE) * (ncol(.)-1)
  data.frame(
    id_twin = .[!n_missing, 1],
    row_sums = row_sums
  )
})

df_cognitive12 <- score_list_cognitive12 %>% reduce(full_join, by = "id_twin")
variable_name_list <- c("id_twin", "lverbal12T1", "lverbal12T2", "lnonverbal12T1", "lnonverbal12T2")
colnames(df_cognitive12) <- variable_name_list
dat <- dat %>% full_join(df_cognitive12)

edu_attain_collapse <- function(data, EA26, EA21, new_item) {
  data[[new_item]] <- ifelse(
    !is.na(data[[EA26]]),
    case_when(
      data[[EA26]] %in% 1 ~ NA, 
      data[[EA26]] %in% c(2, 3, 4) ~ 10, 
      data[[EA26]] %in% c(5, 6) ~ 12, 
      data[[EA26]] %in% c(7, 8) ~ 14, 
      data[[EA26]] == 9 ~ 15, 
      data[[EA26]] == 10 ~ 16, 
      data[[EA26]] == 11 ~ 20 
    ),
    ifelse( 
      !is.na(data[[EA21]]), 
      case_when(
        data[[EA21]] %in% 1 ~ NA,
        data[[EA21]] %in% c(2, 3, 4) ~ 10,
        data[[EA21]] %in% c(5, 6) ~ 12,
        data[[EA21]] %in% c(7, 8) ~ 14,
        data[[EA21]] == 9 ~ 15,
        data[[EA21]] == 10 ~ 16,
        data[[EA21]] == 11 ~ 20
      ),
      NA  
    )
  )
  return(data)
}

dat <- edu_attain_collapse(dat, "zmhhqual1", "u1chqualp1", "zEA1")
dat <- edu_attain_collapse(dat, "zmhhqual2", "u1chqualp2", "zEA2")
dat$zEA1 <- as.numeric(dat$zEA1)
dat$zEA2 <- as.numeric(dat$zEA2)
source(VarList_path)

# =============================================================================
# 1. PGIQ from SMTpred
# =============================================================================
SMTpred <- read.table(PGIQ_path, header = TRUE, sep = "", stringsAsFactors = FALSE)
names(SMTpred)[2] <- "id_twin"
names(SMTpred)[1] <- "id_fam"
names(SMTpred)[3] <- "PGg_EA"
names(SMTpred)[4] <- "PGg_IQ" 

SMTpred$id_fam <- as.numeric(SMTpred$id_fam) 
SMTpred$id_twin <- as.numeric(SMTpred$id_twin) 

SMTpred_merged <- left_join(dat, SMTpred)
SMTpred_merged <- SMTpred_merged %>%
  rename(
    Savage_g_PGS       = YL_cog_final_pred_auto,
    Okbay_EA_PGS       = EA4_no23andme_Okbay2022
)
SMTpred_merged_UN <- SMTpred_merged %>% filter(selectunpaired == 1)

selected_vars <- SMTpred_merged_UN[, c("PGg_EA", "Okbay_EA_PGS", "Savage_g_PGS", "brawg1", "crawg1", "drawg1",  "gcg1", "icg1", "jcg1", "lcg1", "ncg1", "pcg1", "ucgt1")] 
colnames(selected_vars) <- c(
  "Polygenic_g_Score", "Okbay_EA_PGS", "Savage_g_PGS", "g_at_2", "g_at_3",
  "g_at_4", "g_at_7", "g_at_9", "g_at_10", "g_at_12", "g_at_14", "g_at_16", "g_at_25"
)
cor_matrix <- cor(selected_vars, use = "pairwise.complete.obs")

png("PGS_g_corrplot.png", width = 2000, height = 2000, res = 300)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "original", tl.cex = 0.8, tl.col = "black",
         addCoef.col = "black", number.cex = 0.7)
dev.off()

# Add the new PCs
pca_projected <- read.table(pca_file_path, header = TRUE, comment.char = "")
colnames(pca_projected)[1] <- "id_fam"
colnames(pca_projected)[2] <- "id_twin"
projected_scores <- pca_projected[, c("id_fam", "id_twin", paste0("SCORE", 1:20, "_AVG"))]

projected_scores <- projected_scores %>%
  rename_with(~ paste0("PC", seq_along(.x)), .cols = starts_with("SCORE"))

pcs_twins <- projected_scores %>% filter(str_ends(id_twin, "[12]"))
pcs_mothers <- projected_scores %>% filter(str_ends(id_twin, "3")) %>% rename_with(~ paste0(.x, "_fp"), .cols = starts_with("PC")) %>% dplyr::select(-id_twin)
pcs_fathers <- projected_scores %>% filter(str_ends(id_twin, "4")) %>% rename_with(~ paste0(.x, "_mp"), .cols = starts_with("PC")) %>% dplyr::select(-id_twin)

updated_PCA <- pcs_twins %>% left_join(pcs_mothers, by = "id_fam") %>% left_join(pcs_fathers, by = "id_fam")
updated_PCA$id_fam <- as.numeric(updated_PCA$id_fam)
updated_PCA$id_twin <- as.numeric(updated_PCA$id_twin)

SMTpred_merged_oldPCrm <- SMTpred_merged %>% dplyr::select(-c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"))
SMTpred_merged_PCupdated <- SMTpred_merged_oldPCrm %>% left_join(updated_PCA, by = c("id_fam", "id_twin"))

# =============================================================================
# 2. Append the MZ co-twins PGS & apply default exclusions
# =============================================================================
SMTpred_merged_PCupdated_imputed <- SMTpred_merged_PCupdated %>%
  group_by(id_fam) %>%
  mutate(
    across(
      c(PGg_EA, PGg_IQ, Savage_g_PGS, Okbay_EA_PGS, chiptype, PC1:PC20),
      ~ if (all(zygos == 1, na.rm = TRUE) && any(!is.na(.x))) {
        first(.x[!is.na(.x)])
      } else {
        .x
      }
    )
  ) %>%
  ungroup()

SMTpred_merged_PCupdated_imputed <- SMTpred_merged_PCupdated_imputed[ SMTpred_merged_PCupdated_imputed$exclude1 == 0, ] 

# =============================================================================
# 3. Scale all outcomes, PGS, and cont. covariates
# =============================================================================
SMTpred_merged_PCupdated_imputed_prep4scale <- SMTpred_merged_PCupdated_imputed
SMTpred_merged_PCupdated_imputed_prep4scale$PGgEA_IQscale <- scale(SMTpred_merged_PCupdated_imputed_prep4scale$PGg_EA, center = TRUE, scale = TRUE) * 15 + 100

all_Varlist <- c(
  G_Composites_Varlist, Verbal_Tests_Varlist, Nonverbal_Tests_Varlist,
  Edu_Achieve_Attain_Varlist, Anxiety_Varlist, Conners_Varlist,
  SDQ_Varlist, Anthro_Varlist, Wellbeing_Varlist, one_rater_Varlist,
  Anxiety_Varlist_oneRater, Conners_Varlist_oneRater, SDQ_Varlist_oneRater
)

target_vars <- unique(sapply(all_Varlist, function(item) paste0(item[1], "1")))
pheno_to_scale <- target_vars[target_vars %in% names(SMTpred_merged_PCupdated_imputed_prep4scale)]

id_vars <- c("id_twin", "id_fam")
vars_to_keep <- c("twin", "random", "sex1", "sex2", "zygos", "sexzyg", "x3zygos", "selectunpaired", "chiptype", "Savage_g_PGS", "Okbay_EA_PGS")
PCs <- c(paste0("PC", 1:10))
ages <- c("atwinage", "bpbage", "cpbage", "dpbage", "gpbage", "hage", "icpage", "itage1", 
          "jcstage1", "jtqage1", "lcqage1", "ltqage1", "ncqage1", "pcbhage1", "pcwebage1", 
          "rcqage1", "rcqalage1", "u1cage1", "ucgage1", "zcage1", "zmhage1")
ses_vars <- c("ases", "gses", "pses", "u1pses")

dat_raw <- SMTpred_merged_PCupdated_imputed_prep4scale[, c(id_vars, vars_to_keep, pheno_to_scale, "PGg_EA", "PGgEA_IQscale", PCs, ages, ses_vars), drop = FALSE]
dat_scaled_for_CFA <- dat_raw 

extra_PGS_to_scale <- c("Savage_g_PGS", "Okbay_EA_PGS")
dat_scaled_for_CFA[, c(pheno_to_scale, "PGg_EA", extra_PGS_to_scale, PCs, ages, ses_vars)] <- scale(dat_scaled_for_CFA[, c(pheno_to_scale, "PGg_EA", extra_PGS_to_scale, PCs, ages, ses_vars)])

write.csv(dat_scaled_for_CFA, file = paste0(sourceFileStem, "dat_scaled_for_CFA.csv"), row.names = FALSE)

# =============================================================================
# 4. Extract common factors from cognitive and mental health outcomes
# =============================================================================
source(CFA_path)
source(CFA_VarList_path)

CF_score_vars <- sapply(CommonFactor_Score_Varlist, `[`, 1)
CF_score_vars_with_suffix <- paste0(CF_score_vars, "1")

scaled_vars_to_add <- dat_scaled_for_CFA %>% select(id_fam, id_twin, all_of(CF_score_vars_with_suffix))
dat_raw <- dat_raw %>% left_join(scaled_vars_to_add, by = c("id_fam", "id_twin"))
dat_scaled <- dat_scaled_for_CFA %>% mutate(across(all_of(CF_score_vars_with_suffix), ~as.numeric(scale(.))))

dat_raw$PGgS <- dat_raw$PGg_EA
dat_scaled$PGgS <- dat_scaled$PGg_EA

# =============================================================================
# 5. Export and save final dataset
# =============================================================================
dat_scaled_selectunpaired <- subset(dat_scaled, selectunpaired==1)
dat_scaled_F <- subset(dat_scaled, sex1==0)
dat_scaled_M <- subset(dat_scaled, sex1==1)
dat_scaled_selectunpaired_F <- subset(dat_scaled, selectunpaired==1 & sex1==0)
dat_scaled_selectunpaired_M <- subset(dat_scaled, selectunpaired==1 & sex1==1)

write.csv(dat_raw, file = paste0(sourceFileStem, "PGIQ_raw.csv"), row.names = FALSE)
write.csv(dat_scaled, file = paste0(sourceFileStem, "PGIQ_scaled.csv"), row.names = FALSE)
write.csv(dat_scaled_selectunpaired, file = paste0(sourceFileStem, "PGIQ_scaled_selectunpaired.csv"), row.names = FALSE)
write.csv(dat_scaled_F, file = paste0(sourceFileStem, "PGIQ_scaled_F.csv"), row.names = FALSE)
write.csv(dat_scaled_M, file = paste0(sourceFileStem, "PGIQ_scaled_M.csv"), row.names = FALSE)
write.csv(dat_scaled_selectunpaired_F, file = paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_F.csv"), row.names = FALSE)
write.csv(dat_scaled_selectunpaired_M, file = paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_M.csv"), row.names = FALSE)
