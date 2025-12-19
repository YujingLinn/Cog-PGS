# data preparation for polygenic g score project ####
# Yujing Lin
# 28 August, 2025 

# 0. organise the vars
# 1. input PG g score & new PCs from hg38
# 2. impute MZ co-twins PGIQ
# 3. scale all vars

library(tibble)
library(Hmisc) # spss.get
library(dplyr)
select <- dplyr::select
library(stringr)
#library(sjlabelled) # label
library(foreign) # read.spss
library(corrplot)
library(purrr) #map()

dat_path <- "/Users/yujinglin/Downloads/743 YL FINAL (August 2025).sav"
PGIQ_path <- "/Users/yujinglin/Desktop/polygenic IQ score/PGIQ_290825.score"
pca_file_path <- "/Users/yujinglin/Desktop/polygenic IQ score/teds_trio_projected_pca.sscore.txt"
sourceFileStem <- '/Users/yujinglin/Desktop/polygenic IQ score/Data220925/'

# dat_path <- "/scratch/users/k21170717/polygenic_IQ_score/743_YL_FINAL_August_2025.sav"
# PGIQ_path <- "/scratch/users/k21170717/polygenic_IQ_score/PGIQ_290825.score"
# pca_file_path <- "/scratch/users/k21170717/polygenic_IQ_score/teds_trio_projected_pca.sscore.txt"
# sourceFileStem <- '/scratch/users/k21170717/polygenic_IQ_score/Data220925/'

dat <- read.spss(dat_path, to.data.frame = TRUE, use.value.labels = FALSE)
dim(dat) # 27856  1703

# see the variables & labels
labels_list <- lapply(dat, sjlabelled::get_label)
all_var_labels <- sapply(labels_list, function(x) if (is.null(x)) NA_character_ else x)

label_df <- data.frame(
  Variable = names(dat),
  Label = all_var_labels,
  stringsAsFactors = FALSE
)
rownames(label_df) <- NULL

filtered_df <- label_df %>%
  filter(!str_detect(Variable, "2$")) # get rid of all the variables with a suffix of 2

dim(filtered_df) # 877 2
#print(filtered_df)[500:877,]

# Find variable names containing "age"
grep("age", names(dat), value = TRUE)

# all ages ####
# "atwinage"  "bpbage"    "cpbage"    "dpbage"    "gpbage"    "hage"      "icpage"    "itage1"    "itage2"    "jcstage1"  "jcstage2"  "jtqage1"   "jtqage2"   "lcqage1"   "lcqage2"   "ltqage1"   "ltqage2"   "ncqage1"   "ncqage2"  
# "pcbhage1"  "pcbhage2"  "pcwebage1" "pcwebage2" "rcqage1"   "rcqage2"   "rcqalage1" "rcqalage2" "u1cage1"   "u1cage2"   "ucgage1"   "ucgage2"   "zcage1"    "zcage2"    "zmhage1"   "zmhage2" 

grep("ses", names(dat), value = TRUE)
# all ses ####
# ases, gses, pses, u1pses, u1pses5in



# =============================================================================
### ✅ 0. VarLists ####
# =============================================================================
# create the age 12 verbal composite from language & reading abilities
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
colnames(df_cognitive12)<-variable_name_list

dat <- dat %>% full_join(df_cognitive12)



edu_attain_collapse <- function(data, EA26, EA21, new_item) {
  data[[new_item]] <- ifelse(
    !is.na(data[[EA26]]),
    case_when(
      data[[EA26]] %in% 1 ~ NA, # no qualification
      data[[EA26]] %in% c(2, 3, 4) ~ 10, # GCSE
      data[[EA26]] %in% c(5, 6) ~ 12, # A-level
      data[[EA26]] %in% c(7, 8) ~ 14, # some higher edu
      data[[EA26]] == 9 ~ 15, # bachelor 
      data[[EA26]] == 10 ~ 16, # master
      data[[EA26]] == 11 ~ 20 # doctoral
    ),
    ifelse( # For those who are NAs for EA26, we will check if their responses are available in EA21
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
      NA  # Only those who are NAs in both EA26 and EA21 will be NAs
    )
  )
  return(data)
}

dat <- edu_attain_collapse(dat, "zmhhqual1", "u1chqualp1", "zEA1")
dat <- edu_attain_collapse(dat, "zmhhqual2", "u1chqualp2", "zEA2")

dat$zEA1 <- as.numeric(dat$zEA1)
dat$zEA2 <- as.numeric(dat$zEA2)

source("./PGIQ Codes/0PGIQ_VarList.R")
# source("/scratch/users/k21170717/polygenic_IQ_score/0PGIQ_VarList.R")




# =============================================================================
### ✅ 1. PGIQ from SMTpred & the new PCs if HG38 ver was used####
# =============================================================================
# see CREATE for details, weighting: PGIQ Score = 0.0838 × EA4_PGS + 0.0450 × IQ3_PGS
SMTpred <- read.table(PGIQ_path, header = TRUE, sep = "", stringsAsFactors = FALSE)

names(SMTpred)[2] <- "id_twin"
names(SMTpred)[1] <- "id_fam"
names(SMTpred)[4] <- "PGIQ" # this is the combined EA4 + IQ3 score targeted for IQ3; the 3rd column is the combined score targeted for EA4
SMTpred[3] <- NULL

SMTpred$id_fam <- as.numeric(SMTpred$id_fam) # don't panic when seeing "Warning message: NAs introduced by coercion" b/c I think there are 5 individuals w/o IDs were included 
SMTpred$id_twin <- as.numeric(SMTpred$id_twin) # same as above 

SMTpred_merged <- left_join(dat, SMTpred)

SMTpred_merged_UN <- SMTpred_merged %>% filter(selectunpaired == 1)

selected_vars <- SMTpred_merged_UN[, c("PGIQ", "YL_cog_final_pred_auto", "EA4_no23andme_Okbay2022")]

cor_matrix <- cor(selected_vars, use = "complete.obs")
print(round(cor_matrix, 2))

# plot
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "original", tl.cex = 0.8, tl.col = "black",
         addCoef.col = "black", number.cex = 0.7)

cor.test(SMTpred_merged_UN$YL_cog_final_pred_auto, SMTpred_merged_UN$PGIQ) # 0.8091935
cor.test(SMTpred_merged_UN$EA4_no23andme_Okbay2022, SMTpred_merged_UN$PGIQ) # 0.8657874



# add the new PCs calculated from the merged twin + parent dataset when using PGIQ_290825.score ####
pca_projected <- read.table(pca_file_path, header = TRUE, comment.char = "")

colnames(pca_projected)[1] <- "id_fam"
colnames(pca_projected)[2] <- "id_twin"
projected_scores <- pca_projected[, c("id_fam", "id_twin", paste0("SCORE", 1:20, "_AVG"))]

projected_scores <- projected_scores %>%
  rename_with(~ paste0("PC", seq_along(.x)), .cols = starts_with("SCORE"))

pcs_twins <- projected_scores %>%
  filter(str_ends(id_twin, "[12]"))

pcs_mothers <- projected_scores %>%
  filter(str_ends(id_twin, "3")) %>%
  rename_with(~ paste0(.x, "_fp"), .cols = starts_with("PC")) %>%
  dplyr::select(-id_twin) # We only need the Family ID for joining

pcs_fathers <- projected_scores %>%
  filter(str_ends(id_twin, "4")) %>%
  rename_with(~ paste0(.x, "_mp"), .cols = starts_with("PC")) %>%
  dplyr::select(-id_twin) # We only need the Family ID for joining

updated_PCA <- pcs_twins %>%
  left_join(pcs_mothers, by = "id_fam") %>%
  left_join(pcs_fathers, by = "id_fam")

updated_PCA$id_fam <- as.numeric(updated_PCA$id_fam)
updated_PCA$id_twin <- as.numeric(updated_PCA$id_twin)

glimpse(updated_PCA)
dim(updated_PCA) # 10325 62
# we actually don't need the parental PCs, but we just keep them here as well 

SMTpred_merged_oldPCrm <- SMTpred_merged %>% select(-c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"))
# nrow = 1694

SMTpred_merged_PCupdated <- SMTpred_merged_oldPCrm %>%
  left_join(updated_PCA, by = c("id_fam", "id_twin"))
# nrow = 1754 (=1694+60, 20 PCs for each member in a trio)

# don't run this, too many variables here 
# glimpse(SMTpred_merged_PCupdated) 
dim(SMTpred_merged_PCupdated) # 27856 1754





# =============================================================================
### ✅ 2. append the MZ co-twins PGS & apply default exclusions ####
# =============================================================================
# some of the MZ co-twins may have a more extreme phenotypic score; by genotyping one MZ twin per pair, we may lose the information
# some of the twins don't have zygos (i.e., zygos = NA), so we need to filter out those twins; b/c only MZ twins are used here
SMTpred_merged_PCupdated_imputed <- SMTpred_merged_PCupdated %>%
  group_by(id_fam) %>%
  mutate(
    across(
      c(PGIQ, chiptype, PC1:PC20),
      ~ if (all(zygos == 1, na.rm = TRUE) && any(!is.na(.x))) {
        first(.x[!is.na(.x)])
      } else {
        .x
      }
    )
  ) %>%
  ungroup()

SMTpred_merged_PCupdated_imputed <- SMTpred_merged_PCupdated_imputed[ SMTpred_merged_PCupdated_imputed$exclude1 == 0, ] #1=Y, 0=N
dim(SMTpred_merged_PCupdated_imputed) # 26040  1754





# =============================================================================
### ✅ 3. scale all outcomes, PGS, and cont. covariates ####
# =============================================================================
SMTpred_merged_PCupdated_imputed_prep4scale <- SMTpred_merged_PCupdated_imputed

# create a mean=100 SD=15 scale for PGIQ ####
SMTpred_merged_PCupdated_imputed_prep4scale$PGIQ_IQscale <- scale(SMTpred_merged_PCupdated_imputed_prep4scale$PGIQ, center = TRUE, scale = TRUE) * 15 + 100

all_Varlist <- c(
  G_Composites_Varlist,
  Verbal_Tests_Varlist,
  Nonverbal_Tests_Varlist,
  Edu_Achieve_Attain_Varlist,
  Anxiety_Varlist,
  Conners_Varlist,
  SDQ_Varlist,
  Anthro_Varlist,
  Wellbeing_Varlist
)

target_vars <- unique(sapply(all_Varlist, function(item) {
  paste0(item[1], "1")
}))

pheno_to_scale <- target_vars[target_vars %in% names(SMTpred_merged_PCupdated_imputed_prep4scale)]
length(pheno_to_scale) # 338
pheno_missing <- target_vars[!(target_vars %in% names(SMTpred_merged_PCupdated_imputed_prep4scale))]
length(pheno_missing) # 0

id_vars <- c("id_twin", "id_fam")
vars_to_keep <- c("twin", "random", "sex1", "sex2", "zygos", "sexzyg", "x3zygos", "selectunpaired", "chiptype")
PCs <- c(paste0("PC", 1:10))
ages <- c("atwinage", "bpbage", "cpbage", "dpbage", "gpbage", "hage", "icpage", "itage1", 
          "jcstage1", "jtqage1", "lcqage1", "ltqage1", "ncqage1", "pcbhage1", "pcwebage1", 
          "rcqage1", "rcqalage1", "u1cage1", "ucgage1", "zcage1", "zmhage1")
age_missing <- ages[!(ages %in% names(SMTpred_merged_PCupdated_imputed_prep4scale))]
length(age_missing) # 0

ses_vars <- c("ases", "gses", "pses", "u1pses")

dat_raw <- SMTpred_merged_PCupdated_imputed_prep4scale[, c(id_vars, vars_to_keep, pheno_to_scale, "PGIQ", "PGIQ_IQscale", PCs, ages, ses_vars), drop = FALSE]
dim(dat_raw) # 26040  386

dat_scaled_for_CFA <- dat_raw 
dat_scaled_for_CFA[, c(pheno_to_scale, "PGIQ", PCs, ages, ses_vars)] <- scale(dat_scaled_for_CFA[, c(pheno_to_scale, "PGIQ", PCs, ages, ses_vars)])
dim(dat_scaled_for_CFA) # 26040  386

write.csv(dat_scaled_for_CFA, file = paste0(sourceFileStem, "dat_scaled_for_CFA.csv"), row.names = FALSE)





# =============================================================================
### ✅ 4. extract common factors from cognitive and mental health outcomes ####
# =============================================================================
source("./PGIQ Codes/1PGIQ_CFA.R")
# source("/scratch/users/k21170717/polygenic_IQ_score/1PGIQ_CFA.R")

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

CF_score_vars <- sapply(CommonFactor_Score_Varlist, `[`, 1)
CF_score_vars_with_suffix <- paste0(CF_score_vars, "1")

scaled_vars_to_add <- dat_scaled_for_CFA %>%
  select(id_fam, id_twin, all_of(CF_score_vars_with_suffix))
# Merge back to dat_raw
dat_raw <- dat_raw %>%
  left_join(scaled_vars_to_add, by = c("id_fam", "id_twin"))

dat_scaled <- dat_scaled_for_CFA %>%
  mutate(across(all_of(CF_score_vars_with_suffix), ~as.numeric(scale(.))))

dim(dat_raw) # 26040  423
dim(dat_scaled) # 26040  423

# check
psych::describe(dat_raw[, CF_score_vars_with_suffix])
psych::describe(dat_scaled[, CF_score_vars_with_suffix])

dat_raw$PGgS <- dat_raw$PGIQ
dat_scaled$PGgS <- dat_scaled$PGIQ





# =============================================================================
### ✅ 5. export and save final dataset ####
# =============================================================================
dat_scaled_selectunpaired <- subset(dat_scaled, selectunpaired==1)
dim(dat_scaled_selectunpaired) # 6973  423

dat_scaled_F <- subset(dat_scaled, sex1==0)
dim(dat_scaled_F) # 13096  423

dat_scaled_M <- subset(dat_scaled, sex1==1)
dim(dat_scaled_M) # 12944  423

dat_scaled_selectunpaired_F <- subset(dat_scaled, selectunpaired==1 & sex1==0)
dim(dat_scaled_selectunpaired_F) # 3628  423

dat_scaled_selectunpaired_M <- subset(dat_scaled, selectunpaired==1 & sex1==1)
dim(dat_scaled_selectunpaired_M) # 3345  423

write.csv(dat_raw, file = paste0(sourceFileStem, "PGIQ_raw.csv"), row.names = FALSE)
write.csv(dat_scaled, file = paste0(sourceFileStem, "PGIQ_scaled.csv"), row.names = FALSE)
write.csv(dat_scaled_selectunpaired, file = paste0(sourceFileStem, "PGIQ_scaled_selectunpaired.csv"), row.names = FALSE)
write.csv(dat_scaled_F, file = paste0(sourceFileStem, "PGIQ_scaled_F.csv"), row.names = FALSE)
write.csv(dat_scaled_M, file = paste0(sourceFileStem, "PGIQ_scaled_M.csv"), row.names = FALSE)
write.csv(dat_scaled_selectunpaired_F, file = paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_F.csv"), row.names = FALSE)
write.csv(dat_scaled_selectunpaired_M, file = paste0(sourceFileStem, "PGIQ_scaled_selectunpaired_M.csv"), row.names = FALSE)
