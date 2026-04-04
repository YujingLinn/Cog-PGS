CommonFactor_Score_Varlist <- list(
  
  # ==========================================
  # --- 1. Cognitive Composites ----
  # ==========================================
  # General Cognitive Ability (g)
  c("g_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_method_Early_MCDI_PARCA", "by Method MCDI/PARCA", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_method_Middle_WISC_Raven", "by Method WISC/Raven", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_method_Later_MillHill_Raven", "by Method MillHill/Raven", "CrossAgeDummy", "Cross Rater", "g"),
  c("g_method_Latent", "by Method Latent", "CrossAgeDummy", "Cross Rater", "g"),
  
  # Verbal Ability
  c("vb_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_method_Early_MCDI", "by Method MCDI", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_method_Middle_WISC", "by Method WISC", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_method_Later_MillHill", "by Method MillHill", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  c("vb_method_Latent", "by Method Latent", "CrossAgeDummy", "Cross Rater", "verbal ability"),
  
  # Nonverbal Ability
  c("nv_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  c("nv_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  c("nv_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  c("nv_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  c("nv_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  c("nv_method_Early_PARCA", "by Method PARCA", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  c("nv_method_MiddleLate_Raven", "by Method Raven", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  c("nv_method_Latent", "by Method Latent", "CrossAgeDummy", "Cross Rater", "nonverbal ability"),
  
  # ==========================================
  # --- 2. Anxiety Traits (ARBQ) ----
  # ==========================================
  # ARBQ Shyness
  c("ARBQ_shy_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Shyness"),
  c("ARBQ_shy_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "ARBQ Shyness"),
  c("ARBQ_shy_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "ARBQ Shyness"),
  c("ARBQ_shy_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "ARBQ Shyness"),
  
  # ARBQ Fear
  c("ARBQ_fear_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Fear"),
  c("ARBQ_fear_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "ARBQ Fear"),
  c("ARBQ_fear_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "ARBQ Fear"),
  c("ARBQ_fear_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "ARBQ Fear"),
  
  # ARBQ OCB
  c("ARBQ_ocb_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Obsessive-Compulsive"),
  c("ARBQ_ocb_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "ARBQ Obsessive-Compulsive"),
  c("ARBQ_ocb_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "ARBQ Obsessive-Compulsive"),
  
  # ARBQ Negative Affect
  c("ARBQ_naff_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Affect"),
  c("ARBQ_naff_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Affect"),
  c("ARBQ_naff_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Affect"),
  
  # ARBQ Negative Cognition
  c("ARBQ_ncog_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Cognition"),
  c("ARBQ_ncog_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Cognition"),
  c("ARBQ_ncog_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "ARBQ Negative Cognition"),
  
  # ARBQ Total
  c("Anxiety_total_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ARBQ Anxiety Total"),
  c("Anxiety_total_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "ARBQ Anxiety Total"),
  c("Anxiety_total_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "ARBQ Anxiety Total"),
  c("Anxiety_total_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "ARBQ Anxiety Total"),
  
  # ==========================================
  # --- 3. Conners Traits (ADHD) ----
  # ==========================================
  # Conners Inattention
  c("Conners_inatt_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Inattention"),
  c("Conners_inatt_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "Inattention"),
  c("Conners_inatt_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "Inattention"),
  c("Conners_inatt_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "Inattention"),
  
  c("Conners_inatt_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Inattention"),
  c("Conners_inatt_P_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Parent", "Inattention"),
  c("Conners_inatt_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "Inattention"),
  
  # Conners Hyperactivity
  c("Conners_hyper_impul_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Hyper-Impuls"),
  c("Conners_hyper_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "Hyper-Impuls"),
  c("Conners_hyper_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "Hyper-Impuls"),
  c("Conners_hyper_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "Hyper-Impuls"),
  
  c("Conners_hyper_impul_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Hyper-Impuls"),
  c("Conners_hyper_P_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Parent", "Hyper-Impuls"),
  c("Conners_hyper_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "Hyper-Impuls"),
  
  # Conners Total
  c("Conners_Total_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "ADHD Total"),
  c("Conners_Total_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "ADHD Total"),
  c("Conners_Total_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "ADHD Total"),
  c("Conners_Total_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "ADHD Total"),
  
  c("Conners_Total_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "ADHD Total"),
  c("Conners_Total_P_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Parent", "ADHD Total"),
  c("Conners_Total_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "ADHD Total"),
  
  # ==========================================
  # --- 4. SDQ Traits ----
  # ==========================================
  # SDQ Conduct
  c("SDQ_Conduct_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Conduct"),
  c("SDQ_Conduct_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "Conduct"),
  c("SDQ_Conduct_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "Conduct"),
  c("SDQ_Conduct_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "Conduct"),
  c("SDQ_Conduct_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "Conduct"),
  c("SDQ_Conduct_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "Conduct"),
  
  c("SDQ_Conduct_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Conduct"),
  c("SDQ_Conduct_P_stage_Early", "by Stage Early", "CrossAgeDummy", "Parent", "Conduct"),
  c("SDQ_Conduct_P_stage_Child", "by Stage Child", "CrossAgeDummy", "Parent", "Conduct"),
  c("SDQ_Conduct_P_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Parent", "Conduct"),
  c("SDQ_Conduct_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "Conduct"),
  
  c("SDQ_Conduct_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "Conduct"),
  
  c("SDQ_Conduct_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "Conduct"),
  
  # SDQ Emotion
  c("SDQ_Emotion_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Emotion"),
  c("SDQ_Emotion_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "Emotion"),
  c("SDQ_Emotion_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "Emotion"),
  c("SDQ_Emotion_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "Emotion"),
  c("SDQ_Emotion_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "Emotion"),
  c("SDQ_Emotion_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "Emotion"),
  
  c("SDQ_Emotion_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Emotion"),
  c("SDQ_Emotion_P_stage_Early", "by Stage Early", "CrossAgeDummy", "Parent", "Emotion"),
  c("SDQ_Emotion_P_stage_Child", "by Stage Child", "CrossAgeDummy", "Parent", "Emotion"),
  c("SDQ_Emotion_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "Emotion"),
  
  c("SDQ_Emotion_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "Emotion"),
  
  c("SDQ_Emotion_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "Emotion"),
  
  # SDQ Hyperactivity
  c("SDQ_Hyperactivity_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Hyperactivity"),
  c("SDQ_Hyperactivity_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "Hyperactivity"),
  c("SDQ_Hyperactivity_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "Hyperactivity"),
  c("SDQ_Hyperactivity_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "Hyperactivity"),
  c("SDQ_Hyperactivity_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "Hyperactivity"),
  c("SDQ_Hyperactivity_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "Hyperactivity"),
  
  c("SDQ_Hyperactivity_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Hyperactivity"),
  c("SDQ_Hyperactivity_P_stage_Early", "by Stage Early", "CrossAgeDummy", "Parent", "Hyperactivity"),
  c("SDQ_Hyperactivity_P_stage_Child", "by Stage Child", "CrossAgeDummy", "Parent", "Hyperactivity"),
  c("SDQ_Hyperactivity_P_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Parent", "Hyperactivity"),
  c("SDQ_Hyperactivity_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "Hyperactivity"),
  
  c("SDQ_Hyperactivity_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "Hyperactivity"),
  
  c("SDQ_Hyperactivity_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "Hyperactivity"),
  
  # SDQ Peer Problems
  c("SDQ_Peer_Problems_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Peer Problems"),
  c("SDQ_Peer_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "Peer Problems"),
  c("SDQ_Peer_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "Peer Problems"),
  c("SDQ_Peer_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "Peer Problems"),
  c("SDQ_Peer_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "Peer Problems"),
  c("SDQ_Peer_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "Peer Problems"),
  
  c("SDQ_Peer_Problems_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Peer Problems"),
  c("SDQ_Peer_P_stage_Early", "by Stage Early", "CrossAgeDummy", "Parent", "Peer Problems"),
  c("SDQ_Peer_P_stage_Child", "by Stage Child", "CrossAgeDummy", "Parent", "Peer Problems"),
  c("SDQ_Peer_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "Peer Problems"),
  
  c("SDQ_Peer_Problems_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "Peer Problems"),
  
  c("SDQ_Peer_Problems_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "Peer Problems"),
  
  # SDQ Prosocial
  c("SDQ_Prosocial_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "Prosocial"),
  c("SDQ_Prosocial_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "Prosocial"),
  c("SDQ_Prosocial_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "Prosocial"),
  c("SDQ_Prosocial_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "Prosocial"),
  c("SDQ_Prosocial_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "Prosocial"),
  c("SDQ_Prosocial_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "Prosocial"),
  
  c("SDQ_Prosocial_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "Prosocial"),
  c("SDQ_Prosocial_P_stage_Early", "by Stage Early", "CrossAgeDummy", "Parent", "Prosocial"),
  c("SDQ_Prosocial_P_stage_Child", "by Stage Child", "CrossAgeDummy", "Parent", "Prosocial"),
  c("SDQ_Prosocial_P_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Parent", "Prosocial"),
  c("SDQ_Prosocial_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "Prosocial"),
  
  c("SDQ_Prosocial_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "Prosocial"),
  
  c("SDQ_Prosocial_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "Prosocial"),
  
  # SDQ Total Problems
  c("SDQ_Total_Problems_CTCR", "Cross Time", "CrossAgeDummy", "Cross Rater", "SDQ Total Problems"),
  c("SDQ_Total_stage_Early", "by Stage Early", "CrossAgeDummy", "Cross Rater", "SDQ Total Problems"),
  c("SDQ_Total_stage_Child", "by Stage Child", "CrossAgeDummy", "Cross Rater", "SDQ Total Problems"),
  c("SDQ_Total_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Cross Rater", "SDQ Total Problems"),
  c("SDQ_Total_stage_Adult", "by Stage Adult", "CrossAgeDummy", "Cross Rater", "SDQ Total Problems"),
  c("SDQ_Total_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Cross Rater", "SDQ Total Problems"),
  
  c("SDQ_Total_Problems_CTWR_P", "Cross Time", "CrossAgeDummy", "Parent", "SDQ Total Problems"),
  c("SDQ_Total_P_stage_Early", "by Stage Early", "CrossAgeDummy", "Parent", "SDQ Total Problems"),
  c("SDQ_Total_P_stage_Child", "by Stage Child", "CrossAgeDummy", "Parent", "SDQ Total Problems"),
  c("SDQ_Total_P_stage_Adol", "by Stage Adol", "CrossAgeDummy", "Parent", "SDQ Total Problems"),
  c("SDQ_Total_P_stage_Latent", "by Stage Latent", "CrossAgeDummy", "Parent", "SDQ Total Problems"),
  
  c("SDQ_Total_Problems_CTWR_T", "Cross Time", "CrossAgeDummy", "Teacher", "SDQ Total Problems"),
  
  c("SDQ_Total_Problems_CTWR_C", "Cross Time", "CrossAgeDummy", "Child", "SDQ Total Problems"),
  
  # ==========================================
  # --- 5. Educational Achievement Latent Factors (Ages 7-12) ----
  # ==========================================
  c("Eng_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "English Achievement"),
  c("Mat_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "Maths Achievement"),
  c("Sci_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "Science Achievement"),
  c("Core_Achieve_Latent", "Cross Time", "CrossAgeDummy", "Teacher", "Core-Subject Achievement")
)

cat("CommonFactor_Score_Varlist length:", length(CommonFactor_Score_Varlist), "\n")

cat("CommonFactor_Score_Varlist Loaded \n\n")
