# VarList
# Yujing Lin
# 29 August, 2025

# g, verbal g, and nonverbal g ####
G_Composites_Varlist <- list(
  # -- g --
  c("brawg", "2 yr", "bpbage", "Child", "g"),
  c("crawg", "3 yr", "cpbage", "Child", "g"),
  c("drawg", "4 yr", "dpbage", "Child", "g"),
  c("gcg", "7 yr", "gpbage", "Child", "g"),
  c("icg", "9 yr", "icpage", "Child", "g"),
  c("jcg", "10 yr", "jcstage1", "Child", "g"),
  c("lcg", "12 yr", "lcqage1", "Child", "g"),
  c("ncg", "14 yr", "ncqage1", "Child", "g"),
  c("pcg", "16 yr", "pcwebage1", "Child", "g"),
  c("ucgt", "25 yr", "ucgage1", "Child", "g"),
  
  # -- verbal ability --
  c("bscv", "2 yr", "bpbage", "Child", "verbal ability"),
  c("cscv", "3 yr", "cpbage", "Child", "verbal ability"),
  c("dscv", "4 yr", "dpbage", "Child", "verbal ability"),
  c("gcl", "7 yr", "gpbage", "Child", "verbal ability"),
  c("icvb", "9 yr", "icpage", "Child", "verbal ability"),
  c("jcvb", "10 yr", "jcstage1", "Child", "verbal ability"),
  c("lverbal12T", "12 yr", "lcqage1", "Child", "verbal ability"),
  c("pcvctota", "16 yr", "pcwebage1", "Child", "verbal ability"),
  c("ucgvbt", "25 yr", "ucgage1", "Child", "verbal ability"),
  
  # -- nonverbal ability --
  c("bscnv", "2 yr", "bpbage", "Child", "nonverbal ability"),
  c("cscnv", "3 yr", "cpbage", "Child", "nonverbal ability"),
  c("dscnv", "4 yr", "dpbage", "Child", "nonverbal ability"),
  c("gcn", "7 yr", "gpbage", "Child", "nonverbal ability"),
  c("icnv", "9 yr", "icpage", "Child", "nonverbal ability"),
  c("jcnv", "10 yr", "jcstage1", "Child", "nonverbal ability"),
  c("lnonverbal12T", "12 yr", "lcqage1", "Child", "nonverbal ability"),
  c("pcrvtota", "16 yr", "pcwebage1", "Child", "nonverbal ability"),
  c("ucgnvt", "25 yr", "ucgage1", "Child", "nonverbal ability")
)

# verbal tests ####
Verbal_Tests_Varlist <- list(
  # -- Early Childhood (2-4 years) --
  c("bvocab", "MB-CDI Vocabulary (2 yr)", "bpbage", "Child", "Early Verbal Abilities"),
  c("buse", "Word Use (2 yr)", "bpbage", "Child", "Early Verbal Abilities"),
  c("bgramma", "Sentence Complexity  (2 yr)", "bpbage", "Child", "Early Verbal Abilities"),
  
  c("ctvoc", "MB-CDI Vocabulary (3 yr)", "cpbage", "Child", "Early Verbal Abilities"),
  c("cuse", "Word Use (3 yr)", "cpbage", "Child", "Early Verbal Abilities"),
  c("cgramma", "Sentence Complexity  (3 yr)", "cpbage", "Child", "Early Verbal Abilities"),
  
  c("dtvoc", "MB-CDI Vocabulary (4 yr)", "dpbage", "Child", "Early Verbal Abilities"),
  c("dpictot", "Picture Vocabulary (4 yr)", "dpbage", "Child", "Early Verbal Abilities"),
  c("dgramma", "Sentence Complexity (4 yr)", "dpbage", "Child", "Early Verbal Abilities"),
  
  # -- Childhood (7-12 years) --
  c("gvocabt", "Vocabulary (7 yr)", "gpbage", "Child", "Childhood Verbal Abilities"),
  c("gsimilt", "Similarities (7 yr)", "gpbage", "Child", "Childhood Verbal Abilities"),
  c("gtowt", "TOWRE Reading (7 yr)", "gpbage", "Child", "Childhood Verbal Abilities"),
  c("gnwrt", "Non-Word Recognition (7 yr)", "gpbage", "Child", "Childhood Verbal Abilities"),
  
  c("icwrdt", "Words/Vocabulary (9 yr)", "icpage", "Child", "Childhood Verbal Abilities"),
  c("icgent", "General Knowledge (9 yr)", "icpage", "Child", "Childhood Verbal Abilities"),
  
  c("jvocta", "Vocabulary (10 yr)", "jcstage1", "Child", "Childhood Verbal Abilities"),
  c("jpiatta", "PIAT Reading (10 yr)", "jcstage1", "Child", "Childhood Verbal Abilities"),
  c("jartt", "Author Recognition (10 yr)", "jcstage1", "Child", "Childhood Verbal Abilities"),
  c("jgenta", "General Knowledge (10 yr)", "jcstage1", "Child", "Childhood Verbal Abilities"),
  
  c("lvctota", "Vocabulary (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  c("ltotota", "TOAL Language (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  c("lpitota", "PIAT Reading (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  c("lyntot", "Yes/No Reading Fluency (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  c("ltowt", "TOWRE Reading (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  c("lartot", "Author Recognition (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  c("lfltot", "Figurative Language (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  c("lgktota", "General Knowledge (12 yr)", "lcqage1", "Child", "Childhood Verbal Abilities"),
  
  # -- Adolescence (14-16 years) --
  c("nvctota", "Vocabulary (14 yr)", "ncqage1", "Child", "Adolescence Verbal Abilities"),
  c("pcvctota", "Vocabulary (16 yr)", "pcwebage1", "Child", "Adolescence Verbal Abilities"),
  
  # -- Early Adulthood (18-25 years) --
  c("ucgvoctot", "Vocabulary (25 yr)", "ucgage1", "Child", "Early Adulthood Verbal Abilities"),
  c("ucgmistot", "Missing Letter (25 yr)", "ucgage1", "Child", "Early Adulthood Verbal Abilities"),
  c("ucgvertot", "Verbal Analogies (25 yr)", "ucgage1", "Child", "Early Adulthood Verbal Abilities"))

# nonverbal tests ####
Nonverbal_Tests_Varlist <- list(
  # -- Early Childhood (2-4 years) --
  c("bparca", "Total PARCA (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  c("breparc", "Parent-Report PARCA (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  c("badparn", "Parent-Admin PARCA (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  c("bdrawt", "Drawing Test (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  c("bmatcht", "Matching Test (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  c("bblockt", "Block Test (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  c("bfoldt", "Folding Test (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  c("bcopyt", "Copy Test (2 yr)", "bpbage", "Child", "Early Nonverbal Abilities"),
  
  c("cparca", "Total PARCA (3 yr)", "cpbage", "Child", "Early Nonverbal Abilities"),
  c("creparc", "Parent-Report PARCA (3 yr)", "cpbage", "Child", "Early Nonverbal Abilities"),
  c("cadparc", "Parent-Admin PARCA (3 yr)", "cpbage", "Child", "Early Nonverbal Abilities"),
  c("coddt", "Odd One Out (3 yr)", "cpbage", "Child", "Early Nonverbal Abilities"),
  c("cdrawt", "Drawing Test (3 yr)", "cpbage", "Child", "Early Nonverbal Abilities"),
  c("cmatcht", "Matching Test (3 yr)", "cpbage", "Child", "Early Nonverbal Abilities"),
  
  c("dparca", "Total PARCA (4 yr)", "dpbage", "Child", "Early Nonverbal Abilities"),
  c("dreparc", "Parent-Report PARCA (4 yr)", "dpbage", "Child", "Early Nonverbal Abilities"),
  c("dadparc", "Parent-Admin PARCA (4 yr)", "dpbage", "Child", "Early Nonverbal Abilities"),
  c("doddt", "Odd One Out (4 yr)", "dpbage", "Child", "Early Nonverbal Abilities"),
  c("ddrawt", "Drawing Test (4 yr)", "dpbage", "Child", "Early Nonverbal Abilities"),
  c("dmant", "Draw-a-Man Test (4 yr)", "dpbage", "Child", "Early Nonverbal Abilities"),
  c("dpuzt", "Puzzle Test (4 yr)", "dpbage", "Child", "Early Nonverbal Abilities"),
  
  # -- Childhood (7-12 years) --
  c("gcongrt", "Conceptual Grouping (7 yr)", "gpbage", "Child", "Childhood Nonverbal Abilities"),
  c("gpicomt", "Picture Completion (7 yr)", "gpbage", "Child", "Childhood Nonverbal Abilities"),
  
  c("icshpt", "Shapes Test (9 yr)", "icpage", "Child", "Childhood Nonverbal Abilities"),
  c("icpuzt", "Puzzle Test (9 yr)", "icpage", "Child", "Childhood Nonverbal Abilities"),
  
  c("jravta", "Ravens (10 yr)", "jcstage1", "Child", "Childhood Nonverbal Abilities"),
  c("jpict", "Picture Completion (10 yr)", "jcstage1", "Child", "Childhood Nonverbal Abilities"),
  c("jmatta", "NFER Maths (10 yr)", "jcstage1", "Child", "Childhood Nonverbal Abilities"),
  
  c("lrvtota", "Ravens (12 yr)", "lcqage1", "Child", "Childhood Nonverbal Abilities"),
  c("lhstota", "Hidden Shapes (12 yr)", "lcqage1", "Child", "Childhood Nonverbal Abilities"),
  c("ljgtota", "Jigsaws (12 yr)", "lcqage1", "Child", "Childhood Nonverbal Abilities"),
  c("lmatota", "NFER Maths (12 yr)", "lcqage1", "Child", "Childhood Nonverbal Abilities"),
  # the NFER maths above was comprised of the 3 subtests below
  c("lma1tot", "Maths: Understanding Numbers (12 yr)", "lcqage1", "Child", "Childhood Nonverbal Abilities"),
  c("lma2tot", "Maths: Non-Numerical Processes (12 yr)", "lcqage1", "Child", "Childhood Nonverbal Abilities"),
  c("lma3tot", "Maths: Computation & Knowledge (12 yr)", "lcqage1", "Child", "Childhood Nonverbal Abilities"),
  
  # -- Adolescence (14-16 years) --
  c("nrvtota", "Ravens (14 yr)", "ncqage1", "Child", "Adolescence Nonverbal Abilities"),
  c("nsctota", "Science (14 yr)", "ncqage1", "Child", "Adolescence Nonverbal Abilities"),
  
  c("pcuntota", "Understanding Number (16 yr)", "pcwebage1", "Child", "Adolescence Nonverbal Abilities"),
  
  # -- Late Adolescence (18 years) --
  c("rcb2rtot", "Bricks 2D Rotation (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("rcb2rvtot", "Bricks 2D Rotation & Visualisation (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("rcb2vtot", "Bricks 2D Visualisation (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("rcb3rtot", "Bricks 3D Rotation (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("rcb3rvtot", "Bricks 3D Rotation & Visualisation (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("rcb3vtot", "Bricks 3D Visualisation (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("rcbt", "Bricks (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("rcnts", "Navigation (18 yr)", "rcqage1", "Child", "Early Adulthood Nonverbal Abilities"),
  
  # -- Early Adulthood (25 years) --
  c("ucgravtot", "Ravens (25 yr)", "ucgage1", "Child", "Early Adulthood Nonverbal Abilities"),
  c("ucgisttot", "Visual Puzzles (25 yr)", "ucgage1", "Child", "Early Adulthood Nonverbal Abilities")) 

# Educational outcomes ####
Edu_Achieve_Attain_Varlist <- list(
  # -- English --
  c("gteng", "7 yr", "gpbage", "Teacher", "English Achievement"),
  c("iteng", "9 yr", "itage1", "Teacher", "English Achievement"),
  c("jteng", "10 yr", "jtqage1", "Teacher", "English Achievement"),
  c("lteng", "12 yr", "ltqage1", "Teacher", "English Achievement"),
  c("pcexgcseenggrdm", "16 yr", "pcwebage1", "Child", "English Achievement"),
  c("rcqalsenggrdm", "18 yr", "rcqalage1", "Child", "English Achievement"),
  
  # -- Maths --
  c("gtmat", "7 yr", "gpbage", "Teacher", "Maths Achievement"),
  c("itmat", "9 yr", "itage1", "Teacher", "Maths Achievement"),
  c("jtmat", "10 yr", "jtqage1", "Teacher", "Maths Achievement"),
  c("ltmat", "12 yr", "ltqage1", "Teacher", "Maths Achievement"),
  c("pcexgcsematgrdm", "16 yr", "pcwebage1", "Child", "Maths Achievement"),
  c("rcqalsmatgrdm", "18 yr", "rcqalage1", "Child", "Maths Achievement"),
  
  # -- Science --
  c("itsci", "9 yr", "itage1", "Teacher", "Science Achievement"),
  c("jtsci", "10 yr", "jtqage1", "Teacher", "Science Achievement"),
  c("ltsci", "12 yr", "ltqage1", "Teacher", "Science Achievement"),
  c("pcexgcsescigrdm", "16 yr", "pcwebage1", "Child", "Science Achievement"),
  c("rcqalsscigrdm", "18 yr", "rcqalage1", "Child", "Science Achievement"),
  
  # -- Core-Subject Composite --
  c("gt2ac", "7 yr", "gpbage", "Teacher", "Core-Subject Achievement"),
  c("it3ac", "9 yr", "itage1", "Teacher", "Core-Subject Achievement"),
  c("jt3ac", "10 yr", "jtqage1", "Teacher", "Core-Subject Achievement"),
  c("lt3ac", "12 yr", "ltqage1", "Teacher", "Core-Subject Achievement"),
  c("npks3t3a", "14 yr", "ncqage1", "Parent", "Core-Subject Achievement"), 
  c("pcexgcsecoregrdm", "16 yr", "pcwebage1", "Child", "Core-Subject Achievement"),
  c("rcqalsgrdm", "18 yr", "rcqalage1", "Child", "Core-Subject Achievement"),
  c("u1cedat", "21 yr", "u1cage1", "Child", "Core-Subject Achievement"),
  
  # -- Attainment --
  c("zEA", "Years of Schooling (26 yr)", "u1cage1", "Child", "Educational Attainment")
)

# ARBQ / anxiety list ####
Anxiety_Varlist <- list(
  # -- ARBQ (Anxiety Related Behaviours Questionnaire) --
  
  # ARBQ Subscale: Shyness / Social Anxiety
  c("canxshyt", "3 yr", "cpbage", "Parent", "ARBQ Shyness"),
  c("danxshyt", "4 yr", "dpbage", "Parent", "ARBQ Shyness"),
  c("gpanxshyt", "7 yr", "gpbage", "Parent", "ARBQ Shyness"),
  c("ipanxshyt", "9 yr", "icpage", "Parent", "ARBQ Shyness"),
  c("ppbhanxshyt", "16 yr", "pcbhage1", "Parent", "ARBQ Shyness"),
  c("gtanxshyt", "7 yr", "gpbage", "Teacher", "ARBQ Shyness"),
  
  # ARBQ Subscale: Fear
  c("canxfeart", "3 yr", "cpbage", "Parent", "ARBQ Fear"),
  c("danxfeart", "4 yr", "dpbage", "Parent", "ARBQ Fear"),
  c("gpanxfeart", "7 yr", "gpbage", "Parent", "ARBQ Fear"),
  c("ipanxfeart", "9 yr", "icpage", "Parent", "ARBQ Fear"),
  c("ppbhanxfeart", "16 yr", "pcbhage1", "Parent", "ARBQ Fear"),
  c("gtanxfeart", "7 yr", "gpbage", "Teacher", "ARBQ Fear"),
  
  # ARBQ Subscale: Obsessive-Compulsive Behaviour (OCB)
  c("danxocbt", "4 yr", "dpbage", "Parent", "ARBQ OCB"),
  c("gpanxocbt", "7 yr", "gpbage", "Parent", "ARBQ OCB"),
  c("ipanxocbt", "9 yr", "icpage", "Parent", "ARBQ OCB"),
  c("ppbhanxocbt", "16 yr", "pcbhage1", "Parent", "ARBQ OCB"),
  c("gtanxocbt", "7 yr", "gpbage", "Teacher", "ARBQ OCB"),
  
  # ARBQ Subscale: Negative Affect
  c("danxnafft", "4 yr", "dpbage", "Parent", "ARBQ Negative Affect"),
  c("gpanxnafft", "7 yr", "gpbage", "Parent", "ARBQ Negative Affect"),
  c("ipanxnafft", "9 yr", "icpage", "Parent", "ARBQ Negative Affect"),
  c("ppbhanxnafft", "16 yr", "pcbhage1", "Parent", "ARBQ Negative Affect"),
  c("gtanxnafft", "7 yr", "gpbage", "Teacher", "ARBQ Negative Affect"),
  
  # ARBQ Subscale: Negative Cognition
  c("danxncogt", "4 yr", "dpbage", "Parent", "ARBQ Negative Cognition"),
  c("gpanxncogt", "7 yr", "gpbage", "Parent", "ARBQ Negative Cognition"),
  c("ipanxncogt", "9 yr", "icpage", "Parent", "ARBQ Negative Cognition"),
  c("ppbhanxncogt", "16 yr", "pcbhage1", "Parent", "ARBQ Negative Cognition"),
  c("gtanxncogt", "7 yr", "gpbage", "Teacher", "ARBQ Negative Cognition"),
  
  # -- Total Anxiety Symptoms --
  c("canxt", "3 yr", "cpbage", "Parent", "ARBQ Total Anxiety"),
  c("danxt", "4 yr", "dpbage", "Parent", "ARBQ Total Anxiety"),
  c("gpanxt", "7 yr", "gpbage", "Parent", "ARBQ Total Anxiety"),
  c("ipanxt", "9 yr", "icpage", "Parent", "ARBQ Total Anxiety"),
  c("ppbhanxt", "16 yr", "pcbhage1", "Parent", "ARBQ Total Anxiety"),
  c("gtanxt", "7 yr", "gpbage", "Teacher", "ARBQ Total Anxiety"))

# Conner's ADHD list ####
Conners_Varlist <- list(
  # -- Inattention --
  c("hconint", "8 yr", "hage", "Parent", "Inattention"),
  c("lpconint", "12 yr", "lcqage1", "Parent", "Inattention"),
  c("npconint", "14 yr", "ncqage1", "Parent", "Inattention"),
  c("ppbhconninat", "16 yr", "pcbhage1", "Parent", "Inattention"),
  c("u1pconinat", "21 yr", "u1cage1", "Parent", "Inattention"),
  c("ntconint", "14 yr", "ncqage1", "Teacher", "Inattention"),
  c("ncconint", "14 yr", "ncqage1", "Child", "Inattention"),
  c("u2cconninat", "21 yr", "u1cage1", "Child", "Inattention"),
  c("zmhconnt", "26 yr", "zmhage1", "Child", "Inattention"),
  
  # -- Hyperactivity-Impulsivity --
  c("hconhit", "8 yr", "hage", "Parent", "Hyper-Impuls"),
  c("lpconhit", "12 yr", "lcqage1", "Parent", "Hyper-Impuls"),
  c("npconhit", "14 yr", "ncqage1", "Parent", "Hyper-Impuls"),
  c("ppbhconnimpt", "16 yr", "pcbhage1", "Parent", "Hyper-Impuls"),
  c("u1pconimpt", "21 yr", "u1cage1", "Parent", "Hyper-Impuls"),
  c("ntconhit", "14 yr", "ncqage1", "Teacher", "Hyper-Impuls"),
  c("ncconhit", "14 yr", "ncqage1", "Child", "Hyper-Impuls"),
  c("u2cconnhypt", "21 yr", "u1cage1", "Child", "Hyper-Impuls"),
  
  # -- Total Score --
  c("hconnt", "8 yr", "hage", "Parent", "ADHD"),
  c("lpconnt", "12 yr", "lcqage1", "Parent", "ADHD"),
  c("npconnt", "14 yr", "ncqage1", "Parent", "ADHD"),
  c("ppbhconnt", "16 yr", "pcbhage1", "Parent", "ADHD"),
  c("u1pcont", "21 yr", "u1cage1", "Parent", "ADHD"),
  c("ntconnt", "14 yr", "ncqage1", "Teacher", "ADHD"),
  c("ncconnt", "14 yr", "ncqage1", "Child", "ADHD"),
  c("u2cconnt", "21 yr", "u1cage1", "Child", "ADHD")
)

# SDQ list ####
SDQ_Varlist <- list(
  # -- Conduct Problems --
  c("bsdqccont", "2 yr", "bpbage", "Parent", "Conduct"),
  c("csdqccont", "3 yr", "cpbage", "Parent", "Conduct"),
  c("dsdqcont", "4 yr", "dpbage", "Parent", "Conduct"),
  c("gpsdqcont", "7 yr", "gpbage", "Parent", "Conduct"),
  c("ipsdqcont", "9 yr", "icpage", "Parent", "Conduct"),
  c("lpsdqcont", "12 yr", "lcqage1", "Parent", "Conduct"),
  c("ppbhsdqcont", "16 yr", "pcbhage1", "Parent", "Conduct"),
  c("u1psdqcont", "21 yr", "u1cage1", "Parent", "Conduct"),
  c("gtsdqcont", "7 yr", "gpbage", "Teacher", "Conduct"),
  c("itsdqcont", "9 yr", "itage1", "Teacher", "Conduct"),
  c("ltsdqcont", "12 yr", "ltqage1", "Teacher", "Conduct"),
  c("lcsdqcont", "12 yr", "lcqage1", "Child", "Conduct"),
  c("pcbhsdqcont", "16 yr", "pcbhage1", "Child", "Conduct"),
  c("u1csdqcont", "21 yr", "u1cage1", "Child", "Conduct"),
  c("zmhsdqcont", "26 yr", "zmhage1", "Child", "Conduct"),
  
  # -- Emotion Problems --
  c("bsdqcemot", "2 yr", "bpbage", "Parent", "Emotion"),
  c("csdqcemot", "3 yr", "cpbage", "Parent", "Emotion"),
  c("dsdqemot", "4 yr", "dpbage", "Parent", "Emotion"),
  c("gpsdqemot", "7 yr", "gpbage", "Parent", "Emotion"),
  c("ipsdqemot", "9 yr", "icpage", "Parent", "Emotion"),
  c("lpsdqemot", "12 yr", "lcqage1", "Parent", "Emotion"),
  c("u1psdqemot", "21 yr", "u1cage1", "Parent", "Emotion"),
  c("gtsdqemot", "7 yr", "gpbage", "Teacher", "Emotion"),
  c("itsdqemot", "9 yr", "itage1", "Teacher", "Emotion"),
  c("ltsdqemot", "12 yr", "ltqage1", "Teacher", "Emotion"),
  c("lcsdqemot", "12 yr", "lcqage1", "Child", "Emotion"),
  c("pcbhsdqemot", "16 yr", "pcbhage1", "Child", "Emotion"),
  c("u1csdqemot", "21 yr", "u1cage1", "Child", "Emotion"),
  c("zmhsdqemot", "26 yr", "zmhage1", "Child", "Emotion"),
  
  # -- Hyperactivity Problems --
  c("bsdqchypt", "2 yr", "bpbage", "Parent", "Hyperactivity"),
  c("csdqchypt", "3 yr", "cpbage", "Parent", "Hyperactivity"),
  c("dsdqhypt", "4 yr", "dpbage", "Parent", "Hyperactivity"),
  c("gpsdqhypt", "7 yr", "gpbage", "Parent", "Hyperactivity"),
  c("ipsdqhypt", "9 yr", "icpage", "Parent", "Hyperactivity"),
  c("lpsdqhypt", "12 yr", "lcqage1", "Parent", "Hyperactivity"),
  c("ppbhsdqhypt", "16 yr", "pcbhage1", "Parent", "Hyperactivity"),
  c("u1psdqhypt", "21 yr", "u1cage1", "Parent", "Hyperactivity"),
  c("gtsdqhypt", "7 yr", "gpbage", "Teacher", "Hyperactivity"),
  c("itsdqhypt", "9 yr", "itage1", "Teacher", "Hyperactivity"),
  c("ltsdqhypt", "12 yr", "ltqage1", "Teacher", "Hyperactivity"),
  c("lcsdqhypt", "12 yr", "lcqage1", "Child", "Hyperactivity"),
  c("pcbhsdqhypt", "16 yr", "pcbhage1", "Child", "Hyperactivity"),
  c("u1csdqhypt", "21 yr", "u1cage1", "Child", "Hyperactivity"),
  c("zmhsdqhypt", "26 yr", "zmhage1", "Child", "Hyperactivity"),
  
  # -- Peer Problems --
  c("bsdqcpert", "2 yr", "bpbage", "Parent", "Peer Problems"),
  c("csdqcpert", "3 yr", "cpbage", "Parent", "Peer Problems"),
  c("dsdqpert", "4 yr", "dpbage", "Parent", "Peer Problems"),
  c("gpsdqpert", "7 yr", "gpbage", "Parent", "Peer Problems"),
  c("ipsdqpert", "9 yr", "icpage", "Parent", "Peer Problems"),
  c("lpsdqpert", "12 yr", "lcqage1", "Parent", "Peer Problems"),
  c("u1psdqpert", "21 yr", "u1cage1", "Parent", "Peer Problems"),
  c("gtsdqpert", "7 yr", "gpbage", "Teacher", "Peer Problems"),
  c("itsdqpert", "9 yr", "itage1", "Teacher", "Peer Problems"),
  c("ltsdqpert", "12 yr", "ltqage1", "Teacher", "Peer Problems"),
  c("lcsdqpert", "12 yr", "lcqage1", "Child", "Peer Problems"),
  c("pcbhsdqpert", "16 yr", "pcbhage1", "Child", "Peer Problems"),
  c("u1csdqpert", "21 yr", "u1cage1", "Child", "Peer Problems"),
  c("zmhsdqpert", "26 yr", "zmhage1", "Child", "Peer Problems"),
  
  # -- Prosocial Behaviour --
  c("bsdqcprot", "2 yr", "bpbage", "Parent", "Prosocial"),
  c("csdqcprot", "3 yr", "cpbage", "Parent", "Prosocial"),
  c("dsdqprot", "4 yr", "dpbage", "Parent", "Prosocial"),
  c("gpsdqprot", "7 yr", "gpbage", "Parent", "Prosocial"),
  c("ipsdqprot", "9 yr", "icpage", "Parent", "Prosocial"),
  c("lpsdqprot", "12 yr", "lcqage1", "Parent", "Prosocial"),
  c("ppbhsdqprot", "16 yr", "pcbhage1", "Parent", "Prosocial"),
  c("u1psdqprot", "21 yr", "u1cage1", "Parent", "Prosocial"),
  c("gtsdqprot", "7 yr", "gpbage", "Teacher", "Prosocial"),
  c("itsdqprot", "9 yr", "itage1", "Teacher", "Prosocial"),
  c("ltsdqprot", "12 yr", "ltqage1", "Teacher", "Prosocial"),
  c("lcsdqprot", "12 yr", "lcqage1", "Child", "Prosocial"),
  c("pcbhsdqprot", "16 yr", "pcbhage1", "Child", "Prosocial"),
  c("u1csdqprot", "21 yr", "u1cage1", "Child", "Prosocial"),
  c("zmhsdqprot", "26 yr", "zmhage1", "Child", "Prosocial"),
  
  # -- SDQ Problems --
  c("bsdqcbeht", "2 yr", "bpbage", "Parent", "SDQ Total Problems"),
  c("csdqcbeht", "3 yr", "cpbage", "Parent", "SDQ Total Problems"),
  c("dsdqbeht", "4 yr", "dpbage", "Parent", "SDQ Total Problems"),
  c("gpsdqbeht", "7 yr", "gpbage", "Parent", "SDQ Total Problems"),
  c("ipsdqbeht", "9 yr", "icpage", "Parent", "SDQ Total Problems"),
  c("lpsdqbeht", "12 yr", "lcqage1", "Parent", "SDQ Total Problems"),
  c("ppbhsdqbeht", "16 yr", "pcbhage1", "Parent", "SDQ Total Problems"),
  c("u1psdqbeht", "21 yr", "u1cage1", "Parent", "SDQ Total Problems"),
  c("gtsdqbeht", "7 yr", "gpbage", "Teacher", "SDQ Total Problems"),
  c("itsdqbeht", "9 yr", "itage1", "Teacher", "SDQ Total Problems"),
  c("ltsdqbeht", "12 yr", "ltqage1", "Teacher", "SDQ Total Problems"),
  c("lcsdqbeht", "12 yr", "lcqage1", "Child", "SDQ Total Problems"),
  c("pcbhsdqbeht", "16 yr", "pcbhage1", "Child", "SDQ Total Problems"),
  c("u1csdqbeht", "21 yr", "u1cage1", "Child", "SDQ Total Problems"),
  c("zmhsdqbeht", "26 yr", "zmhage1", "Child", "SDQ Total Problems")
)

# Anthro list ####
Anthro_Varlist <- list(
  c("ablen", "birth", "atwinage", "Parent/Child", "Height"), # in cm
  c("ghtcm", "7 yr", "gpbage", "Parent/Child", "Height"),
  c("lchtcm", "12 yr", "lcqage1", "Parent/Child", "Height"),
  c("nchtcm", "14 yr", "ncqage1", "Parent/Child", "Height"),
  c("pcqdhtcm", "16 yr", "pcwebage1", "Parent/Child", "Height"),
  c("u1chtcm", "21 yr", "u1cage1", "Parent/Child", "Height"),
  c("zmhheight", "26 yr", "zmhage1", "Parent/Child", "Height"),
  
  c("akidgr", "birth (weight)", "atwinage", "Parent/Child", "BMI"), # weight, in grams, not BMI
  c("ccbmi", "3 yr", "cpbage", "Parent/Child", "BMI"),
  c("dcbmi", "4 yr", "dpbage", "Parent/Child", "BMI"),
  c("gbmi", "7 yr", "gpbage", "Parent/Child", "BMI"),
  c("lbmi", "12 yr", "lcqage1", "Parent/Child", "BMI"),
  c("ncbmi", "14 yr", "ncqage1", "Parent/Child", "BMI"),
  c("pcbmi", "16 yr", "pcwebage1", "Parent/Child", "BMI"),
  c("u1cbmi", "21 yr", "u1cage1", "Parent/Child", "BMI"),
  c("zmhbmi", "26 yr", "zmhage1", "Parent/Child", "BMI")
)

# Other outcomes ####
Wellbeing_Varlist <- list(
  # -- Educational Achievements --
  c("pcexgcsehumgrdm", "GCSE Humanities", "pcwebage1", "Child", "GCSE"),
  c("pcexgcselangrdm", "GCSE Language", "pcwebage1", "Child", "GCSE"),
  c("rcqalstecgrdm", "AL Technology", "rcqalage1", "Child", "A/AS-Level"),
  c("rcqalshumgrdm", "AL Humanities", "rcqalage1", "Child", "A/AS-Level"),
  c("rcqalslangrdm", "AL Language", "rcqalage1", "Child", "A/AS-Level"),
  c("rcqalsvocgrdm", "AL Vocational", "rcqalage1", "Child", "A/AS-Level"),
  # -- Home Environment Scales --
  c("jlitenv", "Literacy Environment (10 yr)", "jcstage1", "Child", "Home Environment"),
  c("jmatenv", "Maths Environment (10 yr)", "jcstage1", "Child", "Home Environment"),
  c("llitenf", "Literacy Environment (12 yr)", "lcqage1", "Child", "Home Environment"),
  c("lmatenv", "Maths Environment (12 yr)", "lcqage1", "Child", "Home Environment"),
  # -- PISA Scales (Age 16) --
  c("pchmwkt", "PISA Homework Behaviour and Feedback (16 yr)", "pcwebage1", "Child", "PISA Outcomes"),
  c("pcatscm", "PISA Attitudes to School (16 yr)", "pcwebage1", "Child", "PISA Outcomes"),
  c("pcmaset", "PISA Maths Self-Efficacy (16 yr)", "pcwebage1", "Child", "PISA Outcomes"),
  c("pcmainm", "PISA Maths Interest (16 yr)", "pcwebage1", "Child", "PISA Outcomes"),
  c("pcmatmt", "PISA Time on Maths (16 yr)", "pcwebage1", "Child", "PISA Outcomes"),
  
  # -- SPAA Scales (Age 18) --
  c("rcbspsabm", "Spatial Abilities (18 yr)", "rcqage1", "Child", "SPAA Scales"),
  c("rcbspsnxm", "Spatial Anxiety (18 yr)", "rcqage1", "Child", "SPAA Scales"),
  c("rcbspmnxm", "Maths Anxiety (18 yr)", "rcqage1", "Child", "SPAA Scales"),
  
  # -- Socioeconomic Status / Finances --
  c("u1cfinam", "Financial Wellbeing (21 yr)", "zmhage1", "Child", "Financial Outcomes"),
  c("u1cfprdm", "Financial Products (21 yr)", "zmhage1", "Child", "Financial Outcomes"),
  c("u1cmonam", "Money Attitudes (21 yr)", "zmhage1", "Child", "Financial Outcomes"),
  c("zmhecvul", "Economic Vulnerability (26 yr)", "zmhage1", "Child", "Financial Outcomes"),
  c("zmhses", "Twin SES Composite (26 yr)", "zmhage1", "Child", "Financial Outcomes"),
  
  # -- Age 16 Measures --
  c("pcwbpsym", "Psychological Wellbeing (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcwbsubm", "Subjective Wellbeing (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pclsfamm", "Life Satisfaction Family (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pclsfrnm", "Life Satisfaction Friends (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pclslvem", "Life Satisfaction Living Env (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pclsschm", "Life Satisfaction School (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pclsslfm", "Life Satisfaction Self (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pclsm", "Life Satisfaction Total (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pchopeagm", "Hopefulness Agency (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pchopepam", "Hopefulness Pathways (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pchopem", "Hopefulness Total (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcshsm", "Subjective Happiness (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcgratm", "Gratitude (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pccuriexm", "Curiosity Explore (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pccuriflm", "Curiosity Flow (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pccurim", "Curiosity Total (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcambim", "Ambition (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcgritcoim", "GRIT Consistency (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcgritperm", "GRIT Perseverance (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcgritm", "GRIT Total (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pclotrm", "Optimism LOT-R (16 yr)", "pcwebage1", "Child", "Adolescence"),
  c("pcsecom", "Academic Self-Concept (16 yr)", "pcwebage1", "Child", "Adolescence"),
  
  # -- Age 21 Measures --
  c("u1cselft", "Self-Control (21 yr)", "u1cage1", "Child", "Adulthood"),
  c("u1cfconm", "Future Consequences (21 yr)", "u1cage1", "Child", "Adulthood"),
  c("u1cgoalfult", "GOALS Fulfilment (21 yr)", "u1cage1", "Child", "Adulthood"),
  c("u1cgoalrelt", "GOALS Relationships (21 yr)", "u1cage1", "Child", "Adulthood"),
  c("u1cpilm", "Purpose in Life (21 yr)", "u1cage1", "Child", "Adulthood"),
  c("u2cambit", "Ambition (21 yr)", "u1cage1", "Child", "Adulthood"), 
  c("u1cmfqt", "Depression (21 yr)", "u1cage1", "Child", "Adulthood"),

  # -- Age 26 Measures --
  c("zmhmfqt", "Depression (26 yr)", "zmhage1", "Child", "Adulthood"),
  c("zmhqolm", "Quality of Life (26 yr)", "zmhage1", "Child", "Adulthood")
)

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

list_names <- ls(pattern = "_Varlist$")

print(list_names)

# one rater list ####
# Anxiety list--one rater ####
Anxiety_Varlist_oneRater <- list(
  # -- ARBQ (Anxiety Related Behaviours Questionnaire) --
  
  # ARBQ Subscale: Shyness / Social Anxiety
  c("canxshyt", "3 yr", "cpbage", "Parent", "Shyness"),
  c("danxshyt", "4 yr", "dpbage", "Parent", "Shyness"),
  c("gpanxshyt", "7 yr", "gpbage", "Parent", "Shyness"),
  c("ipanxshyt", "9 yr", "icpage", "Parent", "Shyness"),
  c("ppbhanxshyt", "16 yr", "pcbhage1", "Parent", "Shyness"),
  
  # ARBQ Subscale: Fear
  c("canxfeart", "3 yr", "cpbage", "Parent", "Fear"),
  c("danxfeart", "4 yr", "dpbage", "Parent", "Fear"),
  c("gpanxfeart", "7 yr", "gpbage", "Parent", "Fear"),
  c("ipanxfeart", "9 yr", "icpage", "Parent", "Fear"),
  c("ppbhanxfeart", "16 yr", "pcbhage1", "Parent", "Fear"),
  
  # ARBQ Subscale: OCB (OCB)
  c("danxocbt", "4 yr", "dpbage", "Parent", "OCB"),
  c("gpanxocbt", "7 yr", "gpbage", "Parent", "OCB"),
  c("ipanxocbt", "9 yr", "icpage", "Parent", "OCB"),
  c("ppbhanxocbt", "16 yr", "pcbhage1", "Parent", "OCB"),
  
  # ARBQ Subscale: Negative Affect
  c("danxnafft", "4 yr", "dpbage", "Parent", "Negative Affect"),
  c("gpanxnafft", "7 yr", "gpbage", "Parent", "Negative Affect"),
  c("ipanxnafft", "9 yr", "icpage", "Parent", "Negative Affect"),
  c("ppbhanxnafft", "16 yr", "pcbhage1", "Parent", "Negative Affect"),
  
  # ARBQ Subscale: Negative Cognition
  c("danxncogt", "4 yr", "dpbage", "Parent", "Negative Cognition"),
  c("gpanxncogt", "7 yr", "gpbage", "Parent", "Negative Cognition"),
  c("ipanxncogt", "9 yr", "icpage", "Parent", "Negative Cognition"),
  c("ppbhanxncogt", "16 yr", "pcbhage1", "Parent", "Negative Cognition"),
  
  # -- Total Anxiety Symptoms --
  c("canxt", "3 yr", "cpbage", "Parent", "Anxiety"),
  c("danxt", "4 yr", "dpbage", "Parent", "Anxiety"),
  c("gpanxt", "7 yr", "gpbage", "Parent", "Anxiety"),
  c("ipanxt", "9 yr", "icpage", "Parent", "Anxiety"),
  c("ppbhanxt", "16 yr", "pcbhage1", "Parent", "Anxiety")
)

# Conner's ADHD list--one rater ####
Conners_Varlist_oneRater <- list(
  # -- Inattention --
  c("hconint", "8 yr", "hage", "Parent", "Inattention"),
  c("lpconint", "12 yr", "lcqage1", "Parent", "Inattention"),
  c("npconint", "14 yr", "ncqage1", "Parent", "Inattention"),
  c("ppbhconninat", "16 yr", "pcbhage1", "Parent", "Inattention"),
  c("u2cconninat", "21 yr", "u1cage1", "Child", "Inattention"),
  c("zmhconnt", "26 yr", "zmhage1", "Child", "Inattention"),
  
  # -- Hyperactivity-Impulsivity --
  c("hconhit", "8 yr", "hage", "Parent", "Hyper-Impuls"),
  c("lpconhit", "12 yr", "lcqage1", "Parent", "Hyper-Impuls"),
  c("npconhit", "14 yr", "ncqage1", "Parent", "Hyper-Impuls"),
  c("ppbhconnimpt", "16 yr", "pcbhage1", "Parent", "Hyper-Impuls"),
  c("u2cconnhypt", "21 yr", "u1cage1", "Child", "Hyper-Impuls"),
  
  # -- ADHD Score --
  c("hconnt", "8 yr", "hage", "Parent", "ADHD"),
  c("lpconnt", "12 yr", "lcqage1", "Parent", "ADHD"),
  c("npconnt", "14 yr", "ncqage1", "Parent", "ADHD"),
  c("ppbhconnt", "16 yr", "pcbhage1", "Parent", "ADHD"),
  c("u2cconnt", "21 yr", "u1cage1", "Child", "ADHD")
)

# SDQ list--one rater ####
SDQ_Varlist_oneRater <- list(
  # -- Conduct Problems --
  c("bsdqccont", "2 yr", "bpbage", "Parent", "Conduct"),
  c("csdqccont", "3 yr", "cpbage", "Parent", "Conduct"),
  c("dsdqcont", "4 yr", "dpbage", "Parent", "Conduct"),
  c("gpsdqcont", "7 yr", "gpbage", "Parent", "Conduct"),
  c("ipsdqcont", "9 yr", "icpage", "Parent", "Conduct"),
  c("lpsdqcont", "12 yr", "lcqage1", "Parent", "Conduct"),
  c("ppbhsdqcont", "16 yr", "pcbhage1", "Parent", "Conduct"),
  c("u1csdqcont", "21 yr", "u1cage1", "Child", "Conduct"),
  c("zmhsdqcont", "26 yr", "zmhage1", "Child", "Conduct"),
  
  # -- Emotion Problems --
  c("bsdqcemot", "2 yr", "bpbage", "Parent", "Emotion"),
  c("csdqcemot", "3 yr", "cpbage", "Parent", "Emotion"),
  c("dsdqemot", "4 yr", "dpbage", "Parent", "Emotion"),
  c("gpsdqemot", "7 yr", "gpbage", "Parent", "Emotion"),
  c("ipsdqemot", "9 yr", "icpage", "Parent", "Emotion"),
  c("lpsdqemot", "12 yr", "lcqage1", "Parent", "Emotion"),
  c("pcbhsdqemot", "16 yr", "pcbhage1", "Child", "Emotion"), 
  c("u1csdqemot", "21 yr", "u1cage1", "Child", "Emotion"),
  c("zmhsdqemot", "26 yr", "zmhage1", "Child", "Emotion"),
  
  # -- Hyperactivity Problems --
  c("bsdqchypt", "2 yr", "bpbage", "Parent", "Hyperactivity"),
  c("csdqchypt", "3 yr", "cpbage", "Parent", "Hyperactivity"),
  c("dsdqhypt", "4 yr", "dpbage", "Parent", "Hyperactivity"),
  c("gpsdqhypt", "7 yr", "gpbage", "Parent", "Hyperactivity"),
  c("ipsdqhypt", "9 yr", "icpage", "Parent", "Hyperactivity"),
  c("lpsdqhypt", "12 yr", "lcqage1", "Parent", "Hyperactivity"),
  c("ppbhsdqhypt", "16 yr", "pcbhage1", "Parent", "Hyperactivity"),
  c("u1csdqhypt", "21 yr", "u1cage1", "Child", "Hyperactivity"),
  c("zmhsdqhypt", "26 yr", "zmhage1", "Child", "Hyperactivity"),
  
  # -- Peer Problems --
  c("bsdqcpert", "2 yr", "bpbage", "Parent", "Peer Problems"),
  c("csdqcpert", "3 yr", "cpbage", "Parent", "Peer Problems"),
  c("dsdqpert", "4 yr", "dpbage", "Parent", "Peer Problems"),
  c("gpsdqpert", "7 yr", "gpbage", "Parent", "Peer Problems"),
  c("ipsdqpert", "9 yr", "icpage", "Parent", "Peer Problems"),
  c("lpsdqpert", "12 yr", "lcqage1", "Parent", "Peer Problems"),
  c("pcbhsdqpert", "16 yr", "pcbhage1", "Child", "Peer Problems"), # the peer problem for parent rating is missing at age 16
  c("u1csdqpert", "21 yr", "u1cage1", "Child", "Peer Problems"),
  c("zmhsdqpert", "26 yr", "zmhage1", "Child", "Peer Problems"),
  
  # -- Prosocial Behaviour --
  c("bsdqcprot", "2 yr", "bpbage", "Parent", "Prosocial"),
  c("csdqcprot", "3 yr", "cpbage", "Parent", "Prosocial"),
  c("dsdqprot", "4 yr", "dpbage", "Parent", "Prosocial"),
  c("gpsdqprot", "7 yr", "gpbage", "Parent", "Prosocial"),
  c("ipsdqprot", "9 yr", "icpage", "Parent", "Prosocial"),
  c("lpsdqprot", "12 yr", "lcqage1", "Parent", "Prosocial"),
  c("ppbhsdqprot", "16 yr", "pcbhage1", "Parent", "Prosocial"),
  c("u1csdqprot", "21 yr", "u1cage1", "Child", "Prosocial"),
  c("zmhsdqprot", "26 yr", "zmhage1", "Child", "Prosocial"),
  
  # -- Total Problems --
  c("bsdqcbeht", "2 yr", "bpbage", "Parent", "Total Problems"),
  c("csdqcbeht", "3 yr", "cpbage", "Parent", "Total Problems"),
  c("dsdqbeht", "4 yr", "dpbage", "Parent", "Total Problems"),
  c("gpsdqbeht", "7 yr", "gpbage", "Parent", "Total Problems"),
  c("ipsdqbeht", "9 yr", "icpage", "Parent", "Total Problems"),
  c("lpsdqbeht", "12 yr", "lcqage1", "Parent", "Total Problems"),
  c("ppbhsdqbeht", "16 yr", "pcbhage1", "Parent", "Total Problems"),
  c("u1csdqbeht", "21 yr", "u1cage1", "Child", "Total Problems"),
  c("zmhsdqbeht", "26 yr", "zmhage1", "Child", "Total Problems")
)

one_rater_Varlist <- c(
  Anxiety_Varlist_oneRater,
  Conners_Varlist_oneRater,
  SDQ_Varlist_oneRater
)

list_names <- ls(pattern = "_Varlist_oneRater$")
print(list_names)

cat("All Varlist Loaded \n\n")

