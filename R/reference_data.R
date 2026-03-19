# ── R/reference_data.R ────────────────────────────────────────────────────────
# Curated Rwanda gender statistics used in the Dashboard.
# Sources: DHS Rwanda 1992–2020 (NISR), EICV series, RLFS 2017–2023,
#          PHC 2012/2022, MINEDUC gender statistics.

dhs_maternal <- data.frame(
  year              = c(1992, 2000, 2005, 2007, 2010, 2014, 2019),
  mmr               = c(1300, 1071,  750,  610,  476,  320,  203),
  skilled_birth_pct = c(  27,   31,   39,   52,   69,   91,   94),
  anc4_pct          = c(  12,   50,   73,   NA,   96,   44,   71),
  cpr_modern_pct    = c(   4,   13,   17,   27,   45,   53,   58),
  survey = c("DHS 1992","DHS 2000","DHS 2005","DHS 2007",
             "DHS 2010","DHS 2014","DHS 2019-20"),
  stringsAsFactors = FALSE
)

dhs_child <- data.frame(
  year         = c(1992, 2000, 2005, 2007, 2010, 2014, 2019),
  u5mr         = c( 164,  196,  152,  112,   76,   50,   45),
  stunting_pct = c(  48,   43,   45,   52,   44,   38,   33),
  hiv_female   = c(  NA,   NA,   NA,   NA,  3.6,  3.5,  3.4),
  hiv_male     = c(  NA,   NA,   NA,   NA,  2.3,  2.3,  2.3)
)

education_df <- data.frame(
  year                        = c(2005, 2010, 2014, 2019, 2022),
  gpi_primary                 = c(0.97, 1.02, 1.01, 1.02, 1.03),
  gpi_secondary               = c(0.84, 0.93, 0.98, 1.01, 1.02),
  female_secondary_completion = c(  10,   18,   22,   28,   32),
  male_secondary_completion   = c(  12,   20,   23,   27,   29)
)

labor_df <- data.frame(
  year   = c(2017, 2018, 2019, 2020, 2021, 2022, 2023),
  female = c(  84,   84,   85,   83,   86,   87,   87),
  male   = c(  84,   84,   85,   84,   86,   87,   88)
)

province_df <- data.frame(
  province            = c("Kigali City","Northern","Southern","Eastern","Western"),
  female_hh_head_pct  = c(38, 32, 30, 28, 29),
  female_land_pct     = c(22, 18, 17, 15, 16),
  female_literacy_pct = c(83, 70, 68, 65, 67)
)

gender_topics_df <- data.frame(
  topic            = c("Maternal Health","Education","Employment",
                       "Land Rights","Food Security","Financial Access",
                       "Violence","Political Participation"),
  studies_covering = c( 7, 12,  8, 6, 6, 4, 3, 5),
  data_quality     = c( 9,  8,  7, 5, 6, 6, 4, 7),
  recency_score    = c( 9,  8,  8, 6, 7, 8, 5, 7)
)
