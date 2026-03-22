# ── R/datahub_catalog.R ───────────────────────────────────────────────────────
# Central registry of all gender datasets available in the Data Hub.
# Each entry: sector → list of datasets with label, file path, description, tags.

DATAHUB_CATALOG <- list(

  agriculture = list(
    label   = "Agriculture",
    icon    = "fas fa-seedling",
    color   = "#2d6e44",
    bg      = "#DCFCE7",
    source  = "Rwanda Agricultural Household Survey (AHS) 2020 · PHC 2022",
    datasets = list(
      agri_hh_summary        = list(label="Agricultural Households by Sex of Head",         file="Agriculture/clean/agri_hh_summary.csv",        desc="Number and % of total HH vs agricultural HH by sex of household head.",         tags=c("households","sex","overview")),
      agri_workers_trend     = list(label="Agricultural Workers Trend 2017–2022",            file="Agriculture/clean/agri_workers_trend.csv",     desc="% of working-age population (16+) in market-oriented and subsistence agriculture by sex and year.", tags=c("employment","trend","sex","year")),
      agri_land_ownership    = list(label="Land Ownership by Category (2018 vs 2021)",       file="Agriculture/clean/agri_land_ownership.csv",    desc="Land ownership (jointly, women, men) as % for 2018 and 2021 (NLA data).", tags=c("land","ownership","trend")),
      agri_land_access       = list(label="Agricultural Land Access by HH Sex",             file="Agriculture/clean/agri_land_access.csv",       desc="% of agricultural HH accessing own, rented, or combined land by sex of HH head.", tags=c("land","access","sex")),
      agri_land_rights       = list(label="Land Rights by Sex",                             file="Agriculture/clean/agri_land_rights.csv",       desc="% of farmers with right to use and right to sell/mortgage land, by sex.", tags=c("land","rights","sex")),
      agri_extension         = list(label="Agricultural Extension Services by Sex",         file="Agriculture/clean/agri_extension.csv",         desc="% of HH receiving 12 types of extension services; female vs male headed HH.", tags=c("extension","services","sex")),
      agri_community_groups  = list(label="Community Group Membership by HH Sex",           file="Agriculture/clean/agri_community_groups.csv", desc="% of agricultural HH belonging to cooperatives, Twigire Muhinzi, and Farmer Field Schools.", tags=c("community","groups","sex")),
      agri_inputs            = list(label="Agricultural Inputs by Province & HH Sex",       file="Agriculture/clean/agri_inputs.csv",            desc="% of HH using improved seeds, organic/inorganic fertilizer, and pesticides by province and sex.", tags=c("inputs","province","sex")),
      agri_livestock         = list(label="Livestock Ownership by Type & HH Sex",           file="Agriculture/clean/agri_livestock.csv",         desc="% of HH raising cattle, goats, pigs, chickens, and other livestock by sex of HH head.", tags=c("livestock","sex")),
      agri_girinka           = list(label="Girinka Programme Beneficiaries",                file="Agriculture/clean/agri_girinka.csv",           desc="% of HH who benefited from Girinka, retention rates, and provider type by sex.", tags=c("program","girinka","sex"))
    )
  ),

  demography = list(
    label   = "Demography",
    icon    = "fas fa-users",
    color   = "#1a5c8a",
    bg      = "#DBEAFE",
    source  = "Rwanda Population and Housing Census (PHC) 2022",
    datasets = list(
      demo_age_distribution       = list(label="Age Distribution by Sex",                    file="Demography/clean/demo_age_distribution.csv",          desc="Population distribution across age groups disaggregated by sex.", tags=c("age","sex","population")),
      demo_population_geo_by_sex  = list(label="Population by Geography & Sex",              file="Demography/clean/demo_population_geo_by_sex.csv",     desc="Population counts by province/district and sex.", tags=c("geography","sex","population")),
      demo_population_change      = list(label="Population Change by Sex",                   file="Demography/clean/demo_population_change_by_sex.csv",  desc="Population change between census periods by sex.", tags=c("trend","sex","population")),
      demo_youth_share            = list(label="Youth Share by Geography",                   file="Demography/clean/demo_youth_share_by_geo.csv",        desc="Share of youth (15-35) in total population by geography.", tags=c("youth","geography")),
      demo_elderly_share          = list(label="Elderly Population Share",                   file="Demography/clean/demo_elderly_share.csv",             desc="Share of elderly (60+) by sex and geography.", tags=c("elderly","sex","geography")),
      demo_internet_use           = list(label="Internet Use by Sex & Geography",            file="Demography/clean/demo_internet_use.csv",              desc="% of population using internet disaggregated by sex and area.", tags=c("digital","internet","sex")),
      demo_school_attendance_7_18 = list(label="School Attendance (7–18 yrs)",               file="Demography/clean/demo_school_7_18_attendance.csv",    desc="School attendance rates for 7-18 year olds by sex.", tags=c("education","attendance","sex")),
      demo_school_13_18_geo       = list(label="School Attendance (13–18) by Geography",     file="Demography/clean/demo_school_13_18_by_geo.csv",       desc="Secondary school attendance by geography and sex.", tags=c("education","attendance","geography","sex")),
      demo_education_attendance   = list(label="Education Attendance Indicators",            file="Demography/clean/demo_education_attendance.csv",      desc="Broad education attendance indicators by sex.", tags=c("education","attendance","sex")),
      demo_intervention_age       = list(label="Population by Intervention Age Groups",      file="Demography/clean/demo_intervention_age_groups.csv",   desc="Population in key intervention age groups by sex.", tags=c("age","sex","population"))
    )
  ),

  education = list(
    label   = "Education",
    icon    = "fas fa-graduation-cap",
    color   = "#2563EB",
    bg      = "#EFF6FF",
    source  = "PHC 2022 Education Thematic Report",
    datasets = list(
      edu_literacy_sex        = list(label="Literacy Rate by Sex & Area",               file="Education/clean/edu_literacy_sex.csv",        desc="Adult literacy rate (%) by sex and urban/rural area.", tags=c("literacy","sex","area")),
      edu_literacy_age        = list(label="Literacy Rate by Age Group & Sex",          file="Education/clean/edu_literacy_age.csv",        desc="Literacy rate by 5-year age group disaggregated by sex.", tags=c("literacy","age","sex")),
      edu_attainment_15plus   = list(label="Educational Attainment (15+)",              file="Education/clean/edu_attainment_15plus.csv",   desc="Highest education level attained by population 15+ by sex.", tags=c("attainment","sex")),
      edu_attainment_area     = list(label="Educational Attainment by Area",            file="Education/clean/edu_attainment_area.csv",     desc="Education attainment by urban/rural area and sex.", tags=c("attainment","area","sex")),
      edu_attainment_trend    = list(label="Educational Attainment Trend",              file="Education/clean/edu_attainment_trend.csv",    desc="Historical trend of educational attainment by level and sex.", tags=c("attainment","trend","sex")),
      edu_attainment_disability=list(label="Educational Attainment by Disability",     file="Education/clean/edu_attainment_disability.csv",desc="Education attainment disaggregated by disability status and sex.", tags=c("attainment","disability","sex")),
      edu_attendance_agegroup = list(label="School Attendance by Age Group",           file="Education/clean/edu_attendance_agegroup.csv", desc="School attendance rate by 5-year age group and sex.", tags=c("attendance","age","sex")),
      edu_attendance_6to17    = list(label="School Attendance (6–17 yrs)",             file="Education/clean/edu_attendance_6to17.csv",    desc="School attendance for primary/secondary age children by sex.", tags=c("attendance","primary","secondary","sex")),
      edu_attendance_3to5     = list(label="Pre-Primary Attendance (3–5 yrs)",         file="Education/clean/edu_attendance_3to5.csv",     desc="Pre-primary school attendance by sex.", tags=c("attendance","preprimary","sex")),
      edu_attendance_status   = list(label="School Attendance Status",                 file="Education/clean/edu_attendance_status.csv",   desc="Attendance status (attending/never/dropped out) by sex.", tags=c("attendance","status","sex")),
      edu_ict_province        = list(label="ICT Literacy by Province & Sex",           file="Education/clean/edu_ict_province.csv",        desc="ICT literacy rate by province and sex.", tags=c("ICT","digital","province","sex")),
      edu_mobile_phone        = list(label="Mobile Phone Access by Sex",               file="Education/clean/edu_mobile_phone.csv",        desc="Mobile phone ownership/access by sex and area.", tags=c("mobile","digital","sex"))
    )
  ),

  employment = list(
    label   = "Employment",
    icon    = "fas fa-briefcase",
    color   = "#7C3AED",
    bg      = "#EDE9FE",
    source  = "Rwanda Labour Force Survey (RLFS) 2025 Q4",
    datasets = list(
      emp_lf_sex          = list(label="Labour Force Indicators by Sex",              file="Employment/clean/emp_lf_sex.csv",             desc="LFPR, unemployment, employment and NEET rates by sex and year/quarter.", tags=c("labour","LFPR","unemployment","sex","trend")),
      emp_lf_youth        = list(label="Youth Labour Force Indicators",               file="Employment/clean/emp_lf_youth.csv",           desc="Youth (15-30) LFPR, unemployment, and NEET rates by age group.", tags=c("youth","NEET","unemployment","age")),
      emp_status_sex      = list(label="Employment Status by Sex",                    file="Employment/clean/emp_status_sex.csv",         desc="Employment status (employee, self-employed, etc.) by sex.", tags=c("status","sex")),
      emp_economic_activity=list(label="Economic Activity by Sex",                   file="Employment/clean/emp_economic_activity_sex.csv",desc="Employment by economic sector/industry disaggregated by sex.", tags=c("sector","industry","sex")),
      emp_occupations_sex = list(label="Occupations by Sex",                         file="Employment/clean/emp_occupations_sex.csv",    desc="Employment by occupation type disaggregated by sex.", tags=c("occupation","sex")),
      emp_education_sex   = list(label="Employment by Education Level & Sex",        file="Employment/clean/emp_education_sex.csv",      desc="Employment and wages by education level and sex.", tags=c("education","wages","sex")),
      emp_agri_sex        = list(label="Agriculture Employment by Sex",               file="Employment/clean/emp_agri_sex.csv",           desc="Agricultural employment share by sex over time.", tags=c("agriculture","sex","trend"))
    )
  ),

  governance = list(
    label   = "Governance",
    icon    = "fas fa-landmark",
    color   = "#d62027",
    bg      = "#FEE2E2",
    source  = "NISR Governance Data",
    datasets = list(
      gov_parliament      = list(label="Parliament Representation by Sex",     file="Governance_data/clean/governance_parliament_gender.csv",  desc="% of women and men in Parliament over time.", tags=c("parliament","sex","trend")),
      gov_ministers       = list(label="Cabinet Ministers by Sex",              file="Governance_data/clean/governance_ministers_gender.csv",   desc="% of women and men in Cabinet/Minister positions.", tags=c("ministers","cabinet","sex")),
      gov_local_leaders   = list(label="Local Leaders by Position & Sex",      file="Governance_data/clean/governance_local_leaders_gender.csv",desc="Local government leadership by position type and sex.", tags=c("local","leadership","sex")),
      gov_judiciary       = list(label="Judiciary by Sex",                     file="Governance_data/clean/governance_judiciary_gender.csv",   desc="Representation of women and men in judiciary/courts.", tags=c("judiciary","courts","sex")),
      gov_prosecutors     = list(label="Prosecutors & Legal Officers by Sex",  file="Governance_data/clean/governance_prosecutors_gender.csv", desc="Prosecution and legal officer positions by sex.", tags=c("legal","prosecutors","sex"))
    )
  ),

  financial_inclusion = list(
    label   = "Financial Inclusion",
    icon    = "fas fa-wallet",
    color   = "#c07718",
    bg      = "#FEF3C7",
    source  = "FinScope Rwanda Consumer Survey 2024",
    datasets = list(
      finscope_2024 = list(label="FinScope 2024 — Full Microdata",   file="Financial_Inclusion/finscope_2024_clean.csv", desc="Full FinScope 2024 respondent-level data including financial access, savings, credit, insurance, and digital finance disaggregated by sex, age, and geography.", tags=c("financial","access","savings","credit","insurance","sex","age","geography"))
    )
  )
)

# ── Helper: resolve dataset file path ─────────────────────────────────────────
datahub_resolve_path <- function(rel_path) {
  candidates <- c(
    file.path(getwd(), "Dashboard_data", rel_path),
    file.path("/srv/shiny-server/Dashboard_data", rel_path)
  )
  for (p in candidates) if (file.exists(p)) return(normalizePath(p))
  # partial name fallback
  base_name <- basename(rel_path)
  hits <- list.files(file.path(getwd(), "Dashboard_data"),
                     pattern = base_name, recursive = TRUE, full.names = TRUE)
  if (length(hits)) return(hits[1])
  NULL
}

# ── Helper: load a dataset by sector + dataset key ────────────────────────────
datahub_load <- function(sector_key, dataset_key) {
  entry <- DATAHUB_CATALOG[[sector_key]]$datasets[[dataset_key]]
  if (is.null(entry)) return(NULL)
  path  <- datahub_resolve_path(entry$file)
  if (is.null(path)) return(NULL)
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE,
                    locale = readr::locale(encoding = "UTF-8")),
    error = function(e) NULL
  )
}

# ── Helper: choices list for a sector ─────────────────────────────────────────
datahub_dataset_choices <- function(sector_key) {
  datasets <- DATAHUB_CATALOG[[sector_key]]$datasets
  setNames(names(datasets), sapply(datasets, `[[`, "label"))
}
