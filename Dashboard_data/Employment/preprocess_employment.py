"""
preprocess_employment.py
Rwanda Quarterly Labour Force Survey (LFS) 2025 Q4 - Employment gender data extractor.
Reads the multi-block Excel tables and outputs tidy long-format CSVs to ./clean/.

Output CSVs:
  emp_lf_sex.csv                - Labour Force indicators by sex (quarterly time series)
  emp_lf_youth.csv              - Labour Force indicators by Youth vs Adult
  emp_occupations_sex.csv       - Occupation distribution by sex (% of employed)
  emp_status_sex.csv            - Employment status by sex (% of employed)
  emp_education_sex.csv         - Education level of employed by sex (% of employed)
  emp_agri_sex.csv              - Agriculture workers share by sex (% of total workforce)
  emp_economic_activity_sex.csv - Economic activity sector by sex (% of employed)
"""

import os
import re
import pandas as pd
import numpy as np

EXCEL_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                          "RW_Quarterly_LFS_Tables_2025Q4.xlsx")
OUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "clean")
os.makedirs(OUT_DIR, exist_ok=True)


def safe_num(v):
    try:
        return float(str(v).replace(",", "").replace(" ", "").replace("\xa0", ""))
    except Exception:
        return float("nan")


def is_year_like(v):
    n = safe_num(v)
    return not np.isnan(n) and 2015 <= n <= 2030


def build_time_axis(df_raw):
    year_row = df_raw.iloc[2]
    qtr_row  = df_raw.iloc[3]
    result   = []
    current_year = None
    for ci in range(1, len(df_raw.columns)):
        yr = year_row.iloc[ci]
        if is_year_like(yr):
            current_year = int(safe_num(yr))
        qt = str(qtr_row.iloc[ci]).strip()
        if current_year and re.match(r"^Q[1-4]$", qt):
            result.append((current_year, qt, ci))
    return result


def parse_lf_indicators_sex():
    df = pd.read_excel(EXCEL_PATH, sheet_name="LFIndicatorsSex", header=None)
    time_axis = build_time_axis(df)

    sex_blocks = {}
    cur_sex = None
    cur_start = None
    for i, row in df.iterrows():
        val = str(row[0]).strip()
        if val in ("Male", "Female"):
            if cur_sex and cur_start is not None:
                sex_blocks[cur_sex] = (cur_start, i - 1)
            cur_sex = val
            cur_start = i + 1
    if cur_sex and cur_start is not None:
        sex_blocks[cur_sex] = (cur_start, len(df) - 1)

    rows = []
    for sex, (r_start, r_end) in sex_blocks.items():
        for ri in range(r_start + 2, r_end + 1):
            ind = str(df.iloc[ri, 0]).strip()
            if not ind or ind.lower() in ("nan", ""):
                continue
            if ind.lower().startswith("source"):
                continue
            for (yr, qt, ci) in time_axis:
                v = safe_num(df.iloc[ri, ci])
                rows.append({
                    "year": yr, "quarter": qt,
                    "sex": sex, "indicator": ind, "value": v
                })

    out = pd.DataFrame(rows)
    out = out.dropna(subset=["value"])
    out.to_csv(os.path.join(OUT_DIR, "emp_lf_sex.csv"), index=False)
    print("  emp_lf_sex.csv -> %d rows" % len(out))
    return out


def parse_lf_indicators_youth():
    df = pd.read_excel(EXCEL_PATH, sheet_name="LFIndicatorsYouthAdult", header=None)
    time_axis = build_time_axis(df)

    group_blocks = {}
    cur_grp = None
    cur_start = None
    for i, row in df.iterrows():
        val = str(row[0]).strip()
        if "Youth" in val or "Adult" in val:
            if cur_grp and cur_start is not None:
                group_blocks[cur_grp] = (cur_start, i - 1)
            label = "Youth (16-30)" if "Youth" in val else "Adult (31-64)"
            cur_grp = label
            cur_start = i + 1
    if cur_grp and cur_start is not None:
        group_blocks[cur_grp] = (cur_start, len(df) - 1)

    rows = []
    for grp, (r_start, r_end) in group_blocks.items():
        for ri in range(r_start + 2, r_end + 1):
            ind = str(df.iloc[ri, 0]).strip()
            if not ind or ind.lower() in ("nan", "") or ind.lower().startswith("source"):
                continue
            for (yr, qt, ci) in time_axis:
                v = safe_num(df.iloc[ri, ci])
                rows.append({
                    "year": yr, "quarter": qt,
                    "group": grp, "indicator": ind, "value": v
                })

    out = pd.DataFrame(rows)
    out = out.dropna(subset=["value"])
    out.to_csv(os.path.join(OUT_DIR, "emp_lf_youth.csv"), index=False)
    print("  emp_lf_youth.csv -> %d rows" % len(out))
    return out


def parse_sex_category_sheet(sheet_name, output_file, cat_col_name):
    df = pd.read_excel(EXCEL_PATH, sheet_name=sheet_name, header=None)
    time_axis = build_time_axis(df)

    sex_blocks = {}
    cur_sex = None
    cur_start = None
    for i, row in df.iterrows():
        val = str(row[0]).strip()
        if val.startswith("Sex: "):
            sex_label = val[5:].strip()
            if cur_sex and cur_start is not None:
                sex_blocks[cur_sex] = (cur_start, i - 1)
            cur_sex = sex_label
            cur_start = i + 1
    if cur_sex and cur_start is not None:
        sex_blocks[cur_sex] = (cur_start, len(df) - 1)

    rows = []
    for sex, (r_start, r_end) in sex_blocks.items():
        # Skip internal time-axis header rows (year-row + quarter-row) at start of each block
        for ri in range(r_start + 2, r_end + 1):
            cat = str(df.iloc[ri, 0]).strip()
            if not cat:
                continue
            cat_lower = cat.lower()
            if cat_lower in {"nan", ""}:
                continue
            if cat_lower.startswith("source"):
                continue

            for (yr, qt, ci) in time_axis:
                v = safe_num(df.iloc[ri, ci])
                if np.isnan(v):
                    continue
                rows.append({
                    "year": yr, "quarter": qt,
                    "sex": sex,
                    cat_col_name: cat,
                    "value": v
                })

    out = pd.DataFrame(rows)
    out.to_csv(os.path.join(OUT_DIR, output_file), index=False)
    print("  %s -> %d rows" % (output_file, len(out)))
    return out


def parse_occupations_sex():
    return parse_sex_category_sheet("OccupationsSex", "emp_occupations_sex.csv", "occupation")


def parse_status_sex():
    return parse_sex_category_sheet("StatusInEmploymentSex", "emp_status_sex.csv", "status")


def parse_education_sex():
    df = pd.read_excel(EXCEL_PATH, sheet_name="EmployedEducaSex", header=None)
    time_axis = build_time_axis(df)
    time_cols = {ci for (_, _, ci) in time_axis}

    sex_blocks = {}
    cur_sex = None
    cur_start = None
    for i, row in df.iterrows():
        val = str(row[0]).strip()
        if val.startswith("Sex: "):
            sex_label = val[5:].strip()
            if cur_sex and cur_start is not None:
                sex_blocks[cur_sex] = (cur_start, i - 1)
            cur_sex = sex_label
            cur_start = i + 1
    if cur_sex and cur_start is not None:
        sex_blocks[cur_sex] = (cur_start, len(df) - 1)

    edu_label_map = {
        "nan": "No schooling",
        "": "No schooling",
        "primary": "Primary",
        "lower secondary": "Lower secondary",
        "upper secondary": "Upper secondary",
        "university": "University/Higher",
    }

    rows = []
    for sex, (r_start, r_end) in sex_blocks.items():
        # Skip first 2 rows of each block (internal year-row and quarter-row headers)
        for ri in range(r_start + 2, r_end + 1):
            raw = str(df.iloc[ri, 0]).strip()
            raw_low = raw.lower()
            if raw_low.startswith("source") or raw_low == "total":
                continue
            if "employed population" in raw_low:
                edu = "_n_employed"
            else:
                edu = edu_label_map.get(raw_low, raw)
            for (yr, qt, ci) in time_axis:
                v = safe_num(df.iloc[ri, ci])
                if np.isnan(v):
                    continue
                rows.append({
                    "year": yr, "quarter": qt,
                    "sex": sex, "education": edu, "value": v
                })

    out = pd.DataFrame(rows)
    out.to_csv(os.path.join(OUT_DIR, "emp_education_sex.csv"), index=False)
    print("  emp_education_sex.csv -> %d rows" % len(out))
    return out


def parse_agri_sex():
    df = pd.read_excel(EXCEL_PATH, sheet_name="ShareAgriWorkersSex", header=None)
    time_axis = build_time_axis(df)

    target_groups = {"Rwanda", "Male", "Female"}
    rows = []
    for i, row in df.iterrows():
        grp = str(row[0]).strip()
        if grp not in target_groups:
            continue
        for (yr, qt, ci) in time_axis:
            v = safe_num(df.iloc[i, ci])
            if np.isnan(v):
                continue
            rows.append({
                "year": yr, "quarter": qt,
                "group": grp, "pct_agri": v
            })

    out = pd.DataFrame(rows)
    out.to_csv(os.path.join(OUT_DIR, "emp_agri_sex.csv"), index=False)
    print("  emp_agri_sex.csv -> %d rows" % len(out))
    return out


def parse_economic_activity_sex():
    MAJOR_SECTORS = {
        "Agriculture forestry and fishing":       "Agriculture",
        "Manufacturing":                          "Manufacturing",
        "Construction":                           "Construction",
        "Whole sale and retail trade; repair of motor vehicles and motorcycles": "Trade & Retail",
        "Transportationa and storage":            "Transport & Storage",
        "Accommodation and food services activities": "Accommodation & Food",
        "Public administration and defense; compulsory social security": "Public Admin.",
        "Education":                              "Education",
        "Human health and social work activities": "Health & Social Work",
        "Information and communication":          "ICT",
        "Financial and insurance activities":     "Finance & Insurance",
        "Professional, scientific and technical activities": "Professional Services",
        "Ativities of house13holds as employers": "Households as Employers",
        "Other services":                         "Other Services",
    }

    df = pd.read_excel(EXCEL_PATH, sheet_name="EconomicActivitySex", header=None)
    time_axis = build_time_axis(df)

    sex_blocks = {}
    cur_sex = None
    cur_start = None
    for i, row in df.iterrows():
        val = str(row[0]).strip()
        if val.startswith("Sex: "):
            sex_label = val[5:].strip()
            if cur_sex and cur_start is not None:
                sex_blocks[cur_sex] = (cur_start, i - 1)
            cur_sex = sex_label
            cur_start = i + 1
    if cur_sex and cur_start is not None:
        sex_blocks[cur_sex] = (cur_start, len(df) - 1)

    rows = []
    for sex, (r_start, r_end) in sex_blocks.items():
        # Skip internal time-axis header rows at start of each block
        for ri in range(r_start + 2, r_end + 1):
            raw = str(df.iloc[ri, 0]).strip()
            if raw in MAJOR_SECTORS:
                sector_label = MAJOR_SECTORS[raw]
            elif raw.lower() in {"total", "nan", ""} or raw.lower().startswith("source"):
                continue
            elif "employed population" in raw.lower():
                sector_label = "_n_employed"
            else:
                sector_label = raw

            for (yr, qt, ci) in time_axis:
                v = safe_num(df.iloc[ri, ci])
                if np.isnan(v):
                    continue
                rows.append({
                    "year": yr, "quarter": qt,
                    "sex": sex, "sector": sector_label, "value": v
                })

    out = pd.DataFrame(rows)
    out.to_csv(os.path.join(OUT_DIR, "emp_economic_activity_sex.csv"), index=False)
    print("  emp_economic_activity_sex.csv -> %d rows" % len(out))
    return out


if __name__ == "__main__":
    print("Reading: " + EXCEL_PATH)
    print("Writing to: " + OUT_DIR + "\n")

    parse_lf_indicators_sex()
    parse_lf_indicators_youth()
    parse_occupations_sex()
    parse_status_sex()
    parse_education_sex()
    parse_agri_sex()
    parse_economic_activity_sex()

    print("\nDone.")
