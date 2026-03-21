import os
import re
import shutil
from pathlib import Path

import pandas as pd


def norm_cell(x):
    if pd.isna(x):
        return None
    s = str(x).replace("\u00a0", " ").strip()
    if s == "":
        return None
    return s


def to_num(x):
    if pd.isna(x):
        return None
    v = pd.to_numeric(x, errors="coerce")
    if pd.isna(v):
        return None
    return float(v)


def extract_table_1_population_by_sex(tmp_excel_path: str):
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 1", header=None)

    province_candidates = set()
    # Province-like labels in this table include "City of Kigali" and strings containing "Province".
    for lab in raw.iloc[:, 1].dropna().unique().tolist():
        s = norm_cell(lab)
        if s is None:
            continue
        if s == "City of Kigali" or ("Province" in s):
            province_candidates.add(s)

    rows = []
    current_prov = None
    for i in range(raw.shape[0]):
        geo = norm_cell(raw.iloc[i, 1])
        if geo is None:
            continue

        # Counts columns (in this table):
        # - col 2: both sexes
        # - col 3: male
        # - col 4: female
        male = to_num(raw.iloc[i, 3])
        female = to_num(raw.iloc[i, 4])
        both = to_num(raw.iloc[i, 2])

        # Skip header/metadata rows (non-numeric).
        if male is None and female is None and both is None:
            continue

        # "Rwanda" is the overall total; we use it for "All" only.
        if geo == "Rwanda":
            continue

        is_province = geo in province_candidates
        if is_province:
            current_prov = geo
            geo_type = "Province"
            province = geo
        else:
            if current_prov is None:
                continue
            geo_type = "District"
            province = current_prov

        if male is not None:
            rows.append(
                {
                    "geo": geo,
                    "geo_type": geo_type,
                    "province": province,
                    "sex": "Male",
                    "value": male,
                }
            )
        if female is not None:
            rows.append(
                {
                    "geo": geo,
                    "geo_type": geo_type,
                    "province": province,
                    "sex": "Female",
                    "value": female,
                }
            )

    return pd.DataFrame(rows)


def extract_table_4_population_change_by_sex_year(tmp_excel_path: str):
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 4", header=None)

    rows = []
    for i in range(raw.shape[0]):
        year = to_num(raw.iloc[i, 1])
        if year is None:
            continue
        if year < 1970 or year > 2035:
            continue

        male = to_num(raw.iloc[i, 3])
        female = to_num(raw.iloc[i, 4])

        if male is not None:
            rows.append({"year": int(year), "sex": "Male", "value": male})
        if female is not None:
            rows.append({"year": int(year), "sex": "Female", "value": female})

    return pd.DataFrame(rows)


def extract_table_5_age_distribution_by_sex_residence(tmp_excel_path: str):
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 5", header=None)

    # Column mapping (0-based indices, from pandas header=None reading):
    # col 1: age group label
    # Rwanda: male=3, female=4
    # Urban:  male=6, female=7
    # Rural:  male=9, female=10
    rows = []
    for i in range(raw.shape[0]):
        age_group = norm_cell(raw.iloc[i, 1])
        if age_group is None:
            continue

        # Skip header bits
        if age_group in ["Age group", "group", "�Age"]:
            continue

        # Keep only plausible age labels
        if not (
            age_group == "Total"
            or re.match(r"^\d{1,2}-\d{1,2}$", age_group)
            or re.match(r"^\d{1,2}$", age_group)
            or age_group.endswith("+")
        ):
            continue

        male_rwanda = to_num(raw.iloc[i, 3])
        female_rwanda = to_num(raw.iloc[i, 4])
        male_urban = to_num(raw.iloc[i, 6])
        female_urban = to_num(raw.iloc[i, 7])
        male_rural = to_num(raw.iloc[i, 9])
        female_rural = to_num(raw.iloc[i, 10])

        for residence, mval, fval in [
            ("Rwanda", male_rwanda, female_rwanda),
            ("Urban", male_urban, female_urban),
            ("Rural", male_rural, female_rural),
        ]:
            if mval is not None:
                rows.append(
                    {
                        "age_group": age_group,
                        "residence": residence,
                        "sex": "Male",
                        "value": mval,
                    }
                )
            if fval is not None:
                rows.append(
                    {
                        "age_group": age_group,
                        "residence": residence,
                        "sex": "Female",
                        "value": fval,
                    }
                )

    return pd.DataFrame(rows)


def extract_table_10_education_attendance_by_sex_residence(tmp_excel_path: str):
    """
    Table 10: Population aged 3 year and above by highest level of educational attended,
    sex and residence.

    This sheet has sections by residence (Rwanda / Urban / Rural). For each education
    level row we extract Male/Female counts and percentages.
    """
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 10", header=None)

    rows = []
    residence = None

    for i in range(raw.shape[0]):
        lab = norm_cell(raw.iloc[i, 1])
        if lab is None:
            continue

        # Residence section headers.
        if lab in ["Rwanda", "Urban", "Rural"]:
            residence = lab
            continue

        # Skip header/metadata rows.
        if lab in ["Level of education attended", "Counts", "Percentages", "Both sexes"]:
            continue
        if residence is None:
            continue

        male_count = to_num(raw.iloc[i, 3])
        female_count = to_num(raw.iloc[i, 4])
        male_pct = to_num(raw.iloc[i, 6])
        female_pct = to_num(raw.iloc[i, 7])

        # Skip non-data rows.
        if all(v is None for v in [male_count, female_count, male_pct, female_pct]):
            continue

        edu_level = lab
        if male_count is not None:
            rows.append(
                {
                    "residence": residence,
                    "education_level": edu_level,
                    "sex": "Male",
                    "count": male_count,
                    "pct": male_pct,
                }
            )
        if female_count is not None:
            rows.append(
                {
                    "residence": residence,
                    "education_level": edu_level,
                    "sex": "Female",
                    "count": female_count,
                    "pct": female_pct,
                }
            )

    return pd.DataFrame(rows)


def extract_table_20_internet_use_by_age_province_residence_sex(tmp_excel_path: str):
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 20", header=None)

    # Table 20 uses sections:
    # - A row containing "Population <N> years and above" sets the age_group section
    # - Following rows contain province (col 1) and values for Total/Urban/Rural + Male/Female.
    rows = []
    current_age_group = None

    for i in range(raw.shape[0]):
        lab = norm_cell(raw.iloc[i, 1])
        if lab is None:
            continue

        if re.match(r"^Population\s+\d+\s+years\s+and\s+above$", lab):
            current_age_group = lab
            continue

        if current_age_group is None:
            continue

        province = lab

        # Value mapping for each row (0-based indices):
        # col 3: male total (Rwanda)
        # col 4: female total (Rwanda)
        # col 6: male urban
        # col 7: female urban
        # col 9: male rural
        # col10: female rural
        male_rwanda = to_num(raw.iloc[i, 3])
        female_rwanda = to_num(raw.iloc[i, 4])
        male_urban = to_num(raw.iloc[i, 6])
        female_urban = to_num(raw.iloc[i, 7])
        male_rural = to_num(raw.iloc[i, 9])
        female_rural = to_num(raw.iloc[i, 10])

        for residence, mval, fval in [
            ("Rwanda", male_rwanda, female_rwanda),
            ("Urban", male_urban, female_urban),
            ("Rural", male_rural, female_rural),
        ]:
            if mval is not None:
                rows.append(
                    {
                        "age_group": current_age_group,
                        "province": province,
                        "residence": residence,
                        "sex": "Male",
                        "value": mval,
                    }
                )
            if fval is not None:
                rows.append(
                    {
                        "age_group": current_age_group,
                        "province": province,
                        "residence": residence,
                        "sex": "Female",
                        "value": fval,
                    }
                )

    return pd.DataFrame(rows)


def sanitize_sheet_filename(name: str) -> str:
    s = re.sub(r"[^\w\-\.]+", "_", str(name).strip())
    s = s.strip("_") or "sheet"
    return s[:120]


def export_all_sheets_to_csv(tmp_excel_path: str, clean_dir: Path) -> Path:
    """
    Export every workbook sheet to clean/sheets/*.csv and write demo_sheet_manifest.csv.
    """
    sheets_dir = clean_dir / "sheets"
    sheets_dir.mkdir(parents=True, exist_ok=True)

    xl = pd.ExcelFile(tmp_excel_path)
    manifest_rows = []
    used_names: dict[str, int] = {}

    for sn in xl.sheet_names:
        safe = sanitize_sheet_filename(sn)
        if safe not in used_names:
            used_names[safe] = 0
            fname = safe + ".csv"
        else:
            used_names[safe] += 1
            fname = f"{safe}__{used_names[safe]}.csv"

        out_path = sheets_dir / fname
        try:
            df = pd.read_excel(tmp_excel_path, sheet_name=sn, header=None)
            df.to_csv(out_path, index=False, encoding="utf-8")
            manifest_rows.append(
                {
                    "sheet_name": sn,
                    "file": f"sheets/{fname}",
                    "rows": int(df.shape[0]),
                    "cols": int(df.shape[1]),
                }
            )
        except Exception as e:
            manifest_rows.append(
                {
                    "sheet_name": sn,
                    "file": "",
                    "rows": 0,
                    "cols": 0,
                    "error": str(e),
                }
            )

    manifest = pd.DataFrame(manifest_rows)
    manifest_path = clean_dir / "demo_sheet_manifest.csv"
    manifest.to_csv(manifest_path, index=False, encoding="utf-8")
    return manifest_path


def extract_table_3_intervention_age_groups_by_sex(tmp_excel_path: str):
    """
    Table 3: Number and population share (%) of intervention target age groups by sex.
    Cols: label, Both/M/F counts, Both/M/F %.
    """
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 3", header=None)

    rows = []
    for i in range(raw.shape[0]):
        label = norm_cell(raw.iloc[i, 1])
        if label is None:
            continue
        label = label.strip()
        if label.startswith("Table "):
            continue
        low = label.lower()
        if low in ("group", "specific age", "population"):
            continue

        both_c = to_num(raw.iloc[i, 2])
        male_c = to_num(raw.iloc[i, 3])
        female_c = to_num(raw.iloc[i, 4])
        both_p = to_num(raw.iloc[i, 5])
        male_p = to_num(raw.iloc[i, 6])
        female_p = to_num(raw.iloc[i, 7])

        if all(v is None for v in [both_c, male_c, female_c, both_p, male_p, female_p]):
            continue

        age_group = label
        for sex, c, p in [
            ("Both sexes", both_c, both_p),
            ("Male", male_c, male_p),
            ("Female", female_c, female_p),
        ]:
            if c is not None or p is not None:
                rows.append(
                    {
                        "age_group": age_group,
                        "sex": sex,
                        "count": c,
                        "pct": p,
                    }
                )

    return pd.DataFrame(rows)


def extract_table_11_school_attendance_7_18_by_sex_residence(tmp_excel_path: str):
    """
    Table 11: Population 7–18 by school attendance status, sex and residence.
    """
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 11", header=None)

    rows = []
    current_res = None
    for i in range(raw.shape[0]):
        c1 = norm_cell(raw.iloc[i, 1])
        c2 = norm_cell(raw.iloc[i, 2])
        if c1 in ["Rwanda", "Urban", "Rural"]:
            current_res = c1
        if c2 not in ["Both sexes", "Male", "Female"]:
            continue
        if current_res is None:
            continue

        sex = c2
        count = to_num(raw.iloc[i, 3])
        for metric, j in [
            ("No longer attending", 5),
            ("Currently attending", 6),
            ("Never attended", 7),
        ]:
            v = to_num(raw.iloc[i, j])
            if v is None:
                continue
            rows.append(
                {
                    "residence": current_res,
                    "sex": sex,
                    "attendance_status": metric,
                    "value_pct": v,
                    "population_count": count,
                }
            )

    return pd.DataFrame(rows)


def extract_triplet_geo_indicator_sheet(tmp_excel_path: str, sheet_name: str, indicator: str):
    """
    Tables 15 & 45: Province/District rows with Total / Urban / Rural blocks,
    each with Both sexes / Male / Female (cols 2–10, 0-based indices 2..10).
    """
    raw = pd.read_excel(tmp_excel_path, sheet_name=sheet_name, header=None)

    province_candidates = set()
    for lab in raw.iloc[:, 1].dropna().unique().tolist():
        s = norm_cell(lab)
        if s is None:
            continue
        if s == "City of Kigali" or ("Province" in s):
            province_candidates.add(s)

    rows = []
    current_prov = None
    for i in range(raw.shape[0]):
        geo = norm_cell(raw.iloc[i, 1])
        if geo is None:
            continue
        if geo in ["Province/District", "Province/ District", ""]:
            continue
        if geo.startswith("Table "):
            continue
        if geo in ["Both sexes", "Male", "Female", "Bothsexea"]:
            continue

        vals = [to_num(raw.iloc[i, j]) for j in range(2, 11)]
        if all(v is None for v in vals):
            continue

        if geo == "Rwanda":
            geo_type = "National"
            province = "Rwanda"
        elif geo in province_candidates:
            current_prov = geo
            geo_type = "Province"
            province = geo
        else:
            if current_prov is None:
                continue
            geo_type = "District"
            province = current_prov

        blocks = [("Rwanda", 0), ("Urban", 3), ("Rural", 6)]
        sexes = ["Both sexes", "Male", "Female"]
        for res_name, off in blocks:
            for si, sex in enumerate(sexes):
                v = vals[off + si]
                if v is not None:
                    rows.append(
                        {
                            "indicator": indicator,
                            "geo": geo,
                            "geo_type": geo_type,
                            "province": province,
                            "residence": res_name,
                            "sex": sex,
                            "value": v,
                        }
                    )

    return pd.DataFrame(rows)


def extract_table_50_elderly_share_by_sex_residence_geo(tmp_excel_path: str):
    raw = pd.read_excel(tmp_excel_path, sheet_name="Table 50", header=None)

    province_candidates = set()
    for lab in raw.iloc[:, 1].dropna().unique().tolist():
        s = norm_cell(lab)
        if s is None:
            continue
        if s == "City of Kigali" or ("Province" in s):
            province_candidates.add(s)

    rows = []
    current_prov = None
    for i in range(raw.shape[0]):
        geo = norm_cell(raw.iloc[i, 1])
        if geo is None:
            continue

        # Skip header bits
        if geo in ["Province/District", "Total"]:
            continue
        if geo == "Rwanda":
            continue

        male_total = to_num(raw.iloc[i, 3])
        female_total = to_num(raw.iloc[i, 4])
        male_urban = to_num(raw.iloc[i, 6])
        female_urban = to_num(raw.iloc[i, 7])
        male_rural = to_num(raw.iloc[i, 9])
        female_rural = to_num(raw.iloc[i, 10])

        # Skip rows with no usable numeric data
        if all(v is None for v in [male_total, female_total, male_urban, female_urban, male_rural, female_rural]):
            continue

        if geo in province_candidates:
            current_prov = geo
            geo_type = "Province"
            province = geo
        else:
            if current_prov is None:
                continue
            geo_type = "District"
            province = current_prov

        for residence, mval, fval in [
            ("Rwanda", male_total, female_total),
            ("Urban", male_urban, female_urban),
            ("Rural", male_rural, female_rural),
        ]:
            if mval is not None:
                rows.append(
                    {
                        "geo": geo,
                        "geo_type": geo_type,
                        "province": province,
                        "residence": residence,
                        "sex": "Male",
                        "value": mval,
                    }
                )
            if fval is not None:
                rows.append(
                    {
                        "geo": geo,
                        "geo_type": geo_type,
                        "province": province,
                        "residence": residence,
                        "sex": "Female",
                        "value": fval,
                    }
                )

    return pd.DataFrame(rows)


def main():
    demog_dir = Path(__file__).resolve().parent
    excel_path = demog_dir / "PHC5-2022_Main_Indicators.xlsx"
    clean_dir = demog_dir / "clean"
    clean_dir.mkdir(parents=True, exist_ok=True)

    tmp_path = demog_dir / "_tmp_PHC5_2022_read.xlsx"
    if tmp_path.exists():
        tmp_path.unlink()

    # Some Excel files can be locked in the user UI; read a local copy for stability.
    shutil.copy2(str(excel_path), str(tmp_path))

    try:
        pop_geo = extract_table_1_population_by_sex(str(tmp_path))
        pop_change = extract_table_4_population_change_by_sex_year(str(tmp_path))
        age_dist = extract_table_5_age_distribution_by_sex_residence(str(tmp_path))
        edu_att = extract_table_10_education_attendance_by_sex_residence(str(tmp_path))
        internet_use = extract_table_20_internet_use_by_age_province_residence_sex(str(tmp_path))
        elderly_share = extract_table_50_elderly_share_by_sex_residence_geo(str(tmp_path))
        intervention_age = extract_table_3_intervention_age_groups_by_sex(str(tmp_path))
        school_7_18 = extract_table_11_school_attendance_7_18_by_sex_residence(str(tmp_path))
        school_13_18_pct = extract_triplet_geo_indicator_sheet(
            str(tmp_path), "Table 15", "School attendance 13-18 (%)"
        )
        youth_share_pct = extract_triplet_geo_indicator_sheet(
            str(tmp_path), "Table 45", "Youth share of population (%)"
        )

        pop_geo.to_csv(clean_dir / "demo_population_geo_by_sex.csv", index=False, encoding="utf-8")
        pop_change.to_csv(clean_dir / "demo_population_change_by_sex_year.csv", index=False, encoding="utf-8")
        age_dist.to_csv(clean_dir / "demo_age_distribution.csv", index=False, encoding="utf-8")
        edu_att.to_csv(clean_dir / "demo_education_attendance.csv", index=False, encoding="utf-8")
        internet_use.to_csv(clean_dir / "demo_internet_use.csv", index=False, encoding="utf-8")
        elderly_share.to_csv(clean_dir / "demo_elderly_share.csv", index=False, encoding="utf-8")
        intervention_age.to_csv(clean_dir / "demo_intervention_age_groups.csv", index=False, encoding="utf-8")
        school_7_18.to_csv(clean_dir / "demo_school_7_18_attendance.csv", index=False, encoding="utf-8")
        school_13_18_pct.to_csv(clean_dir / "demo_school_13_18_by_geo.csv", index=False, encoding="utf-8")
        youth_share_pct.to_csv(clean_dir / "demo_youth_share_by_geo.csv", index=False, encoding="utf-8")

        export_all_sheets_to_csv(str(tmp_path), clean_dir)

        # Convenience mapping for cascading filters (province → districts)
        geo_map = (
            pop_geo[pop_geo["geo_type"].eq("District")]
            .loc[:, ["province", "geo"]]
            .drop_duplicates()
            .rename(columns={"geo": "district"})
        )
        geo_map.to_csv(clean_dir / "demo_geo_province_district_map.csv", index=False, encoding="utf-8")

        print("[Demography] Clean CSVs written to:", str(clean_dir))
    finally:
        # Windows/Excel may keep a lock briefly; ignore unlink failures.
        try:
            if tmp_path.exists():
                tmp_path.unlink()
        except OSError:
            pass


if __name__ == "__main__":
    main()

