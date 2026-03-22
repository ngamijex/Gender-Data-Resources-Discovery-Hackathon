"""
preprocess_agriculture.py
Extracts 10 clean CSVs from 'agriculture gender data.xlsx'
Run: python preprocess_agriculture.py
"""
import os, shutil, pathlib
import openpyxl
import pandas as pd

# ── paths ──────────────────────────────────────────────────────────────────
BASE    = pathlib.Path(__file__).parent
SRC     = BASE / "agriculture gender data.xlsx"
TMP     = BASE / "_tmp_agri_read.xlsx"
OUT_DIR = BASE / "clean"
OUT_DIR.mkdir(exist_ok=True)

shutil.copy2(SRC, TMP)
wb = openpyxl.load_workbook(TMP, read_only=True, data_only=True)

def rows(sheet_name):
    return list(wb[sheet_name].iter_rows(values_only=True))

def save(df, name):
    path = OUT_DIR / name
    df.to_csv(path, index=False, encoding="utf-8")
    print(f"  saved {path.name}  ({len(df)} rows)")

# ── Table 1: Agricultural households by sex of HH head ─────────────────────
def parse_hh_summary():
    data = [
        {"hh_type": "Total HH in Rwanda",      "total": 3312743, "male_headed": 2355298, "female_headed": 957445},
        {"hh_type": "Total Agricultural HH",   "total": 2280854, "male_headed": 1639073, "female_headed": 641781},
        {"hh_type": "% of Agricultural HH",    "total": 68.9,    "male_headed": 69.6,    "female_headed": 67.0},
    ]
    df = pd.DataFrame(data)
    save(df, "agri_hh_summary.csv")

# ── Table 2: Land ownership by category (2018 vs 2021) ─────────────────────
def parse_land_ownership():
    rs = rows("table 2")
    # data rows 7-9 → Jointly, Women, Men
    records = []
    for r in rs[7:10]:
        if r[1] and r[2] is not None:
            records.append({"owner_type": str(r[1]).strip(), "pct_2018": r[2], "pct_2021": r[3]})
    df = pd.DataFrame(records)
    save(df, "agri_land_ownership.csv")

# ── Table 3: Agricultural land access by land ownership & sex ───────────────
def parse_land_access():
    rs = rows("table 3")
    # rows 6-8: Rwanda, Male HH, Female HH
    records = []
    for r in rs[6:9]:
        if r[1]:
            records.append({
                "hh_type":        str(r[1]).strip(),
                "own_land":       r[2],
                "rented_land":    r[3],
                "complemented":   r[4],
            })
    df = pd.DataFrame(records)
    save(df, "agri_land_access.csv")

# ── Table 4: Right to land by sex ─────────────────────────────────────────
def parse_land_rights():
    rs = rows("table 4")
    # rows 6-7
    records = []
    for r in rs[6:8]:
        if r[1]:
            records.append({
                "right_type": str(r[1]).strip(),
                "rwanda": r[2],
                "male":   r[3],
                "female": r[4],
            })
    df = pd.DataFrame(records)
    save(df, "agri_land_rights.csv")

# ── Table 5: Extension services ───────────────────────────────────────────
def parse_extension():
    rs = rows("table 5")
    records = []
    for r in rs[4:16]:   # 12 service types
        if r[1] and r[2] is not None:
            records.append({
                "service":    str(r[1]).strip(),
                "total_pct":  r[2],
                "female_pct": r[3],
                "male_pct":   r[4],
            })
    df = pd.DataFrame(records)
    save(df, "agri_extension.csv")

# ── Table 6a: Community groups ────────────────────────────────────────────
def parse_community_groups():
    rs = rows("table 6")
    records = []
    for r in rs[9:12]:   # Rwanda, Male-headed, Female-headed
        if r[1]:
            records.append({
                "hh_type":              str(r[1]).strip(),
                "cooperatives":         r[2],
                "twigire_muhinzi":      r[3],
                "farmer_field_school":  r[4],
            })
    df = pd.DataFrame(records)
    save(df, "agri_community_groups.csv")

# ── Table 6b: Agricultural inputs by province & HH sex ────────────────────
def parse_inputs():
    rs = rows("table 6")
    records = []
    # Province rows 19-23
    province_map = {"Kigali": "City of Kigali", "South": "Southern Province",
                    "West": "Western Province", "North": "Northern Province",
                    "East": "Eastern Province"}
    for r in rs[17:27]:
        lbl = r[1]
        if not lbl or str(lbl).strip() in ("By Province", "By HHH sex"):
            continue
        clean = str(lbl).strip()
        group = province_map.get(clean, clean)
        records.append({
            "category":          group,
            "improved_seeds":    r[2],
            "organic_fert":      r[3],
            "inorganic_fert":    r[4],
            "pesticides":        r[5],
        })
    df = pd.DataFrame(records)
    save(df, "agri_inputs.csv")

# ── Table 7: Agricultural workers trend 2017-2022 ─────────────────────────
def parse_workers_trend():
    rs = rows("table 7")
    # r4 has years, r5-r7 have data
    years = [2017, 2018, 2019, 2020, 2021, 2022]
    worker_rows = rs[5:8]
    labels = ["Market-oriented", "Subsistence", "Market-oriented + Subsistence"]
    records = []
    for lbl, r in zip(labels, worker_rows):
        # r[2:8] = Female 2017-2022, r[8:14] = Male 2017-2022
        for i, yr in enumerate(years):
            records.append({"worker_type": lbl, "sex": "Female", "year": yr, "pct": r[2+i]})
            records.append({"worker_type": lbl, "sex": "Male",   "year": yr, "pct": r[8+i]})
    df = pd.DataFrame(records)
    save(df, "agri_workers_trend.csv")

# ── Table 8: Girinka program ───────────────────────────────────────────────
def parse_girinka():
    rs = rows("table 8")
    records = [
        {"indicator": "Benefited from Girinka (2020)",  "rwanda": 4.1,  "male": 3.8,  "female": 4.8},
        {"indicator": "Still have cow from Girinka",    "rwanda": 85.4, "male": 86.0, "female": 84.3},
        {"indicator": "Provided by Government",         "rwanda": 93.4, "male": 93.3, "female": 93.7},
        {"indicator": "Provided by NGO/Company",        "rwanda": 6.6,  "male": 6.7,  "female": 6.3},
    ]
    df = pd.DataFrame(records)
    save(df, "agri_girinka.csv")

# ── Table 9: Livestock by sex of HH head ─────────────────────────────────
def parse_livestock():
    rs = rows("table 9")
    records = []
    for r in rs[4:13]:   # 9 livestock types
        if r[1]:
            records.append({
                "livestock":      str(r[1]).strip(),
                "total":          r[2],
                "male_headed":    r[3],
                "female_headed":  r[4],
            })
    df = pd.DataFrame(records)
    save(df, "agri_livestock.csv")

# ── Run all ────────────────────────────────────────────────────────────────
print("Processing Agriculture Excel...")
parse_hh_summary()
parse_land_ownership()
parse_land_access()
parse_land_rights()
parse_extension()
parse_community_groups()
parse_inputs()
parse_workers_trend()
parse_girinka()
parse_livestock()
print("Done. All 10 CSVs written to", OUT_DIR)
wb.close()
