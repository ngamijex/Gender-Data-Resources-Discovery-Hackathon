# preprocess_education.py
# Extracts tables from Education_Thematic_Report.xlsx into tidy CSVs
# NOTE: All tables have labels in col[1], data in col[2]+. Col[0] is always NaN.

import os, re, shutil
import pandas as pd
import numpy as np

BASE_DIR  = os.path.dirname(os.path.abspath(__file__))
EXCEL_SRC = os.path.join(BASE_DIR, "Education_Thematic_Report.xlsx")
OUT_DIR   = os.path.join(BASE_DIR, "clean")
os.makedirs(OUT_DIR, exist_ok=True)

TMP = os.path.join(os.environ.get("TEMP", BASE_DIR), "edu_preprocess_tmp.xlsx")
shutil.copy2(EXCEL_SRC, TMP)
XL  = TMP

print("Reading: " + EXCEL_SRC)
print("Writing to: " + OUT_DIR)
print()

# ── helpers ────────────────────────────────────────────────────────────────────
def safe_num(v):
    if v is None: return np.nan
    s = str(v).strip().replace(",","").replace("\xa0","").replace(" ","")
    s = s.replace("-\ufffd","").replace("\ufffd-","")
    if s in ("","nan","-","*","...","NS","notstated"): return np.nan
    # handle things like "4 1.4" (space inside number due to cell merge)
    s = re.sub(r"\s+", "", s)
    try: return float(s)
    except: return np.nan

def lbl(row): return str(row[1]).strip().lstrip()
def num(row, i): return safe_num(row[i])

LEVEL_MAP_31 = {
    "never attended": "No Education",
    "pre-nursary": "Pre-Nursery/ECD",
    "pre-primary": "Pre-primary",
    "primary": "Primary",
    "ingoboka": "Vocational/INGOBOKA",
    "lower secondary": "Lower Secondary",
    "upper secondary": "Upper Secondary",
    "university": "University/Higher",
}

LEVEL_MAP_41 = {
    "no primary": "No Primary Schooling",
    "some primary": "Some Primary",
    "completed primary": "Primary Completed",
    "lower secondary": "Lower Secondary",
    "upper secondary": "Upper Secondary",
    "short cycle": "Short Cycle Tertiary",
    "bachelor": "Bachelor",
    "masters": "Masters",
    "doctoral": "Doctoral",
}

TREND_COLS = {2: "No Education", 3: "Primary", 4: "Post-Primary", 5: "Secondary", 6: "University"}

AREA_LABELS = ("Rwanda", "Urban", "Rural")
SEX_LABELS  = ("Both sexes", "Male", "Female")

def is_area(s):
    return s in AREA_LABELS or "province" in s.lower() or "kigali" in s.lower()

def map_province(s):
    sl = s.lower()
    if "total" in sl and len(s) < 12: return "Rwanda"
    if "kigali" in sl: return "City of Kigali"
    if "southern" in sl: return "Southern Province"
    if "western" in sl: return "Western Province"
    if "northern" in sl: return "Northern Province"
    if "eastern" in sl: return "Eastern Province"
    return None

# ── Table 3.1: Education level (all pop) by sex & area ──────────────────────
def parse_attainment_area():
    df = pd.read_excel(XL, sheet_name="Table 3. 1", header=None)
    rows = []; area = None
    for _, row in df.iterrows():
        v = lbl(row)
        if v.lower() in ("nan","","total","not stated"): continue
        if is_area(v): area = v; continue
        # level?
        vl = v.lower()
        level = None
        for k, nm in LEVEL_MAP_31.items():
            if k in vl: level = nm; break
        if level is None: continue
        for sex, c_cnt, c_pct in [("Both sexes",2,5),("Male",3,6),("Female",4,7)]:
            rows.append({"area": area, "sex": sex, "level": level,
                         "count": num(row, c_cnt), "pct": num(row, c_pct)})
    out = pd.DataFrame(rows).dropna(subset=["pct"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attainment_area.csv"), index=False)
    print("  edu_attainment_area.csv -> " + str(len(out)) + " rows")

# ── Table 3.3: Historical census trend by sex ────────────────────────────────
def parse_attainment_trend():
    df = pd.read_excel(XL, sheet_name="Table 3. 3", header=None)
    rows = []; sex = None
    for _, row in df.iterrows():
        v = lbl(row)
        if v.lower() in ("nan", ""): continue
        if v in SEX_LABELS: sex = v; continue
        yr = safe_num(v)
        if np.isnan(yr): continue
        year = int(yr)
        for ci, lv in TREND_COLS.items():
            val = num(row, ci)
            if not np.isnan(val):
                rows.append({"year": year, "sex": sex, "level": lv, "pct": val})
    out = pd.DataFrame(rows).dropna(subset=["pct"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attainment_trend.csv"), index=False)
    print("  edu_attainment_trend.csv -> " + str(len(out)) + " rows")

# ── Table 3.4: Education by sex & disability ────────────────────────────────
def parse_attainment_disability():
    df = pd.read_excel(XL, sheet_name="Table 3. 4", header=None)
    # Row 1: col[1]=header, col[2]=Total(skip), col[3+]=level labels
    hdr = df.iloc[1]
    level_cols = {}
    for ci in range(3, len(hdr)):
        s = str(hdr[ci]).strip()
        if s.lower() in ("nan","","total"): continue
        sl = s.lower()
        lv = s
        for k, nm in LEVEL_MAP_31.items():
            if k in sl: lv = nm; break
        level_cols[ci] = lv
    rows = []; sex = None; in_pct = True
    for _, row in df.iterrows():
        v = lbl(row)
        vl = v.lower().strip()
        if "count" in vl: in_pct = False
        if not in_pct: continue
        if v in SEX_LABELS: sex = v; continue
        if vl in ("nan","","percentage","sex and disability sta","sex and disability"): continue
        if "without" in vl: disab = "Without disability"
        elif "with disab" in vl: disab = "With disability"
        elif "total" in vl: disab = "Total"
        else: continue
        for ci, lv in level_cols.items():
            val = num(row, ci)
            if not np.isnan(val):
                rows.append({"sex": sex, "disability": disab, "level": lv, "pct": val})
    out = pd.DataFrame(rows).dropna(subset=["pct"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attainment_disability.csv"), index=False)
    print("  edu_attainment_disability.csv -> " + str(len(out)) + " rows")

# ── Tables 4.1-4.3: 15+ educational attainment by sex & area ────────────────
def parse_attainment_15plus():
    area_sheets = [("Table 4. 1","Rwanda"),("Table 4. 2","Urban"),("Table 4. 3","Rural")]
    rows = []
    for sname, area in area_sheets:
        df = pd.read_excel(XL, sheet_name=sname, header=None)
        for _, row in df.iterrows():
            v = lbl(row)
            if v.lower() in ("nan","","total","not stated"): continue
            vl = v.lower()
            level = None
            for k, nm in LEVEL_MAP_41.items():
                if k in vl: level = nm; break
            if level is None: continue
            for sex, c_cnt, c_pct in [("Both sexes",2,5),("Male",3,6),("Female",4,7)]:
                rows.append({"area": area, "sex": sex, "level": level,
                             "count": num(row, c_cnt), "pct": num(row, c_pct)})
    out = pd.DataFrame(rows).dropna(subset=["pct"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attainment_15plus.csv"), index=False)
    print("  edu_attainment_15plus.csv -> " + str(len(out)) + " rows")

# ── Table 5.2: School attendance (3-17) by sex & area ───────────────────────
def parse_attendance_status():
    df = pd.read_excel(XL, sheet_name="Table 5. 2", header=None)
    # col[1]=area (or NaN continuation), col[2]=sex, col[3]=total,
    # col[4]=no_longer, col[5]=currently, col[6]=never, col[7]=not_stated
    # col[9]=pct_no_longer, col[10]=pct_currently, col[11]=pct_never
    rows = []; area = None
    for _, row in df.iterrows():
        v1 = str(row[1]).strip() if len(row) > 1 else "nan"
        v2 = str(row[2]).strip() if len(row) > 2 else "nan"
        if v1 in AREA_LABELS: area = v1
        sex = v2 if v2 in SEX_LABELS else (v1 if v1 in SEX_LABELS else None)
        if sex is None: continue
        rows.append({
            "area": area, "sex": sex,
            "total":         num(row, 3),
            "no_longer":     num(row, 4),
            "currently":     num(row, 5),
            "never":         num(row, 6),
            "pct_no_longer": num(row, 9),
            "pct_currently": num(row, 10),
            "pct_never":     num(row, 11),
        })
    out = pd.DataFrame(rows).dropna(subset=["total"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attendance_status.csv"), index=False)
    print("  edu_attendance_status.csv -> " + str(len(out)) + " rows")

# ── Table 5.3: Pre-primary (3-5 yr) attendance by sex & area ────────────────
def parse_attendance_3to5():
    df = pd.read_excel(XL, sheet_name="Table 5. 3", header=None)
    # col[1]=sex, col[2]=total, col[3]=currently, col[4]=not_yet, col[5]=not_stated,
    # col[6]=total_pct, col[7]=pct_currently, col[8]=pct_not_yet
    rows = []; area = None
    for _, row in df.iterrows():
        v = lbl(row)
        if v in AREA_LABELS: area = v; continue
        if v not in SEX_LABELS: continue
        rows.append({
            "area": area, "sex": v,
            "total": num(row, 2), "currently": num(row, 3), "not_yet": num(row, 4),
            "pct_attending": num(row, 7),
        })
    out = pd.DataFrame(rows).dropna(subset=["total"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attendance_3to5.csv"), index=False)
    print("  edu_attendance_3to5.csv -> " + str(len(out)) + " rows")

# ── Table 5.5: Primary-school-age (6-17) attendance by sex & area ────────────
def parse_attendance_6to17():
    df = pd.read_excel(XL, sheet_name="Table 5. 5", header=None)
    # col[1]=sex (area in col[1] with no data), col[2]=total, col[3]=prev, col[4]=currently,
    # col[5]=never, col[6]=not_stated, col[7]=pct_total, col[8]=pct_prev,
    # col[9]=pct_currently, col[10]=pct_never
    rows = []; area = None
    for _, row in df.iterrows():
        v = lbl(row)
        if v in AREA_LABELS: area = v; continue
        if v not in SEX_LABELS: continue
        rows.append({
            "area": area, "sex": v,
            "total": num(row, 2), "previously": num(row, 3),
            "currently": num(row, 4), "never": num(row, 5),
            "pct_previously": num(row, 8),
            "pct_currently":  num(row, 9),
            "pct_never":      num(row, 10),
        })
    out = pd.DataFrame(rows).dropna(subset=["total"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attendance_6to17.csv"), index=False)
    print("  edu_attendance_6to17.csv -> " + str(len(out)) + " rows")

# ── Table 5.16: Attendance rate by age group and sex ────────────────────────
def parse_attendance_agegroup():
    df = pd.read_excel(XL, sheet_name="Table 5. 16", header=None)
    # col[1]=age_group, col[2-4]=total BS/M/F, col[5-7]=attending BS/M/F, col[8-10]=pct BS/M/F
    rows = []
    AGE_MAP = {
        "3 to 5": "3-5 years", "6 to 11": "6-11 years",
        "12 to 1": "12-17 years", "total population age 3": "3-17 years (total)"
    }
    for _, row in df.iterrows():
        v = lbl(row)
        vl = v.lower()
        if vl in ("nan",""): continue
        age_grp = None
        for k, nm in AGE_MAP.items():
            if k in vl: age_grp = nm; break
        if age_grp is None: continue
        for sex, c_tot, c_att, c_pct in [
            ("Both sexes",2,5,8), ("Male",3,6,9), ("Female",4,7,10)
        ]:
            rows.append({"age_group": age_grp, "sex": sex,
                         "total": num(row, c_tot), "attending": num(row, c_att),
                         "pct": num(row, c_pct)})
    out = pd.DataFrame(rows).dropna(subset=["pct"])
    out.to_csv(os.path.join(OUT_DIR, "edu_attendance_agegroup.csv"), index=False)
    print("  edu_attendance_agegroup.csv -> " + str(len(out)) + " rows")

# ── Table 7.1: Literacy by sex & area ───────────────────────────────────────
def parse_literacy_sex():
    df = pd.read_excel(XL, sheet_name="Table 7. 1", header=None)
    # col[1]=label, col[2]=illiterate, col[3]=literate, col[4]=total,
    # col[5]=pct_illiterate, col[6]=pct_literate
    rows = []
    KEEP = {"Rwanda": ("Rwanda","Both sexes"), "Male": (None,"Male"), "Female": (None,"Female"),
            "Urban": ("Urban","Both sexes"), "Rural": ("Rural","Both sexes")}
    area = "Rwanda"
    for _, row in df.iterrows():
        v = lbl(row)
        if v == "Rwanda":
            area = "Rwanda"
            # This row also has national Both sexes data
            rows.append({"area":"Rwanda","sex":"Both sexes",
                         "illiterate": num(row,2), "literate": num(row,3),
                         "total": num(row,4), "pct_literate": num(row,6)})
        elif v == "Urban":
            area = "Urban"
            rows.append({"area":"Urban","sex":"Both sexes",
                         "illiterate": num(row,2), "literate": num(row,3),
                         "total": num(row,4), "pct_literate": num(row,6)})
        elif v == "Rural":
            area = "Rural"
            rows.append({"area":"Rural","sex":"Both sexes",
                         "illiterate": num(row,2), "literate": num(row,3),
                         "total": num(row,4), "pct_literate": num(row,6)})
        elif v in ("Male","Female"):
            rows.append({"area": area, "sex": v,
                         "illiterate": num(row,2), "literate": num(row,3),
                         "total": num(row,4), "pct_literate": num(row,6)})
    out = pd.DataFrame(rows).dropna(subset=["pct_literate"])
    out.to_csv(os.path.join(OUT_DIR, "edu_literacy_sex.csv"), index=False)
    print("  edu_literacy_sex.csv -> " + str(len(out)) + " rows")

# ── Tables 7.2-7.4: Literacy by age group for BS, Male, Female ──────────────
def parse_literacy_age():
    sex_sheets = [("Table 7. 2","Both sexes"), ("Table 7. 3","Male")]
    try:
        pd.read_excel(XL, sheet_name="Table 7. 4", header=None, nrows=3)
        sex_sheets.append(("Table 7. 4","Female"))
    except Exception:
        pass
    rows = []
    for sname, sex in sex_sheets:
        df = pd.read_excel(XL, sheet_name=sname, header=None)
        for _, row in df.iterrows():
            v = lbl(row)
            if v.lower() in ("nan","","total","rwanda","male","female","both sexes"): continue
            if re.match(r"^\d+[-]\d+$", v.strip()) or v.strip() == "85+":
                rows.append({"age_group": v.strip(), "sex": sex,
                             "illiterate": num(row,2), "literate": num(row,3),
                             "total": num(row,4), "pct_literate": num(row,6)})
    out = pd.DataFrame(rows).dropna(subset=["pct_literate"])
    out.to_csv(os.path.join(OUT_DIR, "edu_literacy_age.csv"), index=False)
    print("  edu_literacy_age.csv -> " + str(len(out)) + " rows")

# ── Table 6.2: ICT literacy % by province & sex ─────────────────────────────
def parse_ict_province():
    df = pd.read_excel(XL, sheet_name="Table 6. 2", header=None)
    # col[1]=label, col[2]=BS_nat, col[3]=M_nat, col[4]=F_nat,
    # col[5]=BS_urb, col[6]=M_urb, col[7]=F_urb, col[8]=BS_rur, col[9]=M_rur, col[10]=F_rur
    rows = []; age_grp = None
    for _, row in df.iterrows():
        v = lbl(row)
        vl = v.lower()
        if "10 year" in vl: age_grp = "10+"; continue
        if "16 year" in vl: age_grp = "16+"; continue
        if "21 year" in vl: age_grp = "21+"; continue
        if vl in ("nan","","percentage","counts"): continue
        prov = map_province(v)
        if prov is None: continue
        for sex, ci_nat, ci_urb, ci_rur in [
            ("Both sexes",2,5,8), ("Male",3,6,9), ("Female",4,7,10)
        ]:
            rows.append({"province": prov, "age_group": age_grp or "10+", "sex": sex,
                         "pct_nat": num(row, ci_nat),
                         "pct_urb": num(row, ci_urb),
                         "pct_rur": num(row, ci_rur)})
    out = pd.DataFrame(rows).dropna(subset=["pct_nat"])
    out.to_csv(os.path.join(OUT_DIR, "edu_ict_province.csv"), index=False)
    print("  edu_ict_province.csv -> " + str(len(out)) + " rows")

# ── Table 6.5: ICT count by province & sex ──────────────────────────────────
def parse_ict_count():
    df = pd.read_excel(XL, sheet_name="Table 6 .5", header=None)
    rows = []; in_counts = False; done_counts = False
    for _, row in df.iterrows():
        v = lbl(row)
        vl = v.lower()
        if "count" in vl: in_counts = True; continue
        if "percent" in vl and in_counts: done_counts = True
        if done_counts: continue
        if not in_counts: continue
        if "10 year" in vl or "16 year" in vl or "21 year" in vl: continue
        if vl in ("nan","",""): continue
        prov = map_province(v)
        if prov is None: continue
        for sex, ci_nat, ci_urb, ci_rur in [
            ("Both sexes",2,5,8), ("Male",3,6,9), ("Female",4,7,10)
        ]:
            rows.append({"province": prov, "sex": sex,
                         "count_nat": num(row, ci_nat),
                         "count_urb": num(row, ci_urb),
                         "count_rur": num(row, ci_rur)})
    out = pd.DataFrame(rows).dropna(subset=["count_nat"])
    out.to_csv(os.path.join(OUT_DIR, "edu_ict_count.csv"), index=False)
    print("  edu_ict_count.csv -> " + str(len(out)) + " rows")

# ── Table 6.6: Mobile phone type by sex & area ──────────────────────────────
def parse_mobile_phone():
    df = pd.read_excel(XL, sheet_name="Table 6. 6", header=None)
    # col[1]=type/area, col[2]=BS_10+, col[3]=M_10+, col[4]=F_10+
    PHONE_MAP = {
        "smart phone": "Smartphone",
        "ordinary phone with r": "Basic Phone (with radio)",
        "ordinary phone wit": "Basic Phone (no radio)",
    }
    rows = []; area = "Rwanda"
    for _, row in df.iterrows():
        v = lbl(row)
        if v in AREA_LABELS: area = v; continue
        vl = v.lower()
        if vl in ("nan","","counts"): continue
        phone = None
        for k, nm in PHONE_MAP.items():
            if k in vl: phone = nm; break
        if phone is None: continue
        for sex, ci in [("Both sexes",2),("Male",3),("Female",4)]:
            val = num(row, ci)
            if not np.isnan(val):
                rows.append({"area": area, "sex": sex, "phone_type": phone, "count": val})
    out = pd.DataFrame(rows).dropna(subset=["count"])
    out.to_csv(os.path.join(OUT_DIR, "edu_mobile_phone.csv"), index=False)
    print("  edu_mobile_phone.csv -> " + str(len(out)) + " rows")

# ── run all ────────────────────────────────────────────────────────────────────
for fn in [
    parse_attainment_area, parse_attainment_trend, parse_attainment_15plus,
    parse_attainment_disability, parse_attendance_status, parse_attendance_3to5,
    parse_attendance_6to17, parse_attendance_agegroup, parse_literacy_sex,
    parse_literacy_age, parse_ict_province, parse_ict_count, parse_mobile_phone,
]:
    try:
        fn()
    except Exception as e:
        import traceback
        print("  ERROR in " + fn.__name__ + ": " + str(e))
        traceback.print_exc()

print()
print("Done.")
