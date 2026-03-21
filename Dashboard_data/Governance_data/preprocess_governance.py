import os
import re
import shutil
from pathlib import Path

import pandas as pd


EXCEL_PATH = os.path.join(
    os.path.dirname(__file__),
    "Governace data -nisr.xlsx",
)

OUT_DIR = os.path.join(os.path.dirname(__file__), "clean")
os.makedirs(OUT_DIR, exist_ok=True)

# When preprocessing, read from a temp copy of the workbook (Windows file locks).
_active_excel_path = EXCEL_PATH


def to_num(x):
    """Best-effort conversion to float, handling Excel artifacts."""
    if x is None or (isinstance(x, float) and pd.isna(x)):
        return None
    s = str(x)
    # Remove non-numeric characters except digits, dot, minus.
    s = s.replace("\xa0", " ")
    s = re.sub(r"[^0-9\.\-]", "", s)
    if s.strip() == "":
        return None
    try:
        return float(s)
    except Exception:
        return None


def fix_misprinted_pct(v_f, v_m):
    """
    Some sheets contain obvious typos like 653.5 instead of 53.5.
    If one side is within [0, 100] and the other is > 100, try subtracting 600.
    """
    v_f = to_num(v_f)
    v_m = to_num(v_m)
    if v_f is None or v_m is None:
        return v_f, v_m

    if v_m > 100 and v_f <= 100 and v_m < 1000:
        cand = v_m - 600
        if 0 <= cand <= 1000:
            v_m = cand
    if v_f > 100 and v_m <= 100 and v_f < 1000:
        cand = v_f - 600
        if 0 <= cand <= 1000:
            v_f = cand

    return v_f, v_m


def load_sheet(sh):
    return pd.read_excel(_active_excel_path, sheet_name=sh, header=0)


def export_ministers_gender():
    # Sheet: Figure47
    df = load_sheet("Figure47")
    # Columns pattern: Unnamed: 0, <title+label>, Unnamed:2..Unnamed:6 (year/value columns)
    sex_col = df.columns[1]
    year_cols = list(df.columns[2:])

    years = [to_num(v) for v in df.iloc[0][year_cols]]
    female_row = df[df[sex_col].astype(str).str.contains("Female", na=False)]
    male_row = df[df[sex_col].astype(str).str.contains("Male", na=False)]
    if female_row.empty or male_row.empty:
        return

    female_vals = [to_num(v) for v in female_row.iloc[0][year_cols]]
    male_vals = [to_num(v) for v in male_row.iloc[0][year_cols]]

    out = []
    for year, vf, vm in zip(years, female_vals, male_vals):
        if year is None:
            continue
        vf, vm = fix_misprinted_pct(vf, vm)
        out.append({"indicator": "ministers_gender_equality", "year": int(year), "sex": "Female", "pct": vf})
        out.append({"indicator": "ministers_gender_equality", "year": int(year), "sex": "Male", "pct": vm})

    pd.DataFrame(out).to_csv(os.path.join(OUT_DIR, "governance_ministers_gender.csv"), index=False)


def export_parliament_gender():
    # Sheet: Figure50
    df = load_sheet("Figure50")
    sex_col = df.columns[1]
    year_cols = list(df.columns[2:])

    years = [to_num(v) for v in df.iloc[2][year_cols]]
    female_row = df[df[sex_col].astype(str).str.contains("Female", na=False)]
    male_row = df[df[sex_col].astype(str).str.contains("Male", na=False)]
    if female_row.empty or male_row.empty:
        return

    female_vals = [to_num(v) for v in female_row.iloc[0][year_cols]]
    male_vals = [to_num(v) for v in male_row.iloc[0][year_cols]]

    out = []
    for year, vf, vm in zip(years, female_vals, male_vals):
        if year is None:
            continue
        vf, vm = fix_misprinted_pct(vf, vm)
        out.append({"indicator": "parliament_senate_seats_gender", "year": int(year), "sex": "Female", "pct": vf})
        out.append({"indicator": "parliament_senate_seats_gender", "year": int(year), "sex": "Male", "pct": vm})

    pd.DataFrame(out).to_csv(os.path.join(OUT_DIR, "governance_parliament_gender.csv"), index=False)


def export_prosecutors_gender():
    # Sheet: Figure52
    df = load_sheet("Figure52")
    year_col = df.columns[1]
    female_col = df.columns[2]
    male_col = df.columns[3]

    # Years appear on rows where year_col is numeric
    out = []
    for _, row in df.iterrows():
        year = to_num(row[year_col])
        if year is None:
            continue
        # Keep only plausible years
        if not (1970 <= year <= 2035):
            continue

        vf = to_num(row[female_col])
        vm = to_num(row[male_col])
        vf, vm = fix_misprinted_pct(vf, vm)
        # Drop broken rows where one side is missing
        if vf is None or vm is None:
            continue
        out.append({"indicator": "national_prosecutors_gender", "year": int(year), "sex": "Female", "pct": vf})
        out.append({"indicator": "national_prosecutors_gender", "year": int(year), "sex": "Male", "pct": vm})

    pd.DataFrame(out).to_csv(os.path.join(OUT_DIR, "governance_prosecutors_gender.csv"), index=False)


def export_judiciary_representation():
    # Sheet: Figure53
    df = load_sheet("Figure53")
    # Row 2 has years at columns 2,4,6,8. Row 3 has sex labels at those columns.
    years = {}
    sex_pairs = []
    # Using column positions (not names) to avoid Unnamed labels surprises.
    for i_year, (c_f, c_m) in enumerate([(2, 3), (4, 5), (6, 7), (8, 9)]):
        year = to_num(df.iloc[2][df.columns[c_f]])
        if year is None:
            continue
        years[i_year] = int(year)
        sex_pairs.append((c_f, c_m, int(year)))

    out = []
    # Indicator rows start at 4
    for r in range(4, len(df)):
        indicator = df.iloc[r][df.columns[1]]
        if indicator is None or (isinstance(indicator, float) and pd.isna(indicator)):
            continue
        indicator = str(indicator).strip()
        if indicator == "" or indicator.lower().startswith("source"):
            continue

        for c_f, c_m, year in sex_pairs:
            vf = to_num(df.iloc[r][df.columns[c_f]])
            vm = to_num(df.iloc[r][df.columns[c_m]])
            vf, vm = fix_misprinted_pct(vf, vm)
            out.append({"indicator": "judiciary_representation_gender", "year": year, "entity": indicator, "sex": "Female", "pct": vf})
            out.append({"indicator": "judiciary_representation_gender", "year": year, "entity": indicator, "sex": "Male", "pct": vm})

    pd.DataFrame(out).to_csv(os.path.join(OUT_DIR, "governance_judiciary_gender.csv"), index=False)


def export_local_government_leaders():
    # Sheet: Table22
    df = load_sheet("Table22")
    # Column positions:
    # Unnamed:1 = position name
    # Year columns at 2,4,6,8,10 (M/F pairs for each year)
    pos_col = df.columns[1]
    year_m_cols = [2, 4, 6, 8, 10]
    year_f_cols = [c + 1 for c in year_m_cols]

    out = []
    for r in range(3, len(df)):
        position = df.iloc[r][pos_col]
        if position is None or (isinstance(position, float) and pd.isna(position)):
            continue
        position = str(position).strip()
        if position == "" or position.lower().startswith("source"):
            continue

        for c_m, c_f in zip(year_m_cols, year_f_cols):
            year = to_num(df.iloc[1][df.columns[c_m]])
            if year is None:
                continue
            vm = to_num(df.iloc[r][df.columns[c_m]])
            vf = to_num(df.iloc[r][df.columns[c_f]])
            # fix_misprinted_pct expects (v_f, v_m)
            vf, vm = fix_misprinted_pct(vf, vm)
            out.append({"indicator": "local_government_leaders_gender", "year": int(year), "entity": position, "sex": "Male", "pct": vm})
            out.append({"indicator": "local_government_leaders_gender", "year": int(year), "entity": position, "sex": "Female", "pct": vf})

    pd.DataFrame(out).to_csv(os.path.join(OUT_DIR, "governance_local_leaders_gender.csv"), index=False)


def sanitize_sheet_filename(name: str) -> str:
    s = re.sub(r"[^\w\-\.]+", "_", str(name).strip())
    s = s.strip("_") or "sheet"
    return s[:120]


def export_all_sheets_to_csv(tmp_excel_path: str, clean_dir: str) -> None:
    """Export every workbook sheet to clean/sheets/*.csv and governance_sheet_manifest.csv."""
    clean_path = Path(clean_dir)
    sheets_dir = clean_path / "sheets"
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

    pd.DataFrame(manifest_rows).to_csv(
        clean_path / "governance_sheet_manifest.csv", index=False, encoding="utf-8"
    )


def main():
    global _active_excel_path
    if not os.path.exists(EXCEL_PATH):
        raise FileNotFoundError(f"Governance Excel not found: {EXCEL_PATH}")

    tmp_path = os.path.join(os.path.dirname(EXCEL_PATH), "_tmp_governance_read.xlsx")
    try:
        shutil.copy2(EXCEL_PATH, tmp_path)
    except OSError:
        tmp_path = EXCEL_PATH

    _orig_active = _active_excel_path
    load_path = tmp_path if os.path.exists(tmp_path) and tmp_path != EXCEL_PATH else EXCEL_PATH
    try:
        _active_excel_path = load_path

        export_ministers_gender()
        export_parliament_gender()
        export_prosecutors_gender()
        export_judiciary_representation()
        export_local_government_leaders()

        export_all_sheets_to_csv(load_path, OUT_DIR)

        print("[Governance] Clean CSVs exported to:", OUT_DIR)
    finally:
        _active_excel_path = _orig_active
        try:
            if os.path.exists(tmp_path) and tmp_path != EXCEL_PATH:
                os.unlink(tmp_path)
        except OSError:
            pass


if __name__ == "__main__":
    main()

