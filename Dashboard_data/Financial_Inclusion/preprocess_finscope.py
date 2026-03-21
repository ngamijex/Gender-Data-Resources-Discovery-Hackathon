"""
preprocess_finscope.py
----------------------
One-time script to convert the raw FinScope 2024 Rwanda Stata file (.dta) into
a lightweight CSV with value labels decoded.  The CSV is what the Shiny app loads.

Run this script whenever the source DTA changes:
  python dashboard_data/Financial_Inclusion/preprocess_finscope.py
"""

import os, sys
import pyreadstat
import pandas as pd

DTA  = os.path.join(os.path.dirname(__file__), "FinScope 2024 Rwanda.dta")
OUT  = os.path.join(os.path.dirname(__file__), "finscope_2024_clean.csv")

# Columns needed by the Shiny dashboard
KEEP = [
    "b1", "b2",                                    # age, sex
    "a1", "a2", "a6",                              # province, district, urban/rural
    "qf1_01", "qf1_02", "qf1_03", "qf1_04",       # ever used: bank, digital, MFI, NDFI
    "qf1_05", "qf1_06", "qf1_07", "qf1_08",       # ever used: mobile money, SACCO, insurance, pension
    "qf1_09", "qf1_10", "qf1_11", "qf1_12",       # ever used: EJOHEZA, savings group, village assoc, none
    "qf4_01", "qf4_05", "qf4_06",                  # currently using: bank, mobile money, SACCO
    "l1",                                           # mobile money account (section L)
    "k1",                                           # has credit
    "e2c", "e7",                                    # savings frequency, income covers costs
    "e9a", "e9b",                                   # financial trend, emergency fund
    "e1c", "e2a", "e2e"                             # financial control, own money, goal progress
]

def main():
    if not os.path.exists(DTA):
        sys.exit(f"ERROR: DTA not found at {DTA}")

    print("Reading DTA …")
    df, meta = pyreadstat.read_dta(DTA)
    print(f"Loaded: {df.shape[0]:,} rows × {df.shape[1]:,} cols")

    keep = [c for c in KEEP if c in df.columns]
    df_clean = df[keep].copy()

    # Apply value labels
    for col in df_clean.columns:
        labels = meta.variable_value_labels.get(col, {})
        if labels:
            df_clean[col] = df_clean[col].map(labels).fillna(df_clean[col])

    df_clean.to_csv(OUT, index=False)
    size_mb = os.path.getsize(OUT) / 1024 / 1024
    print(f"Saved: {OUT}  ({size_mb:.2f} MB, {df_clean.shape[1]} columns)")

if __name__ == "__main__":
    main()
