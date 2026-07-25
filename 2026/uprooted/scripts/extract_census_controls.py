"""
Extract district-level urbanisation/wealth controls from Census 2022
for the Meta History project (referee revision — address African-neighbour placebo).

Outputs:
  Data/census_controls_by_district.csv — internet, formal housing, urbanisation, car
  ownership by census district code

Usage:
  python extract_census_controls.py
"""

import csv
import io
import os
import zipfile
from collections import defaultdict

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "Data")


def extract_controls():
    """Read Census 2022 household + geography CSVs, aggregate controls by district."""

    zip_path = os.path.join(DATA_DIR, "sa-census-2022-v1-csv.zip")
    if not os.path.exists(zip_path):
        print(f"ERROR: Census zip not found at {zip_path}")
        return

    zf = zipfile.ZipFile(zip_path)

    # --- Step 1: Build QID → District + Geo_type lookup from geography file ---
    print("Reading geography file...")
    qid_to_geo = {}
    with zf.open("sa-census-2022-geography-v1.csv") as f:
        reader = csv.DictReader(io.TextIOWrapper(f, encoding="latin-1"))
        for row in reader:
            qid_to_geo[row["QID"]] = {
                "district": row["District"],
                "geo_type": row["Geo_type"],
            }
    print(f"  Geography records: {len(qid_to_geo)}")

    # --- Step 2: Read household file, aggregate by district ---
    print("Reading household file...")

    # Accumulators per district
    district_stats = defaultdict(lambda: {
        "n_hh": 0,
        "wgt_sum": 0.0,
        "internet_wgt": 0.0,       # any internet access
        "internet_home_wgt": 0.0,  # home connection specifically
        "formal_wgt": 0.0,         # formal dwelling
        "car_wgt": 0.0,            # motor car
        "computer_wgt": 0.0,       # computer
        "cellphone_wgt": 0.0,      # cellphone
        "urban_wgt": 0.0,          # urban geo_type
    })

    internet_has_access = {
        "Use Cellphone or any other mobile device",
        "Home with an internet connection in the dwelling",
        "From an internet caf\x92",   # note: encoding artefact
        "Public Wi-Fi?",
        "From place of work",
        "From school/university/college",
        "From a library/community hall/Thusong centre",
        "Other",
    }

    # Also match partial strings for robustness against encoding variations
    internet_no_access = {"No access to internet services", "Unspecified"}

    n_matched = 0
    n_unmatched = 0

    with zf.open("sa-census-2022-household-v1.csv") as f:
        reader = csv.DictReader(io.TextIOWrapper(f, encoding="latin-1"))
        for row in reader:
            qid = row["QID"]
            geo = qid_to_geo.get(qid)
            if geo is None:
                n_unmatched += 1
                continue

            n_matched += 1
            district = geo["district"]
            geo_type = geo["geo_type"]

            # Household weight
            try:
                wgt = float(row["HH_WGT"])
            except (ValueError, TypeError):
                wgt = 1.0

            ds = district_stats[district]
            ds["n_hh"] += 1
            ds["wgt_sum"] += wgt

            # Internet access: anything other than "No access" and "Unspecified"
            internet_val = row.get("H13_INTERNET_ACCESS", "")
            if internet_val not in internet_no_access and internet_val.strip() != "":
                ds["internet_wgt"] += wgt

            if "Home with an internet connection" in internet_val:
                ds["internet_home_wgt"] += wgt

            # Formal dwelling
            dwelling = row.get("H02_MAINDWELLING", "")
            if "Formal dwelling" in dwelling or "formal dwelling" in dwelling:
                ds["formal_wgt"] += wgt

            # Motor car
            if row.get("H12_MOTOR_CAR", "") == "Yes":
                ds["car_wgt"] += wgt

            # Computer
            if row.get("H12_COMPUTER", "") == "Yes":
                ds["computer_wgt"] += wgt

            # Cellphone
            if row.get("H12_CELLPHONE", "") == "Yes":
                ds["cellphone_wgt"] += wgt

            # Urban
            if "Urban" in geo_type:
                ds["urban_wgt"] += wgt

    zf.close()

    print(f"  Matched households: {n_matched}")
    print(f"  Unmatched QIDs: {n_unmatched}")
    print(f"  Districts: {len(district_stats)}")

    # --- Step 3: Compute shares and write output ---
    out_path = os.path.join(DATA_DIR, "census_controls_by_district.csv")

    rows_out = []
    for district in sorted(district_stats.keys()):
        ds = district_stats[district]
        w = ds["wgt_sum"]
        if w <= 0:
            continue

        # Extract the district code (e.g., "1. CPT" → "CPT", "40. DC4" → "DC4")
        parts = district.split(". ", 1)
        district_code = parts[1] if len(parts) == 2 else parts[0]

        rows_out.append({
            "census_district": district,
            "district_code": district_code,
            "n_hh": ds["n_hh"],
            "pct_internet": round(ds["internet_wgt"] / w, 4),
            "pct_internet_home": round(ds["internet_home_wgt"] / w, 4),
            "pct_formal": round(ds["formal_wgt"] / w, 4),
            "pct_car": round(ds["car_wgt"] / w, 4),
            "pct_computer": round(ds["computer_wgt"] / w, 4),
            "pct_cellphone": round(ds["cellphone_wgt"] / w, 4),
            "pct_urban": round(ds["urban_wgt"] / w, 4),
        })

    fieldnames = [
        "census_district", "district_code", "n_hh",
        "pct_internet", "pct_internet_home", "pct_formal",
        "pct_car", "pct_computer", "pct_cellphone", "pct_urban",
    ]

    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows_out)

    print(f"\nOutput written to: {out_path}")
    print(f"Districts: {len(rows_out)}")

    # Summary statistics
    print("\nSummary statistics (weighted shares):")
    for var in ["pct_internet", "pct_internet_home", "pct_formal",
                "pct_car", "pct_computer", "pct_urban"]:
        vals = [r[var] for r in rows_out]
        mean_val = sum(vals) / len(vals) if vals else 0
        min_val = min(vals) if vals else 0
        max_val = max(vals) if vals else 0
        print(f"  {var:20s}: mean={mean_val:.3f}  min={min_val:.3f}  max={max_val:.3f}")

    # Show top/bottom 5 by internet access
    rows_sorted = sorted(rows_out, key=lambda x: x["pct_internet"], reverse=True)
    print("\nTop 5 districts by internet access:")
    for r in rows_sorted[:5]:
        print(f"  {r['district_code']:6s}: {r['pct_internet']:.3f}")
    print("Bottom 5:")
    for r in rows_sorted[-5:]:
        print(f"  {r['district_code']:6s}: {r['pct_internet']:.3f}")


if __name__ == "__main__":
    extract_controls()
