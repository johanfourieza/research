"""
Extract Census 2022 language shares by district and process slave emancipation
farm-name matching for the Meta History project.

Outputs:
  data/language_by_district.csv  — language shares per Census district
  data/slave_origins_by_district.csv — slave origin shares per GADM2 district

Usage:
  python extract_census_language.py
"""

import csv
import io
import math
import os
import zipfile
from collections import Counter, defaultdict

import openpyxl

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "Data")


# ============================================================================
# PART 1: Census 2022 Language Shares by District
# ============================================================================

def extract_language_shares():
    """Read Census 2022 person + geography CSVs, aggregate language by district."""

    zip_path = os.path.join(DATA_DIR, "sa-census-2022-v1-csv.zip")
    if not os.path.exists(zip_path):
        print(f"WARNING: Census zip not found at {zip_path}, skipping language extraction")
        return

    print("Reading Census 2022 data from zip...")

    # Step 1: Read geography file to build QID -> district_code lookup
    qid_to_district = {}
    with zipfile.ZipFile(zip_path) as z:
        print("  Loading geography file...")
        with z.open("sa-census-2022-geography-v1.csv") as f:
            reader = csv.DictReader(io.TextIOWrapper(f, encoding="utf-8"))
            for row in reader:
                # District format: "1. CPT", "10. DC10" -> strip prefix
                raw = row["District"]
                # Split on ". " to get code after the number prefix
                parts = raw.split(". ", 1)
                district_code = parts[1] if len(parts) == 2 else raw
                qid_to_district[row["QID"]] = district_code

    print(f"  {len(qid_to_district)} individuals with district mapping")

    # Step 2: Read person file, aggregate language by district
    lang_counts = defaultdict(Counter)  # district_code -> {language: count}
    with zipfile.ZipFile(zip_path) as z:
        print("  Loading person file (may take a minute)...")
        with z.open("sa-census-2022-person-v1.csv") as f:
            reader = csv.DictReader(io.TextIOWrapper(f, encoding="utf-8", errors="replace"))
            n = 0
            for row in reader:
                qid = row["QID"]
                lang = row["P08_LANGUAGE"]
                if qid in qid_to_district and lang and lang not in ("Unspecified", "Not applicable"):
                    lang_counts[qid_to_district[qid]][lang] += 1
                n += 1
                if n % 1_000_000 == 0:
                    print(f"    processed {n:,} records...")

    print(f"  {n:,} person records processed")

    # Step 3: Compute percentages and write output
    out_path = os.path.join(DATA_DIR, "language_by_district.csv")
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["census_district", "language", "n", "pct"])
        for district in sorted(lang_counts.keys()):
            total = sum(lang_counts[district].values())
            for lang, count in sorted(lang_counts[district].items(), key=lambda x: -x[1]):
                writer.writerow([district, lang, count, round(count / total, 6)])

    print(f"  Written to {out_path}")
    print(f"  {len(lang_counts)} districts, languages per district: "
          f"{min(len(v) for v in lang_counts.values())}-{max(len(v) for v in lang_counts.values())}")

    # Print summary
    print("\n  Top languages nationally:")
    national = Counter()
    for d in lang_counts.values():
        national.update(d)
    total_nat = sum(national.values())
    for lang, count in national.most_common(15):
        print(f"    {lang}: {count:,} ({count/total_nat*100:.1f}%)")


# ============================================================================
# PART 2: Slave Emancipation Farm-Name Matching
# ============================================================================

# Historical slave district -> candidate GADM2 districts (for disambiguation)
# Based on geographic overlap between 1830s Cape districts and modern GADM2
SLAVE_DISTRICT_TO_GADM2 = {
    "Cape":         ["ZAF.9.3_1", "ZAF.9.1_1"],
    "Stellenbosch": ["ZAF.9.1_1", "ZAF.9.3_1"],
    "Worcester":    ["ZAF.9.1_1", "ZAF.8.3_1", "ZAF.9.5_1"],
    "Swellendam":   ["ZAF.9.2_1", "ZAF.9.4_1", "ZAF.9.5_1"],
    "George":       ["ZAF.9.4_1", "ZAF.9.2_1"],
    "Graaff Reinet":["ZAF.1.4_1", "ZAF.1.5_1", "ZAF.8.4_1"],
    "Beaufort":     ["ZAF.9.5_1", "ZAF.9.6_1", "ZAF.8.4_1"],
    "Clanwilliam":  ["ZAF.9.6_1", "ZAF.8.3_1"],
    "Somerset":     ["ZAF.1.4_1", "ZAF.1.5_1"],
    "Uitenhage":    ["ZAF.1.7_1", "ZAF.1.4_1"],
    "Albany":       ["ZAF.1.4_1", "ZAF.1.2_1"],
}

# Origin_exact -> ISO2 mapping for slave origins
ORIGIN_TO_ISO2 = {
    # East Africa / Indian Ocean
    "Mozambique":        "MZ",
    "Machava":           "MZ",
    "Massina":           "MZ",
    "Musquette":         "MZ",
    "Madagascar":        "MG",
    "Mauritius":         "MU",
    "Bourbon":           "MU",  # Reunion, closest modern match
    "St Helena":         None,  # British Atlantic island, no clear partner
    # South & Southeast Asia
    "Bengale":           "IN",
    "Malabar":           "IN",
    "Madras":            "IN",
    "Coromandel Coast":  "IN",
    "Pondicherry":       "IN",
    "Surat":             "IN",
    "Cochin":            "IN",
    "Tranquebar":        "IN",  # Danish India, but geographically IN
    "Goedeloer":         "IN",  # Cuddalore, Tamil Nadu
    "Ceylon":            "LK",
    "Java":              "ID",
    "Batavia":           "ID",
    "Bali":              "ID",
    "Macassar":          "ID",
    "Nias":              "ID",
    "Tabanan":           "ID",
    "Amboyna/Ambon":     "ID",
    "Celebes":           "ID",
    "Bima":              "ID",
    "Banten":            "ID",
    "Ternate":           "ID",
    "Timor":             "TL",
    "Napai":             "ID",  # likely Nias variant or minor Indonesian island
    "Malacca":           "MY",
    "Terenganu / Trangan": "MY",
    "Macao":             "CN",
    # West Africa
    "Guinea":            "GN",
    "Elmina":            "GH",
    "Senegal":           "SN",
    "West African Coast": "GH",  # generic, assign to Gold Coast
    "Bougies":           "DZ",  # Bejaia, Algeria
    # Other
    "America":           "US",
    "West-Indies":       None,  # too vague
    "Portugal":          "PT",
    # Exclude
    "Cape":              None,  # local born
    "Not Specified":     None,
}


def haversine_km(lon1, lat1, lon2, lat2):
    """Great-circle distance in km."""
    R = 6371.0
    phi1, phi2 = math.radians(lat1), math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlam = math.radians(lon2 - lon1)
    a = math.sin(dphi/2)**2 + math.cos(phi1) * math.cos(phi2) * math.sin(dlam/2)**2
    return R * 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))


def process_slave_origins():
    """Match slave Place_a to Bewaarders farm names, aggregate origins by GADM2 district."""

    farm_csv = os.path.join(DATA_DIR, "farm_classifications.csv")
    slave_xlsx = os.path.join(DATA_DIR, "Slave Emancipation Dataset.xlsx")

    if not os.path.exists(farm_csv) or not os.path.exists(slave_xlsx):
        print(f"WARNING: Missing farm_classifications.csv or Slave Emancipation Dataset.xlsx")
        return

    # Step 1: Load farm name -> [(GID_2, lon, lat), ...] lookup
    print("\nLoading farm classifications...")
    farms_by_name = defaultdict(list)
    with open(farm_csv, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            fn = row["farm_name"].strip()
            if not fn:
                continue
            farms_by_name[fn].append({
                "GID_2": row["GID_2"],
                "lon": float(row["lon"]) if row["lon"] else None,
                "lat": float(row["lat"]) if row["lat"] else None,
            })

    print(f"  {len(farms_by_name)} unique farm names loaded")

    # Compute GADM2 centroids from farm coordinates (for disambiguation)
    gid_centroids = {}
    gid_coords = defaultdict(lambda: {"lons": [], "lats": []})
    for entries in farms_by_name.values():
        for e in entries:
            if e["lon"] and e["lat"]:
                gid_coords[e["GID_2"]]["lons"].append(e["lon"])
                gid_coords[e["GID_2"]]["lats"].append(e["lat"])
    for gid, coords in gid_coords.items():
        gid_centroids[gid] = (
            sum(coords["lons"]) / len(coords["lons"]),
            sum(coords["lats"]) / len(coords["lats"]),
        )

    # Step 2: Load slave emancipation records
    print("Loading slave emancipation dataset...")
    wb = openpyxl.load_workbook(slave_xlsx, read_only=True)
    ws = wb.active
    headers = None
    slaves = []
    for i, row in enumerate(ws.iter_rows(values_only=True)):
        if i == 0:
            headers = list(row)
            continue
        record = dict(zip(headers, row))
        slaves.append(record)
    wb.close()
    print(f"  {len(slaves)} slave records loaded")

    # Step 3: Match Place_a to farm names with historical district disambiguation
    matched = 0
    unmatched_with_place = 0
    no_place = 0
    ambiguous_resolved = 0
    results = []  # (GID_2, origin_iso2)

    for rec in slaves:
        place_a = rec.get("Place_a")
        if not place_a or not str(place_a).strip():
            no_place += 1
            continue

        place_a = str(place_a).strip()
        origin_exact = str(rec.get("Origin_exact", "")).strip()
        district_name = str(rec.get("District_name", "")).strip()

        # Map origin to ISO2
        iso2 = ORIGIN_TO_ISO2.get(origin_exact)
        if not iso2:
            continue  # Cape-born, Not Specified, or unmappable

        # Look up farm name
        if place_a not in farms_by_name:
            unmatched_with_place += 1
            continue

        candidates = farms_by_name[place_a]
        unique_gids = list(set(e["GID_2"] for e in candidates))

        if len(unique_gids) == 1:
            # Unambiguous match
            results.append((unique_gids[0], iso2))
            matched += 1
        else:
            # Ambiguous: use historical district to disambiguate
            preferred_gids = SLAVE_DISTRICT_TO_GADM2.get(district_name, [])

            # Find candidates that fall within preferred GADM2 districts
            overlap = [g for g in unique_gids if g in preferred_gids]
            if len(overlap) == 1:
                results.append((overlap[0], iso2))
                matched += 1
                ambiguous_resolved += 1
            elif len(overlap) > 1:
                # Multiple preferred districts match — pick closest to district centroid
                # Use the first preferred district (ordered by likelihood)
                results.append((overlap[0], iso2))
                matched += 1
                ambiguous_resolved += 1
            else:
                # No overlap with preferred districts — pick the GADM2 district
                # whose centroid is closest to any preferred district centroid
                if preferred_gids and preferred_gids[0] in gid_centroids:
                    ref_lon, ref_lat = gid_centroids[preferred_gids[0]]
                    best_gid = min(
                        unique_gids,
                        key=lambda g: haversine_km(
                            ref_lon, ref_lat,
                            gid_centroids.get(g, (0, 0))[0],
                            gid_centroids.get(g, (0, 0))[1]
                        ) if g in gid_centroids else 9999
                    )
                    results.append((best_gid, iso2))
                    matched += 1
                    ambiguous_resolved += 1
                else:
                    # Last resort: take first candidate
                    results.append((unique_gids[0], iso2))
                    matched += 1

    print(f"\n  Matching results:")
    print(f"    Matched to GADM2: {matched}")
    print(f"    Ambiguous resolved via historical district: {ambiguous_resolved}")
    print(f"    No Place_a: {no_place}")
    print(f"    Place_a not in Bewaarders: {unmatched_with_place}")
    print(f"    Skipped (Cape-born/Not Specified/no ISO2): "
          f"{len(slaves) - matched - unmatched_with_place - no_place}")

    # Step 4: Aggregate by (GID_2, origin_iso2)
    origin_counts = defaultdict(Counter)  # GID_2 -> {iso2: count}
    for gid, iso2 in results:
        origin_counts[gid][iso2] += 1

    # Compute shares
    out_path = os.path.join(DATA_DIR, "slave_origins_by_district.csv")
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["GID_2", "origin_iso2", "n_slaves", "total_foreign_slaves", "pct_origin"])
        for gid in sorted(origin_counts.keys()):
            total = sum(origin_counts[gid].values())
            for iso2, count in sorted(origin_counts[gid].items(), key=lambda x: -x[1]):
                writer.writerow([gid, iso2, count, total, round(count / total, 6)])

    print(f"\n  Written to {out_path}")
    print(f"  {len(origin_counts)} GADM2 districts with slave origin data")

    # Print summary
    print("\n  Origin distribution across matched records:")
    total_by_origin = Counter()
    for gid_counts in origin_counts.values():
        total_by_origin.update(gid_counts)
    for iso2, count in total_by_origin.most_common():
        print(f"    {iso2}: {count}")

    print("\n  Records per GADM2 district:")
    for gid in sorted(origin_counts.keys()):
        total = sum(origin_counts[gid].values())
        top = origin_counts[gid].most_common(3)
        top_str = ", ".join(f"{iso2}={n}" for iso2, n in top)
        print(f"    {gid}: {total} slaves ({top_str})")


# ============================================================================
# MAIN
# ============================================================================

if __name__ == "__main__":
    extract_language_shares()
    process_slave_origins()
    print("\nDone.")
