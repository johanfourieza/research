"""
Process New Zealand 1881 Census birthplace data (boroughs) and NZ place names
for the Meta History project.

Outputs:
  data/nz_borough_birthplace.csv — birthplace shares per borough, with GADM1 mapping
  data/nz_region_birthplace.csv  — aggregated birthplace shares per GADM1 region

Usage:
  python process_nz_data.py
"""

import csv
import os
import re

import openpyxl

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "Data")


# ============================================================================
# Borough → GADM1 region mapping
# ============================================================================
# Based on geographic location of each 1881 borough within modern GADM1 regions.
# Sources: Te Ara Encyclopedia, Wikipedia articles on NZ boroughs.

BOROUGH_TO_GADM1 = {
    # Auckland provincial district boroughs
    "Auckland":           ("NZL.1_1",  "Auckland"),
    "Parnell":            ("NZL.1_1",  "Auckland"),
    "Onehunga":           ("NZL.1_1",  "Auckland"),
    "Thames":             ("NZL.17_1", "Waikato"),
    "Gisborne":           ("NZL.5_1",  "Gisborne"),
    "Hamilton":           ("NZL.17_1", "Waikato"),
    # Taranaki
    "New Plymouth":       ("NZL.15_1", "Taranaki"),
    # Wellington provincial district
    "Wanganui":           ("NZL.7_1",  "Manawatu-Wanganui"),
    "Marton":             ("NZL.7_1",  "Manawatu-Wanganui"),
    "Palmerston North":   ("NZL.7_1",  "Manawatu-Wanganui"),
    "Wellington":         ("NZL.18_1", "Wellington"),
    "Masterton":          ("NZL.18_1", "Wellington"),
    "Greytown":           ("NZL.18_1", "Wellington"),
    # Hawke's Bay
    "Napier":             ("NZL.6_1",  "Hawke's Bay"),
    # Marlborough
    "Picton":             ("NZL.8_1",  "Marlborough"),
    "Blenheim":           ("NZL.8_1",  "Marlborough"),
    # Nelson
    "Nelson":             ("NZL.9_1",  "Nelson"),
    # Westland (West Coast)
    "Westport":           ("NZL.19_1", "West Coast"),
    "Greymouth":          ("NZL.19_1", "West Coast"),
    "Hokitika":           ("NZL.19_1", "West Coast"),
    "Ross":               ("NZL.19_1", "West Coast"),
    "Kumara":             ("NZL.19_1", "West Coast"),
    # Canterbury
    "Ashburton":          ("NZL.3_1",  "Canterbury"),
    "Kaiapoi":            ("NZL.3_1",  "Canterbury"),
    "Akaroa":             ("NZL.3_1",  "Canterbury"),
    "Lyttelton":          ("NZL.3_1",  "Canterbury"),
    "Christchurch":       ("NZL.3_1",  "Canterbury"),
    "Sydenham":           ("NZL.3_1",  "Canterbury"),
    "Timaru":             ("NZL.3_1",  "Canterbury"),
    "Rangiora":           ("NZL.3_1",  "Canterbury"),
    "Waimate":            ("NZL.3_1",  "Canterbury"),
    # Otago
    "Oamaru":             ("NZL.12_1", "Otago"),
    "Hampden":            ("NZL.12_1", "Otago"),
    "Palmerston South":   ("NZL.12_1", "Otago"),
    "West Hawkesbury":    ("NZL.12_1", "Otago"),
    "Port Chalmers":      ("NZL.12_1", "Otago"),
    "Dunedin":            ("NZL.12_1", "Otago"),
    "Roslyn":             ("NZL.12_1", "Otago"),
    "Caversham":          ("NZL.12_1", "Otago"),
    "Mornington":         ("NZL.12_1", "Otago"),
    "Maori Hill":         ("NZL.12_1", "Otago"),
    "St. Kilda":          ("NZL.12_1", "Otago"),
    "South Dunedin":      ("NZL.12_1", "Otago"),
    "North-East Valley":  ("NZL.12_1", "Otago"),
    "West Harbour":       ("NZL.12_1", "Otago"),
    "Milton":             ("NZL.12_1", "Otago"),
    "Green Island":       ("NZL.12_1", "Otago"),
    "Balclutha":          ("NZL.12_1", "Otago"),
    "Tapanui":            ("NZL.12_1", "Otago"),
    "Lawrence":           ("NZL.12_1", "Otago"),
    "Roxburgh":           ("NZL.12_1", "Otago"),
    "Naseby":             ("NZL.12_1", "Otago"),
    "Alexandra":          ("NZL.12_1", "Otago"),
    "Cromwell":           ("NZL.12_1", "Otago"),
    "Arrowtown":          ("NZL.12_1", "Otago"),
    "Queenstown":         ("NZL.12_1", "Otago"),
    # Southland
    "Invercargill":       ("NZL.14_1", "Southland"),
    "Invercargill South": ("NZL.14_1", "Southland"),
    "Invercargill East":  ("NZL.14_1", "Southland"),
    "Invercargill North": ("NZL.14_1", "Southland"),
    "Campbelltown":       ("NZL.14_1", "Southland"),
    "Avenal":             ("NZL.14_1", "Southland"),
    "Gladstone":          ("NZL.14_1", "Southland"),
    "Winton":             ("NZL.14_1", "Southland"),
    "Riverton":           ("NZL.14_1", "Southland"),
}


def parse_number(val):
    """Parse a number from the census data, handling commas and OCR artifacts."""
    if val is None:
        return 0
    s = str(val).strip()
    if s in ("", "—", "–", "�", "-"):
        return 0
    # Remove commas
    s = s.replace(",", "")
    # Fix common OCR errors
    s = s.replace("l", "1").replace("O", "0").replace("S", "5")
    # Handle 'M75' -> '1175' (Christchurch Ireland entry)
    s = s.replace("M", "1")
    try:
        return int(s)
    except ValueError:
        try:
            return int(float(s))
        except ValueError:
            print(f"  WARNING: Could not parse '{val}' -> '{s}'")
            return 0


def extract_borough_data():
    """Parse NZ.xlsx Boroughs sheet to extract birthplace by borough."""

    xlsx_path = os.path.join(DATA_DIR, "NZ.xlsx")
    wb = openpyxl.load_workbook(xlsx_path, read_only=True)
    ws = wb["Buroughs"]
    all_rows = list(ws.iter_rows(values_only=True))

    # Find all section headers ("Where Born." rows)
    sections = []
    for i, row in enumerate(all_rows):
        vals = list(row)
        if vals[0] and "Where Born" in str(vals[0]):
            boroughs = []
            for j, v in enumerate(vals[1:], 1):
                if v is not None and str(v).strip().endswith(".") and len(str(v)) > 2:
                    name = str(v).strip().rstrip(".")
                    boroughs.append((j, name))
            sections.append((i, boroughs))

    # For each section, extract key birthplace rows
    results = []
    for header_row, boroughs in sections:
        # Scan forward to find Total row
        for offset in range(1, 30):
            row_idx = header_row + offset
            if row_idx >= len(all_rows):
                break
            vals = list(all_rows[row_idx])
            label = str(vals[0]).strip() if vals[0] else ""

            if label == "Total":
                # Now extract each borough's data
                for col, bname in boroughs:
                    total = parse_number(vals[col] if col < len(vals) else None)

                    # Scan subsequent rows for birthplace categories
                    data = {"borough": bname, "total": total}
                    for off2 in range(1, 25):
                        r2 = row_idx + off2
                        if r2 >= len(all_rows):
                            break
                        v2 = list(all_rows[r2])
                        l2 = str(v2[0]).strip().lstrip("\xa0") if v2[0] else ""
                        val = v2[col] if col < len(v2) else None

                        if l2 == "New Zealand":
                            data["nz_born"] = parse_number(val)
                        elif l2 == "Australian Colonies":
                            data["australia"] = parse_number(val)
                        elif l2 == "England":
                            data["england"] = parse_number(val)
                        elif l2 == "Wales":
                            data["wales"] = parse_number(val)
                        elif l2 == "Scotland":
                            data["scotland"] = parse_number(val)
                        elif l2 == "Ireland":
                            data["ireland"] = parse_number(val)
                        elif l2 == "Germany":
                            data["germany"] = parse_number(val)
                        elif l2 == "China":
                            data["china"] = parse_number(val)
                        elif l2 == "Denmark":
                            data["denmark"] = parse_number(val)
                        elif l2 == "Norway":
                            data["norway"] = parse_number(val)
                        elif l2 == "Sweden":
                            data["sweden"] = parse_number(val)

                    # Add GADM1 mapping
                    if bname in BOROUGH_TO_GADM1:
                        data["gadm1_code"] = BOROUGH_TO_GADM1[bname][0]
                        data["gadm1_name"] = BOROUGH_TO_GADM1[bname][1]
                    else:
                        data["gadm1_code"] = "UNKNOWN"
                        data["gadm1_name"] = "UNKNOWN"
                        print(f"  WARNING: No GADM1 mapping for borough '{bname}'")

                    results.append(data)
                break

    wb.close()

    # Write borough-level CSV
    out_path = os.path.join(DATA_DIR, "nz_borough_birthplace.csv")
    fieldnames = ["borough", "gadm1_code", "gadm1_name", "total", "nz_born",
                  "england", "scotland", "ireland", "wales", "australia",
                  "germany", "china", "denmark", "norway", "sweden"]
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        for r in results:
            writer.writerow(r)

    print(f"Borough-level data written to {out_path}")
    print(f"  {len(results)} boroughs extracted")

    # Aggregate to GADM1 region level
    from collections import defaultdict
    region_totals = defaultdict(lambda: defaultdict(int))
    for r in results:
        gid = r["gadm1_code"]
        for key in ["total", "nz_born", "england", "scotland", "ireland",
                     "wales", "australia", "germany", "china"]:
            region_totals[gid][key] += r.get(key, 0)
        region_totals[gid]["gadm1_name"] = r["gadm1_name"]
        region_totals[gid]["n_boroughs"] = region_totals[gid].get("n_boroughs", 0) + 1

    # Compute shares and write
    out_path2 = os.path.join(DATA_DIR, "nz_region_birthplace.csv")
    with open(out_path2, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["gadm1_code", "gadm1_name", "n_boroughs", "total_pop",
                         "born_england", "born_scotland", "born_ireland", "born_wales",
                         "born_nz", "born_australia", "born_germany", "born_china",
                         "pct_english", "pct_scottish", "pct_irish",
                         "pct_uk_total", "pct_nz_born"])
        for gid in sorted(region_totals.keys()):
            d = region_totals[gid]
            total = d["total"]
            if total == 0:
                continue
            pct_eng = d["england"] / total
            pct_scot = d["scotland"] / total
            pct_ire = d["ireland"] / total
            pct_uk = (d["england"] + d["scotland"] + d["ireland"] + d["wales"]) / total
            pct_nz = d["nz_born"] / total
            writer.writerow([
                gid, d["gadm1_name"], d.get("n_boroughs", 0), total,
                d["england"], d["scotland"], d["ireland"], d["wales"],
                d["nz_born"], d["australia"], d["germany"], d["china"],
                round(pct_eng, 6), round(pct_scot, 6), round(pct_ire, 6),
                round(pct_uk, 6), round(pct_nz, 6)
            ])

    print(f"Region-level data written to {out_path2}")
    print(f"  {len(region_totals)} GADM1 regions")

    # Summary
    print("\nRegion summary:")
    print(f"{'Region':<25} {'Pop':>8} {'%Eng':>7} {'%Scot':>7} {'%Irish':>7} {'%UK':>7}")
    print("-" * 65)
    for gid in sorted(region_totals.keys()):
        d = region_totals[gid]
        total = d["total"]
        if total == 0:
            continue
        print(f"{d['gadm1_name']:<25} {total:>8} "
              f"{d['england']/total*100:>6.1f}% "
              f"{d['scotland']/total*100:>6.1f}% "
              f"{d['ireland']/total*100:>6.1f}% "
              f"{(d['england']+d['scotland']+d['ireland']+d['wales'])/total*100:>6.1f}%")

    return results


if __name__ == "__main__":
    extract_borough_data()
    print("\nDone.")
