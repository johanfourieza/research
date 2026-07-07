#!/usr/bin/env python3
"""
01_enslaved_series.py  --  THoTF paper, quantitative backbone (enslaved + settler stocks)

Reads the SHARED opgaafrolle panel and produces year-series for the 1713 smallpox window:
  * enslaved stocks by gender, RAW (all rows) and BALANCED (households present in both
    adjacent years, via hhid) -- the balanced series nets out panel entry/exit churn;
  * settler stocks by year, to cross-check the literature's "settler population -20%" claim;
  * an excess-decline summary for the 1713->1714 epidemic window vs the 1708-1712 baseline.

slaves_died / settlers_died are NOT usable here (populated only in 1825), so enslaved
mortality can only be INFERRED from stock dynamics -- reported as caveated bounds, never a
single false-precise rate.  Run with: python 01_enslaved_series.py
"""
import gzip, csv, os, statistics
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.normpath(os.path.join(HERE, "..", "..", "sources", "data"))
PANEL = os.path.join(DATA, "stel-L-L", "stellenbosch_long_linked_wsaf_fixed_feb2026_.csv.gz")
OUT = os.path.join(HERE, "outputs"); os.makedirs(OUT, exist_ok=True)

def num(x):
    try: return float(x)
    except (TypeError, ValueError): return 0.0

# ---- load the columns we need ------------------------------------------------
with gzip.open(PANEL, "rt", encoding="utf-8", errors="replace") as fh:
    rd = csv.reader(fh); hdr = next(rd); low = [c.lower() for c in hdr]
    ix = {k: low.index(k) for k in [
        "year", "hhid", "slave_men", "slave_women", "slave_children",
        "settler_men", "settler_women", "settler_children"]}
    rows = []
    for r in rd:
        try: y = int(float(r[ix["year"]]))
        except (ValueError, IndexError): continue
        if 1700 <= y <= 1725:
            rows.append((y, r[ix["hhid"]],
                         num(r[ix["slave_men"]]), num(r[ix["slave_women"]]),
                         num(r[ix["slave_children"]]),
                         num(r[ix["settler_men"]]), num(r[ix["settler_women"]]),
                         num(r[ix["settler_children"]])))

YEARS = sorted({r[0] for r in rows})

# ---- RAW stocks by year ------------------------------------------------------
raw = defaultdict(lambda: defaultdict(float))
nrows = defaultdict(int)
for (y, hh, sm, sw, sc, em, ew, ec) in rows:
    nrows[y] += 1
    raw[y]["slave_men"] += sm; raw[y]["slave_women"] += sw; raw[y]["slave_children"] += sc
    raw[y]["settler_men"] += em; raw[y]["settler_women"] += ew; raw[y]["settler_children"] += ec

with open(os.path.join(OUT, "raw_stocks_by_year.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["year", "n_household_rows", "slave_men", "slave_women",
        "slave_children", "slaves_total", "settler_men", "settler_women", "settler_children",
        "settlers_total"])
    for y in YEARS:
        st = raw[y]["slave_men"] + raw[y]["slave_women"] + raw[y]["slave_children"]
        se = raw[y]["settler_men"] + raw[y]["settler_women"] + raw[y]["settler_children"]
        w.writerow([y, nrows[y], int(raw[y]["slave_men"]), int(raw[y]["slave_women"]),
                    int(raw[y]["slave_children"]), int(st), int(raw[y]["settler_men"]),
                    int(raw[y]["settler_women"]), int(raw[y]["settler_children"]), int(se)])

# ---- BALANCED panel: households present in BOTH year t-1 and t ----------------
# For each adjacent pair, sum enslaved (and settler) counts over the SAME hhids in both
# years, and report the within-set change. This removes entry/exit churn but, by
# construction, drops households whose head died/vanished -- which biases the enslaved
# decline DOWNWARD (toward zero). We flag that explicitly in the paper.
by_year_hh = defaultdict(dict)  # year -> hhid -> [slaves, settlers]
for (y, hh, sm, sw, sc, em, ew, ec) in rows:
    if not hh: continue
    d = by_year_hh[y].setdefault(hh, [0.0, 0.0, 0.0, 0.0])  # sl_men, sl_women, sl_all, set_all
    d[0] += sm; d[1] += sw; d[2] += sm + sw + sc; d[3] += em + ew + ec

bal_rows = []
for y in YEARS:
    if (y - 1) not in by_year_hh: continue
    common = set(by_year_hh[y]) & set(by_year_hh[y - 1])
    if not common: continue
    a = lambda yr, j: sum(by_year_hh[yr][h][j] for h in common)
    smn0, smn1 = a(y-1, 0), a(y, 0)
    swn0, swn1 = a(y-1, 1), a(y, 1)
    sl0, sl1 = a(y-1, 2), a(y, 2)
    g = lambda new, old: (new - old) / old if old else float("nan")
    bal_rows.append([y, len(common), int(sl0), int(sl1), round(g(sl1, sl0), 4),
                     int(smn0), int(smn1), round(g(smn1, smn0), 4),
                     int(swn0), int(swn1), round(g(swn1, swn0), 4)])

with open(os.path.join(OUT, "balanced_enslaved_change.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["to_year", "n_hh_in_both", "slaves_total_prev",
        "slaves_total_now", "g_slaves_total", "slave_men_prev", "slave_men_now",
        "g_slave_men", "slave_women_prev", "slave_women_now", "g_slave_women"])
    w.writerows(bal_rows)

# ---- excess-decline summary for the epidemic window --------------------------
# Baseline = mean balanced growth of total enslaved over pairs ending 1709..1712.
bal = {r[0]: r for r in bal_rows}
base_pairs = [bal[y][4] for y in (1709, 1710, 1711, 1712) if y in bal and bal[y][4] == bal[y][4]]
baseline_g = statistics.mean(base_pairs) if base_pairs else float("nan")
def excess(to_year, col=4):
    if to_year in bal and bal[to_year][col] == bal[to_year][col]:
        return round(bal[to_year][col] - baseline_g, 4)
    return float("nan")

summary = [
    ["baseline_mean_growth_enslaved_1709_1712", round(baseline_g, 4)],
    ["balanced_g_enslaved_1712to1713", bal.get(1713, [None]*5)[4]],
    ["balanced_g_enslaved_1713to1714", bal.get(1714, [None]*5)[4]],
    ["excess_growth_1713to1714_vs_baseline", excess(1714)],
    ["raw_slave_men_1712_1713_1714", f"{int(raw[1712]['slave_men'])}/{int(raw[1713]['slave_men'])}/{int(raw[1714]['slave_men'])}"],
    ["raw_slave_women_1712_1713_1714", f"{int(raw[1712]['slave_women'])}/{int(raw[1713]['slave_women'])}/{int(raw[1714]['slave_women'])}"],
    ["raw_settlers_total_1712_1713_1714", f"{int(sum(raw[1712][k] for k in ('settler_men','settler_women','settler_children')))}/"
        f"{int(sum(raw[1713][k] for k in ('settler_men','settler_women','settler_children')))}/"
        f"{int(sum(raw[1714][k] for k in ('settler_men','settler_women','settler_children')))}"],
]
with open(os.path.join(OUT, "epidemic_window_summary.csv"), "w", newline="", encoding="utf-8") as f:
    csv.writer(f).writerows([["metric", "value"]] + summary)

# ---- console report ----------------------------------------------------------
print("RAW stocks (year: n_rows  slaveM  slaveW  | settlersTotal):")
for y in YEARS:
    se = int(sum(raw[y][k] for k in ('settler_men','settler_women','settler_children')))
    print(f"  {y}: n={nrows[y]:4d}  sm={int(raw[y]['slave_men']):4d}  sw={int(raw[y]['slave_women']):4d}  | settlers={se}")
print("\nBALANCED within-household growth (to_year: nHH  slavesTotal g%  | menG%  womenG%):")
for r in bal_rows:
    print(f"  {r[0]}: nHH={r[1]:4d}  g_all={r[4]*100:6.1f}%  | g_men={r[7]*100:6.1f}%  g_women={r[10]*100:6.1f}%")
print("\nEpidemic-window summary:")
for k, v in summary: print(f"  {k}: {v}")
print("\nWritten to", OUT)
