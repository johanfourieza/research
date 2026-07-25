#!/usr/bin/env python3
# Adapted for the public replication package: reads ../data, writes figures to ../output.
"""
11_age_structure.py -- age-structured reading of the 1713 epidemic in the opgaaf cross-sections.

Motivation (readers Riswick & Marco Gracia): push the source beyond two headline figures by
reading mortality by age and sex, for BOTH the enslaved and the settlers, in BOTH districts, and
by asking whether there is a geographic gradient.

Design logic. In a virgin-soil epidemic the people with no prior exposure are the most vulnerable.
Among the enslaved, adult stock is continuously topped up by the slave trade, so the adult count is
a poor instrument; the child count is not trade-replenished and is the cleaner mortality signal.
The within-group age gradient (children minus adults) also nets out whole-household migration and
census disruption, which move adults and their children together -- so it is the age *differential*,
not the level, that carries information for the confounded settler population too.

Reads : outputs/slave_crosssection_by_district_year.csv  (needs the settler_men/women/sons/daught
        columns added to 04_slave_crosssection.py)
Writes: outputs/age_structure_change.csv     (1712->1714 change by group x age x district + pooled)
        outputs/age_structure_indexed.csv     (index=100 at 1712, 1708-1718, for the figure)
        ../paper/figures/fig3_agestructure.{png,pdf}   (2x2 indexed panels)
Run   : python 11_age_structure.py
"""
import csv, os
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.normpath(os.path.join(HERE, "..", "data")); os.makedirs(OUT, exist_ok=True)
FIG = os.path.normpath(os.path.join(HERE, "..", "output")); os.makedirs(FIG, exist_ok=True)
XS = os.path.join(OUT, "slave_crosssection_by_district_year.csv")

rows = {(r["district"], int(r["year"])): r for r in csv.DictReader(open(XS, encoding="utf-8"))}
def gi(d, y, c): return int(rows[(d, y)][c])

DISTRICTS = ["Cape", "StelDrak"]
# group -> (adult columns, child columns)
GROUPS = {
    "enslaved": (["slave_men", "slave_women"], ["slave_boys", "slave_girls"]),
    "settler":  (["settler_men", "settler_women"], ["settler_sons", "settler_daught"]),
}

def total(d, y, cols):
    return sum(gi(d, y, c) for c in cols)

def pct(a, b):
    return (b / a - 1.0) * 100.0 if a else float("nan")

# ---- 1712 -> 1714 change table (bracketing the disrupted 1713 roll) ----------
change_rows = []
print("1712 -> 1714 change, by group / age / district (1713 roll disrupted, excluded):\n")
print(f"{'group':9s} {'district':9s} {'adults12':>8s} {'adults14':>8s} {'ad_%':>7s} "
      f"{'child12':>7s} {'child14':>7s} {'ch_%':>7s} {'diff_pp':>7s}")
for g, (adc, chc) in GROUPS.items():
    for d in DISTRICTS + ["pooled"]:
        if d == "pooled":
            a12 = sum(total(x, 1712, adc) for x in DISTRICTS); a14 = sum(total(x, 1714, adc) for x in DISTRICTS)
            c12 = sum(total(x, 1712, chc) for x in DISTRICTS); c14 = sum(total(x, 1714, chc) for x in DISTRICTS)
        else:
            a12, a14 = total(d, 1712, adc), total(d, 1714, adc)
            c12, c14 = total(d, 1712, chc), total(d, 1714, chc)
        ad_pc, ch_pc = pct(a12, a14), pct(c12, c14)
        diff = ch_pc - ad_pc  # child excess decline over adults (nets out household migration)
        change_rows.append([g, d, a12, a14, round(ad_pc, 1), c12, c14, round(ch_pc, 1), round(diff, 1)])
        print(f"{g:9s} {d:9s} {a12:8d} {a14:8d} {ad_pc:+7.1f} {c12:7d} {c14:7d} {ch_pc:+7.1f} {diff:+7.1f}")
    print()

with open(os.path.join(OUT, "age_structure_change.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["group", "district", "adults_1712", "adults_1714", "adults_pct",
                "children_1712", "children_1714", "children_pct", "age_differential_pp"])
    w.writerows(change_rows)

# ---- indexed series (base 1712 = 100) for the recovery figure -----------------
YEARS = [1708, 1709, 1711, 1712, 1714, 1716, 1717, 1718]  # skip disrupted 1713; Cape lacks 1710/1715
idx_rows = []
series = {}  # (group, district, age) -> {year: index}
for g, (adc, chc) in GROUPS.items():
    for d in DISTRICTS:
        for age, cols in [("adults", adc), ("children", chc)]:
            base = total(d, 1712, cols)
            s = {}
            for y in YEARS:
                if (d, y) in rows:
                    s[y] = 100.0 * total(d, y, cols) / base if base else float("nan")
            series[(g, d, age)] = s
            for y, v in s.items():
                idx_rows.append([g, d, age, y, round(v, 1)])
with open(os.path.join(OUT, "age_structure_indexed.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["group", "district", "age", "year", "index_1712_100"])
    w.writerows(sorted(idx_rows))

# ---- figure: 2x2 indexed panels (rows=group, cols=district) -------------------
PLUM = "#5C2346"; BLUE = "#3D8EB9"; GREY = "#AAAAAA"; SPINE = "#4A4A4A"; GRID = "#E0E0E0"
AXLAB = "#4A4A4A"; AXTXT = "#5A5A5A"; TITLE = "#2D2D2D"
plt.rcParams.update({"font.family": "sans-serif", "font.sans-serif": ["DejaVu Sans", "Arial"],
                     "font.size": 10})

def style(ax, title=None):
    for s in ("top", "right"): ax.spines[s].set_visible(False)
    for s in ("bottom", "left"):
        ax.spines[s].set_color(SPINE); ax.spines[s].set_linewidth(0.8)
    ax.set_axisbelow(True); ax.yaxis.grid(True, color=GRID, linewidth=0.5); ax.xaxis.grid(False)
    ax.tick_params(colors=SPINE, length=3, labelsize=9)
    for lab in ax.get_xticklabels() + ax.get_yticklabels(): lab.set_color(AXTXT)
    if title: ax.set_title(title, fontsize=10.5, fontweight="bold", color=TITLE, loc="left", pad=8)

ROWLAB = {"enslaved": "Enslaved", "settler": "Settlers"}
COLLAB = {"Cape": "Cape District (port)", "StelDrak": "Stellenbosch–Drakenstein (frontier)"}
fig, axes = plt.subplots(2, 2, figsize=(10.5, 7.4), sharex=True)
for i, g in enumerate(["enslaved", "settler"]):
    for j, d in enumerate(DISTRICTS):
        ax = axes[i][j]
        ax.axhline(100, color=GREY, lw=0.8, ls=(0, (4, 3)), zorder=1)
        ax.axvspan(1712.5, 1713.5, color=GREY, alpha=0.16, lw=0, zorder=0)
        for age, col, mk in [("adults", BLUE, "o"), ("children", PLUM, "s")]:
            s = series[(g, d, age)]
            xs = sorted(s)
            ax.plot(xs, [s[y] for y in xs], "-" + mk, color=col, ms=5, lw=1.6,
                    label=age)
        style(ax, f"{ROWLAB[g]} — {COLLAB[d]}")
        if i == 1: ax.set_xlabel("Year", fontsize=10, color=AXLAB)
        if j == 0: ax.set_ylabel("Index (1712 = 100)", fontsize=10, color=AXLAB)
axes[0][0].legend(frameon=False, fontsize=9, loc="lower left")
fig.tight_layout()
fig.savefig(os.path.join(FIG, "fig3_agestructure.png"), bbox_inches="tight", dpi=600)
fig.savefig(os.path.join(FIG, "fig3_agestructure.pdf"), bbox_inches="tight")
plt.close(fig)
print("wrote fig3_agestructure ; tables -> age_structure_change.csv, age_structure_indexed.csv")
