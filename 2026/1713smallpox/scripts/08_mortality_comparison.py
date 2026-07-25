#!/usr/bin/env python3
# Adapted for the public replication package: reads ../data, writes figures to ../output.
"""
08_mortality_comparison.py -- Cape smallpox mortality as a new datapoint beside Schneider &
Davenport. Computes settler and enslaved population mortality from the quantitative records and
sets them against Iceland 1707-09, Sweden (endemic) and Leiden 1635. Primary metric = population
mortality (deaths as % of the at-risk population, comparable to Iceland/Leiden); implied CFR is
secondary (population mortality / assumed virgin-soil attack rate). Outputs a comparison CSV and a
LEAP/SHM-style dot-and-range figure. Run: python 08_mortality_comparison.py
"""
import csv, os
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.normpath(os.path.join(HERE, "..", "data")); os.makedirs(OUT, exist_ok=True)
FIG = os.path.normpath(os.path.join(HERE, "..", "output")); os.makedirs(FIG, exist_ok=True)
xs = os.path.join(OUT, "slave_crosssection_by_district_year.csv")

def rd(p):
    with open(p, encoding="utf-8") as f: return list(csv.DictReader(f))

rows = {(r["district"], int(r["year"])): r for r in rd(xs)}
def g(d, y, col): return int(rows[(d, y)][col])

# ---- Cape settlers: triangulated population-mortality bounds -----------------
set12 = g("Cape",1712,"settler_total") + g("StelDrak",1712,"settler_total")   # 1967
set14 = g("Cape",1714,"settler_total") + g("StelDrak",1714,"settler_total")   # 1488
census_decline = 100*(1 - set14/set12)                                        # upper bound ~24%
probate_floor = 5.0   # documented floor among propertied (PS bounded-CFR appendix: ~4-6%)
# Cape Town, all groups: 160 dead 1 Apr-13 Jun 1713, town population order 1500-2000
town_lo, town_hi = 100*160/2000, 100*160/1500                                 # ~8-11%

# ---- Cape enslaved -----------------------------------------------------------
# Company slaves (muster): ~200 gross deaths of ~568 -> population mortality
comp_deaths, comp_base = 200, 568
comp_mort = 100*comp_deaths/comp_base                                         # ~35%
ATTACK = (0.60, 0.95)                                                         # virgin-soil attack rate
comp_cfr = (comp_mort/ATTACK[1], comp_mort/ATTACK[0])                         # implied CFR range
# Settler-farm (private) enslaved: adults masked by the trade; children the cleaner signal
def child(d,y): return g(d,y,"slave_boys")+g(d,y,"slave_girls")
ch12 = child("Cape",1712)+child("StelDrak",1712)
ch14 = child("Cape",1714)+child("StelDrak",1714)
child_decline = 100*(1 - ch14/ch12)                                          # ~24%

def cfr(m): return (m/ATTACK[1], m/ATTACK[0])

# ---- assemble comparison table ----------------------------------------------
# (label, pop_mortality_point, pop_mortality_lo, pop_mortality_hi, implied_cfr_lo, implied_cfr_hi, source/note, group)
T = [
 ("Cape settlers (colony)", None, probate_floor, census_decline, *cfr(probate_floor)[:1]+cfr(census_decline)[1:], "opgaaf 1712-14 (ceiling) & probate (floor)", "cape"),
 ("Cape Town, all groups", 9.1, town_lo, town_hi, *cfr(town_lo)[:1]+cfr(town_hi)[1:], "dagregister body-count (160 dead, Apr-Jun 1713)", "cape"),
 ("Cape Company slaves", comp_mort, None, None, comp_cfr[0], comp_cfr[1], "Company slave muster (~200 of ~568)", "cape"),
 ("Cape enslaved children", child_decline, None, None, None, None, "opgaaf cross-section, boys+girls 1712-14", "cape"),
 ("Iceland 1707-09", 26.4, None, None, 42.9, 54.8, "Schneider & Davenport (virgin-soil epidemic)", "comp"),
 ("Leiden 1635", 33.0, None, None, None, None, "van Besouw et al. (>1/3 of the city died)", "comp"),
 ("Sweden, endemic", None, None, None, 8.0, 10.0, "Schneider & Davenport (CFR only; childhood disease)", "comp"),
 ("Foula 1720", 97.0, None, None, None, None, "Duncan et al. (island virgin-soil; 6 of ~200 survived)", "comp"),
 ("Amsterdam 1870-72", 0.8, None, None, None, None, "Muurling et al. (post-vaccination)", "comp"),
 ("Iceland 1785-87", 3.4, None, None, None, None, "Schneider & Davenport (later, milder epidemic)", "comp"),
 ("Chester 1774", None, None, None, 14.6, 14.6, "Duncan et al. (CFR, 202 of 1,385)", "comp"),
]
with open(os.path.join(OUT,"mortality_comparison.csv"),"w",newline="",encoding="utf-8") as f:
    w=csv.writer(f); w.writerow(["population","pop_mortality_pct","pm_lo","pm_hi","cfr_lo","cfr_hi","source","group"])
    for r in T: w.writerow(r)

print("Cape settlers: census ceiling %.1f%%, probate floor %.0f%%; Cape Town body-count %.1f-%.1f%%"
      % (census_decline, probate_floor, town_lo, town_hi))
print("Company slaves: %.1f%% pop mortality; implied CFR %.0f-%.0f%%" % (comp_mort, comp_cfr[0], comp_cfr[1]))
print("Enslaved children decline: %.1f%%" % child_decline)

# ---- figure: population mortality, dot-and-range, LEAP/SHM style -------------
PLUM="#5C2346"; BLUE="#3D8EB9"; GREY="#AAAAAA"; SPINE="#4A4A4A"; GRID="#E0E0E0"; INK="#2D2D2D"
plt.rcParams.update({"font.family":"sans-serif","font.sans-serif":["DejaVu Sans","Arial"],"font.size":10})
fig, ax = plt.subplots(figsize=(9, 5.8))
# rows to plot on the population-mortality axis (exclude CFR-only rows: Sweden, Chester)
plot = [t for t in T if not (t[1] is None and t[2] is None)]
plot = plot[::-1]  # top-to-bottom reading order
ax.axvspan(20, 30, color=GREY, alpha=0.14, lw=0)  # consensus 20-30% band
ax.text(25, len(plot)-0.30, "consensus 20–30%", color=SPINE, fontsize=8, ha="center")
labels=[]
for i,(label,pt,lo,hi,*_rest,src,grp) in enumerate(plot):
    col = PLUM if grp=="cape" else BLUE
    if lo is not None and hi is not None:
        ax.plot([lo,hi],[i,i], color=col, lw=2.4, solid_capstyle="round", zorder=2)
        ax.plot([lo,hi],[i,i], "|", color=col, ms=10, zorder=3)
        if pt is not None: ax.plot(pt,i,"o",color=col,ms=7,zorder=4)
    else:
        ax.plot(pt,i,"o",color=col,ms=8,zorder=4)
    labels.append(label)
ax.set_yticks(range(len(plot))); ax.set_yticklabels(labels, fontsize=9.5, color=INK)
ax.set_ylim(-0.6, len(plot)-0.05); ax.set_xlim(0, 100)
ax.set_xlabel("Population mortality (%)", fontsize=10, color=SPINE)
for s in ("top","right"): ax.spines[s].set_visible(False)
ax.spines["left"].set_visible(False); ax.tick_params(axis="y", length=0)
ax.spines["bottom"].set_color(SPINE); ax.spines["bottom"].set_linewidth(0.8)
ax.tick_params(colors=SPINE, labelsize=9)
ax.xaxis.grid(True, color=GRID, lw=0.5); ax.set_axisbelow(True)
fig.savefig(os.path.join(FIG,"fig4_comparison.png"), bbox_inches="tight", dpi=600)
fig.savefig(os.path.join(FIG,"fig4_comparison.pdf"), bbox_inches="tight")
print("figure -> fig4_comparison; table -> mortality_comparison.csv")
