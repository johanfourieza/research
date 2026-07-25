#!/usr/bin/env python3
# Adapted for the public replication package: reads ../data, writes figures to ../output.
"""07_figures.py -- figures for the THoTF manuscript in LEAP house style.
LEAP identity reproduced in matplotlib (R/ggplot2 unavailable here): plum/blue/grey palette,
white ground, horizontal gridlines only, bottom+left spines, sans font, bold left titles,
no bar borders, 10x6in @ 600 dpi, PNG+PDF. Run: python 07_figures.py"""
import csv, os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.normpath(os.path.join(HERE, "..", "data"))
PSOUT = os.path.normpath(os.path.join(HERE, "..", "data"))
FIG = os.path.normpath(os.path.join(HERE, "..", "output")); os.makedirs(FIG, exist_ok=True)

# LEAP palette
PLUM="#5C2346"; BLUE="#3D8EB9"; SAGE="#6B8E5E"; GOLD="#D4A03E"; GREY="#AAAAAA"
SPINE="#4A4A4A"; GRID="#E0E0E0"; TITLE="#2D2D2D"; AXLAB="#4A4A4A"; AXTXT="#5A5A5A"
plt.rcParams.update({
    "font.family": "sans-serif", "font.sans-serif": ["DejaVu Sans","Arial","Helvetica"],
    "font.size": 10, "figure.dpi": 150,
    "axes.facecolor": "white", "figure.facecolor": "white",
})

def leap_ax(ax, title=None, xlabel=None, ylabel=None):
    for s in ("top","right"): ax.spines[s].set_visible(False)
    for s in ("bottom","left"):
        ax.spines[s].set_color(SPINE); ax.spines[s].set_linewidth(0.8)
    ax.set_axisbelow(True)
    ax.yaxis.grid(True, color=GRID, linewidth=0.5); ax.xaxis.grid(False)
    ax.tick_params(colors=SPINE, length=3, labelsize=9)
    for lab in ax.get_xticklabels()+ax.get_yticklabels(): lab.set_color(AXTXT)
    if title: ax.set_title(title, fontsize=11, fontweight="bold", color=TITLE, loc="left", pad=12)
    if xlabel: ax.set_xlabel(xlabel, fontsize=10, color=AXLAB)
    if ylabel: ax.set_ylabel(ylabel, fontsize=10, color=AXLAB)

def save(fig, name):
    fig.savefig(os.path.join(FIG, name+".png"), bbox_inches="tight", dpi=600)
    fig.savefig(os.path.join(FIG, name+".pdf"), bbox_inches="tight")
    plt.close(fig); print("wrote", name)

def readcsv(p):
    with open(p, encoding="utf-8") as f: return list(csv.DictReader(f))

# --- Fig 1: probate inventories per year ------------------------------------
prob = {int(r["yr"]): int(r["n_inv"]) for r in readcsv(os.path.join(PSOUT, "probate_by_year.csv"))}
yrs = [y for y in range(1700, 1721) if y in prob]
fig, ax = plt.subplots(figsize=(10, 5))
ax.bar(yrs, [prob[y] for y in yrs], color=[PLUM if y==1713 else GREY for y in yrs], width=0.72)
leap_ax(ax, None, "Year", "Probate inventories")
ax.annotate("1713: 54", xy=(1713, prob[1713]), xytext=(1714, prob[1713]-3),
            color=PLUM, fontsize=10, fontweight="bold")
ax.margins(y=0.05)
save(fig, "fig1_probate")

# --- Fig 2: dagregister disease vs smallpox attention by year ---------------
dr = {int(r["year"]): r for r in readcsv(os.path.join(OUT, "dagregister_year_counts.csv"))}
yrs2 = [y for y in range(1700, 1721) if y in dr]
fig, ax = plt.subplots(figsize=(10, 5))
ax.bar([y-0.19 for y in yrs2], [int(dr[y]["disease"]) for y in yrs2], width=0.38,
       color=GREY, label="any disease or death mention")
ax.bar([y+0.19 for y in yrs2], [int(dr[y]["smallpox"]) for y in yrs2], width=0.38,
       color=PLUM, label="smallpox named")
leap_ax(ax, None, "Year", "Daily entries per year")
ax.annotate("smallpox enters\nthe record in 1713", xy=(1713.2, 15), xytext=(1715.2, 17),
            color=PLUM, fontsize=9.5,
            arrowprops=dict(arrowstyle="->", color=PLUM, lw=1))
ax.legend(frameon=False, fontsize=9, loc="upper right")
ax.margins(y=0.08)
save(fig, "fig2_dagregister")

# --- Fig 3 (age structure): moved to 11_age_structure.py --------------------
# The enslaved adults-vs-children figure is superseded by the indexed, four-panel
# age-structure figure (enslaved + settlers, both districts) produced by
# 11_age_structure.py -> figures/fig3_agestructure. Run that script for Figure 3.
print("LEAP-style figures (fig1, fig2) ->", FIG)
