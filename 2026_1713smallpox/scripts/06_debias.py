#!/usr/bin/env python3
# Adapted for the public replication package: reads ../data, writes figures to ../output.
"""
06_debias.py  --  w33344 (Ludwig-Mullainathan-Rambachan) validation-debiased estimates for the
dagregister disease measurement, 1700-1720. Combines the cheap primary classifier over the full
population with the gold-labelled random validation sample to deliver consistent, debiased
estimates with bootstrap CIs (the "design-based supervised learning" / PPI estimator).

debiased_mean = mean_primary(full population) - mean(primary - gold | validation)
Bootstrap: resample the full population (plug-in term) and the validation sample (bias term)
independently; percentile CIs. Also reports the cheap classifier's error (precision/recall) and
the who-distribution among genuine (gold V1=1) disease entries.

Inputs: outputs/primary_labels_1700_1720.csv, notes/gold_labels.csv
Output: outputs/debiased_estimates.csv (+ console). Uses a fixed RNG seed (no Date/Math.random).
Run: python 06_debias.py
"""
import csv, os, random
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.normpath(os.path.join(HERE, "..", "data"))
NOTES = os.path.normpath(os.path.join(HERE, "..", "data"))
rng = random.Random(20260624)  # fixed seed

# --- load primary labels (full population) ------------------------------------
prim = {}
for r in csv.DictReader(open(os.path.join(OUT, "primary_labels_1700_1720.csv"), encoding="utf-8")):
    prim[r["id"]] = {"year": int(r["year"]), "V1": int(r["V1"]), "V3": int(r["V3"]),
                     "V4": int(r["V4"]), "who": r["who"]}
# --- load gold labels (validation), dedupe by id ------------------------------
gold = {}
for r in csv.DictReader(open(os.path.join(NOTES, "gold_labels.csv"), encoding="utf-8")):
    if r["id"] in gold: continue
    gold[r["id"]] = {"V1": int(r["gV1"]), "V3": int(r["gV3"]), "V4": int(r["gV4"]),
                     "who": r.get("gWho", "") or ""}
valid_ids = [i for i in gold if i in prim]
print(f"Full population: {len(prim)} entries;  validation (gold) usable: {len(valid_ids)}")

def mean(xs): return sum(xs) / len(xs) if xs else 0.0

def debiased(concept, B=4000):
    full_prim = [prim[i][concept] for i in prim]                       # Vhat over population
    v_prim = [prim[i][concept] for i in valid_ids]                     # Vhat on validation
    v_gold = [gold[i][concept] for i in valid_ids]                     # V (gold) on validation
    plugin = mean(full_prim)
    bias = mean([p - g for p, g in zip(v_prim, v_gold)])
    deb = plugin - bias
    valid_only = mean(v_gold)                                          # validation-only estimate
    # bootstrap
    pop = full_prim; vpairs = list(zip(v_prim, v_gold))
    boot = []
    for _ in range(B):
        pm = mean([pop[rng.randrange(len(pop))] for _ in range(len(pop))])
        vb = [vpairs[rng.randrange(len(vpairs))] for _ in range(len(vpairs))]
        bm = mean([p - g for p, g in vb])
        boot.append(pm - bm)
    boot.sort()
    lo, hi = boot[int(0.025 * B)], boot[int(0.975 * B)]
    vo_boot = sorted(mean([vpairs[rng.randrange(len(vpairs))][1] for _ in range(len(vpairs))]) for _ in range(B))
    return dict(plugin=plugin, bias=bias, debiased=deb, lo=lo, hi=hi,
                valid_only=valid_only, vo_lo=vo_boot[int(0.025*B)], vo_hi=vo_boot[int(0.975*B)])

# --- classifier error on the validation sample --------------------------------
def confusion(concept):
    tp=fp=fn=tn=0
    for i in valid_ids:
        p, g = prim[i][concept], gold[i][concept]
        tp += (p==1 and g==1); fp += (p==1 and g==0)
        fn += (p==0 and g==1); tn += (p==0 and g==0)
    prec = tp/(tp+fp) if tp+fp else float('nan')
    rec  = tp/(tp+fn) if tp+fn else float('nan')
    return tp,fp,fn,tn,prec,rec

rows=[]
print("\n=== Debiased population means (w33344) with 95% bootstrap CI ===")
print(f"{'concept':10} {'plugin':>8} {'debiased':>9} {'95% CI':>20} {'valid-only':>11} {'prec/rec(cheap)':>16}")
for c,label in [("V1","disease/death"),("V3","smallpox"),("V4","epi-medicine")]:
    d = debiased(c)
    tp,fp,fn,tn,prec,rec = confusion(c)
    print(f"{label:10} {d['plugin']:8.4f} {d['debiased']:9.4f}  [{d['lo']:.4f},{d['hi']:.4f}]  "
          f"{d['valid_only']:7.4f}      P={prec:.2f} R={rec:.2f}")
    rows.append([label, round(d['plugin'],4), round(d['debiased'],4), round(d['lo'],4), round(d['hi'],4),
                 round(d['valid_only'],4), tp,fp,fn,tn, round(prec,3) if prec==prec else '', round(rec,3) if rec==rec else ''])

# --- who-distribution among GENUINE (gold V1=1) disease entries ---------------
groups = ["company_slaves","private_slaves","settlers","khoesan","soldiers_sailors","general"]
who_g = {g:0 for g in groups}; n_dis=0
for i in valid_ids:
    if gold[i]["V1"]==1:
        n_dis += 1
        for g in groups:
            if g in gold[i]["who"]: who_g[g]+=1
print(f"\n=== Who is named in GENUINE disease/death entries (gold V1=1; N={n_dis}) ===")
for g in groups:
    k=who_g[g]; share=k/n_dis if n_dis else 0
    # Wilson-ish normal-approx binomial CI
    import math
    se=math.sqrt(share*(1-share)/n_dis) if n_dis else 0
    print(f"  {g:16} {k:3d}/{n_dis}  = {share*100:4.1f}%  (+/- {1.96*se*100:.1f})")

with open(os.path.join(OUT,"debiased_estimates.csv"),"w",newline="",encoding="utf-8") as f:
    w=csv.writer(f); w.writerow(["concept","plugin","debiased","ci_lo","ci_hi","valid_only","tp","fp","fn","tn","precision","recall"])
    w.writerows(rows)
    w.writerow([]); w.writerow(["who_among_disease_entries (gold V1=1)","count","of_N",n_dis])
    for g in groups: w.writerow([g, who_g[g]])
print("\nWritten -> outputs/debiased_estimates.csv")
