# Frozen codebook — dagregister disease/death measurement, 1700–1720 (DRIL discipline)

Unit = one daily dagregister entry (`id`, `date_value`, `entry`). Population = ALL entries with
`date_value` in 1700–1720 (~7,665), text or "no entry". This codebook is FROZEN before labelling.

## Concepts (the gold measurement f* a careful reader would assign)
- **V1 disease_death** (0/1): the entry refers to any HUMAN sickness, disease, death, burial,
  corpse, or epidemic among people at the Cape or its ships/garrison. Routine "weather/ships
  with no sick or dead" = 0. Livestock-only death (cattle plague) = 0. (Ship arrivals that
  report deaths/sick among crew = 1.)
- **V2 who** (set, only if V1=1): which group(s) the sickness/death concerns —
  {company_slaves, private_slaves, settlers/colonists, khoesan, soldiers_sailors, general/unclear}.
- **V3 smallpox** (0/1): smallpox named (kinderpok/kinderpokjes/pokken/pocken) OR unambiguously
  the 1713 epidemic disease.
- **V4 medicine_for_sick** (0/1): the entry mentions medicine/treatment/cure/surgeon/doctor/
  apothecary/hospital/quarantine, OR an import from Batavia, *in connection with treating the
  sick of the epidemic*. Routine hospital intake of sick sailors counts as 0 for the EPIDEMIC
  question but is flagged separately (V4_routine_hospital) so we can distinguish.
- **evidence** (text span): the verbatim Dutch phrase that triggered the label.

## Coding rules / edge cases
- "Geen teks" / empty entry → V1=0, all 0.
- Company slaves = "'s Comp:s slaven / lijfeigenen / mancipia". Private slaves = owned by burgers.
- Khoesan = Hottentots/Sonqua/Bosjesman/inboorlingen/Afrikaanse inboorlingen/wilden.
- A single entry may get multiple V2 groups.
- V4: distinguish (a) epidemic treatment/medicine/Batavia-import [the claim] from (b) routine
  Company hospital admitting sick crews and routine Batavia provision ships. Only (a) sets V4=1.

## Estimation targets (downstream parameters, w33344)
- T1: yearly mean of V1 and of V3 (disease- and smallpox-attention rate per entry), 1700–1720.
- T2: among V1-and-death entries, the share whose V2 includes {khoesan} vs {slaves} vs {settlers}.
- T3: overall mean of V4 (epidemic medicine/Batavia rate) — expected ≈ 0.
Each reported as plug-in (cheap classifier) AND debiased (validation-corrected) with bootstrap CIs.

## Pipeline (DRIL: design frozen above; implementation below)
- Primary labels: transparent keyword/rule classifier over the full population (`05_*`).
- Validation labels: random sample of N≈400 entries, gold-labelled by careful full-context
  reading per this codebook (agents/Johan). Gaps recorded explicitly (illegible/ambiguous → flag).
- Debias: `debiased_mean = mean_primary(full) − mean(primary − gold | validation)`, bootstrap CIs.
