"""Generate appendix tables from the saved revision outputs, with invariants."""
from pathlib import Path
import csv

REV=Path(__file__).resolve().parents[1]
OUT=REV/'outputs'; TEX=REV/'manuscript'
def read(name):
    with (OUT/name).open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def table(name,spec,header,rows):
    text='\\begin{tabular}{'+spec+'}\n\\toprule\n'+header+' \\\\\n\\midrule\n'
    text+='\n'.join(' & '.join(str(x) for x in row)+' \\\\' for row in rows)
    text+='\n\\bottomrule\n\\end{tabular}\n'
    (TEX/name).write_text(text,encoding='utf-8')
def n(x):return str(int(float(x)))
a=read('census_annual.csv')
look={(r['district'],int(r['year'])):r for r in a}
assert float(look['Pooled',1712]['settler_total'])==1967
assert float(look['Pooled',1714]['settler_total'])==1488
assert float(look['Pooled',1712]['slave_children'])==232
assert float(look['Pooled',1714]['slave_children'])==176
rows=[]
for d in ('Cape','StelDrak'):
    for r in a:
        if r['district']==d:
            rows.append(['Cape' if d=='Cape' else 'S--D',r['year']]+[n(r[f]) for f in ['tax_units','settler_adults','settler_children','slave_adults','slave_children']])
table('table_annual.tex','llrrrrr','District & Year & Units & Settler adults & Children & Enslaved adults & Children',rows)
m=read('census_missingness.csv')
assert all(int(r['explicit_zero_cells'])==0 for r in m if r['year'] in ('1712','1714'))
fields=['settler_men','settler_women','settler_sons','settler_daughters','slave_men','slave_women','slave_sons','slave_daughters']
labels=['Settler men','Settler women','Settler sons','Settler daughters','Enslaved men','Enslaved women','Enslaved boys','Enslaved girls']
ml={(r['district'],r['year'],r['field']):r for r in m}
rows=[]
for f,label in zip(fields,labels):
    rows.append([label]+[ml[d,y,f]['missing_cells']+'/'+ml[d,y,f]['tax_units'] for d,y in [('Cape','1712'),('Cape','1714'),('StelDrak','1712'),('StelDrak','1714')]])
table('table_missing.tex','lrrrr','Category & Cape 1712 & Cape 1714 & S--D 1712 & S--D 1714',rows)
t=read('census_accounting_thresholds.csv')
rows=[]
for d in ['Cape','StelDrak','Pooled']:
    rr=[r for r in t if r['district']==d]
    rows.append([{'Cape':'Cape District','StelDrak':'Stellenbosch--Drakenstein','Pooled':'Pooled'}[d]]+[f"{float(next(r for r in rr if r['group']==g)['child_shortfall_relative_to_adult_change']):.1f}" for g in ['settler','slave']])
table('table_threshold.tex','lrr','District & Settler children & Enslaved children',rows)
b=read('probate_baselines.csv')
table('table_baselines.tex','lrrr','Baseline & Annual mean & 1713 / mean & 1713--1714 / expected',[[r['start']+'--'+r['end']]+[f"{float(r[k]):.2f}" for k in ['mean','ratio_1713','ratio_1713_1714_annualised']] for r in b])
v=read('validation_agreement.csv')
table('table_validation.tex','llrrrr','Reader & Variable & Answered & Agree & Disagree & Positive',[[{'karli':'De Kock','martie':'Van Wyk'}[r['reader']],r['variable']]+[r[k] for k in ['answered','agreements','disagreements','human_positive']] for r in v])
print('Five appendix tables generated; focal count and missingness assertions passed.')
