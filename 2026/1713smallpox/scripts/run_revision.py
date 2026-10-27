"""Reproduce the published numerical tables and figures from deposited aggregates.

No custodial workbook, model API, LaTeX installation or author directory is
required. The other numbered scripts preserve the source-extraction workflow;
this public runner deliberately begins with the released aggregate CSVs.
"""
from pathlib import Path
import argparse
import csv
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output-dir', type=Path, default=ROOT / 'reproduced',
                    help='Destination for tables/ and figures/ (default: package/reproduced).')
args = parser.parse_args()
R = args.output_dir.resolve()
FIG = R / 'figures'
M = TEX = R / 'tables'
for folder in (FIG, M):
    folder.mkdir(parents=True, exist_ok=True)
DATA = ROOT / 'data'
O = OUT = DATA / 'census'
inputs = {p.name: p for p in DATA.rglob('*.csv')}

def read(path):
    with inputs[Path(path).name].open(encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))

agg = pd.read_csv(DATA / 'census/census_annual.csv').to_dict('records')
annual = pd.read_csv(DATA / 'journal/annual_lexical_counts.csv').to_dict('records')
assert len(annual) == 21 and sum(x['entries'] for x in annual) == 7666
assert sum(x['pox_reference_entries'] for x in annual) == 17
assert sum(x['illness_entries'] for x in annual) == 644
assert sum(x['hospital_entries'] for x in annual) == 250
assert len(read('pox_candidate_review.csv')) == 18
assert len(read('medical_passage_review.csv')) == 94
probate = {int(x['year']): int(x['documents']) for x in read('probate_by_year.csv')}
assert probate[1713] == 54 and probate[1714] == 36
assert sum(probate[y] for y in range(1708, 1713)) / 5 == 10.8

# Exhibits: no smoothing or interpolation through missing census years.
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':12,'axes.titlesize':12,'axes.titleweight':'normal',
 'axes.labelsize':12,'axes.spines.top':False,'axes.spines.right':False,'legend.frameon':False,'pdf.fonttype':42})
PLUM='#5C2346'; BLUE='#3D8EB9'; GREY='#686868'
df=pd.DataFrame(agg)
fig,axs=plt.subplots(2,2,figsize=(7.6,6.6),sharex=True,layout='constrained')
for ri,g in enumerate(['settler','slave']):
 for ci,d in enumerate(['Cape','StelDrak']):
    ax=axs[ri,ci];q=df[df.district==d].sort_values('year');base=q[q.year==1712].iloc[0]
    for suffix,col,marker,label in [('adults',PLUM,'o','Adult categories'),('children',BLUE,'s','Child categories')]:
        ax.plot(q.year,q[g+'_'+suffix]/base[g+'_'+suffix]*100,color=col,marker=marker,label=label,lw=1.4,ms=4)
    ax.axhline(100,color=GREY,lw=.6,ls=':');ax.axvspan(1712.7,1713.3,color='#D5D5D5',alpha=.5)
    ax.set_title(('Settler categories' if g=='settler' else 'Privately enslaved people')+'\n'+('Cape District' if d=='Cape' else 'Stellenbosch–Drakenstein'))
    ax.set_xticks([1708,1710,1712,1714,1716,1718]);ax.set_ylim(35,170)
    if ci==0:ax.set_ylabel('Recorded count, 1712 = 100')
axs[0,0].legend(fontsize=11,loc='upper left')
fig.savefig(FIG/'census_index.pdf');fig.savefig(FIG/'census_index.png',dpi=170);plt.close(fig)
fig,axs=plt.subplots(2,2,figsize=(7.6,6.6),sharex=True,layout='constrained')
for ri,g in enumerate(['settler','slave']):
 for ci,d in enumerate(['Cape','StelDrak']):
    ax=axs[ri,ci];q=df[df.district==d].sort_values('year')
    for suffix,col,marker,label in [('men',PLUM,'o','Men'),('women',BLUE,'s','Women'),('sons','#D4A03E','^','Sons / boys'),('daughters','#6B8E5E','D','Daughters / girls')]:
        ax.plot(q.year,q[g+'_'+suffix],color=col,marker=marker,label=label,lw=1.2,ms=3)
    ax.axvspan(1712.7,1713.3,color='#D5D5D5',alpha=.5)
    ax.set_title(('Settler categories' if g=='settler' else 'Privately enslaved people')+'\n'+('Cape District' if d=='Cape' else 'Stellenbosch–Drakenstein'))
    ax.set_xticks([1708,1710,1712,1714,1716,1718]);ax.set_ylim(bottom=0)
    if ci==0:ax.set_ylabel('Recorded people')
axs[0,0].legend(fontsize=10,ncol=2)
fig.savefig(FIG/'census_levels.pdf');fig.savefig(FIG/'census_levels.png',dpi=170);plt.close(fig)
p=pd.read_csv(DATA/'probate/probate_by_year.csv')
count=next(c for c in p.columns if c!='year')
fig,ax=plt.subplots(figsize=(7,3.6),layout='constrained')
q=p[(p.year>=1700)&(p.year<=1720)]
ax.bar(q.year,q[count],color=[PLUM if y in (1713,1714) else BLUE for y in q.year],width=.75)
ax.axhline(10.8,color=GREY,ls='--',lw=1,label='1708–1712 mean: 10.8 documents')
ax.set(ylabel='Documents dated in the heading',xlabel='Year');ax.set_xticks(range(1700,1721,2));ax.legend(fontsize=11)
for y in [1713,1714]:
    v=q.loc[q.year==y,count].iloc[0];ax.text(y,v+.6,str(int(v)),ha='center')
fig.savefig(FIG/'probate_documents.pdf');fig.savefig(FIG/'probate_documents.png',dpi=170);plt.close(fig)

# Reproducible main and appendix tables.
annual=read(O/'census_annual.csv');look={(r['district'],int(r['year'])):r for r in annual}
lines=[r'\begin{tabular}{lrrrrrr}',r'\toprule',r' & \multicolumn{3}{c}{Cape District} & \multicolumn{3}{c}{Stellenbosch--Drakenstein} \\',r'\cmidrule(lr){2-4}\cmidrule(lr){5-7}',r'Category & 1712 & 1714 & Change (\%) & 1712 & 1714 & Change (\%) \\',r'\midrule']
for f,label in [('tax_units','Tax units'),('settler_men','Settler men'),('settler_women','Settler women'),('settler_sons','Settler sons'),('settler_daughters','Settler daughters'),('settler_total','Settler total'),('slave_men','Enslaved men'),('slave_women','Enslaved women'),('slave_sons','Enslaved boys'),('slave_daughters','Enslaved girls'),('slave_total','Enslaved people, total')]:
 vals=[]
 for d in ['Cape','StelDrak']:
  a,b=[float(look[d,y][f]) for y in [1712,1714]];vals.extend([f'{a:,.0f}',f'{b:,.0f}',f'{100*(b/a-1):.1f}'])
 lines.append(label+' & '+' & '.join(vals)+r' \\')
lines.extend([r'\bottomrule',r'\end{tabular}']);(M/'table_census.tex').write_text('\n'.join(lines),encoding='utf-8')
lines=[r'\begin{tabular}{lrrrr}',r'\toprule',r' & \multicolumn{2}{c}{Cape District} & \multicolumn{2}{c}{Stellenbosch--Drakenstein} \\',r'Measure & 1712 & 1714 & 1712 & 1714 \\',r'\midrule']
for f,label in [('settler_children_per_tax_unit','Recorded settler children per tax unit'),('slave_children_per_tax_unit','Recorded enslaved children per tax unit'),('settler_child_adult_ratio','Settler child/adult count ratio'),('slave_child_adult_ratio','Enslaved child/adult count ratio'),('settler_share_units_positive_child_count',r'Units with positive settler child count (\%)'),('slave_share_units_positive_child_count',r'Units with positive enslaved child count (\%)')]:
 pct='share' in f;vals=[float(look[d,y][f])*(100 if pct else 1) for d in ['Cape','StelDrak'] for y in [1712,1714]]
 lines.append(label+' & '+' & '.join(f'{v:.1f}' if pct else f'{v:.2f}' for v in vals)+r' \\')
lines.extend([r'\bottomrule',r'\end{tabular}']);(M/'table_composition.tex').write_text('\n'.join(lines),encoding='utf-8')


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

# Restore the corpus aggregate after the census-table code's local annual variable.
annual = pd.read_csv(DATA / 'journal/annual_lexical_counts.csv').to_dict('records')
# Appendix tables.
t=[r'\begin{tabular}{rrrrrrr}',r'\toprule',r'Year & Texts & Words & Illness & Death & Hospital & Smallpox \\',r'\midrule']
for a in annual:t.append(f"{a['year']} & {a['entries']} & {a['words']:,} & {a['illness_entries']} & {a['death_entries']} & {a['hospital_entries']} & {a['pox_reference_entries']} "+chr(92)*2)
t += [r'\bottomrule',r'\end{tabular}'];(M/'table_full_corpus.tex').write_text('\n'.join(t)+'\n',encoding='utf-8')

# One figure compares generic lexical indicators to disease-specific references.
# The original corpus figure runs in a fresh process, with default plot settings.
plt.rcdefaults()
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':11,'axes.spines.top':False,'axes.spines.right':False,'pdf.fonttype':42})
fig,axs=plt.subplots(2,1,figsize=(8,6.3),sharex=True,gridspec_kw={'height_ratios':[1.3,1]},layout='constrained')
years=[r['year'] for r in annual]
for k,label,col,ls in [('illness','Illness vocabulary','#5C2346','-'),('hospital','Hospital vocabulary','#3D8EB9','--')]:
    axs[0].plot(years,[a[k+'_per_100_entries'] for a in annual],label=label,color=col,linestyle=ls,marker='o',markersize=3)
axs[0].set_ylabel('Entries per 100 usable entries');axs[0].set_title('A. Routine health vocabulary',loc='left',fontsize=11)
axs[0].legend(frameon=False,ncol=2,loc='upper left');axs[0].set_ylim(0,17)
axs[1].bar(years,[r['pox_reference_entries'] for r in annual],color='#5C2346',width=.6)
axs[1].set_title('B. Smallpox references, inspected in context',loc='left',fontsize=11)
axs[1].set_ylabel('Entries');axs[1].set_ylim(0,19);axs[1].set_yticks([0,4,8,12,16]);axs[1].set_xticks(range(1700,1721,2));axs[1].set_xlabel('Year')
for a in axs:
    a.axvspan(1712.6,1714.4,color='#D4A03E',alpha=.13,zorder=-1)
    a.grid(axis='y',alpha=.18);a.set_axisbelow(True)
axs[1].text(1713,16.5,'16',ha='center');axs[1].text(1714,1.5,'1',ha='center')
for ext in ['pdf','png']:fig.savefig(R/'figures'/('journal_full_corpus.'+ext),dpi=220)
plt.close(fig)

print(f'Reproduced seven numerical tables and four figures in {R}.')
print('Raw-source extraction and historical interpretation are separate from this aggregate rebuild.')
