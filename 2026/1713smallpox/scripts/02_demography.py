"""Revision-specific, read-only integration of the Households v1 release.

Sums refer to recorded numeric counts, not imputed population totals. A source
blank remains missing in the row-level file. Explicit historical spellings
omitted by v1 are recovered from the independently extracted local transcription;
the shared release is never changed. See the saved override ledger.
"""
from pathlib import Path
import csv, hashlib, collections, json
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

REV=Path(__file__).resolve().parents[1]
HH=REV.parents[2]/'Fourie_Households'
OUT=REV/'outputs'; FIG=REV/'figures'; SRC=REV/'sources'
FIELDS=['settler_men','settler_women','settler_sons','settler_daughters',
        'slave_men','slave_women','slave_sons','slave_daughters']
def read(p):
    with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def write(p,rows):
    if not rows:return
    with p.open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(dict.fromkeys(k for r in rows for k in r)))
        w.writeheader();w.writerows(rows)
def number(v):
    if v in ('','NA','NaN',None):return None
    return float(v)
sources=[HH/'Data/v1'/n for n in ['opgaaf_tax_units_clean_v1.csv','census_schema_registry_v1.csv','census_sheet_registry_v1.csv']]
write(SRC/'households_input_manifest.csv',[{'path':str(p),'sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'bytes':p.stat().st_size} for p in sources])
with sources[0].open(encoding='utf-8-sig',newline='') as f:
    units=[r for r in csv.DictReader(f) if r['source_key'] in ('cape_early','stellenbosch_early') and 1700<=int(r['year'])<=1720]
write(OUT/'households_v1_1700_1720.csv',units)
raw={(r['district'],r['year'],int(r['excel_row'])):r for r in read(OUT/'opgaaf_household_rows.csv')}
schema={(r['source_key'],r['year'],r['excel_column']):r for r in read(sources[1])}
overrides=[]; derived=[]; differences=[]
for u in units:
    y=int(u['year'])
    if not 1708<=y<=1718:continue
    d='Cape' if u['source_key']=='cape_early' else 'StelDrak'
    key=(d,u['year'],int(u['head_source_row']))
    assert key in raw, key
    r=raw[key]
    out={k:u[k] for k in ['tax_unit_id','source_key','source_sheet','head_source_row','residential_household_verified','names_men','names_women']}
    out.update(district=d,year=y)
    for field in FIELDS:
        val=number(u[field]); old={'slave_sons':'slave_boys','slave_daughters':'slave_girls'}.get(field,field)
        oldval=number(r[old])
        if field=='slave_daughters' and val is None:
            s=next((v for k,v in schema.items() if k[:2]==(u['source_key'],u['year']) and v.get('header_direct','').strip().lower()=='meijsies'),{})
            if s.get('header_direct','').strip().lower()=='meijsies' and not s.get('canonical_field'):
                val=None if r[old+'_blank']=='1' else oldval
                overrides.append({'tax_unit_id':u['tax_unit_id'],'district':d,'year':y,'excel_row':u['head_source_row'],
                    'column':s['excel_column'],'historical_header':'Meijsies','field':field,'old_release_value':'NA',
                    'revision_value':val,'reason':'Unmapped spelling of enslaved girls; blank preserved',
                    'source':'Local original transcription, outputs/opgaaf_household_rows.csv'})
        if r[old+'_blank']!='1' and val!=oldval:
            differences.append({'district':d,'year':y,'row':u['head_source_row'],'field':field,'canonical':val,'local':oldval})
        out[field]=val
    derived.append(out)
write(SRC/'census_release_reconciliation.csv',differences)
# The release joins an additional, unnumbered male entry to the 1708 unit.
assert all(r['district']=='StelDrak' and r['year']==1708 and r['row']=='114' and r['field']=='settler_men' and r['canonical']==2 and r['local']==1 for r in differences), differences[:10]
write(OUT/'census_analysis_rows.csv',derived);write(SRC/'census_mapping_overrides.csv',overrides)
groups=collections.defaultdict(list)
for r in derived:groups[r['district'],r['year']].append(r)
agg=[]; missing=[]
for (d,y),rs in sorted(groups.items()):
    a={'district':d,'year':y,'tax_units':len(rs)}
    for field in FIELDS:
        vals=[r[field] for r in rs if r[field] is not None]
        a[field]=sum(vals) if vals else None
        missing.append({'district':d,'year':y,'field':field,'tax_units':len(rs),'numeric_cells':len(vals),
                        'missing_cells':len(rs)-len(vals),'explicit_zero_cells':sum(v==0 for v in vals),'positive_cells':sum(v>0 for v in vals),
                        'observed_sum':a[field]})
    for prefix in ('settler','slave'):
        a[prefix+'_adults']=a[prefix+'_men']+a[prefix+'_women']
        a[prefix+'_children']=a[prefix+'_sons']+a[prefix+'_daughters']
        a[prefix+'_total']=a[prefix+'_adults']+a[prefix+'_children']
        a[prefix+'_children_per_tax_unit']=a[prefix+'_children']/len(rs)
        a[prefix+'_child_adult_ratio']=a[prefix+'_children']/a[prefix+'_adults']
        n=sum(any((r[prefix+'_'+s] or 0)>0 for s in ('sons','daughters')) for r in rs)
        a[prefix+'_units_positive_child_count']=n
        a[prefix+'_share_units_positive_child_count']=n/len(rs)
    agg.append(a)
for y in sorted(set(r['year'] for r in agg)):
    rs=[r for r in agg if r['year']==y]
    if len(rs)!=2:continue
    a={'district':'Pooled','year':y,'tax_units':sum(r['tax_units'] for r in rs)}
    for f in FIELDS:a[f]=sum(r[f] for r in rs)
    for g in ('settler','slave'):
        for f in ('adults','children','total','units_positive_child_count'):a[g+'_'+f]=sum(r[g+'_'+f] for r in rs)
        a[g+'_children_per_tax_unit']=a[g+'_children']/a['tax_units']
        a[g+'_child_adult_ratio']=a[g+'_children']/a[g+'_adults']
        a[g+'_share_units_positive_child_count']=a[g+'_units_positive_child_count']/a['tax_units']
    agg.append(a)
write(OUT/'census_annual.csv',agg);write(OUT/'census_missingness.csv',missing)
changes=[]
lookup={(r['district'],r['year']):r for r in agg}
for (d,y),a in sorted(lookup.items()):
    b=lookup.get((d,y+2))
    if b is None:continue
    for f in FIELDS+['tax_units','settler_adults','settler_children','settler_total','slave_adults','slave_children','slave_total']:
        changes.append({'district':d,'start':y,'end':y+2,'category':f,'before':a[f],'after':b[f],
                        'change':b[f]-a[f],'change_pct':100*(b[f]/a[f]-1) if a[f] else None})
write(OUT/'census_two_year_changes.csv',changes)
thresholds=[]
for d in ['Cape','StelDrak','Pooled']:
    a,b=lookup[d,1712],lookup[d,1714]
    for g in ['settler','slave']:
        c0,c1=a[g+'_children'],b[g+'_children'];a0,a1=a[g+'_adults'],b[g+'_adults']
        thresholds.append({'district':d,'group':g,'child_net_change':c1-c0,'adult_net_change':a1-a0,
            'child_change_if_adult_proportional':c0*(a1/a0-1),
            'child_shortfall_relative_to_adult_change':c0*a1/a0-c1,
            'description':'Descriptive benchmark only; shared proportional enumeration/flows not established'})
write(OUT/'census_accounting_thresholds.csv',thresholds)

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
p=pd.read_csv(OUT/'probate_by_year.csv')
print('Probate columns',p.columns.tolist())
count=next(c for c in p.columns if c!='year')
fig,ax=plt.subplots(figsize=(7,3.6),layout='constrained')
q=p[(p.year>=1700)&(p.year<=1720)]
ax.bar(q.year,q[count],color=[PLUM if y in (1713,1714) else BLUE for y in q.year],width=.75)
ax.axhline(10.8,color=GREY,ls='--',lw=1,label='1708–1712 mean: 10.8 documents')
ax.set(ylabel='Documents dated in the heading',xlabel='Year');ax.set_xticks(range(1700,1721,2));ax.legend(fontsize=11)
for y in [1713,1714]:
    v=q.loc[q.year==y,count].iloc[0];ax.text(y,v+.6,str(int(v)),ha='center')
fig.savefig(FIG/'probate_documents.pdf');fig.savefig(FIG/'probate_documents.png',dpi=170);plt.close(fig)
print('Revision rows',len(derived),'overrides',len(overrides))
print(df[df.year.isin([1712,1714])][['district','year','tax_units','settler_total','slave_total','settler_children','slave_children','settler_children_per_tax_unit','slave_children_per_tax_unit']].to_string(index=False))
print(pd.DataFrame(thresholds).to_string(index=False))
