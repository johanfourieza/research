"""Full supplied corpus: lexical counts, inspected pox candidates and source registers.

This is descriptive enumeration, not an estimated semantic classifier. The
interpretive decisions below were made with AI assistance (OpenAI's Codex Astra; glosses corrected with Anthropic's Claude Code Fable, 8 October 2026) after reading the
retrieved passages. The author checked the 18 smallpox-candidate decisions by hand (8 October 2026); the other interpretive notes are AI-assisted.
"""
from pathlib import Path
import csv,json,re,collections,datetime
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
R=Path(__file__).resolve().parents[1];D=R/'full_corpus';M=R/'manuscript'
def read(p):
    with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def write(p,rs):
    with p.open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rs[0]));w.writeheader();w.writerows(rs)
rows=read(D/'retrieval.csv');entries={r['id']:r for r in rows};usable=[r for r in rows if r['has_text']=='1']
# All 18 broad pox candidates, including the capok false match. Each summary
# identifies the event in context rather than assigning every group in an entry
# to the illness mentioned elsewhere in it.
EVENTS={
 'vc-1713-63':(0,'Capok in a criminal proceeding; no smallpox reference.','Retrieval false positive',''),
 'vc-1713-90':(1,'Illness and smallpox increasing among Company-owned enslaved people.','Illness',''),
 'vc-1713-99':(1,'About 150 Company-owned enslaved people bedridden with illness including smallpox; many already dead.','Illness and death',''),
 'vc-1713-108':(1,'Daily burials among Company-owned enslaved people; eleven deaths in the preceding day; many locally born people (inboorlingen) also infected.','Illness and death',''),
 'vc-1713-111':(1,'An enslaved person thought to be nearly one hundred died of smallpox.','Death',''),
 'vc-1713-121':(1,'Smallpox deaths spreading among Cape-born people (Affricaanse inboorlingen).','Death',''),
 'vc-1713-123':(1,'Illness appearing to abate somewhat among Company-owned enslaved people, increasing sharply among Cape-born people (Affricanen) who had never had smallpox.','Illness and prior exposure',''),
 'vc-1713-126':(1,'Thirteen white bodies, all locally born or previously unexposed, await burial; separately, Khoesan deaths and the clerk\'s attribution of unfamiliarity with healing.','Death and belief','10732:114-115'),
 'vc-1713-130':(1,'Smallpox reaches country inhabitants; concern about cultivation.','Illness and labour',''),
 'vc-1713-139':(1,'Reported flight of remaining Cape Khoesan from disease; another group killed all but one for fear of transmission.','Flight and exclusion','10732:121'),
 'vc-1713-162':(1,'Thanksgiving for fifteen recoveries; disease easing locally but continuing in the countryside.','Recovery and geographic spread',''),
 'vc-1713-164':(1,'Reported cumulative 160 deaths of men, women and children at this place since 1 April.','Death','10732:135-136'),
 'vc-1713-175':(1,'Severe illness among Cape-born people (Affricanen) in Drakenstein, fewer than twenty healthy; concern about the next harvest.','Illness and labour',''),
 'vc-1713-214':(1,'Continuing reports of deaths among country inhabitants and farmers.','Death',''),
 'vc-1713-238':(1,'Company muster: 441 present, 127 fewer than previously; loss near 200 allowing for arrivals and births.','Stock and flows','10732:176-177'),
 'vc-1713-332':(1,'People still ill in Drakenstein; farmers using scythes because most Khoesan harvest workers had been carried off.','Ongoing illness and labour adjustment','10732:260'),
 'vc-1713-351':(1,'Thirty-second marriage since 21 July; clerk connects renewed marriage to unions broken by smallpox.','Family reconstruction','10732:275'),
 'vc-1714-44':(1,'Survivors from four Khoesan communities report one in ten remaining and seek successor captains, appointed from the dead captains\' kin on 15 February.','Community reconstruction','10733:51-52'),
}
assert set(EVENTS)=={r['id'] for r in rows if r['pox']=='1'}
audit=[]
for ident,(positive,summary,kind,scans) in EVENTS.items():
    r=entries[ident]
    audit.append(dict(id=ident,date=r['date'],pox_reference=positive,summary=summary,theme=kind,
        image_concordance=scans,review='Hand and AI verified: AI-assisted reading, decision checked by hand by the author (8 October 2026)',text=r['text']))
write(D/'pox_candidate_review.csv',audit)
positive=[r for r in audit if r['pox_reference']]
assert len(positive)==17
annual=[]
for year in range(1700,1721):
    rs=[r for r in usable if r['year']==str(year)];n=len(rs)
    a=dict(year=year,entries=n,words=sum(len(r['text'].split()) for r in rs))
    for k in ['illness','death','hospital','medicine','maritime','khoesan','enslaved','settler']:
        a[k+'_entries']=sum(int(r[k]) for r in rs)
        a[k+'_per_100_entries']=100*a[k+'_entries']/n
    a['pox_reference_entries']=sum(r['date'].startswith(str(year)) for r in positive)
    annual.append(a)
write(D/'annual_lexical_counts.csv',annual)
write(D/'epidemic_chronology.csv',positive)
spans=read(D/'retrieval_spans.csv')
medical=[]
special={
 'vc-1704-306':'Letter requests medicines; reply orders medicines from local stocks, water, vegetables and men for ships in Saldanha Bay.',
 'vc-1704-320':'Dispatch instructions specify water, bread, vegetables and medicines for the two ships.',
 'vc-1708-313':'Hospital treatment of Company servants for venereal disease; surgeon remuneration, patient medicine charges and prior examination.',
 'vc-1710-48':'Daily prescriptions to be entered in a book, following Batavia and other Asian establishments; hospital oversight.',
 'vc-1711-103':'Sixteen enslaved men to assist hospital patients and clean; prescriptions entered in a book so that the medicines given for particular illnesses could be checked; hospital oversight.',
 'vc-1713-63':'Criminal testimony about alleged poisoning and powders; not smallpox treatment.',
 'vc-1713-119':'Quarantine concerns a vessel in European waters and plague; not local smallpox quarantine.',
 'vc-1713-126':'Clerk attributes unfamiliarity with healing to Khoesan people; no medicine procurement recorded.',
 'vc-1713-162':'Thanksgiving for recovery of fifteen people; no specified therapeutic action.',
 'vc-1713-343':'Surgeon reports on a wound in a criminal matter; not epidemic treatment.',
 'vc-1714-37':'Enslaved fugitive described as healing a self-inflicted throat wound with olive leaves; not smallpox treatment.',
 'vc-1717-314':'Overfull hospital and surgeons working day and night to assist numerous sick arrivals.',
 'vc-1720-278':'Continued illness despite remedies delays a ship; does not concern the 1713 epidemic.',
}
for r in usable:
    if r['medicine']!='1':continue
    passages=[s['passage'] for s in spans if s['id']==r['id'] and s['net']=='medicine']
    medical.append(dict(id=r['id'],date=r['date'],matched_passages=' || '.join(passages),
        contextual_note=special.get(r['id'],'Retrieved medical passages screened in context; no located allegation of selective medicine imports for the 1713 epidemic.'),
        review_scope='AI-assisted reading of retrieved passages; full-entry follow-up for cited interpretive examples; not a human gold label.'))
write(D/'medical_passage_review.csv',medical)
controls=[]
images={'vc-1710-48':'NA VOC 1.04.02, inv. 10730, scans 58 and 61-63; prescription instructions on 62',
        'vc-1711-103':'NA VOC 1.04.02, inv. 10731, scans 162-167; hospital labour on 165, prescriptions on 166'}
for ident in ['vc-1704-306','vc-1704-320','vc-1708-313','vc-1710-48','vc-1711-103','vc-1713-119','vc-1714-37','vc-1717-314','vc-1720-278']:
    r=entries[ident];controls.append(dict(id=ident,date=r['date'],finding=special[ident],source='Tracing History Trust dagregister transcription',image_concordance=images.get(ident,'Not matched to an original image in this revision'),text=r['text']))
write(D/'care_context_register.csv',controls)
(D/'care_context_register.md').write_text('\n\n'.join(f"## {r['date']} | {r['id']}\n\n{r['finding']}\n\nImage concordance (AI-assisted): {r['image_concordance']}.\n\n{r['text']}" for r in controls),encoding='utf-8')
# Exact, frozen indicators: no confidence interval is warranted for a census of
# matches in this supplied text. Unmeasured transcription/semantic error remains.
out=dict(supplied_rows=len(rows),usable_texts=len(usable),missing_texts=len(rows)-len(usable),
    whitespace_words=sum(len(r['text'].split()) for r in usable),pox_candidates=len(audit),pox_references=len(positive),
    pox_1713=sum(r['date'].startswith('1713') for r in positive),pox_1714=sum(r['date'].startswith('1714') for r in positive),
    medical_candidates=len(medical),illness_lexical_entries=sum(int(r['illness']) for r in usable),
    illness_and_maritime_lexical_entries=sum(r['illness']=='1' and r['maritime']=='1' for r in usable),
    hospital_lexical_entries=sum(int(r['hospital']) for r in usable),
    interpretation='Exact lexical counts in the supplied transcription; reviewed pox references. No estimate of total semantic health attention or mortality.',
    negative_search='No corroborating passage located for selective import of smallpox medicines from Batavia; not proof of no treatment, equal access or no unrecorded transaction.')
(D/'results.json').write_text(json.dumps(out,indent=2),encoding='utf-8')
# Appendix tables.
t=[r'\begin{tabular}{rrrrrrr}',r'\toprule',r'Year & Texts & Words & Illness & Death & Hospital & Smallpox \\',r'\midrule']
for a in annual:t.append(f"{a['year']} & {a['entries']} & {a['words']:,} & {a['illness_entries']} & {a['death_entries']} & {a['hospital_entries']} & {a['pox_reference_entries']} "+chr(92)*2)
t += [r'\bottomrule',r'\end{tabular}'];(M/'table_full_corpus.tex').write_text('\n'.join(t)+'\n',encoding='utf-8')
# One figure compares generic lexical indicators to disease-specific references.
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
print(json.dumps(out,indent=2))
