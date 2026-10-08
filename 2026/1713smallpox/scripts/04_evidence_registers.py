"""Create auditable source/case registers and manuscript tables from saved results."""
from pathlib import Path
import csv,re,json,hashlib,shutil
R=Path(__file__).resolve().parents[1];O=R/'outputs';S=R/'sources';M=R/'manuscript'
def read(p):
 with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def write(p,rs):
 with p.open('w',encoding='utf-8-sig',newline='') as f:
  w=csv.DictWriter(f,fieldnames=list(dict.fromkeys(k for r in rs for k in r)));w.writeheader();w.writerows(rs)
def tex(s):return str(s).replace('&',r'\&').replace('_',r'\_').replace('%',r'\%')
base='https://docs.globalise.huygens.knaw.nl/tanap/cape-transcriptions/Orphan-Chamber/MOOC8/MOOC8_1-5/'
cases=[
('MOOC8/2.67','1713-04-28','Abraham de Veij / Maria Jacobs','Widow of a free Chinese man receives assets against her inheritance, including an enslaved person.','Neither European-only estate coverage nor equitable treatment of enslaved people follows.','identity and unequal inheritance'),
('MOOC8/2.72','1713-05-22','Jan Nijs / Lijsbet Jansze de Roode','Both parents deceased; three named children benefit.','No cause or dates of death; do not infer all children are minors.','bereavement'),
('MOOC8/2.75','1713-05-11','Dirk Dirksze van Schalkwijk / Maritie Olivier','Both deceased; four minor children explicitly stated.','Names/ages not supplied in introductory gap; do not fill from inference.','bereavement'),
('MOOC8/2.76; MOOC8/2.77','1713-06-28; 1713-06-30','Willem Basson / Helena Clement','Both deceased; Arnoldus and Matthijs named as minor children; two property locations recorded. Pasqual and two children appear in the second inventory.','Two documents are not two separate estates. No epidemic cause stated. Pasqual relationship implied by grouping, not a complete pedigree.','bereavement and enslaved kin visibility'),
('MOOC8/2.101','1714-01-15','Jan Oberholster / Helena du Toit','Widower inventories estate before marrying Judith du Plessi, herself a widow.','Former spouses death dates unknown; inventory date is not death date.','remarriage'),
('MOOC8/2.105','1714-03-10','Pieter Christiaanse de Jager','Self-declaration dated 8 December 1713, submitted 10 March 1714.','Cross-year document lag established; no death date established.','administrative timing'),
('MOOC8/3.37; MOOC8/3.38; MOOC8/3.39','1713-07-05; 1713-08-09; 1713-08-11','Catharina Cruse / Corssenaer children','Widow leaves five minors aged 18, 16, 13, 10, 6. Goods reserved at relatives request, delivered to named kin/others where children live. Added note: goods sent with Willem to Batavia 16 April 1714.','No smallpox attribution or timing of fathers death; asset receipts do not assign each childs exact residence. Tamer and a child recorded among enslaved people in estate.','kin support and mobility'),
('MOOC8/3.48','1713-07-31','Gerrit Elbertsz','Farm left under Jeronimus Stevensz at one gulden per day because the knecht is sick and incapacitated.','Sickness not named smallpox; appointment administers farm, not a demonstrated nursing arrangement.','illness and work'),
('MOOC8/3.49','1713-07-04','Johannes Viellion / Chatrina Snijman','Inventory at initiative of Hercules des Pres, a relation, and sick visitor Harmanus Bosman; goods secured for surviving child.','No specific cause of death; krankbesoeker not automatically a medical doctor.','kin and local institutions'),
('MOOC8/3.56; MOOC8/3.57; MOOC8/3.58; MOOC8/3.59','1713-07-07; 1713-07-31; 1714-01-11; 1713-08-24','Wessel Pretorius / Geertruij Elberts','Estate inventoried in 1713, division in January 1714; livestock and two enslaved men distributed to married daughters household; sale of other half for minor son.','Direct evidence of continued administration in 1714, not a 1714 death. No evidence on enslaved mens family relationships or experience.','inheritance and transfer'),
]
write(S/'family_case_register.csv',[dict(source_ids=a,date=b,people=c,evidence=d,limits=e,theme=f,public_transcription=base+'#'+re.sub(r'[^a-z0-9]','',a.split(';')[0].lower())) for a,b,c,d,e,f in cases])
clusters=[['2.76','2.77'],['2.111','2.111 1/2'],['3.24','3.25'],['3.26','3.27'],['3.30','3.31','3.33'],['3.37','3.38','3.39'],['3.46','3.47','3.48'],['3.52','3.53'],['3.56','3.57','3.58','3.59']]
reg=[]
for r in read(O/'probate_register_1713_1714.csv'):
 ident=r['id'].replace('MOOC8/','');match=next((g for g in clusters if ident in g),None)
 r['document_key']=r['id']+'@'+r['date_value'] # MOOC8/3.54 occurs twice: preserve both.
 r['linked_document_cluster']='MOOC8/'+match[0] if match else ''
 r['link_basis']='Explicit repeated deceased/couple and associated property/division in text' if match else 'No linked document established in this audit'
 r['death_date_status']='Not systematically identifiable; heading not substituted for death date'
 r['smallpox_attribution']='Not established from heading or estate inventory alone'
 reg.append(r)
assert len({r['document_key'] for r in reg})==90
write(S/'probate_document_audit.csv',reg)
sources=[
dict(collection='Opgaaf',series='Cape / Stellenbosch-Drakenstein tax returns',repository='Nationaal Archief / Western Cape archives',version='Households Data/v1, July 2026; local transcription mapping supplements',
 transcription='Cape of Good Hope Panel transcription project; Chris de Wit and Hans Heese acknowledged in Fourie et al.; focal index credits Hans',
 locator='1712: VOC 4068, pp. 220-263; 1714: VOC 4073, pp. 14-33 (workbook index references)',
 scope='Two districts, observed annual sheets 1708-1718; Cape 1710 and both 1715 unavailable',limits='Counts are fiscal categories, not deaths or verified residential families; blanks preserved; source-image audit not comprehensive',url='https://doi.org/10.1080/02582473.2025.2500410'),
dict(collection='MOOC8',series='Orphan Chamber inventories and associated estate papers',repository='Western Cape Archives and Records Service',version='Local TEPC XML; public GLOBALISE rendering',
 transcription='TEPC Transcription Project, 2004-2008',locator='MOOC8/2 and MOOC8/3 for focal documents; full extracted headings 1695-1720',scope='90 document headings dated 1713-1714',limits='Repeated estates, declarations by living persons, filing lags, property/kin selection; no general death register',url=base),
dict(collection='Dagregister',series='Cape daily journal, distinct from Council resolutions',repository='Nationaal Archief',version='VOC 1.04.02, inventories 10730 (1710), 10731 (1711), 10732 (1713), 10733 (1714), digital images',
 transcription='Selected passages compared to supplied Dutch transcription by AI-assisted image inspection, then checked by hand by the author',locator='See journal_concordance.csv and archival_image_manifest.csv',scope='Selected decisive passages; no claim to have authenticated every year',limits='Image checks establish specific passage concordances; Tracing History Trust provenance confirmed by Johan; selected image comparisons do not certify full-period completeness',url='https://www.nationaalarchief.nl/onderzoeken/archief/1.04.02/invnr/10732'),
dict(collection='Journal workbook',series='vc_datastel (v3).xlsx',repository='Author-supplied research files',version='Input SHA256 in input_manifest.csv and full_corpus/input.json',transcription='Transcribed and supplied by the Tracing History Trust; shared by Helena Liebenberg. Provenance confirmed by Johan Fourie, 23 September 2026',locator='Stable vc-year-entry IDs; full_corpus/entry and retrieval ledgers',scope='Full corpus: 7670 IDs, 7666 with text, 1700-1720 inclusive; annual lexical counts, inspected smallpox candidates and medical passages',limits='Full supplied transcription analysed; archival completeness not independently established. Lexical frequencies are not exhaustive semantic shares. Negative search is bounded to the supplied journal and retrieval protocol.',url=''),
dict(collection='Published journal extracts',series='Leibbrandt 1896, Journal 1699-1732',repository='University of Pretoria copy / Internet Archive',version='Printed edition, pp. 255-258',transcription='H. C. V. Leibbrandt, selection/translation',locator='precisofarchives00cape_7',scope='Epidemic chronology and corroboration',limits='Abridged; dates differ from original for 160 tally and delegation. Original used for dates.',url='https://archive.org/details/precisofarchives00cape_7'),
]
write(S/'source_register.csv',sources)
concord=[('1710-02-17','vc-1710-48',10730,'58;61-63','Hospital prescriptions to be entered in a book, following Batavia and other Asian establishments','Date on scans 58 and 61; prescription instructions on scan 62; separate medical record, not proof of smallpox treatment'),
('1711-04-13','vc-1711-103',10731,'162-167','Sixteen enslaved men retained to assist hospital patients and clean; prescriptions entered in a book so that the medicines applied for particular illnesses could be checked','Date on scan 162; sixteen men and duties on 165; prescription instructions on 166; pre-epidemic provision, not measured access or efficacy during 1713'),
('1713-05-06','vc-1713-126',10732,'114-115','Thirteen white dead, all locally born or without prior smallpox; separately, Khoesan deaths and attributed unfamiliarity with disease and healing','Substantive Dutch wording compared with image; no inference of absence of care'),
('1713-05-07','vc-1713-127',10732,'115-116','Nine Khoesan bodies buried; sorrow in streets','Direct image passage'),
('1713-05-19','vc-1713-139',10732,'121','Flight from disease; report of killing by another Khoesan group fearing infection','Conditional wording preserved; image confirms report, not independent corroboration of the events'),
('1713-06-13','vc-1713-164',10732,'135-136','160 men women children since 1 April at this place','Original date 13 June; Leibbrandt pp.256 puts count under 11 June. Group coverage not stated'),
('1713-08-26','vc-1713-238',10732,'176-177','441 people,127 fewer, Ceylon recruitment and births, nearly200 loss','Direct image numbers and wording; losses chiefly attributed to smallpox, not exclusively'),
('1713-11-28','vc-1713-332',10732,'260','Ongoing illness in Drakenstein; the majority (het meerendeel) of the Khoesan harvest workers carried off; farmers using scythes','Direct image wording; reported loss of workers not a population death rate or lasting technology effect'),
('1713-12-17','vc-1713-351',10732,'275','Thirty-second marriage since 21 July; clerk connects renewed marriage to unions broken by smallpox','Direct image count and connection; not all couples established as remarriages or widowed partners'),
('1714-02-13','vc-1714-44',10733,'51-52','Four kraals; request new captains; one in ten of their company remained','Direct image; delegation date13, not15 in abridged Leibbrandt'),
('1714-02-15','vc-1714-46',10733,'56','Four successors, all kin: Scipio Africanus (brother and heir of Asdrubal), Hanibal (brother and heir of Jazon), Hercules (nephew, and heir of Hartloop), Kolinga (son and heir of Koeinga the Elder); staffs of office','Hercules and Kolinga visible on scan 56; the two brothers are in the supplied transcription and Leibbrandt (1896, p. 258) but their portion was not located in the image sequence')]
write(S/'journal_concordance.csv',[dict(date=d,workbook_id=i,inventory=v,scans=s,evidence=e,check_and_limits=l,checker='Hand and AI verified: AI-assisted image/text comparison (Codex Astra, 23 September 2026; Claude Code Fable, 8 October 2026), checked by hand by the author against the scans (8 October 2026)') for d,i,v,s,e,l in concord])
ledger=[
('Previous Company stock','Year before 26 August 1713','568','Derived as 441 + 127; not a separately recovered individual muster'),
('Company stock','26 August 1713','441','Stated in journal'),
('Net Company stock loss','Between annual musters','127','Stated; chiefly attributed to smallpox'),
('Arrivals and births','Current year','Not separately numbered','Journal explicitly mentions recruitment from Ceylon and local births'),
('Company loss after allowing for additions','Current year','Nearly 200','Contemporary approximate calculation; not a reconciled individual cohort'),
('Private imports and transfers','1712-1714','Not quantified','Cannot use Company arrivals to explain private adult stocks as an established fact'),
]
write(O/'company_flow_ledger.csv',[dict(quantity=a,window=b,value=c,interpretation=d) for a,b,c,d in ledger])

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
print('Saved source, family, probate, journal, Company registers and tables.')
