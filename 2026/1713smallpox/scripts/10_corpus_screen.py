"""Transparent, overlapping retrieval nets over every supplied 1700-1720 entry.

These are candidate and lexical indicators, not adjudicated semantic labels.
"""
from pathlib import Path
import csv,re,json,collections,hashlib
import openpyxl
R=Path(__file__).resolve().parents[1];D=R/'full_corpus';D.mkdir(exist_ok=True)
def read(p):
    with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def write(p,rs):
    with p.open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(dict.fromkeys(k for r in rs for k in r)));w.writeheader();w.writerows(rs)
source=R.parent.parent/'sources/data/vc_datastel (v3).xlsx'
wb=openpyxl.load_workbook(source,read_only=True,data_only=True)
entries=[]
for row in wb.worksheets[0].iter_rows(min_row=2,max_col=4,values_only=True):
    if not row[1] or not str(row[1])[:4].isdigit():continue
    year=int(str(row[1])[:4])
    if not 1700<=year<=1720:continue
    t=re.sub(r'\s+',' ',re.sub('<[^>]+>',' ',str(row[3] or ''))).strip()
    entries.append(dict(id=row[0],date=str(row[1])[:10],year=str(year),text=t,has_text=str(int(t.lower() not in ('','geen teks')))))
wb.close()
assert len(entries)==7670 and len({r['id'] for r in entries})==7670
write(D/'entries.csv',entries)
(D/'input.json').write_text(json.dumps({'path':str(source),'sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'transcription':'Tracing History Trust; supplied by Helena Liebenberg','extraction':'First four workbook columns; strip markup and collapse whitespace; preserve calendar date and original ID.'},indent=2),encoding='utf-8')
NETS={
 'pox':r'\b(?:\w*pok\w*|\w*pock\w*|\w*pocq\w*|variol\w*)\b',
 'illness':r'\b(?:s[iy]e[ck]+\w*|z[iy]e[ck]+\w*|siek\w*|ziek\w*|krank\w*|kran[ck]+\w*|koorts\w*|besmet\w*|pest\w*|epidemi\w*)\b',
 'death':r'\b(?:overle[deefv]\w*|overlij\w*|gestorv\w*|sterf\w*|sterv\w*|dood\w*|doot\w*|dode\w*|dooden|doode|lijken|lijkken|lijcken|l[yi]jk|lyck|l[yi]jcken|begra[avf]\w*|begr[ae]ven\w*|afgestorv\w*|wijlen|wylen|omgebragt|omgebracht|moord\w*|vermoord\w*|homicid\w*|geexecute\w*|geexecut\w*)\b',
 'medicine':r'\b(?:medic\w*|medec\w*|genee?[sz]\w*|(?:opper)?chirurg\w*|chijrurg\w*|sirurg\w*|do[ck]t[oe]r\w*|doctor\w*|apot\w*|remedi\w*|remedij\w*|remedie\w*|(?:ge)?cureer\w*|curer\w*|cuur\w*|kuur\w*|balsem\w*|droger\w*|quaran\w*|quaren\w*|inent\w*|inocul\w*|recept(?:en)?|saffraan\w*|poeij[er]\w*|poeier\w*|poeder\w*|(?:ge)?reconval\w*)\b',
 'hospital':r'\b(?:hospit\w*|hosp[ei]t\w*|gasthu[yi]s\w*|sie[ck]+enhu[yi]s\w*|sie[ck]+hu[yi]s\w*)\b',
 'recovery_care':r'\b(?:herstel\w*|verple[eg]\w*|oppass\w*|besorg\w*|besorging\w*|gesond\w*|gezond\w*|ververs\w*|verfris\w*|assist\w*|onderstand\w*)\b',
 'khoesan':r'hottent|son[ckq]+ua|bosjesman|bossiesman|\bwilden\b',
 'enslaved':r'\b(?:sla[av]+\w*|lij[fv]e[yi]g\w*|lijve[yi]g\w*|lyfeyg\w*|mancip\w*)\b',
 'settler':r'\b(?:burger\w*|burgher\w*|coloni\w*|ingeset\w*|ingezet\w*|inwoond\w*|inwoon\w*|vrijman\w*|vrijlieden\w*)\b',
 'maritime':r'\b(?:schip\w*|scheep\w*|schep\w*|matro\w*|solda\w*|bootsg\w*|equipag\w*|scheepsvolk\w*)\b',
 'batavia':r'\bbatavia\w*\b',
}
compiled={k:re.compile(v,re.I) for k,v in NETS.items()}
# 'Dead calm' (dood stil, doodstil, doodelijke stilte) is weather, not death.
CALM=re.compile(r'\b(?:doo?d|doot)\w*[\s-]*stil\w*',re.I)
out=[];snips=[]
for r in entries:
    t=r['text'];a={k:r[k] for k in ['id','date','year','has_text']}
    for k,rx in compiled.items():
        tk=CALM.sub(lambda m:' '*len(m[0]),t) if k=='death' else t
        matches=list(rx.finditer(tk));a[k]=int(bool(matches))
        if k in ['pox','illness','death','medicine','hospital']:
            for m in matches:snips.append(dict(id=r['id'],date=r['date'],net=k,term=m[0],passage=t[max(0,m.start()-180):min(len(t),m.end()+230)]))
    a['health_candidate']=int(any(a[k] for k in ['pox','illness','death','medicine','hospital']))
    a['text']=t;out.append(a)
write(D/'retrieval.csv',out);write(D/'retrieval_spans.csv',snips)
(D/'retrieval_protocol.json').write_text(json.dumps({'scope':'Every supplied 1700-1720 calendar entry; missing text excluded from denominators','nets':NETS,'interpretation':'Lexical retrieval only; co-occurrence is not group-specific attribution; expanded pox net intentionally retains capok negatives; the death net ignores dead-calm weather phrases; Affricaan and inboorling denote Cape-born people in this period and are not Khoesan terms.'},indent=2),encoding='utf-8')
for name,rs in [
 ('pox_candidates',[r for r in out if r['pox']]),
 ('medical_candidates',[r for r in out if r['medicine']]),
 ('epidemic_context',[r for r in out if r['year'] in ('1713','1714') and r['health_candidate']]),
 ]:
    (D/(name+'.md')).write_text('\n\n'.join(f"## {r['id']} | {r['date']}\n\n{r['text']}" for r in rs),encoding='utf-8')
    print(name,len(rs),'words',sum(len(r['text'].split()) for r in rs))
print('Net counts', {k:sum(r[k] for r in out) for k in NETS})
# Disjoint, complete batches of the original sample, with labels hidden for first-pass review.
sample=read(R/'validation/validation_all_400.csv');sample=sorted(sample,key=lambda r:(r['date'],r['id']))
folder=D/'sample_batches';folder.mkdir(exist_ok=True)
for i in range(0,len(sample),25):
    rs=sample[i:i+25]
    (folder/f'batch_{i//25+1:02}.md').write_text('\n\n'.join(f"## {r['id']} | {r['date']}\n\n{r['text']}" for r in rs),encoding='utf-8')
