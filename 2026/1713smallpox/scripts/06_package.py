"""Package review copies and check invariants without altering shared sources."""
from pathlib import Path
import csv, json, re, subprocess, difflib, hashlib, sys, importlib.metadata, textwrap
import fitz

R=Path(__file__).resolve().parents[1]; M=R/'manuscript'; RESP=R/'response'; LOG=R/'logs'
def read(p):
    with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def flatten(p):
    s=p.read_text(encoding='utf-8')
    return re.sub(r'\\input\{([^}]+)\}',lambda m:flatten(p.parent/(m[1] if '.' in Path(m[1]).name else m[1]+'.tex')) if (p.parent/(m[1] if '.' in Path(m[1]).name else m[1]+'.tex')).exists() else m[0],s)
def plain(p):
    source=flatten(p)
    title=re.search(r'\\title\{([^}]+)\}',source)
    abstract=re.search(r'\\begin\{abstract\}(.*?)\\end\{abstract\}',source,re.S)
    front=(title[1].replace('--','–')+'\n\n' if title else '')
    if abstract:
        front+='Abstract\n\n'+subprocess.run(['pandoc','--from=latex','--to=plain','--wrap=none'],input=abstract[1],text=True,encoding='utf-8',capture_output=True,check=True).stdout+'\n'
    proc=subprocess.run(['pandoc','--from=latex','--to=plain','--wrap=none'],input=source.replace(r'\endnote{',r'\footnote{'),text=True,encoding='utf-8',capture_output=True,check=True,cwd=M)
    return front+proc.stdout
old=plain(R/'baseline/manuscript.tex');new=plain(M/'manuscript.tex');supp=plain(M/'supplement.tex')
(M/'manuscript_reading_copy.txt').write_text(new,encoding='utf-8')
(M/'supplement_reading_copy.txt').write_text(supp,encoding='utf-8')
(M/'submitted_reading_copy.txt').write_text(old,encoding='utf-8')
def wrapped(s):
    return [line for para in s.split('\n\n') for line in textwrap.wrap(para,85)+['']]
html=difflib.HtmlDiff(wrapcolumn=85).make_file(wrapped(old),wrapped(new),'Submitted manuscript','Revised working draft',context=False,charset='utf-8')
html=html.replace('</head>','<style>body{font-family:Arial,sans-serif;margin:2rem}table.diff{width:100%;font-size:11px}td{vertical-align:top} .diff_header{background:#eee} .diff_add{background:#dbefdc}.diff_sub{background:#f5d9df}.diff_chg{background:#fff0c9}</style></head>')
html=html.replace('<body>','<body><h1>Comparison with the submitted manuscript</h1><p>Complete side-by-side text comparison. Green marks additions; red marks deletions. This is a reading aid generated through Pandoc, not a comparison of rendered figures or formatting. Consult the two PDFs for exhibits and the source files for archival notes. Major restructuring produces large replacement blocks.</p>')
(M/'comparison_with_submission.html').write_text(html,encoding='utf-8')
(M/'source_changes.diff').write_text(''.join(difflib.unified_diff(flatten(R/'baseline/manuscript.tex').splitlines(True),flatten(M/'manuscript.tex').splitlines(True),fromfile='submitted manuscript (expanded)',tofile='revised manuscript (expanded)')),encoding='utf-8')

# Response coverage and a completed decision register.
s=(RESP/'response_to_referees.md').read_text(encoding='utf-8')
parts=re.split(r'(?m)^### (R[123]\.\d{2}) — ([^\n]+)\n',s)
rows=[]
qualified={'R1.04','R1.05','R1.07','R1.08','R1.10','R1.13','R1.15','R2.06','R2.08','R2.09','R2.10','R2.12','R3.04','R3.16','R3.22','R3.26','R3.28'}
for i in range(1,len(parts),3):
    ident,title,body=parts[i:i+3]
    location=re.search(r'(?m)^Location: (.+)',body)[1]
    response=re.search(r'Response: (.*?)(?:\n\nLocation:)',body,re.S)[1].strip()
    status='Implemented in working draft'
    if ident in qualified:status='Addressed through rebuilt measurement, source analysis and explicit scope'
    if ident=='R2.14':status='Disclosure revised; Johan will undertake further manuscript work and final author review'
    rows.append(dict(id=ident,concern=title,status=status,location=location,action=response))
expected={f'R{r}.{i:02}' for r,n in [(1,15),(2,14),(3,28)] for i in range(1,n+1)}
assert {r['id'] for r in rows}==expected and len(rows)==57
with (RESP/'completion_register.csv').open('w',encoding='utf-8-sig',newline='') as f:
    w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
md='# Referee concern completion register\n\nImplementation status of the working draft; not a certification of acceptance or author approval. See AUTHOR_REVIEW.md for remaining submission issues.\n\n| ID | Concern | Status | Location |\n|---|---|---|---|\n'
md+='\n'.join('| '+' | '.join(r[k] for k in ['id','concern','status','location'])+' |' for r in rows)
(RESP/'completion_register.md').write_text(md+'\n',encoding='utf-8')

# Final PDF labels and starting page map, generated after the final LaTeX run.
labels=[]
for doc in ('manuscript','supplement'):
    aux=(M/(doc+'.aux')).read_text(encoding='utf-8')
    labels+=[(l[0],l[1],l[2]+(' (supplement)' if doc=='supplement' else ''),l[3]) for l in re.findall(r'\\newlabel\{([^}]+)\}\{\{([^}]+)\}\{([^}]+)\}\{([^}]+)',aux) if not l[0].startswith(('S-','M-'))]
page_map='# Revised manuscript page map\n\nPage numbers refer to manuscript/manuscript.pdf for the article and to manuscript/supplement.pdf for Appendices A–E (online supplementary material). Sections begin on the listed pages; use the cited subsection heading within that section.\n\n| Element | Title | Starts on page |\n|---|---|---|\n'
for key,num,page,title in labels:
    kind={'sec':'Section','app':'Appendix','fig':'Figure','tab':'Table','eq':'Equation'}[key.split(':')[0]]
    title=title.split('.')[0].replace('--','–').split(chr(92))[0]
    page_map+=f'| {kind} {num} | {title} | {page} |\n'
(RESP/'page_map.md').write_text(page_map,encoding='utf-8')
# Add the page map to distributable response versions, keeping the editable letter uncluttered.
(RESP/'response_with_page_map.md').write_text(s+'\n\n'+page_map,encoding='utf-8')
subprocess.run(['pandoc',str(RESP/'response_with_page_map.md'),'-o',str(RESP/'response_to_referees.docx')],check=True)
subprocess.run(['pandoc',str(RESP/'response_with_page_map.md'),'-o',str(RESP/'response_to_referees.pdf'),'--pdf-engine=pdflatex','-V','geometry:margin=25mm','-V','fontsize=11pt','-V','colorlinks=true'],check=True,capture_output=True)

# Verify inputs still match the extraction's saved hashes.
checks=[]
for f,base in [('input_manifest.csv',R.parent.parent),('households_input_manifest.csv',None)]:
    for x in read(R/'sources'/f):
        p=Path(x['path']) if base is None else base/x['path']
        checks.append(dict(path=str(p),unchanged=digest(p)==x['sha256']))
assert all(c['unchanged'] for c in checks)
prob=read(R/'outputs/probate_register_1713_1714.csv');v=read(R/'validation/validation_all_400.csv')
assert len(prob)==90 and len(v)==400
assert sum(x['year']=='1713' for x in prob)==54 and sum(x['year']=='1714' for x in prob)==36
assert len({x['id']+'@'+x['date_value'] for x in prob})==90
assert sum(x['stored_V1']!='' for x in v)==393
assert sum(x['karli_V1']!='' for x in v)==79
assert sum(x['martie_V1']!='' for x in v)==77
corpus=read(R/'full_corpus/retrieval.csv');pox=read(R/'full_corpus/pox_candidate_review.csv')
annual=read(R/'full_corpus/annual_lexical_counts.csv')
assert len(corpus)==7670 and sum(r['has_text']=='1' for r in corpus)==7666
assert len({r['id'] for r in corpus})==7670 and len(annual)==21
assert len(pox)==18 and sum(r['pox_reference']=='1' for r in pox)==17
assert sum(int(r['pox_reference_entries']) for r in annual)==17
assert {r['year']:int(r['pox_reference_entries']) for r in annual if int(r['pox_reference_entries'])}=={'1713':16,'1714':1}
assert len(read(R/'full_corpus/medical_passage_review.csv'))==94
assert len(read(R/'sources/journal_concordance.csv'))==11
ci=json.loads((R/'full_corpus/input.json').read_text(encoding='utf-8'))
assert digest(Path(ci['path']))==ci['sha256']
assert all(not x['author_decision'] for x in v), 'Do not overwrite author decisions; store separately.'
for doc in ('manuscript','supplement'):
    log=(M/(doc+'.log')).read_text(errors='replace')
    assert 'undefined references' not in log and 'Overfull' not in log and 'LaTeX Error' not in log and 'Reference `' not in log, doc
    assert '??' not in '\n'.join(p.get_text() for p in fitz.open(M/(doc+'.pdf'))), doc
pdf=fitz.open(M/'manuscript.pdf')
bold=[]
for i,p in enumerate(pdf):
    for block in p.get_text('dict')['blocks']:
        for line in block.get('lines',[]):
            for span in line['spans']:
                if 'Bold' in span['font'] or 'Demi' in span['font']:
                    bold.append({'page':i+1,'text':span['text'],'font':span['font']})
(LOG/'bold_spans.json').write_text(json.dumps(bold,indent=2),encoding='utf-8')
abstract=re.search(r'\\begin\{abstract\}(.*?)\\end\{abstract\}',(M/'manuscript.tex').read_text(encoding='utf-8'),re.S)[1]
report=dict(date='2026-10-08',pdf_pages=len(pdf),response_points=len(rows),abstract_words=len(abstract.split()),
    reading_copy_words=len(new.split()),main_through_conclusion_approx_words=len(new.split('Acknowledgements')[0].split()),
    article_words_excluding_references=len(new.split('\nReferences\n')[0].split())+len(new.split('\n[1] ',1)[-1].split()),
    supplement_pages=len(fitz.open(M/'supplement.pdf')),
    appendix_approx_words=len(supp.split('\nReferences\n')[0].split()),
    inputs_checked=len(checks),all_source_hashes_unchanged=True,probate_documents=90,sample_ids=400,stored_reference_labels=393,
    karli_answers=79,martie_answers=77,full_corpus_entries=7670,full_corpus_usable_texts=7666,
    pox_candidates_reviewed=18,pox_references=17,medical_candidates=94,image_concordance_entries=11,
    latex_undefined_references=False,latex_overfull_boxes=False,
    comparison='Complete text HTML and expanded-source unified diff; no marked PDF',
    human_adjudication='Not certified; no generated label substituted for human or author decisions',
    submission_status='Resubmission build after source audit of 8 October 2026; see AUDIT_REPORT_2026-10-08.md.')
(LOG/'verification.json').write_text(json.dumps(report,indent=2),encoding='utf-8')
(LOG/'source_hash_checks.json').write_text(json.dumps(checks,indent=2),encoding='utf-8')
(R/'replication/requirements.txt').write_text('\n'.join(f'{x}=={importlib.metadata.version(x)}' for x in ['openpyxl','pandas','matplotlib','PyMuPDF','requests','beautifulsoup4'])+'\n',encoding='utf-8')
(R/'replication/environment.json').write_text(json.dumps({'python':sys.version,'executable':sys.executable},indent=2),encoding='utf-8')
print(json.dumps(report,indent=2))
