"""Run from any directory. Shared inputs are read-only; all outputs stay in revision."""
from pathlib import Path
import subprocess,sys
R=Path(__file__).resolve().parents[1]
for name in ['01_extract.py','02_demography.py','04_evidence_registers.py','05_appendix_tables.py','10_corpus_screen.py','11_corpus_results.py']:
    print('Running',name,flush=True)
    with (R/'logs'/('rebuild_'+name+'.log')).open('w',encoding='utf-8') as f:
        subprocess.run([sys.executable,'-X','utf8',str(R/'analysis'/name)],cwd=R,stdout=f,stderr=subprocess.STDOUT,check=True)
# The article and its online supplement cross-reference each other (xr-hyper),
# so they are compiled alternately until labels settle.
for i in (1,2,3):
    for doc in ('supplement','manuscript'):
        with (R/'logs'/f'{doc}_build_{i}.txt').open('w',encoding='utf-8') as f:
            subprocess.run(['pdflatex','-interaction=nonstopmode','-halt-on-error',doc+'.tex'],cwd=R/'manuscript',stdout=f,stderr=subprocess.STDOUT,check=True)
subprocess.run([sys.executable,'-X','utf8',str(R/'analysis/06_package.py')],cwd=R,check=True)
print('Revision rebuilt and checked. Author review remains separate from these automated checks.')
