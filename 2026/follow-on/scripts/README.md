# Replication scripts

The full pipeline runs from `run_all.R` (nine steps, about three minutes).
It is included for transparency, with one caveat: step `01_data.R` reads the
licensed DAFT first-class database, which cannot be redistributed. Without it
the pipeline cannot run end to end.

Everything in the paper can nevertheless be checked from the released file.
`../followon_matches.csv` contains the derived analysis variables for all
22,113 matches, and `../load_data.R` walks through the first stage, the naive
comparison, the regression discontinuity estimate, the manipulation test and
the donut check from that file alone. It needs the `rdrobust` package.

Researchers who want the source database can license it from its compilers,
Ric Finlay and Jim Palfreyman, or contact the author (johanf@sun.ac.za).
