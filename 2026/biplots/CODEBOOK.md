# Source records and analysis variables

The revised paper is “Biplots for historical household data: Evidence from Cape tax records”. This codebook describes generated objects, not a claim that every recorded quantity is measured without error.

## Original inputs

| File in data/raw | Role |
|---|---|
| stellenbosch_temp.csv | 142,293 linked household-year records, 235 source columns, 1685–1844 |
| 1825 series.xlsx | Eleven district sheets dated 1823–1825 |
| spouse_temp.csv | Retained reference file; not used by the revised analysis |
| 1830s series.xlsx | Retained reference workbook; not used by the revised analysis |

Raw-file SHA-256 hashes are checked before and after each build. Raw files are access-restricted and are not included in this release.

## Generated samples

The objects below are produced by the pipeline from the restricted inputs. They contain household-level records and are not included in this release; `data/results/` holds the aggregate outputs derived from them.

| File in data/analysis | Primary population or contents |
|---|---|
| opgaaf_clean.rds | Source identifiers, numeric economic fields, coverage-aware totals and audit flags |
| regime_D_biplot.rds | 20,913 household-years, 8,072 distinct household identifiers, 13 active variables |
| regime_B_biplot.rds | 39,850 household-years, 11,350 identifiers, eight common variables |
| bridge_biplot.rds | 60,522 household-years, 18,787 identifiers, eight common variables |
| colony_1825.rds | 8,405 economically observed district household records |
| regime_D_blank_assumption.rds | Conditional broader D population, assigning wholly blank eligible returns zero |
| regime_B_blank_assumption.rds | Corresponding broader earlier population |
| bridge_blank_assumption.rds | Corresponding broader common-variable population |
| colony_blank_assumption.rds | Corresponding broader district population |
| var_availability.rds | Raw field-year observed coverage, zero and blank counts, and evidence definitions |
| analysis_config.rds | Variable lists, definitions, source assumptions, limits and sample flow |
| revision_results.rds | Complete models, saved coordinates, results tables, sensitivity and estimation design |

Each linked panel observation has a unique `hhobs` and a source CSV row. `hhid` defines the resampling unit. Household histories may have gaps. Colony identifiers use the district and original Excel row; they are not links to panel households. District records retain actual sheet year, sheet name and source row.

D has observed comparable years 1804, 1805, 1806, 1809, 1811–1814, 1816–1822, 1824 and 1825. The common comparison spans covered years in 1738–1795 and 1804–1825. Neither is a balanced annual panel. Bridge eligibility uses its own active raw components, so its later-period membership is not identical to D.

## Variables and units

The generated dictionary is `data/results/variable_definitions_revised.csv`, copied to `docs/variable_definitions.csv`. It also supplies the manuscript table.

| Variable | Definition | Source unit |
|---|---|---|
| slave_men; slave_women | Recorded adult enslaved men and women, separately | Counts |
| slaves_total | Men, women, sons and daughters | Count |
| khoe_total | Khoekhoe men, women, sons and daughters | Count |
| cattle_total | Earlier aggregate; later work plus breeding categories | Count |
| horses_total | Earlier aggregate; later riding plus breeding categories | Count |
| sheep_total | Earlier aggregate; later breeding, wethers and wool-bearing categories | Count |
| vines | Recorded vines | Count |
| wine; brandy | Reported output | Leaguers |
| wheat_vol | Reported wheat harvest | Muids |
| grain_other | Barley plus rye harvest; oats excluded throughout | Muids |
| wagons; goats | Recorded quantities | Counts |

The district variables use analogous totals named `total_slaves`, `total_khoe`, `horses`, `cattle`, `sheep`, `goats`, `wheat_reaped`, `grain_other_reaped`, `wine` and `brandy`. Source column maps and exact workbook headers are in `data/results/source_column_map_colony.csv`.

## Recording and exclusion rules

The cleaning script reads the panel as text and explicitly parses economic columns. An entire unrecorded field-year remains missing. Totals require all constituent fields to be available; missing components are not dropped from addition. Within economically observed returns, a genuinely blank covered field is conditionally assigned zero. A wholly blank active economic return is excluded from the primary population and retained only in the broader reporting-assumption sensitivity. A numeric zero counts as an observation. No three-positive-variable requirement is applied to the primary sample.

Workbook numeric parsing accepts decimals, scientific notation, Unicode and ASCII fractions, mixed numbers and written sixteenths. A single numeric line accompanied by nonnumeric annotation can be interpreted; multiple distinct numeric candidates in a cell remain ambiguous. Source text is preserved in the parse register. A head marker does not establish economic observability.

The panel's field-year coverage register is a proxy based on observed transcription cells. It is not independent verification that every original form in that year contained a field. Extreme quantities are flags, not proven errors. Four district records exceeding the former ceilings are retained if nonnegative and excluded in a sensitivity comparison. Neither source transcripts nor archival scans were rewritten.

## Estimation objects

The baseline uses standardised log(1+x) in documented source units. Saved objects contain centres, scales, transformations, eigenvalues, rotations, row-principal scores, sample identities, predictivity and reconstruction errors. Covariances use normalised population weights; this changes the eigenvalue divisor relative to an n−1 convention but not ordinary PCA directions or variance shares.

The temporal model is estimated once from the pooled eligible common-variable population. Frames use that transformation and coordinate system. Clustering uses full numerical rank as its reference representation and evaluates truncated dimensions separately. Resampling retains linked household histories. Theil T is calculated on unshifted nonnegative source quantities, with zero contributions defined by continuity and all-zero-population inequality left undefined.

All numerical tables, source registers and manuscript macros are regenerated. Removed compositional, event-study and 1830s objects are absent from the active final output directory.

