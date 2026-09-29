# Replication data: The Dismal State of the Dismal Science in South Africa

Aggregate data for

> Fourie, J. (2026). "The Dismal State of the Dismal Science in South Africa."
> Working paper, Department of Economics, Stellenbosch University; submitted to the
> *South African Journal of Economics*.

Canonical public home of this package:
https://github.com/johanfourieza/research/tree/main/2026/dismal-science-south-africa

The paper measures how often economists at South African institutions, and economists
educated in South Africa, publish in the leading economics journals over 1990-2025,
compares South Africa with 25 other countries, and examines topic composition,
publication growth, research networks, doctoral training and the composition of
leading-journal authors.

## What this release contains

This first release contains **aggregates only**: every table in the paper and its
online appendix as a CSV file, the aggregate data behind every figure, and every
number cited in the text. Together these reproduce the tables and redraw the figures.

- `data/tables/`: one CSV per table, named by its number in the submitted paper
  (`Table_1` to `Table_4` in the main text, `Table_S1` to `Table_S17` in the online
  appendix). Cells are as printed; `12 (3.4)` means 12 articles and 3.4 fractional
  article-equivalents where the table note says so.
- `data/figures/`: the aggregate series plotted in each figure.
- `data/text_numbers.csv`: every number cited in the text, with the LaTeX macro name
  used in the manuscript.
- `scripts/load_data.R`: loads everything into a named list in R.

Article-level data (the publication corpora with affiliation and topic
classifications, the validation sample and the policy-speech corpus) and the full
R and Python pipeline will be added on acceptance.

## Data not released

The paper reports the population group and gender of domestic leading-journal
authors. These were hand-coded for named individuals from public biographical
information and are sensitive personal information, so the person-level coding is
not released. Only the aggregate shares printed in the paper are included; no group
of fewer than five people is reported.

## Files

| Item | Description | File |
|---|---|---|
| Table 1 | Topic shares in research and policy speeches. | `data/tables/Table_1_policy_attention_split.csv` |
| Table 2 | Journal placement and university rankings. | `data/tables/Table_2_journal_tier_counts.csv` |
| Table 3 | Concentration of domestic nineteen-journal output. | `data/tables/Table_3_author_concentration.csv` |
| Table 4 | Leading-journal articles by authors' country of education, 1990-2025. | `data/tables/Table_4_education_counts.csv` |
| Table S1 | Top-five shares under alternative journal denominators. | `data/tables/Table_S1_journal_denominators.csv` |
| Table S2 | National sample construction by period. | `data/tables/Table_S2_national_sample_flow.csv` |
| Table S3 | Topic ratios under alternative benchmarks. | `data/tables/Table_S3_topic_benchmarks.csv` |
| Table S4 | Corrected topic shares and intervals. | `data/tables/Table_S4_topic_corrected_levels.csv` |
| Table S5 | Publication growth within base-period sources. | `data/tables/Table_S5_source_growth.csv` |
| Table S6 | Unresolved affiliations and output-growth sensitivity. | `data/tables/Table_S6_affiliation_missingness.csv` |
| Table S7 | Doctoral routes and missing-country bounds. | `data/tables/Table_S7_census_routes.csv` |
| Table S8 | Prior connections by journal basket, 2015-24. | `data/tables/Table_S8_network_baskets.csv` |
| Table S9 | University rankings and recent economics publication. | `data/tables/Table_S9_university_recent_comparison.csv` |
| Table S10 | Prior collaboration with top-five authors, 2015-24. | `data/tables/Table_S10_prior_network_countries.csv` |
| Table S11 | Classifier precision and recall by topic. | `data/tables/Table_S11_classifier_precision_recall.csv` |
| Table S12 | Abstract coverage by journal and period. | `data/tables/Table_S12_abstract_coverage_by_journal_period.csv` |
| Table S13 | Research on South Africa by journal subset and period. | `data/tables/Table_S13_attention_subsets.csv` |
| Table S14 | Publication growth and journal placement by country. | `data/tables/Table_S14_peer_growth.csv` |
| Table S15 | South African author positions in top-five journals, by publication type. | `data/tables/Table_S15_topfive_audit.csv` |
| Table S16 | Research on South Africa in regular top-five articles. | `data/tables/Table_S16_regular_topfive_attention.csv` |
| Table S17 | Descriptive comparisons of firm-topic shares. | `data/tables/Table_S17_firm_event_study.csv` |
| Figure 1 | Research on South Africa: publication rates and authorship shares by bin | `data/figures/Figure_1_fig1_wedge_data.csv` |
| Figure 1 | Coverage-corrected attention series | `data/figures/Figure_1_attention_coverage.csv` |
| Figure 2 | Main data source by period: article counts and domestic share | `data/figures/Figure_2_fig2_data_grid.csv` |
| Figure 3 | Observed and predicted topic shares | `data/figures/Figure_3_fig3_topic_residuals.csv` |
| Figure 3 | Topic ratios across benchmark specifications | `data/figures/Figure_3_topic_benchmark_sensitivity.csv` |
| Figure 4 | Topic map: hexagon-level comparator and domestic mass | `data/figures/Figure_4_fig3_map_hexes_journal_main.csv` |
| Figure 4 | Topic map: topic label positions | `data/figures/Figure_4_fig3_map_topic_labels_journal_main.csv` |
| Figure 5 | Output, leading-journal placement and top-decile impact series | `data/figures/Figure_5_fig4_series.csv` |
| Figure 6 | Output and placement growth by country | `data/figures/Figure_6_all_country_source_growth.csv` |
| Figure 7 | Researcher counts and doctoral routes by country | `data/figures/Figure_7_fig6_peer_comparison.csv` |
| Figure 7 | Doctoral-route missingness bounds by country | `data/figures/Figure_7_census_route_missingness.csv` |
| Figure S1 | Attention to South Africa by journal subset | `data/figures/Figure_S1_attention_subsets.csv` |
| Figure S2 | Classifier-corrected topic ratios | `data/figures/Figure_S2_fig3_topic_residuals_corrected.csv` |
| Figure S3 | Annual levels (see Figure 5 series) | `data/figures/Figure_S3_fig4_series.csv` |
| Figure S4 | Firm-topic shares by country and year | `data/figures/Figure_S4_firm_event_panel.csv` |
| Figure S4 | Firm-topic event-study coefficients | `data/figures/Figure_S4_firm_event_study_coefficients.csv` |
| Figure S4 | Firm-topic synthetic comparison series | `data/figures/Figure_S4_firm_synthetic_series.csv` |
| Text | Every number cited in the text (LaTeX macro name and value) | `data/text_numbers.csv` |

## Sources

Publication records: OpenAlex and Crossref (downloaded August 2026). University
rankings: Times Higher Education World University Rankings 2026. Student and
accredited-author comparisons: Branson and Whitelaw (2024), *Women in Economics in
South Africa*, International Economic Association.

## License

CC BY 4.0. See `LICENSE`.

## Contact

Johan Fourie, johanf@sun.ac.za, https://www.johanfourie.com
