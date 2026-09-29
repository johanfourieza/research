# Codebook

One section per file. Column lists are taken from the files. Tables reproduce the
printed tables in the paper; see the paper's table notes for definitions.

## `data/tables/Table_1_policy_attention_split.csv`

Table 1: Topic shares in research and policy speeches.. 13 rows.

- `Topic`
- `All addresses`
- `SONA`
- `Budget`
- `SA- produced`
- `About-SA frontier`
- `Comparators`

## `data/tables/Table_2_journal_tier_counts.csv`

Table 2: Journal placement and university rankings.. 26 rows.

- `Country`
- `Regular top five`
- `Ten journals`
- `Nineteen journals`
- `Top-five share (%)`
- `THE top 500 (2026)`

## `data/tables/Table_3_author_concentration.csv`

Table 3: Concentration of domestic nineteen-journal output.. 20 rows.

- `Country`
- `Period`
- `Authors`
- `Article- eq.`
- `Top 5 (%)`
- `Top 10 (%)`
- `Top 20 (%)`
- `Effective authors`
- `2 articles`
- `3 articles`

## `data/tables/Table_4_education_counts.csv`

Table 4: Leading-journal articles by authors' country of education, 1990-2025.. 15 rows.

- `Country`
- `Identified authors`
- `Regular top five`
- `Top five, home-based`
- `Ten journals`
- `Ten, home-based`
- `Nineteen journals`
- `Top five per million`

## `data/tables/Table_S1_journal_denominators.csv`

Table S1: Top-five shares under alternative journal denominators.. 26 rows.

- `Country`
- `19 journals`
- `18 journals`
- `17 journals`
- `17 rank`
- `10 journals`
- `10 rank`

## `data/tables/Table_S2_national_sample_flow.csv`

Table S2: National sample construction by period.. 9 rows.

- `Period`
- `Collected`
- `Journals`
- `With abstracts`
- `Classified journals`
- `Positive SA weight`
- `Weight retained (%)`

## `data/tables/Table_S3_topic_benchmarks.csv`

Table S3: Topic ratios under alternative benchmarks.. 6 rows.

- `Portfolio`
- `Topic`
- `Baseline`
- `Period only`
- `Linear time`
- `Matched eight`
- `Leave-one-out`

## `data/tables/Table_S4_topic_corrected_levels.csv`

Table S4: Corrected topic shares and intervals.. 12 rows.

- `Topic`
- `Domestic (%)`
- `About South Africa (%)`

## `data/tables/Table_S5_source_growth.csv`

Table S5: Publication growth within base-period sources.. 26 rows.

- `Country`
- `All Q`
- `Base-source Q`
- `New sources (%)`
- `F`
- `Original rank`
- `Base-source rank`

## `data/tables/Table_S6_affiliation_missingness.csv`

Table S6: Unresolved affiliations and output-growth sensitivity.. 25 rows.

- `Country`
- `Unknown, early (%)`
- `Unknown, late (%)`
- `Q growth range`
- `Any affiliation`

## `data/tables/Table_S7_census_routes.csv`

Table S7: Doctoral routes and missing-country bounds.. 15 rows.

- `Country`
- `Identified`
- `Known PhD`
- `Unknown`
- `Foreign`
- `Foreign, known (%)`
- `Full-frame range (%)`

## `data/tables/Table_S8_network_baskets.csv`

Table S8: Prior connections by journal basket, 2015-24.. 6 rows.

- `Country`
- `19 journals`
- `17 journals`
- `10 journals`

## `data/tables/Table_S9_university_recent_comparison.csv`

Table S9: University rankings and recent economics publication.. 26 rows.

- `Country`
- `THE top 200`
- `THE top 500`
- `THE top 1000`
- `Regular top five`
- `Ten journals`
- `Top-five share (%)`

## `data/tables/Table_S10_prior_network_countries.csv`

Table S10: Prior collaboration with top-five authors, 2015-24.. 6 rows.

- `Country`
- `Author positions`
- `Prior connection`
- `Share (%)`

## `data/tables/Table_S11_classifier_precision_recall.csv`

Table S11: Classifier precision and recall by topic.. 12 rows.

- `Topic`
- `Precision`
- `Recall`
- `Precision (secondary accepted)`
- `Human positives`
- `Model positives`

## `data/tables/Table_S12_abstract_coverage_by_journal_period.csv`

Table S12: Abstract coverage by journal and period.. 19 rows.

- `Journal`
- `1990–99`
- `2000–09`
- `2010–19`
- `2020–25`
- `All years`
- `Articles`

## `data/tables/Table_S13_attention_subsets.csv`

Table S13: Research on South Africa by journal subset and period.. 10 rows.

- `Subset`
- `Window`
- `Early`
- `Late`
- `Rate ratio`
- `95% interval`

## `data/tables/Table_S14_peer_growth.csv`

Table S14: Publication growth and journal placement by country.. 26 rows.

- `Country`
- `Base F`
- `Q growth`
- `F growth`
- `T growth`
- `g`
- `Rank`

## `data/tables/Table_S15_topfive_audit.csv`

Table S15: South African author positions in top-five journals, by publication type.. 5 rows.

- `Author`
- `Year`
- `Publication type`

## `data/tables/Table_S16_regular_topfive_attention.csv`

Table S16: Research on South Africa in regular top-five articles.. 2 rows.

- `Comparison`
- `Early articles`
- `Late articles`
- `Ratio`
- `95% interval`

## `data/tables/Table_S17_firm_event_study.csv`

Table S17: Descriptive comparisons of firm-topic shares.. 4 rows.

- `Estimator`
- `Estimate (pp)`
- `Placebo rank`
- `Tail fraction`
- `Placebo SD (pp)`

## `data/figures/Figure_1_fig1_wedge_data.csv`

Figure 1: Research on South Africa: publication rates and authorship shares by bin. 7 rows.

- `bin`
- `n_articles`
- `per_1000`
- `ci_lo`
- `ci_hi`
- `frac_sa_based`
- `frac_sa_diaspora`
- `frac_foreign`
- `authorship_sa_based`
- `authorship_sa_diaspora`
- `authorship_foreign`
- `authorship_total`
- `n_corpus_articles`
- `bin_start`
- `bin_mid`

## `data/figures/Figure_1_attention_coverage.csv`

Figure 1: Coverage-corrected attention series. 7 rows.

- `bin`
- `bin_start`
- `bin_mid`
- `n_observed`
- `n_corpus`
- `per_1000_observed`
- `ci_lo`
- `ci_hi`
- `n_uncovered`
- `n_missed_expected`
- `per_1000_corrected`
- `n_missed_expected_period`
- `per_1000_corrected_period`
- `n_missed_expected_journal`
- `per_1000_corrected_journal`
- `n_missed_upper`
- `per_1000_upper`
- `n_restricted`
- `n_corpus_restricted`
- `per_1000_restricted`
- `min_covered_cell`
- `miss_share_used`

## `data/figures/Figure_2_fig2_data_grid.csv`

Figure 2: Main data source by period: article counts and domestic share. 24 rows.

- `class`
- `period`
- `n`
- `sa_share`

## `data/figures/Figure_3_fig3_topic_residuals.csv`

Figure 3: Observed and predicted topic shares. 12 rows.

- `topic`
- `observed_sa_share`
- `predicted_sa_share`
- `weighted_n`
- `raw_n`
- `world_share`
- `world_weighted_n`
- `world_raw_n`
- `observed_share_za`
- `observed_share_aboutsa`
- `predicted_share_za`
- `predicted_share_aboutsa`
- `log2_ratio_za`
- `log2_ratio_aboutsa`
- `country_ci_low_za`
- `country_ci_low_aboutsa`
- `country_ci_high_za`
- `country_ci_high_aboutsa`
- `author_ci_low_za`
- `author_ci_low_aboutsa`
- `author_ci_high_za`
- `author_ci_high_aboutsa`
- `displayed_ci_low_za`
- `displayed_ci_low_aboutsa`
- `displayed_ci_high_za`
- `displayed_ci_high_aboutsa`
- `displayed_ci_source_za`
- `displayed_ci_source_aboutsa`
- `prediction_robust_se_za`
- `prediction_robust_se_aboutsa`
- `weighted_n_za`
- `weighted_n_aboutsa`
- `raw_n_za`
- `raw_n_aboutsa`
- `raw_documents_za`
- `raw_documents_aboutsa`
- `total_analysis_weight_za`
- `total_analysis_weight_aboutsa`
- `world_log2_ratio_za`
- `world_log2_ratio_aboutsa`

## `data/figures/Figure_3_topic_benchmark_sensitivity.csv`

Figure 3: Topic ratios across benchmark specifications. 96 rows.

- `target`
- `topic`
- `predicted`
- `benchmark`
- `observed`
- `ratio`

## `data/figures/Figure_4_fig3_map_hexes_journal_main.csv`

Figure 4: Topic map: hexagon-level comparator and domestic mass. 4431 rows.

- `hex_id`
- `vertex_order`
- `x`
- `y`
- `world_n_raw`
- `za_n_raw`
- `world_mass`
- `za_mass`
- `raw_lq`
- `smoothed_lq`
- `display_log2_lq`
- `low_support`
- `support_k`
- `eb_prior_shape`

## `data/figures/Figure_4_fig3_map_topic_labels_journal_main.csv`

Figure 4: Topic map: topic label positions. 12 rows.

- `topic`
- `label`
- `threshold`
- `x`
- `y`
- `n_docs`
- `weighted_mass`

## `data/figures/Figure_5_fig4_series.csv`

Figure 5: Output, leading-journal placement and top-decile impact series. 36 rows.

- `year`
- `quantity_raw`
- `top_decile_raw`
- `frontier_raw`
- `p90_cited_by`
- `quantity_index`
- `top_decile_index`
- `frontier_index`
- `quantity_ma3`
- `top_decile_ma3`
- `frontier_ma3`

## `data/figures/Figure_6_all_country_source_growth.csv`

Figure 6: Output and placement growth by country. 26 rows.

- `cc`
- `base_Q`
- `end_Q`
- `end_fixed`
- `growth_Q_check`
- `growth_Q_fixed`
- `new_source_pct`
- `country`
- `base_F`
- `growth_Q`
- `growth_F`
- `growth_T`
- `g`
- `rank`
- `thin_base`
- `g_fixed`
- `rank_fixed`

## `data/figures/Figure_7_fig6_peer_comparison.csv`

Figure 7: Researcher counts and doctoral routes by country. 15 rows.

- `unit`
- `unit_name`
- `group`
- `machine_count`
- `rate`
- `rate_low`
- `rate_high`
- `machine_file_n`
- `phd_known_n`
- `phd_home_n`
- `phd_foreign_n`
- `foreign_phd_share`
- `foreign_phd_low`
- `foreign_phd_high`
- `count_matches`
- `display_group`

## `data/figures/Figure_7_census_route_missingness.csv`

Figure 7: Doctoral-route missingness bounds by country. 15 rows.

- `unit`
- `unit_name`
- `group`
- `machine_count`
- `rate`
- `rate_low`
- `rate_high`
- `machine_file_n`
- `phd_known_n`
- `phd_home_n`
- `phd_foreign_n`
- `foreign_phd_share`
- `foreign_phd_low`
- `foreign_phd_high`
- `count_matches`
- `display_group`
- `machine_per_million`
- `unknown`
- `unknown_pct`
- `route_lower`
- `route_upper`

## `data/figures/Figure_S1_attention_subsets.csv`

Figure S1: Attention to South Africa by journal subset. 35 rows.

- `subset`
- `label`
- `bin`
- `bin_start`
- `bin_mid`
- `n_articles`
- `n_corpus_articles`
- `per_1000`
- `ci_lo`
- `ci_hi`

## `data/figures/Figure_S2_fig3_topic_residuals_corrected.csv`

Figure S2: Classifier-corrected topic ratios. 12 rows.

- `topic`
- `corrected_share_za`
- `corrected_share_aboutsa`
- `corrected_predicted_za`
- `corrected_predicted_aboutsa`
- `corrected_log2_ratio_za`
- `corrected_log2_ratio_aboutsa`
- `cls_ci_low_share_za`
- `cls_ci_high_share_za`
- `cls_ci_low_share_aboutsa`
- `cls_ci_high_share_aboutsa`
- `cls_ci_low_ratio_za`
- `cls_ci_high_ratio_za`
- `cls_ci_low_ratio_aboutsa`
- `cls_ci_high_ratio_aboutsa`
- `point_at_floor_za`
- `point_at_floor_aboutsa`
- `boot_floor_share_za`
- `boot_floor_share_aboutsa`
- `ratio_floor`
- `argmax_matrix_log2_ratio_za`
- `argmax_matrix_log2_ratio_aboutsa`
- `validation_rows_human_topic`
- `small_topic`
- `classifier_bootstrap_b`
- `observed_share_za`
- `observed_share_aboutsa`
- `predicted_share_za`
- `predicted_share_aboutsa`
- `log2_ratio_za`
- `log2_ratio_aboutsa`
- `displayed_ci_low_za`
- `displayed_ci_high_za`
- `displayed_ci_low_aboutsa`
- `displayed_ci_high_aboutsa`
- `weighted_n`
- `raw_n`
- `world_log2_ratio_za`
- `world_log2_ratio_aboutsa`

## `data/figures/Figure_S3_fig4_series.csv`

Figure S3: Annual levels (see Figure 5 series). 36 rows.

- `year`
- `quantity_raw`
- `top_decile_raw`
- `frontier_raw`
- `p90_cited_by`
- `quantity_index`
- `top_decile_index`
- `frontier_index`
- `quantity_ma3`
- `top_decile_ma3`
- `frontier_ma3`

## `data/figures/Figure_S4_firm_event_panel.csv`

Figure S4: Firm-topic shares by country and year. 442 rows.

- `cc`
- `year`
- `n_records`
- `weight_sum`
- `firm_share`
- `household_share`
- `finance_share`
- `firm_share_unit`

## `data/figures/Figure_S4_firm_event_study_coefficients.csv`

Figure S4: Firm-topic event-study coefficients. 17 rows.

- `treated`
- `year`
- `estimate`
- `band_low`
- `band_high`
- `band_p05`
- `band_p95`
- `ri_rank`
- `ri_p`

## `data/figures/Figure_S4_firm_synthetic_series.csv`

Figure S4: Firm-topic synthetic comparison series. 17 rows.

- `year`
- `actual`
- `synthetic`
- `gap`

## `data/text_numbers.csv`

Text: Every number cited in the text (LaTeX macro name and value). 188 rows.

- `macro`
- `value`
- `source`

## Common abbreviations

- `cc`, `unit`: ISO 3166-1 alpha-2 country code.
- `Q`: journal-confirmed publication output; `F`: nineteen-journal placement;
  `T`: top-decile citation impact (fractional first-affiliation article-equivalents).
- `za`: South Africa; `aboutsa`: leading-journal research about South Africa.
- `ci_lo`, `ci_hi`, `*_low`, `*_high`: 95 per cent interval bounds.
