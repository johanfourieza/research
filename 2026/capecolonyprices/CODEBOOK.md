# Variable definitions

Every variable in `data/` is listed below. CSV files use UTF-8 and blank missing values. Quoted source labels describe historical records; they do not verify underlying currency or unit conventions. Index intervals are conditional on fitted parameters and assumptions.

## `data/inputs/accepted_individual_sales.csv`

5,715 rows.

| Variable | Type | Definition |
|---|---|---|
| `record_id` | character | Stable identifier of an accepted historical sale row; no personal identifier is inferred. |
| `auction_id` | character | Identifier grouping linked records belonging to one historical auction event. |
| `year` | integer | Calendar year of observation or estimate. |
| `decade` | integer | First calendar year of the decade; the final index decade ends in 1834. |
| `price_rd` | double | Recorded total individual sale price in nominal rixdollars. |
| `sex` | character | Sex category as recorded or classified from the historical description, including unspecified. |
| `origin_group` | character | Broad recorded origin label; not an inferred ethnicity. |
| `age_lower` | double | Lower bound on recorded age in years; blank when unrecorded. |
| `age_band` | character | Recorded age-band category, including unknown; no missing ages are imputed. |
| `skilled_mentioned` | logical | Whether the source explicitly mentions a skill; absence of mention does not establish absence of skill. |
| `health_mentioned` | logical | Whether the source explicitly mentions health; absence of mention does not establish health. |

## `data/inputs/accepted_price_ledger.csv`

1,119 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `product_sheet` | character | Commodity sheet in the inherited historical-price workbook. |
| `source_label` | character | Published or archival source attribution retained from the compilation. |
| `currency` | character | Currency label in the source workbook. |
| `unit` | character | Physical unit label in the source workbook. |
| `price` | double | Numerical quoted workbook price before the package's currency and physical-unit conversion. |
| `sheet` | character | Original workbook worksheet name. |
| `cell` | character | Original workbook cell address. |
| `formula` | character | Original workbook formula when present; otherwise blank. |
| `status` | character | Source-specific descriptive status; read together with the inclusion flag. |
| `source_column` | character | Original workbook column identifier. |
| `source_id` | character | Stable source/specification identifier before institutional regime splitting. |
| `observation_id` | character | Unique accepted-ledger observation identifier, retaining historical cell or document locator. |
| `source_family` | character | Broad provenance family, including published compilations and auction records. |
| `currency_status` | character | Audit status of the source currency attribution. |
| `formula_status` | character | Audit status of workbook arithmetic or cached formula values. |
| `usable_as_independent_validation` | logical | Whether the source can be regarded as independent validation under the original classification. |
| `source_type` | character | Original source-type classification, where available. |
| `gbp_per_rd` | double | Inherited assumed pounds sterling per rixdollar for the year; not a verified parity series. |
| `rate_status` | character | Status of the inherited dated or interpolated monetary rate. |
| `silver_per_gbp_inherited` | double | Inherited grams of silver per pound sterling used only for accounting comparisons. |
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `native_amount` | double | Quoted price in the recorded native currency and unit. |
| `native_currency` | character | Currency attached to the native amount. |
| `native_unit` | character | Physical unit attached to the native amount. |
| `source_role` | character | Transaction-role classification, such as auction, official valuation, procurement or trade unit value. |
| `dependency_group` | character | Common-source group used to flag dependence between observations. |
| `conversion_rule` | character | Declared currency conversion convention; conditional assumptions are not verified exchange rates. |
| `currency_factor` | double | Multiplier converting native currency to nominal rixdollars. |
| `physical_factor` | double | Divisor converting the native physical unit to the standard unit. |
| `physical_rule` | character | Declared physical-unit conversion convention. |
| `standard_unit` | character | Final price denominator: kg, square metre, litre or MBtu. |
| `exclusion_reason` | character | Reason an observation is excluded from the main consumption reconstruction. |
| `price_rd_standard` | double | Native amount times currency_factor divided by physical_factor, in rixdollars per standard unit. |
| `row_status` | character | Provenance and processing status of the observation. |
| `n_documents` | integer | Number of source documents represented in a ledger aggregate. |
| `n_lots` | integer | Number of accepted historical lots represented in a ledger aggregate. |
| `precision_weight` | integer | Original relative precision annotation; model aggregation applies its declared precision rule. |
| `specification` | character | Commodity or product specification used to define a source channel. |
| `source_locator` | character | Historical document/row or workbook-cell locator for the quote. |
| `conversion_assumption` | logical | Whether a currency or unit conversion involves a declared assumption. |
| `original_file` | character | Basename of the upstream source artifact; retained as attribution, not an executable path. |
| `variant_use` | character | Declared main, purchase-side, broad-grain or excluded use of the candidate row. |
| `action` | character | Accepted audit action applied before estimation. |
| `reason` | character | Recorded justification for the audit action. |
| `currency_rule_override` | character | Accepted replacement currency rule, where applicable. |
| `currency_factor_override` | double | Accepted replacement currency multiplier, where applicable. |
| `source_role_override` | character | Accepted replacement transaction-role classification, where applicable. |
| `native_unit_override` | character | Accepted replacement native-unit label, where applicable. |
| `exclude_from_annual_observed_support` | logical | Flag excluding a row from direct dated-quote support. |
| `audit_note` | character | Accepted source-audit annotation; not a new verification of the historical source. |
| `document` | character | Historical archival document identifier. |
| `grain` | character | Accepted grain subtype, where relevant. |
| `category` | character | Historical asset or product category; household categories identify the eight equipment items. |
| `product` | character | Accepted commodity specification or product class. |
| `language` | character | Historical description-language or unit-label convention. |
| `included` | logical | Whether the candidate is eligible before the model excludes purchase-side roles. |
| `regime` | character | Institutional-period classification used for channel definitions. |
| `model_source_id` | character | Source/specification/role channel, including any declared institutional split. |

## `data/inputs/additional_soap_document_observations.csv`

2 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `document` | character | Historical archival document identifier. |
| `product` | character | Accepted commodity specification or product class. |
| `language` | character | Historical description-language or unit-label convention. |
| `n_lots` | integer | Number of accepted historical lots represented in a ledger aggregate. |
| `price_rd_standard` | double | Native amount times currency_factor divided by physical_factor, in rixdollars per standard unit. |
| `source_locator` | character | Historical document/row or workbook-cell locator for the quote. |
| `physical_factor` | double | Divisor converting the native physical unit to the standard unit. |
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `source_id` | character | Stable source/specification identifier before institutional regime splitting. |
| `observation_id` | character | Unique accepted-ledger observation identifier, retaining historical cell or document locator. |
| `native_amount` | double | Quoted price in the recorded native currency and unit. |
| `native_unit` | character | Physical unit attached to the native amount. |
| `native_currency` | character | Currency attached to the native amount. |
| `currency_factor` | integer | Multiplier converting native currency to nominal rixdollars. |
| `conversion_rule` | character | Declared currency conversion convention; conditional assumptions are not verified exchange rates. |
| `physical_rule` | character | Declared physical-unit conversion convention. |
| `standard_unit` | character | Final price denominator: kg, square metre, litre or MBtu. |
| `source_role` | character | Transaction-role classification, such as auction, official valuation, procurement or trade unit value. |
| `dependency_group` | character | Common-source group used to flag dependence between observations. |
| `specification` | character | Commodity or product specification used to define a source channel. |
| `n_documents` | integer | Number of source documents represented in a ledger aggregate. |
| `precision_weight` | integer | Original relative precision annotation; model aggregation applies its declared precision rule. |
| `row_status` | character | Provenance and processing status of the observation. |
| `conversion_assumption` | logical | Whether a currency or unit conversion involves a declared assumption. |
| `exclusion_reason` | character | Reason an observation is excluded from the main consumption reconstruction. |
| `variant_use` | character | Declared main, purchase-side, broad-grain or excluded use of the candidate row. |
| `included` | logical | Whether the candidate is eligible before the model excludes purchase-side roles. |
| `original_file` | character | Basename of the upstream source artifact; retained as attribution, not an executable path. |
| `audit_note` | character | Accepted source-audit annotation; not a new verification of the historical source. |

## `data/inputs/auction_audit_decisions.csv`

103 rows.

| Variable | Type | Definition |
|---|---|---|
| `audit_id` | character | Identifier of an accepted probability-sample audit decision. |
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `source_group` | character | Retained or nonretained sampling domain in the accepted source audit. |
| `stratum` | character | Sampling stratum of the source-audit row. |
| `stratum_N` | integer | Number of historical lot rows in the sampling stratum. |
| `sampling_weight` | double | Inverse within-stratum inclusion probability for Horvitz-Thompson aggregation. |
| `definite_error` | logical | Accepted disagreement with the prior pipeline disposition or arithmetic. |

## `data/inputs/currency_audit_actions.csv`

43 rows.

| Variable | Type | Definition |
|---|---|---|
| `observation_id` | character | Unique accepted-ledger observation identifier, retaining historical cell or document locator. |
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `old_year` | integer | Year before the accepted audit correction. |
| `raw_year` | integer | Year in the underlying workbook transcription. |
| `old_native_amount` | double | Native quoted amount before the accepted audit correction. |
| `raw_mean_gbp` | double | Mean source-transcribed price in pounds sterling. |
| `arithmetic_agrees` | logical | Whether the compiled amount agrees with the underlying transcription arithmetic. |
| `raw_cells` | character | Source workbook cells supporting the audit decision. |
| `raw_year_cells` | character | Source workbook cells supporting the date correction. |
| `raw_notes` | character | Source-transcription notes retained for the accepted decision. |
| `printed_source` | character | Published source locator supporting the audit decision, where inspected. |
| `aggregation` | character | Rule used to aggregate source-transcribed quotes. |
| `year_discrepancy` | logical | Recorded difference between compiled and source-transcribed dates. |
| `currency_provenance` | character | Evidence and remaining uncertainty about the quoted currency. |
| `action` | character | Accepted audit action applied before estimation. |
| `new_year` | integer | Accepted corrected observation year. |
| `new_native_amount` | double | Accepted corrected native quoted amount, where applicable. |
| `reason` | character | Recorded justification for the audit action. |

## `data/inputs/currency_audit_additions_ledger.csv`

1 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `product_sheet` | character | Commodity sheet in the inherited historical-price workbook. |
| `source_label` | character | Published or archival source attribution retained from the compilation. |
| `currency` | character | Currency label in the source workbook. |
| `unit` | character | Physical unit label in the source workbook. |
| `price` | double | Numerical quoted workbook price before the package's currency and physical-unit conversion. |
| `sheet` | character | Original workbook worksheet name. |
| `cell` | character | Original workbook cell address. |
| `formula` | character | Original workbook formula when present; otherwise blank. |
| `status` | character | Source-specific descriptive status; read together with the inclusion flag. |
| `source_column` | character | Original workbook column identifier. |
| `source_id` | character | Stable source/specification identifier before institutional regime splitting. |
| `observation_id` | character | Unique accepted-ledger observation identifier, retaining historical cell or document locator. |
| `source_family` | character | Broad provenance family, including published compilations and auction records. |
| `currency_status` | character | Audit status of the source currency attribution. |
| `formula_status` | character | Audit status of workbook arithmetic or cached formula values. |
| `usable_as_independent_validation` | logical | Whether the source can be regarded as independent validation under the original classification. |
| `source_type` | character | Original source-type classification, where available. |
| `gbp_per_rd` | double | Inherited assumed pounds sterling per rixdollar for the year; not a verified parity series. |
| `rate_status` | character | Status of the inherited dated or interpolated monetary rate. |
| `silver_per_gbp_inherited` | double | Inherited grams of silver per pound sterling used only for accounting comparisons. |
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `native_amount` | double | Quoted price in the recorded native currency and unit. |
| `native_currency` | character | Currency attached to the native amount. |
| `native_unit` | character | Physical unit attached to the native amount. |
| `source_role` | character | Transaction-role classification, such as auction, official valuation, procurement or trade unit value. |
| `dependency_group` | character | Common-source group used to flag dependence between observations. |
| `conversion_rule` | character | Declared currency conversion convention; conditional assumptions are not verified exchange rates. |
| `currency_factor` | double | Multiplier converting native currency to nominal rixdollars. |
| `physical_factor` | double | Divisor converting the native physical unit to the standard unit. |
| `physical_rule` | character | Declared physical-unit conversion convention. |
| `standard_unit` | character | Final price denominator: kg, square metre, litre or MBtu. |
| `exclusion_reason` | character | Reason an observation is excluded from the main consumption reconstruction. |
| `price_rd_standard` | double | Native amount times currency_factor divided by physical_factor, in rixdollars per standard unit. |
| `row_status` | character | Provenance and processing status of the observation. |
| `n_documents` | character | Number of source documents represented in a ledger aggregate. |
| `n_lots` | character | Number of accepted historical lots represented in a ledger aggregate. |
| `precision_weight` | integer | Original relative precision annotation; model aggregation applies its declared precision rule. |
| `specification` | character | Commodity or product specification used to define a source channel. |
| `source_locator` | character | Historical document/row or workbook-cell locator for the quote. |
| `conversion_assumption` | logical | Whether a currency or unit conversion involves a declared assumption. |
| `original_file` | character | Basename of the upstream source artifact; retained as attribution, not an executable path. |
| `variant_use` | character | Declared main, purchase-side, broad-grain or excluded use of the candidate row. |
| `action` | character | Accepted audit action applied before estimation. |
| `reason` | character | Recorded justification for the audit action. |
| `currency_rule_override` | character | Accepted replacement currency rule, where applicable. |
| `currency_factor_override` | character | Accepted replacement currency multiplier, where applicable. |
| `source_role_override` | character | Accepted replacement transaction-role classification, where applicable. |
| `native_unit_override` | character | Accepted replacement native-unit label, where applicable. |
| `exclude_from_annual_observed_support` | character | Flag excluding a row from direct dated-quote support. |
| `audit_note` | character | Accepted source-audit annotation; not a new verification of the historical source. |
| `document` | character | Historical archival document identifier. |
| `grain` | character | Accepted grain subtype, where relevant. |
| `category` | character | Historical asset or product category; household categories identify the eight equipment items. |
| `product` | character | Accepted commodity specification or product class. |
| `language` | character | Historical description-language or unit-label convention. |
| `included` | logical | Whether the candidate is eligible before the model excludes purchase-side roles. |
| `regime` | character | Institutional-period classification used for channel definitions. |
| `model_source_id` | character | Source/specification/role channel, including any declared institutional split. |

## `data/inputs/estimation_initial_values.csv`

7 rows.

| Variable | Type | Definition |
|---|---|---|
| `parameter` | character | Model parameter identifier; initial values are logs of innovation or observation standard deviations. |
| `log_initial_value` | double | Declared log standard deviation used to initialise the primary optimiser. |

## `data/inputs/excluded_asset_annual_counts.csv`

694 rows.

| Variable | Type | Definition |
|---|---|---|
| `category` | character | Historical asset or product category; household categories identify the eight equipment items. |
| `source_series` | character | Original series identifier used to count excluded asset lots. |
| `year` | integer | Calendar year of observation or estimate. |
| `lots` | integer | Number of accepted historical sale lots. |

## `data/inputs/historical_events.csv`

10 rows.

| Variable | Type | Definition |
|---|---|---|
| `event_id` | character | Identifier of a dated historical event used for descriptive plot alignment. |
| `start_year` | integer | First year of an event or observation span. |
| `end_year` | integer | Last year of an event or observation span. |
| `label` | character | Display label for the historical event. |
| `group` | character | Scenario group or historical-event lane, according to file. |
| `citation_key` | character | Reference key identifying the historical event source. |
| `source_locator` | character | Historical document/row or workbook-cell locator for the quote. |
| `interpretation` | character | Scope or interpretation note; event alignment is not a causal estimate. |

## `data/inputs/household_observed_annual.csv`

1,080 rows.

| Variable | Type | Definition |
|---|---|---|
| `category` | character | Historical asset or product category; household categories identify the eight equipment items. |
| `year` | integer | Calendar year of observation or estimate. |
| `price_observed_rd` | double | Observed annual household-item price in rixdollars; blank years are subsequently interpolated or smoothed. |
| `auctions` | integer | Number of contributing auction events in the year. |

## `data/inputs/inherited_basket_monetary_scenarios_NOT_HEADLINE.csv`

270 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `basket` | character | Inherited basket definition; cost_formula denotes the nine-good inherited comparison. |
| `monetary_scenario` | character | Inherited silver-conversion convention, distinguished by pre-1795 guilder parity. |
| `index` | double | Basket index, normalised to the 1770-1779 mean of 100; in joint-draw files one value belongs to one complete draw path. |
| `inflation_pct` | double | 100 times the ratio of current to preceding annual index minus 100; blank in the first year. |
| `index_nominal_conditional` | double | Inherited nominal rixdollar comparison index conditional on the declared monetary conversion. |
| `inflation_nominal_conditional` | double | Annual percentage change in the inherited nominal comparison index. |

## `data/inputs/inherited_monetary_assumptions_NOT_VERIFIED_PARITIES.csv`

135 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `gbp_per_rd` | double | Inherited assumed pounds sterling per rixdollar for the year; not a verified parity series. |
| `source` | character | Attribution for the inherited monetary schedule. |
| `rate_observed` | logical | Whether the inherited exchange schedule records a dated rate rather than interpolation. |
| `rate_status` | character | Status of the inherited dated or interpolated monetary rate. |
| `silver_per_guilder_inherited` | double | Inherited grams of silver per guilder used for accounting comparisons. |
| `silver_per_gbp_inherited` | double | Inherited grams of silver per pound sterling used only for accounting comparisons. |
| `silver_per_rd_2.4` | double | Inherited grams of silver per rixdollar under the 2.4-guilder pre-1795 convention. |
| `silver_per_rd_3` | double | Inherited grams of silver per rixdollar under the 3-guilder pre-1795 convention. |

## `data/inputs/validation/actual_late_missingness_template.csv`

270 rows.

| Variable | Type | Definition |
|---|---|---|
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `year` | integer | Calendar year of observation or estimate. |
| `observed_channels` | integer | Number of retained observed channels for the commodity-year in the missingness template. |
| `missing` | logical | Whether the commodity-year has no observed channel in the missingness template. |

## `data/inputs/validation/fold_ledger.csv`

50 rows.

| Variable | Type | Definition |
|---|---|---|
| `fold_id` | character | Prespecified blocked-holdout identifier. |
| `fold_type` | character | Type of holdout mask, including portfolio, component and partial-channel designs. |
| `description` | character | Description of the prespecified holdout mask. |
| `n_test` | integer | Number of held-out channel-year cells in the fold. |
| `n_train` | integer | Number of retained training cells in the fold. |
| `status` | character | Source-specific descriptive status; read together with the inclusion flag. |
| `paired_constant_mask` | logical | Whether the mask is paired with the earlier constant-offset design. |
| `constant_component_id` | character | Overlap component in the stable-channel design used to define a transported mask. |
| `component_id` | character | Same-good same-year source-overlap component identifier. |
| `n_preferred_components` | integer | Number of preferred-design components represented in the holdout mask. |
| `size_rank` | integer | Within-commodity rank of a source-overlap component by observed-cell count. |

## `data/inputs/validation/overlap_splice_selected_design.csv`

7 rows.

| Variable | Type | Definition |
|---|---|---|
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `target` | character | Target source channel whose selected quotes are withheld. |
| `donor` | character | Source channel supplying the overlap-splice baseline. |
| `year_from` | integer | First held-out year in the partial-channel mask. |
| `year_to` | integer | Last held-out year in the partial-channel mask. |
| `held_years` | character | Prespecified held-out years for the target source channel. |
| `test_row_ids` | character | Semicolon-separated held-out panel-row identifiers. |
| `n_training_overlap` | integer | Number of overlapping training years between target and donor channels. |
| `donor_role` | character | Transaction role of the selected donor channel. |
| `donor_role_rank` | integer | Prespecified priority rank of the donor role. |
| `donor_training_n` | integer | Number of training quotes available for the donor channel. |
| `target_training_n` | integer | Number of training quotes available for the target channel. |
| `within_good_rank` | integer | Prespecified candidate rank within the commodity. |
| `fold_id` | character | Prespecified blocked-holdout identifier. |

## `data/inputs/validation/paired_observation_mapping.csv`

632 rows.

| Variable | Type | Definition |
|---|---|---|
| `row_id` | integer | Stable channel-year panel-row identifier used by the accepted holdout masks. |
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `year` | integer | Calendar year of observation or estimate. |
| `stable_model_source_id` | character | Source/specification/role identifier without institutional regime splitting. |
| `model_source_id` | character | Source/specification/role channel, including any declared institutional split. |

## `data/inputs/validation/source_overlap_component_sizes.csv`

35 rows.

| Variable | Type | Definition |
|---|---|---|
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `component_id` | character | Same-good same-year source-overlap component identifier. |
| `n_rows` | integer | Number of observed panel cells in the source-overlap component. |
| `size_rank` | integer | Within-commodity rank of a source-overlap component by observed-cell count. |

## `data/inputs/validation/test_membership.csv`

1,745 rows.

| Variable | Type | Definition |
|---|---|---|
| `fold_id` | character | Prespecified blocked-holdout identifier. |
| `row_id` | integer | Stable channel-year panel-row identifier used by the accepted holdout masks. |

## `data/series/alternative_annual_series.csv`

5,805 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `index_point` | double | Fixed-quantity basket cost from exponentiated smoothed log prices, normalised to arithmetic mean 1770–1779 = 100; not a posterior mean |
| `index_q05` | double | 5th, 50th and 95th percentiles of 1,000 joint conditional basket draws, normalised within each draw |
| `index_draw_median` | double | 5th, 50th and 95th percentiles of 1,000 joint conditional basket draws, normalised within each draw |
| `index_q95` | double | 5th, 50th and 95th percentiles of 1,000 joint conditional basket draws, normalised within each draw |
| `inflation_pct` | double | 100 times the ratio of current to preceding annual index minus 100; blank in the first year. |
| `inflation_q05` | double | Percentiles of annual percentage changes computed WITHIN each joint draw; NA in 1700 |
| `inflation_draw_median` | double | Percentiles of annual percentage changes computed WITHIN each joint draw; NA in 1700 |
| `inflation_q95` | double | Percentiles of annual percentage changes computed WITHIN each joint draw; NA in 1700 |
| `annual_cost_rd` | double | Sum of fixed basket quantities times exponentiated fitted log prices, in assumed nominal rixdollars; not a comprehensive household budget |
| `variant_id` | character | Peak timing for full and post-1795 windows; first chronological maximiser for ties. Persistent-price-only target removes realised wheat transitory state from same fit, not a refit without that state |

## `data/series/alternative_assumption_interpretation.csv`

43 rows.

| Variable | Type | Definition |
|---|---|---|
| `variant_id` | character | Peak timing for full and post-1795 windows; first chronological maximiser for ties. Persistent-price-only target removes realised wheat transitory state from same fit, not a refit without that state |
| `kind` | character | Executable alternative-model change type. |
| `value` | double | Numerical value of the alternative setting; mean shifts are natural logs, scales are standard deviations. |
| `group` | character | Scenario group or historical-event lane, according to file. |
| `shared_loading` | character | Channel-loading choice for a shared source-offset scenario. |
| `source_prior_mean_log` | double | Prior mean shift in source quote log levels. |
| `source_prior_level_factor` | double | Exponentiated source prior mean, a multiplicative quote-level factor. |
| `source_prior_percent_deviation` | double | 100 times the source prior level factor minus 100. |
| `interpretation` | character | Scope or interpretation note; event alignment is not a causal estimate. |

## `data/series/alternative_model_settings.csv`

43 rows.

| Variable | Type | Definition |
|---|---|---|
| `variant_id` | character | Peak timing for full and post-1795 windows; first chronological maximiser for ties. Persistent-price-only target removes realised wheat transitory state from same fit, not a refit without that state |
| `kind` | character | Executable alternative-model change type. |
| `value` | double | Numerical value of the alternative setting; mean shifts are natural logs, scales are standard deviations. |
| `group` | character | Scenario group or historical-event lane, according to file. |

## `data/series/annual_index_joint_draws.csv.gz`

135,000 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `draw_id` | integer | Identifier for one joint trajectory; preserved across years within a model only. |
| `index` | double | Basket index, normalised to the 1770-1779 mean of 100; in joint-draw files one value belongs to one complete draw path. |

## `data/series/annual_inflation_1700_1834.csv`

135 rows.

| Variable | Type | Definition |
|---|---|---|
| `variant_id` | character | Peak timing for full and post-1795 windows; first chronological maximiser for ties. Persistent-price-only target removes realised wheat transitory state from same fit, not a refit without that state |
| `year` | integer | Calendar year of observation or estimate. |
| `index_point` | double | Fixed-quantity basket cost from exponentiated smoothed log prices, normalised to arithmetic mean 1770–1779 = 100; not a posterior mean |
| `index_q05` | double | 5th, 50th and 95th percentiles of 1,000 joint conditional basket draws, normalised within each draw |
| `index_draw_median` | double | 5th, 50th and 95th percentiles of 1,000 joint conditional basket draws, normalised within each draw |
| `index_q95` | double | 5th, 50th and 95th percentiles of 1,000 joint conditional basket draws, normalised within each draw |
| `inflation_pct` | double | 100 times the ratio of current to preceding annual index minus 100; blank in the first year. |
| `inflation_q05` | double | Percentiles of annual percentage changes computed WITHIN each joint draw; NA in 1700 |
| `inflation_draw_median` | double | Percentiles of annual percentage changes computed WITHIN each joint draw; NA in 1700 |
| `inflation_q95` | double | Percentiles of annual percentage changes computed WITHIN each joint draw; NA in 1700 |
| `annual_cost_rd` | double | Sum of fixed basket quantities times exponentiated fitted log prices, in assumed nominal rixdollars; not a comprehensive household budget |
| `interval_scope` | character | Intervals condition on fitted variance parameters, specified source priors, currency/unit conversions and model |
| `observed_level_weight_share` | double | Sum of fixed 1770s basket weights for goods with any retained dated quote in the year |
| `observed_change_weight_share` | double | Identical aliases: fixed base-weight share with a retained quote in the same channel in consecutive years; NA in 1700 |
| `market_level_weight_share` | double | Fixed base-weight share with auction, transcribed auction or newspaper quotes in the year |
| `market_change_weight_share` | double | Fixed base-weight share with consecutive-year quotes in the same market channel; NA in 1700 |
| `flat_official_change_weight_share` | double | Fixed base-weight share with a consecutive-year official quote unchanged within numerical tolerance; overlap with other support measures is possible |
| `weighted_anchor_distance` | double | Base-weighted mean distance in years to nearest retained commodity quote |
| `beyond_anchor_weight` | double | Fixed base-weight share of goods outside their first-to-last observed-year range |
| `dated_quote_change_weight_share` | double | Identical aliases: fixed base-weight share with a retained quote in the same channel in consecutive years; NA in 1700 |
| `support_class` | character | Level_only in 1700; otherwise class based on market_change_weight_share thresholds 0, .4 and .75; support is not estimated accuracy |
| `release_status` | character | v2_conditional_reconstruction; this field is not a claim that every analysis/review is complete |

## `data/series/basket_quantities_weights.csv`

9 rows.

| Variable | Type | Definition |
|---|---|---|
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `quantity` | double | Fixed annual physical quantity in the stated standard unit. |
| `standard_unit` | character | Final price denominator: kg, square metre, litre or MBtu. |
| `base_weight` | double | Commodity cost share in the fitted arithmetic mean 1770-1779 basket; not an observed expenditure share. |

## `data/series/commodity_price_summaries.csv`

1,215 rows.

| Variable | Type | Definition |
|---|---|---|
| `commodity` | character | One of the nine provisioning commodities, or the source commodity label before exclusion. |
| `year` | integer | Calendar year of observation or estimate. |
| `log_price` | double | Smoothed commodity log price in rixdollars per standard unit. |
| `log_price_se` | double | Conditional standard error of the smoothed commodity log price. |
| `price_median` | double | Exponentiated smoothed commodity log price, in rixdollars per standard unit. |
| `price_q05` | double | Conditional fifth percentile of the commodity price, in rixdollars per standard unit. |
| `price_q95` | double | Conditional ninety-fifth percentile of the commodity price, in rixdollars per standard unit. |

## `data/series/decadal_index_joint_intervals.csv`

14 rows.

| Variable | Type | Definition |
|---|---|---|
| `decade` | integer | First calendar year of the decade; the final index decade ends in 1834. |
| `q05` | double | Fifth percentile of the relevant within-draw statistic (historical ratio or decadal mean index). |
| `median` | double | Median of the relevant within-draw statistic (historical ratio or decadal mean index). |
| `q95` | double | Ninety-fifth percentile of the relevant within-draw statistic (historical ratio or decadal mean index). |

## `data/series/historical_relatives.csv`

6 rows.

| Variable | Type | Definition |
|---|---|---|
| `variant_id` | character | Peak timing for full and post-1795 windows; first chronological maximiser for ties. Persistent-price-only target removes realised wheat transitory state from same fit, not a refit without that state |
| `functional` | character | Named historical ratio; date windows are arithmetic means before taking ratios within draws. |
| `point` | double | Historical ratio evaluated on the point index path. |
| `q05` | double | Fifth percentile of the relevant within-draw statistic (historical ratio or decadal mean index). |
| `median` | double | Median of the relevant within-draw statistic (historical ratio or decadal mean index). |
| `q95` | double | Ninety-fifth percentile of the relevant within-draw statistic (historical ratio or decadal mean index). |
| `prob_gt1` | double | Fraction of conditional joint-draw ratios exceeding one. |
| `prob_gt3` | double | Fraction of conditional joint-draw ratios exceeding three. |
| `draws` | integer | Number of conditional joint trajectories used. |

## `data/series/joint_draw_metadata.csv`

1 rows.

| Variable | Type | Definition |
|---|---|---|
| `variant_id` | character | Peak timing for full and post-1795 windows; first chronological maximiser for ties. Persistent-price-only target removes realised wheat transitory state from same fit, not a refit without that state |
| `base_period` | character | Normalisation convention for the annual index. |
| `years` | integer | Number of calendar years represented. |
| `draws` | integer | Number of conditional joint trajectories used. |
| `seed` | integer | Random-number seed for the conditional state draws. |
| `draw_order` | character | Order of year and draw identifiers in the portable joint export. |
| `scope` | character | Interpretive scope and conditioning assumptions of the released draws. |
| `point_estimator` | character | Definition of the published point estimate. |
| `quantile_estimator` | character | Definition of conditional quantiles after within-draw normalisation. |
| `peak_tie_rule` | character | Tie-breaking rule for a peak year: first chronological maximiser. |
| `primary_offset_sd` | double | Fixed channel-offset prior standard deviation (0.5 log units). |
| `official_split_year` | integer | Year at which the preferred official channel offsets reset (1795). |

## `data/series/offsets_constant_joint_draws.csv.gz`

135,000 rows.

| Variable | Type | Definition |
|---|---|---|
| `year` | integer | Calendar year of observation or estimate. |
| `draw_id` | integer | Identifier for one joint trajectory; preserved across years within a model only. |
| `index` | double | Basket index, normalised to the 1770-1779 mean of 100; in joint-draw files one value belongs to one complete draw path. |

## `data/series/peak_timing_diagnostics.csv`

8 rows.

| Variable | Type | Definition |
|---|---|---|
| `variant_id` | character | Peak timing for full and post-1795 windows; first chronological maximiser for ties. Persistent-price-only target removes realised wheat transitory state from same fit, not a refit without that state |
| `target` | character | Basket with realised transitory wheat or persistent-price-only target within the same fitted draws. |
| `window` | integer | Calendar-year window in which the maximum is located. |
| `point_peak_year` | integer | First year attaining the maximum of the point index in the window. |
| `peak_q05` | integer | Fifth percentile of the discrete conditional peak-year distribution. |
| `peak_median` | integer | Median of the conditional peak-year distribution. |
| `peak_q95` | integer | Ninety-fifth percentile of the discrete conditional peak-year distribution. |
| `prob_peak_before1825` | double | Fraction of conditional trajectories with a peak before 1825. |
| `prob_peak_before1826` | double | Fraction of conditional trajectories with a peak before 1826. |
| `prob_peak1818_1824` | double | Fraction of conditional trajectories peaking between 1818 and 1824 inclusive. |
| `draws` | integer | Number of conditional joint trajectories used. |
