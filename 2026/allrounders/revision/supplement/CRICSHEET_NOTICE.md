# Cricsheet attribution and archive version

Independent match records originate from [Cricsheet](https://cricsheet.org/). Its JSON data and the derived match manifest are supplied under the [Open Data Commons Attribution License 1.0](https://opendatacommons.org/licenses/by/1.0/); see the [provider licensing notice](https://cricsheet.org/licensing/). Credit Cricsheet in any reuse. The provider does not endorse this paper or its corrections.

The included manifest is an index derived from the previously held Cricsheet JSON archive. It retains date, teams, match type, gender, match identifier and year. Domestic competition types are encoded as CCH (County Championship), BWT (Bob Willis Trophy) and SSH (Sheffield Shield) for matching. The study uses men's matches through 31 December 2025. No ball-by-ball archive is included in this code bundle.

Exact archive and manifest sizes and SHA-256 hashes are recorded in `../audit/independent_archive_manifest.csv`. Place the matching archive at `raw/cricsheet/all_json.zip` under `ALLROUNDERS_AUDIT_ARCHIVE`, and the supplied manifest at `raw/cricsheet/MANIFEST_matches.csv`. Current provider downloads change over time and may withhold some matches, so they are not guaranteed to reproduce the archived pilot or identity census exactly. The original local archive remains available to the author; the manifest and hashes document the version used.

DAFT scorecards are a separate licensed source and are not covered by Cricsheet's licence. They are not supplied here.
