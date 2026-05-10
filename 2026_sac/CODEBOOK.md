# Codebook: `data/sac_clean.csv`

Cleaned, analysis-ready dataset of South African Constabulary recruits enlisted between 1900 and 1908 (with a small tail running into 1909). 10,399 rows, 89 variables.

Each row is one recruit-spell. Recruits who re-enlisted appear in more than one row and share the same `id`; the row-level identifier is `row`.

Missing values are stored as empty strings in the CSV. In R, the companion loader (`scripts/load_data.R`) parses these back to `NA`.

---

## Identifiers

**`id`** — *character*  
Recruit identifier from the SAC enlistment register. Not unique across rows: recruits who re-enlisted appear in multiple spells and share the same id.

**`row`** — *integer*  
Row index in the cleaned dataset (1..10399), used as a row-level identifier for merging the manual-review file.

**`surname`** — *character*  
Recruit surname as transcribed from the register, lower-cased and trimmed.

**`names_orig`** — *character*  
Recruit given name(s) as originally transcribed.

**`img`** — *character*  
Reference identifier pointing to the underlying archival image of the register page from which the row was transcribed.

## Afrikaner classification

**`afrikaans`** — *integer (0/1)*  
Original hand-coded Afrikaner indicator, based on surname and birthplace. Preserved as the legacy baseline.

**`afrikaans_reviewed`** — *integer (0/1)*  
Reviewed-baseline Afrikaner indicator: original hand-coded indicator updated by row-level manual review of disputed cases. This is the PRIMARY treatment variable used throughout the paper.

**`afrikaner_manual_override`** — *integer (0/1)*  
Flag for rows where the manual review changed the original hand-coded indicator.

**`afrikaans_strict`** — *integer (0/1)*  
Algorithmic alternative used in the §7.2 robustness check: strict surname match against the BoerSurnames list.

**`afrikaans_region`** — *integer (0/1)*  
Algorithmic alternative: surname AND region of origin both match an Afrikaner-associated profile (the conservative recruiter-observable rule).

**`afrikaans_broad`** — *integer (0/1)*  
Algorithmic alternative: surname OR region of origin match (over-inclusive).

## Enlistment timing

**`date_enlist`** — *date*  
Date of enlistment (1900-03-01 to 1909-10-28).

**`enlist_year`** — *numeric*  
Year of enlistment (1900..1909).

**`enlist_ym`** — *character*  
Year-month string used for clustering of standard errors in the linear models (enlistment year-month clusters).

**`e_day`** — *numeric*  
Day of enlistment month (1..31).

**`e_month`** — *numeric*  
Month of enlistment (1..12).

**`e_year`** — *numeric*  
Year of enlistment (numeric duplicate of enlist_year).

**`post_vereeniging`** — *integer (0/1)*  
Indicator for enlistments after the Treaty of Vereeniging (31 May 1902).

**`days_from_treaty`** — *numeric*  
Days from the Treaty of Vereeniging (negative = pre-Treaty).

**`months_from_treaty`** — *numeric*  
Months from the Treaty of Vereeniging.

**`quarters_from_treaty`** — *numeric*  
Calendar quarters from the Treaty of Vereeniging.

## Discharge timing

**`date_discharge`** — *date*  
Date of discharge from the SAC.

**`d_day`** — *numeric*  
Day of discharge month.

**`d_month`** — *numeric*  
Month of discharge.

**`d_year`** — *numeric*  
Year of discharge.

## Wages

**`wage`** — *numeric*  
Daily wage in shillings per day.

**`lwage`** — *numeric*  
Natural logarithm of the daily wage.

**`wage_schedule`** — *numeric*  
Wage schedule rate associated with the recruit's rank/class assignment (administrative pay rate).

**`contract_wage_schedule`** — *numeric*  
Wage rate implied by the contract terms (length and assignment).

## Contract

**`contract_years`** — *numeric*  
Contract length in years (0.5, 1, 2, or 3 years; or fractional values parsed from special enlistment strings).

**`enlisted_years_orig`** — *character*  
Original 'Enlisted years' string from the register before parsing ("3", "2", "1", "6 months", "12 months", "Special", or "1 yr 233 days" type strings).

## Rank and title

**`title_orig`** — *character*  
Original recruit title from the register (87 unique strings).

**`title_clean`** — *character*  
Standardised rank/title (27 categories).

**`rank_cat`** — *character*  
Rank category: Trooper, Constable, NCO, or Specialist.

**`is_trooper_3c`** — *integer (0/1)*  
Indicator for assignment to 3rd-class Trooper, the entry rank to which most Afrikaner recruits were assigned.

## Demographics

**`age`** — *numeric*  
Age at enlistment, in years.

**`age_sq`** — *numeric*  
Age squared.

**`height_cm`** — *numeric*  
Height in centimetres.

**`height_sq`** — *numeric*  
Height squared (cm²).

**`weight_kg`** — *numeric*  
Weight in kilograms.

**`bmi`** — *numeric*  
Body Mass Index (kg / m²).

**`chest_diff`** — *numeric*  
Chest expansion: difference between maximum and minimum chest measurement.

**`married`** — *integer (0/1)*  
Indicator for marital status married at enlistment.

**`marital_status_clean`** — *character*  
Cleaned marital status: Single, Married, or Widower.

## Skills

**`can_ride`** — *integer (0/1)*  
Recruit reported as able to ride a horse.

**`can_shoot`** — *integer (0/1)*  
Recruit reported as able to shoot.

**`can_swim`** — *integer (0/1)*  
Recruit reported as able to swim.

**`prior_army`** — *integer (0/1)*  
Recruit reported prior military service before enlisting in the SAC.

**`served_army`** — *integer (0/1)*  
Companion indicator for prior military service (parallel coding from a different register field).

**`speaks_african_lang`** — *integer (0/1)*  
Recruit reported as able to speak an African language.

**`speaks_french`** — *integer (0/1)*  
Recruit reported as able to speak French.

**`speaks_dutch`** — *integer (0/1)*  
Recruit reported as able to speak Dutch.

## Occupational class

**`class`** — *integer (1..3)*  
SAC occupational class (1 = lowest, 3 = highest), defined for the Trooper grading ladder. Missing for many NCOs, Constables, and Specialists.

**`hisco`** — *character*  
HISCO occupational code (1-digit major group).

**`hisco2`** — *character*  
HISCO occupational code (2-digit minor group).

**`calling`** — *character*  
Original 'calling' (pre-enlistment occupation) string from the register.

**`farmer`** — *integer (0/1)*  
Indicator for pre-enlistment occupation farmer.

## Geography

**`region`** — *character*  
Region of origin (10 categories).

**`country`** — *character*  
Country of origin (42 categories).

**`where_recruited`** — *character*  
Place where the recruit was recruited.

**`signed_at`** — *character*  
Place where the recruit signed the contract.

**`place_origin`** — *character*  
Place of origin (cleaned).

**`place`** — *character*  
Place (general).

**`region_dum`** — *numeric*  
Region of origin as numeric code (1..10).

## Tenure and separation

**`tenure_days`** — *numeric*  
Tenure in days from enlistment to discharge.

**`tenure_days_calc`** — *numeric*  
Alternative tenure calculation (used as a consistency check).

**`discharge_cause_orig`** — *character*  
Original discharge-cause string from the register (18 unique strings).

**`discharge_cause_clean`** — *character*  
Cleaned discharge-cause category (14 categories).

**`dismissed_deserted`** — *integer (0/1)*  
Indicator for dismissal or desertion (employer-initiated separation).

**`voluntary_exit`** — *integer (0/1)*  
Indicator for voluntary exit (employee-initiated separation).

**`time_expired`** — *integer (0/1)*  
Indicator for separation at contract expiry (time expired).

**`medically_unfit`** — *integer (0/1)*  
Indicator for separation on medical grounds.

**`retrenched`** — *integer (0/1)*  
Indicator for separation by retrenchment.

**`deceased`** — *integer (0/1)*  
Indicator for death in service.

**`transfer`** — *integer (0/1)*  
Indicator for transfer out of the SAC.

**`event_type`** — *integer*  
Competing-risks event-type code: 0 = censored, 1 = dismissal/desertion, 2 = voluntary exit, 3 = time expired.

**`discharged`** — *integer (0/1)*  
Indicator for any discharge event (1 = discharged for any reason).

## Character ratings

**`character_orig`** — *character*  
Original character rating string at discharge (149 unique strings).

**`character_clean`** — *character*  
Cleaned character rating: Bad, Very Bad, Indifferent, Fair, Good, Very Good, Exemplary.

**`character_score`** — *integer (1..7)*  
Numeric character score: 1 = Bad, 2 = Very Bad, 3 = Indifferent, 4 = Fair, 5 = Good, 6 = Very Good, 7 = Exemplary.

**`good_character`** — *integer (0/1)*  
Indicator for a character rating of Good or above.

## Other indicators

**`afr_post`** — *integer (0/1)*  
Interaction indicator: afrikaans_reviewed × post_vereeniging.

**`repeat_enlister`** — *integer (0/1)*  
Indicator for recruits whose id appears more than once in the data (re-enlisters).

**`first_enlistment`** — *integer (0/1)*  
Indicator for the recruit's first enlistment spell.

**`spell_number`** — *integer*  
Spell number within recruit (1 for first enlistment, 2 for second, etc.).

**`religion`** — *character*  
Religion as recorded in the register (14 categories).

**`jewish`** — *numeric (0/1)*  
Jewish indicator derived from religion.

**`catholic`** — *numeric (0/1)*  
Catholic indicator derived from religion.

**`died`** — *numeric (0/1)*  
Death indicator (consistent with deceased; included as a duplicate encoding from the cleaning script).
