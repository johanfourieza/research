# Linkage protocol and departures from it

The protocol below was written and locked before any estimate with the final links was computed. Like the analysis code and every link decision, it was hashed (SHA-256 and MD5). It is a protocol for this linkage, not a preregistration of the study's hypotheses. The pipeline checks two checksums before it estimates anything: the census workbook (`data/inputs/source_md5.txt`) and the link decisions (`data/linkage/link_decisions_md5.txt`).

## Protocol

1. **Parsing.** Head, wife and female-headed entries are parsed in all eleven district returns (`code/parse_names.R`).
2. **Candidate pairs.** Candidates are blocked on exact or approximate surnames within each Voortrekker's district search set. Men without an in-district proposal are then searched outside it on exact surnames.
3. **Training labels.** Two language models (Claude and Codex) review the hand-labeled pairs and a supplementary sample blind. They see identity evidence but no household characteristics. The authors decide pairs on which the models disagree with each other or with the hand label.
4. **Classifier.** A random forest uses spouse-evidence states: agree, missing or inconclusive, and contradict.
   - **Validation:** cross-validation is five-fold, with folds grouped by connected person–household components. Fifteen percent of these components are held out from training and tuning as an audit sample (97 of the 910 labeled candidate pairs).
   - **Thresholds:** chosen by F0.5.
   - **Automatic acceptance:** pairs without agreeing wives are accepted automatically only if a stricter threshold reaches the target precision. Pairs whose wives contradict are never accepted automatically.
5. **Review of links.**
   - The two models review the proposals, the review-band pairs and supplementary candidate pairs blind.
   - A proposal accepted by both models is linked.
   - Pairs on which the models disagree, and pairs involving competing identities, go to the authors.
   - Each census head is linked to at most one person.
6. **Wife-blind linkage.** As a sensitivity check, a classifier without spouse features is trained on the same labels, and its proposals are taken without review.

## Departures from the protocol

1. **Fuzzy blocking.** The protocol specified Double Metaphone. Because that R package was not installed, blocking uses Soundex, or Jaro-Winkler similarity of at least 0.92, on surnames without particles or spaces.
2. **Parser rules.** The following rules were added after an independent audit of the parser against the source images:
   - widow markers in the annotation and women's columns count as widow entries, and "widower" is excluded from the widow rule;
   - numbered rows are not attached as continuation wives;
   - transcription placeholders are not names;
   - "Surname. First" is read as "Surname, First".
   Names without a comma (19 heads and 26 wives) keep the whole string as the surname.
3. **Code corrections after an independent code review, before any label or proposal was used:**
   - final links take the classifier's scored rows, and decided pairs outside the candidate set are featured and scored separately;
   - threshold increments are calibrated only on pairs their rule can accept, without a floor; if no threshold reaches the target precision, the rule accepts nothing automatically;
   - a conflict on either the wife's first name or her surname sends a pair to review, even if the other name agrees;
   - a wife recorded under her married surname is compared at a similarity threshold of 0.85;
   - the similarity between the genealogy man's surname and the census wife's surname is set to zero unless both records name a wife;
   - every scored pair in the review band is reviewed, not only each person's top candidate;
   - the cross-district pass covers men with no in-district candidate;
   - heads are standardised with the same function as wives, and fuzzy blocking compares surnames without particles;
   - the robustness analysis reports the three spouse-evidence states separately;
   - the review queue covers supplementary candidate pairs and changed partners, and finalisation enforces one person per census head;
   - during the linkage, estimation checked the locked inputs and required regenerated proposals to match the locked ones (the public code checks the checksums of the census workbook and the link decisions).
4. **Scope of author adjudication.** Of the 458 pairs queued for the authors, they decided only the 230 that at least one model accepted. The 228 that no model accepted are not linked.
5. **Men recorded twice.** Five linked men appear in two census records: four moved between the 1823 and 1825 enumerations, and one is a double entry. Each is linked to one record, the 1825 record for the four movers. The other record is excluded from the comparison group (`data/linkage/duplicate_households.csv`).
6. **Analyses added after estimation.** The married-household analyses (`code/couples_analysis.R`) were added after the estimation runs. They do not use outcomes to change any link.
7. **Changes to the analysis code and tables after estimation.** None of these changes any link:
   - The leader and destination analyses use every link: an inherited score filter (at least 0.70) that dropped 66 links was removed. With nine qualifying leaders, a chart's colour palette was extended.
   - Presentation only, with no estimate changed:
     - the variable-importance figure is drawn from the linkage classifier;
     - the score histogram marks only the classifier threshold;
     - the six-method table prints a dash where a p-value is undefined;
     - the timing figure's axis is labelled "Census District";
     - where the pipeline draws a figure twice, the paper uses the later output.
   - In the paper's tables:
     - the male-headed-controls table reports the full sample, male-headed controls, and classifier-proposed links against male-headed controls;
     - the clustered-inference table reports the full sample, classifier-proposed links and married couples.
     Further samples computed by the pipeline are not shown.

## Records

- `data/linkage/training_labels.csv`: the resolved training labels.
- `data/linkage/link_decisions.csv`: every reviewed pair, with its decision and basis.
- `reviews/`: the evidence packets the models reviewed, their verdicts, and the reconciliation of the two reviews (see `reviews/README.md`).
