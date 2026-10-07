# Model reviews of the linkage

Two language models, Anthropic's Claude and OpenAI's Codex, reviewed candidate pairs independently and blind to household characteristics. Their instructions are in `review_instructions.txt`. Each evidence packet gives, for a genealogy person and a census household:
- names, spouses, and birth, marriage and death years;
- the declared and census districts;
- census annotations;
- other census candidates.

It contains no household counts, wealth or classifier scores.

| Folder | Contents |
|---|---|
| `training_labels/` | Review of the training pairs: 902 hand-labeled pairs and 150 pairs sampled from the candidate set. |
| `links/` | Review of the classifier proposals, review-band pairs and supplementary candidate pairs (923 pairs). |
| `owner_links/` | Review of the compensation-owner candidates (886 genealogy records, 7,404 candidate owners); see below. |

Each folder holds three kinds of file:
- `packets/`: the evidence packets, in batches, with `*_all.json` combining all batches.
- `claude/` and `codex/`: the verdicts for each pair (`ACCEPT`, `REJECT` or `UNCERTAIN`), with a short reason and the spouse evidence.
- A reconciliation file combining both verdicts for every pair: `training_reconciliation_all.csv` or `link_reconciliation_all.csv`.

In the training review, Claude reviewed batch 16 as two smaller batches, numbered 28 and 29; they contain the same pairs. Codex reviewed batch 16 whole.

Columns of the reconciliation files that are not self-explanatory:

| Column | Description |
|---|---|
| `sample` | `hand-labeled pair` (labeled by the four labelers) or `supplementary sample` (sampled from the candidate set). |
| `hand_label` | The hand label (1 = match, 0 = non-match), for hand-labeled pairs. |
| `queue` (training review) | Why a pair went to the authors: the models disagree, or they agree with each other but not with the hand label. All queued training pairs were decided by the authors. |
| `queue` (link review) | Why a pair was queued for the authors: the models disagree; the pair is a supplementary candidate that is not a classifier proposal accepted by both models; the classifier did not propose it although both models accept it; competing identities (two persons for one census head, or one person for two heads); or an alternative candidate for the same person. Of the 458 queued pairs, the authors decided the 230 that at least one model accepted (`sheet` = `Adjudicate`); the 228 that no model accepted are not linked (`sheet` = `Not linked (no model accepts)`). |
| `classifier`, `spouse_evidence`, `auto_link` | Classifier status, spouse-evidence state, and whether a classifier proposal was accepted by both models. |
| `supplementary_candidate` | The pair was identified by hand and entered the review as a supplementary candidate. |
| `sheet` | Sheet of the authors' adjudication workbook on which the pair appeared. |

The resolved labels and decisions are in `../data/linkage/training_labels.csv` and `../data/linkage/link_decisions.csv`.

## Compensation-owner review (`owner_links/`)

Claude Opus 5.5 and GPT-6 Astra each reviewed every genealogy record that has at least one candidate owner, with the instructions in `owner_links/instructions.txt`. A packet shows the genealogy record (names, birth and death years, declared district, father and wives) and every candidate owner in random order (name, district, parentage notes, localities, and minor or deceased flags), with no slave numbers, valuations or payments. Each model chose one candidate label or `NONE`.

- `packets/`: the evidence packets, in batches.
- `opus/` and `astra/`: each model's choice and reason for every record.
- `reconciliation.csv`: both choices for every record, whether they agree, the agreed owner, whether that owner is shared with another record, and whether the record was left unresolved (`unresolved`).
- `labels.csv`: the owner behind every candidate label of every packet.

The decisions are in `../data/inputs/owner_link_decisions.csv` and `owner_exclusions.csv`.

## Identity check of the census links (`identity_check/`)

A seeded random sample of 120 census links, stratified by how the link was made (classifier proposals accepted by both reviewers, classifier proposals decided by the authors, links made by adjudication alone, and links made only by the wife-blind linkage), was re-identified by Claude Fable and GPT-6 Astra with the instructions in `identity_check/instructions.txt`. Each model saw the genealogical record and every census head with the same surname and close given names, in random order and without household information or any indication of the link, and chose one head or `NONE`. `packets.json` holds the packets; `cases.csv` gives each model's choice and reason, the linked head's label, and whether each model, or both, rejected the link. The results are reported in the Online Appendix.
