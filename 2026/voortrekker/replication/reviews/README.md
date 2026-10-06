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

Each folder holds three kinds of file:
- `packets/`: the evidence packets, in batches, with `*_all.json` combining all batches.
- `claude/` and `codex/`: the verdicts for each pair (`ACCEPT`, `REJECT` or `UNCERTAIN`), with a short reason and the spouse evidence.
- A reconciliation file combining both verdicts for every pair: `training_reconciliation_all.csv` or `link_reconciliation_all.csv`.

In the training review, Claude reviewed batch 16 as two smaller batches, numbered 28 and 29; they contain the same pairs. Codex reviewed batch 16 whole.

Columns of the reconciliation files that are not self-explanatory:

| Column | Description |
|---|---|
| `set` | `original training pair` (hand-labeled by the four labelers) or `new sample` (sampled from the candidate set). |
| `original_label` | The hand label (1 = match, 0 = non-match), for hand-labeled pairs. |
| `queue` (training review) | Why a pair went to the authors: the models disagree, or they agree with each other but not with the hand label. All queued training pairs were decided by the authors. |
| `queue` (link review) | Why a pair was queued for the authors: the models disagree; the pair is a supplementary candidate (`old link`) that is not a classifier proposal accepted by both models; the classifier did not propose it although both models accept it; competing identities (two persons for one census head, or one person for two heads); or a changed partner. Of the 458 queued pairs, the authors decided the 230 that at least one model accepted (`sheet` = `Adjudicate`); the 228 that no model accepted are not linked (`sheet` = `Not linked (no model accepts)`). |
| `classifier`, `spouse_evidence`, `auto_link` | Classifier status, spouse-evidence state, and whether a classifier proposal was accepted by both models. |
| `old_link` | The pair was linked in a manual linkage of these records and entered the review as a supplementary candidate. |
| `sheet` | Sheet of the authors' adjudication workbook on which the pair appeared. |

The resolved labels and decisions are in `../data/linkage/training_labels.csv` and `../data/linkage/link_decisions.csv`.
