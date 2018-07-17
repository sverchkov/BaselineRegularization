# Reproducing the KDD 2017 experiments

This folder contains code and notes for reproducing the KDD 2017 experiments.
Since the data used are not sharable, the code here assumes access to a database that contains the data, and may be
used to "reproduce" the experiments on other data.

## Cohort selection and data processing notes

Data from the EHR needs to be processed in order to obtain the granularity of drugs and conditions used in the paper.
For the experiments in the paper, only the OMOP DOIs and HOIs (drugs and conditions of interest) are considered for
each patient, and only patients that have at least one occurrence of a drug of interest and a condition of interest are
included in the cohort.
What follows are the definitions of these drugs and conditions of interest.

### Drugs and conditions of interest

[citation needed] defines 10 drugs of interest with associated `concept_id`s:

| Drug | `concept_id` |
|------|--------------|
|   | 600000001 |
| ... | ... |
|   | 600000010 |

and 10 conditions of interest, that are broken up into sub-categories of conditions with corresponding `concept_ids`.

| Condition | Sub-condition | `concept_id` |
|----|----|----|
|   |    | 500000101 |
|   |    |    |
|   |    | 500001002 |

### Matching drugs and conditions of interest to the EHR

While the EHR records we have locally follow the OMOP CDM, the drugs and conditions in the record do not directly correspond to the `concept_id`s of the conditions of interest listed above, since the latter are more abstract.
For drugs, we can simply use the `concept_ancestor` table to match the drugs of interest to the more granular drugs specified in the EHR.

For conditions, we need to go through thre steps:

1. There is a one-to-many map between the condition categories and the sub-condition `concept_id`s
2. `concept_ancestor` helps match the conditions of interest to standard, granular condition `concept_id`s, but these are mostly SNOMED conditions, and our particular EHR DB uses mostly ICD9 condition codes.
3. To match the ICD9 codes to standard condition concepts, we use the `Maps to` relations listed in the `concept_relation` vocabulary table.

