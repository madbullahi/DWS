# Mass balance and removal efficiency

`mass_balance_plots.R` reads the raw spreadsheets in the repo root, writes tidy
tables and cross-checks to `output/`, and writes the box plots to `output/figures/`.

```
Rscript mass_balance/mass_balance_plots.R
```

## Figures

| File | What it shows |
|---|---|
| `Fig_PFAS_mass_balance_boxplot` | PFOS, PFOA and PFOS+PFOA recovery in medium, Daphnia tissue and total, with and without PET, per genotype (3 replicates x 3 days) |
| `Fig_mass_balance_water_tissue_combined` | Water (dashed) and Daphnia tissue (solid) recovery in one panel per genotype, one colour per compound (PET, PFOS, PFOA and their combinations) |
| `Fig_mass_balance_water_tissue_by_day` | Same as the combined plot, split by day (3 replicates per PFAS box; PET is a single value per day) |
| `Fig_mass_balance_water_tissue_barplot` | Bar plot of the combined figure: mean with SD error bars and individual values, days pooled (PFAS n = 9, PET n = 3) |
| `Fig_mass_balance_water_tissue_barplot_by_day` | Same bar plot split by day (PFAS n = 3; PET n = 1, no error bar) |
| `Fig_MP_mass_balance_boxplot` | PET recovery in medium, tissue and total, per genotype (one value per treatment and day) |
| `Fig_removal_efficiency_boxplot` | Removal from water of PFOS, diclofenac, atrazine and arsenic per genotype, recomputed from Table S2 raw |
| `Fig_arsenic_water_tissue_boxplot` | Arsenic removal from water next to arsenic in tissue (exposed vs control Daphnia) |
| `Fig_arsenic_water_vs_tissue_scatter` | Water removal vs tissue concentration for matched genotype/day/replicate |

## Data issues found

1. **PET+PFAS blocks reuse non-PET replicates.** In the PFAS sheet, 27 of the
   54 "+PET" replicate rows are identical (medium and tissue) to a row in the
   matching non-PET block, usually 2 of the 3 replicates per day
   (`output/check_PET_blocks_duplicating_noPET.csv`). **Resolved:** this is
   by design (incomplete factorial; measurements shared between PFAS-only and
   PFAS+PET treatments). The data are presented descriptively as proof of
   principle; SDs of "+PET" bars understate variability, and no formal
   PFAS-only vs PFAS+PET test should be run.
2. **Table S2 (uptake_removal.docx) uses only the first replicate.** Its values
   are the first replicate row of each day, rounded, not the mean of 3.
3. **MP sheet has no replicate-level data.** There is one value per treatment
   and day. The "SD" column is the SD across *different treatments* on the
   same day (e.g. `=STDEV(E4,K4,Q4)`), not across replicates.
4. **Table S3 vs Table S2 raw** (`output/check_TableS3_vs_raw.csv`):
   - DM1980 replicate 1, Day 1 is missing from Table S3 (all four chemicals).
   - PFOS DM1900 Day 3 in S3 uses the Day 1 final concentrations
     (13.54 / 29.83 instead of 19.54 / 45.92).
   - PFOS DM1960 Day 2 in S3 uses the Day 1 final concentrations
     (43.97 / 60.31 instead of 60.12 / 41.05).
   - All other IC, FC and RE values match.
5. **Trimethoprim** uptake and removal were never measured, so it is not part
   of this analysis (resolved; not missing data).
6. **As_massbalance.xlsx, water sheet**: the Treatment and Day headers are
   swapped, and DM1900 Day 1 and Day 3 have both replicates labelled 1
   (fixed in the script; matches Table S2 raw). Control water values are
   identical on all three days (858.4 / 719.1).
7. **Tissue units**: the arsenic tissue values are ug/L in the digested
   Daphnia samples (confirmed; the "ng/L" header in the sheet is wrong).
   Daphnia were not weighed, so they cannot be expressed per mass. Using a
   1 mL digest and 10 Daphnia per sample (recalled, not recorded),
   `arsenic_water_tissue_matched.csv` adds ng per sample (0.23-0.83) and
   pg per Daphnia (23-83).
8. **Genotype names differ between files**: `LR2_36_01` / `LRII_36` / `LRII3_16`
   and `LRV_01` / `LRV0_1` / `LRV_0_1`. The correct names are `LRII_36` and
   `LRV0_1`; the script uses these in all outputs.

9. **Figure S2 (current paper version)**: the "PET" panel shows the same bars
   as the "PFOS" panel (e.g. LRV0_1 Day 1 = 52 + 24 = 76 in both), so PET-only
   recovery is not actually plotted. The PET data are in the MPs sheet.

10. **Tia's agreed dataset** (`mass_balance_TS.xlsx`, the version Luisa and
    Mohamed agreed): all 108 PFAS replicate values match the raw data used
    here, apart from rounding (`output/check_TS_workbook_vs_raw.csv`). It has
    no PET-alone sheet; PET recovery still comes from the MPs sheet.
11. **`Mass_Balance_metadata.xlsx`**: one row per genotype and day. Medium and
    tissue are replicate 1 only (not the mean); SD is the SD of the 3
    replicate totals. The extra **PFOS_in_PS_PA** sheet is not measured data:
    every value is PFOS-alone replicate 1 multiplied by a fixed factor per day
    (0.906, 0.896, 0.863 for Days 1-3), and its SD column is copied from
    PFOS+PFOA. It is not used in the plots.

Removal efficiency = (mean no-Daphnia control on that day - final) / control x 100,
which reproduces the IC and RE columns of Table S3.
