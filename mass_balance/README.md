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
| `Fig_MP_mass_balance_boxplot` | PET recovery in medium, tissue and total, per genotype (one value per treatment and day) |
| `Fig_removal_efficiency_boxplot` | Removal from water of PFOS, diclofenac, atrazine and arsenic per genotype, recomputed from Table S2 raw |
| `Fig_arsenic_water_tissue_boxplot` | Arsenic removal from water next to arsenic in tissue (exposed vs control Daphnia) |
| `Fig_arsenic_water_vs_tissue_scatter` | Water removal vs tissue concentration for matched genotype/day/replicate |

## Data issues found

1. **PET+PFAS blocks reuse non-PET replicates.** In the PFAS sheet, 27 of the
   54 "+PET" replicate rows are identical (medium and tissue) to a row in the
   matching non-PET block, usually 2 of the 3 replicates per day
   (`output/check_PET_blocks_duplicating_noPET.csv`).
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
5. **Trimethoprim** is not in any of the files.
6. **As_massbalance.xlsx, water sheet**: the Treatment and Day headers are
   swapped, and DM1900 Day 1 and Day 3 have both replicates labelled 1
   (fixed in the script; matches Table S2 raw). Control water values are
   identical on all three days (858.4 / 719.1).
7. **Tissue units**: the tissue sheet says "ng/L"; this needs checking
   (probably per mass of Daphnia), so the plots say "as recorded".
8. **Genotype names differ between files**: `LR2_36_01` / `LRII_36` / `LRII3_16`
   and `LRV_01` / `LRV0_1` / `LRV_0_1`. The plots use `LR2_36_01` and `LRV0_1`.

Removal efficiency = (mean no-Daphnia control on that day - final) / control x 100,
which reproduces the IC and RE columns of Table S3.
