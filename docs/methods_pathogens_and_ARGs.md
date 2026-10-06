# Methods and results text: pathogen screening and antibiotic resistance genes

Draft text for the manuscript. Items in [square brackets] need checking against
the lab records before submission. Numbers come from `analysis/04_pathogen_screening.R`
(outputs in `results/tables/`).

---

## Methods

### Screening for potential pathogens and faecal indicators

Genus-level relative abundances were obtained from the 16S rRNA gene amplicon data
processed in QIIME 2 [version] with DADA2 and taxonomy assigned against SILVA
[release 138] with a [Naive Bayes] classifier. Genera were screened against a
reference list of 21 bacterial genera that include waterborne pathogens listed in the
WHO *Guidelines for Drinking-water Quality* (4th edition, Table 7.1), opportunistic
pathogens (WHO GDWQ chapter 11 and the WHO bacterial priority pathogens list), an
emerging enteric pathogen (*Arcobacter*) and faecal indicators (*Enterococcus*,
*Clostridium* sensu stricto, *Bacteroides*). Because 16S rRNA amplicons resolve
taxa reliably only to genus level, matches were interpreted as genera containing
potentially pathogenic species and not as confirmation of pathogenic species or
strains.

For each genus, relative abundance in wastewater with *Daphnia* (n = 28 samples;
four genotypes; days 1–3) was compared with wastewater without *Daphnia*
(n = 9 samples; days 1–3) using two-sided Wilcoxon rank-sum tests, both for each
day and with days pooled. P-values were adjusted for multiple comparisons with
the Benjamini–Hochberg procedure, and adjusted values (q) < 0.05 were considered
significant. Analyses were run in R [version] and the code is available at
https://github.com/madbullahi/DWS.

### Screening amplicon sequences against an antibiotic resistance gene database

As an exploratory step, amplicon sequence variants (ASVs; n = 5,349) were aligned
with BLASTN 2.6.0+ against the nucleotide version of the Structured Antibiotic
Resistance Gene database (SARG, release 2019-03-25; 1,164,479 sequences). Hits were
retained only if they met commonly used annotation thresholds: ≥ 80 % nucleotide
identity over ≥ 75 % of the query length and E-value ≤ 1 × 10⁻¹⁰. ASVs were also
screened for residual Illumina adapter sequence (TruSeq core AGATCGGAAGAGC and
Nextera CTGTCTCTTATACACATCT, both strands).

---

## Results

### Potential pathogens

No obligate waterborne pathogens listed by the WHO (*Campylobacter*,
*Escherichia-Shigella*, *Salmonella*, *Vibrio*, *Yersinia*) were detected.
*Leptospira*, *Arcobacter* and *Aeromonas* occurred sporadically at trace levels
(≤ 0.08 % relative abundance, in 2–4 of 37 samples), only in wastewater with
*Daphnia*, and did not differ significantly from the control (q > 0.5).

Genera containing opportunistic pathogens were common. *Pseudomonas* was
detected in 36 of 37 samples and was about three-fold more abundant with *Daphnia*
than without (mean 3.42 % vs 1.17 %; q = 0.012). *Acinetobacter* was also
more abundant with *Daphnia* (0.66 % vs 0.08 %), but the difference was not
significant after correction (p = 0.031, q = 0.12). *Legionella*
(0.026 % vs 0.029 %) and *Mycobacterium* (0.084 % vs 0.093 %) did not differ
between treatments (q > 0.5).

Suggested discussion point: *Pseudomonas* and *Acinetobacter* are frequent
members of the *Daphnia* gut and carapace microbiome [cite], so their increase
probably reflects bacteria associated with the animals rather than the growth of
pathogenic strains. Species-level methods (culture on selective media or qPCR for
*P. aeruginosa* and *A. baumannii*) would be needed to assess any public-health
relevance.

### Antibiotic resistance genes

No ASV met the annotation thresholds for an antibiotic resistance gene. BLAST
returned 35 alignments from 31 ASVs (0.6 % of ASVs), but all were short local
alignments of 28–70 bp covering 8–21 % of the query (E = 1 × 10⁻⁴ to 5 × 10⁻³).
This number is close to the ~21 alignments expected by chance at E ≤ 0.004 across
5,349 queries. Twelve of the alignments, the only ones with 100 % identity, were to
a 28-bp stretch of Illumina TruSeq adapter present in 13 ASVs (0.02 % of reads)
and in the matched database entry. The remaining matched entries were annotated as
transporters and housekeeping proteins (ABC transporters, RND efflux membrane-fusion
proteins, a histidine kinase). The 16S rRNA gene does not encode resistance genes,
so these alignments were considered spurious, and resistance genes were not
assessed from the amplicon data.

### Short version (if space is limited)

> Screening 16S rRNA ASVs against the SARG database (BLASTN) produced no alignments
> meeting annotation thresholds (≥ 80 % identity over ≥ 75 % of the query,
> E ≤ 1 × 10⁻¹⁰). All matches were short local alignments (28–70 bp) consistent with
> chance, including residual adapter sequence. Antibiotic resistance genes were
> therefore not assessed from amplicon data.

---

## Limitations and suggested future work

- 16S rRNA amplicons identify genera, not pathogenic species or strains.
- Resistance genes require targeted methods: qPCR or high-throughput qPCR for
  wastewater marker genes (e.g. *sul1*, *tetW*, *ermB*, *bla*TEM and the integrase
  *intI1*), or shotgun metagenomics analysed with ARGs-OAP or CARD/RGI.
- Adapter sequence should be trimmed (e.g. with cutadapt) before DADA2; it was
  present in 13 ASVs here and did not affect community-level results.
