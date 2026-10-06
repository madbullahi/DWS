# Functions for screening 16S data for potential pathogens and for checking
# BLAST hits against an antibiotic-resistance-gene (ARG) database.
# Used by analysis/04_pathogen_screening.R and tests/test_pathogen_functions.R.

# ---- Taxonomy-based pathogen screen ------------------------------------------

# Read a QIIME2 genus-level table exported with `biom convert --to-tsv`
# (rows = taxonomy strings, columns = samples). Returns long format with
# one row per sample and genus, and relative abundance rescaled to sum to 1.
read_genus_table <- function(path) {
  raw <- read.delim(path, skip = 1, check.names = FALSE, comment.char = "")
  names(raw)[1] <- "taxon"
  long <- tidyr::pivot_longer(raw, -taxon, names_to = "sample", values_to = "abundance")
  long %>%
    dplyr::group_by(sample) %>%
    dplyr::mutate(rel_abundance = abundance / sum(abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(genus = taxon_genus(taxon))
}

# Extract the genus name from a SILVA-style taxonomy string ("...;g__Legionella").
# Returns NA when the string is not resolved to genus level.
taxon_genus <- function(taxon) {
  g <- sub(".*g__", "", taxon)
  g <- sub(";.*", "", g)
  g[!grepl("g__", taxon) | g == ""] <- NA
  trimws(g)
}

# Match genera against the target list. `targets` needs columns `genus`,
# `silva_pattern` (regular expression on the SILVA genus) and `group`.
match_targets <- function(genera, targets) {
  out <- rep(NA_character_, length(genera))
  for (i in seq_len(nrow(targets))) {
    hit <- is.na(out) & !is.na(genera) & grepl(targets$silva_pattern[i], genera)
    out[hit] <- targets$genus[i]
  }
  out
}

# Compare a treatment group against the control for each target and day.
# Returns mean relative abundance (%) per group, % reduction relative to the
# control, and a two-sided Wilcoxon rank-sum test with Benjamini-Hochberg q-values.
compare_to_control <- function(long, treatment, control, by = c("target", "Day")) {
  long %>%
    dplyr::filter(Treatment %in% c(treatment, control)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      n_treatment     = sum(Treatment == treatment),
      n_control       = sum(Treatment == control),
      mean_treatment  = mean(rel_abundance[Treatment == treatment]) * 100,
      mean_control    = mean(rel_abundance[Treatment == control]) * 100,
      p_value = tryCatch(
        suppressWarnings(stats::wilcox.test(rel_abundance[Treatment == treatment],
                                            rel_abundance[Treatment == control],
                                            exact = FALSE)$p.value),
        error = function(e) NA_real_),
      .groups = "drop") %>%
    dplyr::mutate(
      reduction_pct = ifelse(mean_control > 0,
                             (mean_control - mean_treatment) / mean_control * 100, NA_real_),
      q_value = stats::p.adjust(p_value, method = "BH"))
}

# ---- BLAST against an ARG database --------------------------------------------

# Parse BLAST+ pairwise output (default -outfmt 0) into one row per query-subject
# pair, keeping the first (best) HSP of each subject. Queries without hits are dropped.
parse_blast_pairwise <- function(path) {
  lines <- readLines(path, warn = FALSE)
  rows <- list()
  query <- NA; qlen <- NA; subject <- NULL; in_header <- FALSE; hsp <- NULL

  flush_hsp <- function() {
    if (!is.null(hsp) && !is.null(subject)) {
      rows[[length(rows) + 1]] <<- data.frame(
        query = query, query_length = qlen, subject = subject,
        bit_score = hsp$bits, evalue = hsp$evalue,
        identity_pct = hsp$ident / hsp$alen * 100, alignment_length = hsp$alen,
        query_start = min(hsp$qpos), query_end = max(hsp$qpos),
        stringsAsFactors = FALSE)
    }
    hsp <<- NULL
  }

  for (ln in lines) {
    if (startsWith(ln, "Query= ")) {
      flush_hsp(); subject <- NULL
      query <- trimws(sub("^Query= ", "", ln)); qlen <- NA
    } else if (is.na(qlen) && is.null(subject) && grepl("^Length=\\d+", ln)) {
      qlen <- as.integer(sub("^Length=", "", ln))
    } else if (startsWith(ln, "> ")) {
      flush_hsp()
      subject <- trimws(sub("^> ", "", ln)); in_header <- TRUE
    } else if (in_header) {
      if (grepl("^Length=", ln)) in_header <- FALSE else subject <- paste(subject, trimws(ln))
    } else if (grepl("^ Score = ", ln)) {
      if (!is.null(hsp)) { hsp$done <- TRUE; next }   # ignore later HSPs of the same subject
      hsp <- list(bits = as.numeric(sub(".*Score = +([0-9.]+) bits.*", "\\1", ln)),
                  evalue = as.numeric(sub(".*Expect(\\(\\d+\\))? = +([0-9.e+-]+).*", "\\2", ln)),
                  qpos = integer(0), done = FALSE)
    } else if (grepl("^ Identities = ", ln) && !is.null(hsp) && is.null(hsp$alen)) {
      m <- regmatches(ln, regexec("Identities = (\\d+)/(\\d+)", ln))[[1]]
      hsp$ident <- as.numeric(m[2]); hsp$alen <- as.numeric(m[3])
    } else if (grepl("^Query +\\d+ ", ln) && !is.null(hsp) && !hsp$done) {
      m <- regmatches(ln, regexec("^Query +(\\d+) +\\S+ +(\\d+)", ln))[[1]]
      hsp$qpos <- c(hsp$qpos, as.integer(m[2]), as.integer(m[3]))
    } else if (startsWith(ln, "Lambda") && !is.null(hsp)) {
      hsp$done <- TRUE
    }
  }
  flush_hsp()
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out$query_coverage_pct <- (out$query_end - out$query_start + 1) / out$query_length * 100
  parts <- strsplit(out$subject, "|", fixed = TRUE)
  out$arg_id <- vapply(parts, `[`, "", 1)
  out$gene   <- vapply(parts, `[`, "", 2)
  out$description <- trimws(sub("^.*?\\|\\|([^|]*).*$", "\\1", out$subject, perl = TRUE))
  out
}

# Flag BLAST hits that meet ARG-annotation thresholds. Defaults follow common
# practice for nucleotide ARG screens (e.g. ARGs-OAP / SARG): >= 80 % identity
# over >= 75 % of the query, E-value <= 1e-10.
classify_arg_hits <- function(hits, min_identity = 80, min_coverage = 75, max_evalue = 1e-10) {
  if (!nrow(hits)) return(hits)
  reasons <- function(i) {
    r <- c(if (hits$identity_pct[i] < min_identity) sprintf("identity %.0f%% < %g%%", hits$identity_pct[i], min_identity),
           if (hits$query_coverage_pct[i] < min_coverage) sprintf("coverage %.0f%% < %g%%", hits$query_coverage_pct[i], min_coverage),
           if (hits$evalue[i] > max_evalue) sprintf("E-value %.1e > %.0e", hits$evalue[i], max_evalue))
    if (length(r)) paste(r, collapse = "; ") else ""
  }
  hits$fail_reason <- vapply(seq_len(nrow(hits)), reasons, "")
  hits$passes <- hits$fail_reason == ""
  hits
}

# ---- Adapter contamination check ------------------------------------------------

# Common Illumina adapter cores. TruSeq's 13-nt core is shared by Read 1 and Read 2
# adapters; Nextera covers Nextera/XT library preps.
illumina_adapters <- c(TruSeq = "AGATCGGAAGAGC", Nextera = "CTGTCTCTTATACACATCT")

reverse_complement <- function(seq) {
  chartr("ACGTacgt", "TGCAtgca", vapply(strsplit(seq, ""), function(x) paste(rev(x), collapse = ""), ""))
}

# Read a FASTA file into a named character vector (names = sequence IDs).
read_fasta <- function(path) {
  lines <- readLines(path, warn = FALSE)
  is_head <- startsWith(lines, ">")
  ids <- sub("^>(\\S+).*", "\\1", lines[is_head])
  seqs <- vapply(split(lines[!is_head], cumsum(is_head)[!is_head]), paste, "", collapse = "")
  setNames(toupper(seqs), ids)
}

# Find sequences containing an adapter (either strand). Returns one row per
# sequence-adapter match with the 1-based start position in the sequence.
find_adapters <- function(seqs, adapters = illumina_adapters) {
  rows <- list()
  for (a in names(adapters)) {
    for (strand in c("forward", "reverse")) {
      motif <- if (strand == "forward") adapters[[a]] else reverse_complement(adapters[[a]])
      pos <- regexpr(motif, seqs, fixed = TRUE)
      hit <- which(pos > 0)
      if (length(hit)) rows[[length(rows) + 1]] <- data.frame(
        id = names(seqs)[hit], adapter = a, strand = strand,
        start = as.integer(pos[hit]), seq_length = nchar(seqs[hit]),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame(id = character(), adapter = character(), strand = character(),
                                       start = integer(), seq_length = integer()))
  do.call(rbind, rows)
}

# Share of all reads in a QIIME2 feature table (biom TSV export) that belong to `ids`.
read_share <- function(feature_table_path, ids) {
  tab <- read.delim(feature_table_path, skip = 1, check.names = FALSE, comment.char = "", row.names = 1)
  totals <- rowSums(tab)
  list(n_reads = sum(totals[names(totals) %in% ids]), total_reads = sum(totals),
       pct = 100 * sum(totals[names(totals) %in% ids]) / sum(totals))
}
