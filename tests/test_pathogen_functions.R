# Run with: Rscript -e 'testthat::test_dir("tests")'
library(testthat)
suppressPackageStartupMessages(library(dplyr))
source(here::here("analysis", "pathogen_functions.R"))

test_that("parse_blast_pairwise keeps the best HSP per subject and skips no-hit queries", {
  hits <- parse_blast_pairwise(here::here("tests", "fixtures", "blast_small.txt"))
  expect_equal(hits$query, c("asv_good", "asv_short"))
  good <- hits[hits$query == "asv_good", ]
  expect_equal(good$query_length, 100)
  expect_equal(good$gene, "tetw")
  expect_equal(good$evalue, 1e-45)
  expect_equal(c(good$query_start, good$query_end), c(1, 98))   # second HSP (200-219) ignored
  expect_equal(good$query_coverage_pct, 98)
  expect_equal(good$description, "tetracycline resistance protein TetW [Some bacterium]")
})

test_that("classify_arg_hits applies identity, coverage and E-value thresholds", {
  hits <- classify_arg_hits(parse_blast_pairwise(here::here("tests", "fixtures", "blast_small.txt")))
  expect_equal(hits$passes, c(TRUE, FALSE))
  expect_match(hits$fail_reason[2], "coverage 10% < 75%")
  expect_match(hits$fail_reason[2], "E-value")
})

test_that("taxon_genus and match_targets handle SILVA strings", {
  tax <- c("d__Bacteria;p__Proteobacteria;g__Legionella",
           "d__Bacteria;p__Firmicutes;g__Clostridium_sensu_stricto_1",
           "d__Bacteria;p__Proteobacteria;f__Alcaligenaceae;__",
           "d__Bacteria;g__Pseudomonas_X")
  g <- taxon_genus(tax)
  expect_equal(g, c("Legionella", "Clostridium_sensu_stricto_1", NA, "Pseudomonas_X"))
  targets <- data.frame(genus = c("Legionella", "Clostridium sensu stricto", "Pseudomonas"),
                        silva_pattern = c("^Legionella$", "^Clostridium_sensu_stricto", "^Pseudomonas$"))
  expect_equal(match_targets(g, targets), c("Legionella", "Clostridium sensu stricto", NA, NA))
})

test_that("compare_to_control reports reduction relative to control", {
  d <- data.frame(target = "X", Day = "D1",
                  Treatment = rep(c("T", "C"), each = 3),
                  rel_abundance = c(0.01, 0.01, 0.01, 0.04, 0.04, 0.04))
  r <- compare_to_control(d, "T", "C")
  expect_equal(r$mean_treatment, 1)
  expect_equal(r$mean_control, 4)
  expect_equal(r$reduction_pct, 75)
})
