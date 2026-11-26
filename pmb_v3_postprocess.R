# =============================================================================
# pmb_v3_postprocess.R
# Post-processing of the 60-replication run stored in pmb_market_gmr_v3.db
#
# George Ray
#
# Replaces the manual sqlite3 -> CSV -> Excel pivot workflow. Produces:
#   * Figure 1 data: conversions per replication for the six campaign conditions
#   * Figure 2 data: opportunity capture per replication for the four learners
#   * Table 2: mean and SD of conversions
#   * Table 3: mean and SD of opportunity capture
#   * Table 4: paired Wilcoxon signed-rank tests (H1 and H2)
#   * Table 5: decomposition of the gap to the oracle
# and writes them all to one Excel workbook, one sheet per item.
#
# Requires: RSQLite, writexl
# =============================================================================

suppressPackageStartupMessages(library(RSQLite))

# ---- Paths: change "C:/R_Code/" to wherever the outputs are saved ------------
db_path  <- "C:/R_Code/pmb_market_gmr_v3.db"
out_xlsx <- "C:/R_Code/pmb_v3_results.xlsx"

# ---- 1. Read the run summary (the SQL query used in the Excel workflow) ------
con <- dbConnect(SQLite(), db_path)
long <- dbGetQuery(con, "
  SELECT replication, market_structure, system, total_conversions,
         expected_conversions, cumulative_creative_regret
  FROM auction_run_summary
  WHERE system IN ('ridge', 'two_way', 'oracle_clairvoyant')")
dbDisconnect(con)

# Paper labels: diagonal = fixed, crossed = rotation; two_way = twoway
structure_label <- c(diagonal = "fixed", crossed = "rotation")
system_label    <- c(ridge = "ridge", two_way = "twoway", oracle_clairvoyant = "oracle")
long$label <- paste(structure_label[long$market_structure],
                    system_label[long$system], sep = "-")

if (anyDuplicated(long[, c("replication", "label")])) {
  stop("The database holds more than one row for a replication and condition. ",
       "Check whether it contains more than one run.")
}

conv_cols <- c("fixed-ridge", "fixed-twoway", "fixed-oracle",
               "rotation-ridge", "rotation-twoway", "rotation-oracle")

# ---- 2. Widen: one row per replication, one column per condition -------------
# (the Excel pivot table: replication in rows, structure/system in columns)
widen <- function(variable) {
  w <- reshape(long[, c("replication", "label", variable)],
               idvar = "replication", timevar = "label", direction = "wide")
  names(w) <- sub(paste0("^", variable, "\\."), "", names(w))
  w <- w[order(w$replication), c("replication", conv_cols)]
  rownames(w) <- NULL
  w
}
results <- widen("total_conversions")      # Figure 1 data
expected <- widen("expected_conversions")
regret   <- widen("cumulative_creative_regret")

n_reps <- nrow(results)
if (anyNA(results)) stop("Some replications are missing a condition.")

# ---- 3. Opportunity capture (columns I to L in the Excel workflow) ------------
capture <- data.frame(
  replication               = results$replication,
  `fixed-ridge-capture`     = results$`fixed-ridge`     / results$`fixed-oracle`,
  `fixed-twoway-capture`    = results$`fixed-twoway`    / results$`fixed-oracle`,
  `rotation-ridge-capture`  = results$`rotation-ridge`  / results$`rotation-oracle`,
  `rotation-twoway-capture` = results$`rotation-twoway` / results$`rotation-oracle`,
  check.names = FALSE)                      # Figure 2 data

# ---- 4. Tables 2 and 3: mean and SD (SD = sample SD, as Excel STDEV.S) --------
summ <- function(df) {
  x <- df[, -1, drop = FALSE]
  out <- rbind(Mean = sapply(x, mean), SD = sapply(x, sd))
  data.frame(statistic = rownames(out), out, check.names = FALSE, row.names = NULL)
}
table2 <- summ(results)
table3 <- summ(capture)

# ---- 5. Table 4: paired Wilcoxon signed-rank tests ----------------------------
# Same tests as the original analysis: one-sample tests on the paired
# differences; H1 two-sided, H2 one-sided (greater).
test_row <- function(comparison, hypothesis, d, alternative) {
  t <- wilcox.test(d, alternative = alternative, conf.int = TRUE)
  data.frame(comparison, hypothesis,
             mean_difference = mean(d),
             ci_lower = t$conf.int[1], ci_upper = t$conf.int[2],
             V = unname(t$statistic), p_value = t$p.value,
             alternative = alternative,
             replications_favouring = sum(d > 0), n = length(d))
}
table4 <- rbind(
  test_row("Ridge opportunity capture, fixed minus rotation", "H1",
           capture$`fixed-ridge-capture` - capture$`rotation-ridge-capture`, "two.sided"),
  test_row("Two-way opportunity capture, fixed minus rotation", "H1",
           capture$`fixed-twoway-capture` - capture$`rotation-twoway-capture`, "two.sided"),
  test_row("Oracle conversions, rotation minus fixed", "H2",
           results$`rotation-oracle` - results$`fixed-oracle`, "greater"))

# ---- 6. Table 5: decomposition of the gap to the oracle ------------------------
# Gap = oracle expected conversions - learner expected conversions.
# Creative-selection loss = cumulative creative regret recorded by the simulation.
# Bidding loss = gap - creative-selection loss.
decomp_row <- function(structure, learner) {
  learner_col <- paste(structure, learner, sep = "-")
  oracle_col  <- paste(structure, "oracle", sep = "-")
  gap      <- expected[[oracle_col]] - expected[[learner_col]]
  creative <- regret[[learner_col]]
  data.frame(campaign = structure, learning_model = learner,
             gap_to_oracle = mean(gap), bidding_loss = mean(gap - creative),
             creative_selection_loss = mean(creative),
             creative_share_of_gap = mean(creative) / mean(gap))
}
table5 <- rbind(decomp_row("fixed", "ridge"), decomp_row("fixed", "twoway"),
                decomp_row("rotation", "ridge"), decomp_row("rotation", "twoway"))

# ---- 7. Statistics quoted in the text ------------------------------------------
text_row <- function(learner) {
  rot_cap   <- capture[[paste0("rotation-", learner, "-capture")]]
  breakeven <- results[[paste0("fixed-", learner)]] / results$`rotation-oracle`
  data.frame(
    learner = learner,
    rotation_capture_mean_of_ratios = mean(rot_cap),
    rotation_capture_ratio_of_means = mean(results[[paste0("rotation-", learner)]]) /
                                      mean(results$`rotation-oracle`),
    breakeven_capture = mean(breakeven),
    shortfall_points  = 100 * mean(breakeven - rot_cap),
    shortfall_min     = 100 * min(breakeven - rot_cap),
    shortfall_max     = 100 * max(breakeven - rot_cap),
    rotation_ahead_of_fixed = sum(rot_cap > breakeven),
    cor_capture_vs_rotation_oracle = cor(rot_cap, results$`rotation-oracle`),
    oracle_expansion = mean(results$`rotation-oracle`) / mean(results$`fixed-oracle`) - 1)
}
text_stats <- rbind(text_row("ridge"), text_row("twoway"))

# ---- 8. Print and save -----------------------------------------------------------
cat(sprintf("Replications read: %d\n\n", n_reps))
cat("Table 2: conversions\n");          print(table2, digits = 5, row.names = FALSE)
cat("\nTable 3: opportunity capture\n"); print(table3, digits = 4, row.names = FALSE)
cat("\nTable 4: Wilcoxon tests\n");      print(table4, digits = 4, row.names = FALSE)
cat("\nTable 5: decomposition\n");       print(table5, digits = 4, row.names = FALSE)

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(list(
    Figure_1_conversions = results,
    Figure_2_capture     = capture,
    Table_2              = table2,
    Table_3              = table3,
    Table_4_wilcoxon     = table4,
    Table_5_decomposition = table5,
    Long_data            = long), out_xlsx)
  cat(sprintf("\nWorkbook written: %s\n", out_xlsx))
} else {
  cat("\nInstall the writexl package to save the workbook: install.packages(\"writexl\")\n")
}
