# PMB-v3

## Purpose and context

This system simulates a budget-constrained programmatic media-buying campaign. A fixed number of ad auctions (`n_auctions`) arrive one at a time, each belonging to an audience segment. A bidding "system" (a simulated buying strategy) decides whether to bid on each auction and, if it wins, which creative to serve. The campaign can only buy a limited number of impressions (`impression_budget`), so every system must decide *which* opportunities are worth spending on, not just how to allocate creative once it has won.

The script is not standalone: it sources a companion file, `pmb_market_sim_gmr_v2.R`, which must live in the same directory (or at `C:/R_Code/`). V3 reuses V2's machinery for:

- generating the underlying "ground truth" conversion probabilities (`make_market_truth`)
- building segment/creative eligibility structures (`make_diagonal_params`, `make_crossed_params`, `validate_market_pair`)
- generating the sequence of auction segment arrivals (`make_replication_environment`)
- fitting the statistical learners (`fit_learner`)
- baseline creative-allocation logic (`allocate_oracle`, `allocate_from_predictions`, `allocate_random`)
- key construction and ground-truth persistence (`make_key`, `write_cell_ground_truth`)

V3's own code adds the **budget constraint**, the **bid decision layer**, and reporting/persistence for the resulting comparison study.

### Market structures simulated

- **Diagonal** (fixed) campaigns: exactly one eligible creative per segment.
- **Crossed** (rotation) campaigns: multiple eligible creatives per segment, requiring an in-flight creative-selection decision.

### Allocation systems simulated

| System | Behaviour |
|---|---|
| `random_exhaust` | Bids on every auction until the budget runs out (no pacing). |
| `random_paced` | Bids randomly, but at a rate that paces spend across the full horizon. |
| `ridge` | Logistic ridge regression learner + adaptive bid pacing. |
| `two_way` | Logistic model with two-way interactions + adaptive bid pacing. |
| `oracle_online` | Knows the true conversion probabilities now, but not the future sequence of auctions. |
| `oracle_clairvoyant` | Knows both the truth and the entire future auction sequence in advance (upper-bound benchmark). |

## Usage

```r
# Put pmb_market_sim_gmr_v2.R and pmb_market_sim_gmr_v3.R in the same folder,
# then set that folder as the working directory (replace with your own path).
setwd("C:/R_Code")

source("pmb_market_sim_gmr_v3.R")
validate_v3_program()

study_v3 <- run_budget_market_comparison(
  n_reps             = 60,
  n_auctions         = 500000,
  impression_budget  = 155000,
  impression_cost    = 1,
  burnin_impressions = 10000,
  refit_interval     = 5000,
  creative_epsilon   = 0.10,
  bid_epsilon        = 0.05,
  allocation_policy  = "proportional",
  db_path            = "C:/R_Code/pmb_market_v3_reproduce.db"
)
print(study_v3)
```

| Parameter | Where used | Meaning |
|---|---|---|
| `n_auctions` | environment / campaign | Total opportunity horizon: the number of auctions the campaign will see, regardless of how many it bids on or wins. |
| `impression_budget` | campaign | Maximum number of impressions the campaign is allowed to purchase (all at equal cost). |
| `impression_cost` | campaign | Cost per impression; used only to translate impression counts into spend/budget units. |
| `burnin_impressions` | campaign | Number of purchased impressions to accumulate before the first model refit for ridge/two_way. |
| `refit_interval` | campaign | Number of additional purchased impressions between subsequent model refits. |
| `creative_epsilon` | creative selection | Share of creative choices made at random (exploration), under either allocation policy. |
| `bid_epsilon` | bid pacing | Blend weight given to flat pacing (vs. score-based selective bidding) in the bid-probability formula. |
| `allocation_policy` | creative selection | `"proportional"` (weight by predicted value) or `"epsilon_greedy"` (mostly best, explore with `creative_epsilon`). |

## Runtime and post-processing

With v3, on a standard desktop or laptop, a simulation with 60 replications will take approximately 60 hours. The simulation outputs a SQLite database that can be used with post-processing routines. As an alternative, the reader is invited to download the SQLite database in this GitHub repository and go straight to post-processing to produce the tables and charts that are reported in the paper. As another alternative, v5 of the system can be used; it spreads the work across multiple logical processors, so the 60-replication simulation can be completed in about 14 hours.

The SQLite database, `pmb_market_gmr_v3.db`, stores the results from the run that produced the `Figure1.xlsx` and `Figure2.xlsx` files of results from a 60-replication run with this version of the PMB system.

### R approach to post-processing

Open RGui, then (please replace C:/R_Code/ with your path to the system files):

```r
setwd("C:/R_Code")
source("pmb_v3_postprocess.R")
```

In addition to the table printouts in RGui the post-processing produces an Excel file with worksheets presenting data used in the post-processing. You can create the Figure 1 Box and Whisker chart by selecting the Figure 1 worksheet and inserting a Box and Whisker chart. Likewise with the Figure 2 worksheet.
You now have the data needed to produce the charts and tables in the report.

### SQLite and Excel approach

To get to the xlsx from the database, follow these commands, replacing `C:/R_Code/` with the path where you have saved the simulation outputs and R code.

**SQLite action**

```
sqlite3 C:/R_Code/pmb_market_gmr_v3.db
.headers on
.mode csv
.output C:/R_Code/v3_conversions.csv
SELECT replication, market_structure, system, total_conversions
  FROM auction_run_summary
  WHERE system IN ('ridge','two_way','oracle_clairvoyant');
.output stdout
.quit
```

**Excel action**

1. Right-click `C:/R_Code/v3_conversions.csv` and select **Open in Excel**.
2. Save `C:/R_Code/v3_conversions.csv` as an Excel workbook.
3. In the `v3_conversions` worksheet, select all the data and insert a pivot table.
4. For the pivot table:
   - Move **Market Structure** to Columns, then move **System** to Columns.
   - Move **Replication** to Rows.
   - Move **Total Conversions** to Values.
5. Select all of the pivot table and copy it.
6. Create a new worksheet and name it `results`.
7. Select cell A1 and paste the pivot table as values.
8. Delete the Crossed Total, Diagonal Total and Grand Total columns.
9. For each column with conversions, calculate the mean and standard deviation.
10. Type labels for columns I through L: `fixed-ridge-capture`, `fixed-twoway-capture`, `rotation-ridge-capture`, `rotation-twoway-capture`.
11. Calculate the capture columns:
    - Column I: diagonal ridge ÷ diagonal clairvoyant
    - Column J: diagonal two-way ÷ diagonal clairvoyant
    - Column K: crossed ridge ÷ crossed clairvoyant
    - Column L: crossed two-way ÷ crossed clairvoyant

You now have the data needed to produce the charts and tables in the report.
