# =============================================================================
# pmb_market_sim_gmr_v3.R
# Budget-constrained programmatic advertising simulation
#
# George Ray
#
# Extends pmb_market_sim_gmr_v2.R:
#   * n_auctions is the opportunity horizon.
#   * impression_budget is the maximum number of equal-cost impressions.
#   * Every opportunity reveals its audience segment before a bid decision.
#   * Fixed/diagonal campaigns have one eligible creative per segment.
#   * Rotation/crossed campaigns have multiple creatives per segment.
#   * Learners may preserve budget by declining low-predicted-value auctions.
#
# Primary systems:
#   random_exhaust     Bid on every auction until budget exhaustion.
#   random_paced       Spend randomly but pace across the full horizon.
#   ridge              Logistic Ridge + adaptive bid pacing.
#   two_way            Logistic Two-way + adaptive bid pacing.
#   oracle_online      Knows truth now, but not the future auction sequence.
#   oracle_clairvoyant Knows truth and the full future auction sequence.
#
# V3 sources V2 for shared truth generation, eligibility, model fitting, and
# creative allocation. Keep both files in the same directory.
# =============================================================================

v2_candidates <- c(
  "C:/R_Code/pmb_market_sim_gmr_v2.R",
  file.path(getwd(), "pmb_market_sim_gmr_v2.R")
)
v2_path <- v2_candidates[file.exists(v2_candidates)][1]
if (is.na(v2_path)) {
  stop("Could not locate pmb_market_sim_gmr_v2.R.")
}
source(v2_path)

# =============================================================================
# 1. AUCTION ENVIRONMENT
# =============================================================================

make_auction_environment <- function(
  truth,
  n_auctions,
  segment_probabilities = NULL
) {
  if (n_auctions < 1L) {
    stop("n_auctions must be positive.")
  }

  environment <- make_replication_environment(
    truth = truth,
    n_impressions = n_auctions,
    segment_probabilities = segment_probabilities
  )
  names(environment)[names(environment) == "segment_arrivals"] <-
    "auction_segments"
  environment$n_auctions <- n_auctions
  environment
}

validate_budget_design <- function(
  n_auctions,
  impression_budget,
  impression_cost
) {
  if (length(n_auctions) != 1L || !is.finite(n_auctions) ||
      n_auctions < 1 || n_auctions != as.integer(n_auctions)) {
    stop("n_auctions must be a positive integer.")
  }
  if (length(impression_budget) != 1L ||
      !is.finite(impression_budget) ||
      impression_budget < 1 ||
      impression_budget != as.integer(impression_budget)) {
    stop("impression_budget must be a positive integer.")
  }
  if (impression_budget > n_auctions) {
    stop("impression_budget cannot exceed n_auctions.")
  }
  if (length(impression_cost) != 1L ||
      !is.finite(impression_cost) ||
      impression_cost <= 0) {
    stop("impression_cost must be positive.")
  }
  invisible(TRUE)
}

# =============================================================================
# 2. CREATIVE AND BID DECISIONS
# =============================================================================

get_segment_cells <- function(params, segment) {
  cells <- params$cells[
    params$cells$segment == segment,
    ,
    drop = FALSE
  ]
  if (nrow(cells) == 0L) {
    stop(sprintf("No eligible creative for segment '%s'.", segment))
  }
  cells
}

lookup_prediction <- function(predictions, key) {
  if (is.null(predictions)) {
    return(NA_real_)
  }
  value <- predictions$predicted[match(key, predictions$key)]
  if (length(value) != 1L || !is.finite(value)) {
    NA_real_
  } else {
    value
  }
}

select_creative_v3 <- function(
  eligible_cells,
  system,
  predictions = NULL,
  epsilon = 0.10,
  allocation_policy = c("proportional", "epsilon_greedy")
) {
  allocation_policy <- match.arg(allocation_policy)

  if (nrow(eligible_cells) == 1L) {
    selected <- eligible_cells
    selected$was_creative_exploration <- FALSE
    selected$creative_selection_probability <- 1
    return(selected)
  }

  if (system %in% c("oracle_online", "oracle_clairvoyant")) {
    selected <- allocate_oracle(eligible_cells)
    names(selected)[names(selected) == "was_exploration"] <-
      "was_creative_exploration"
    names(selected)[names(selected) == "selection_probability"] <-
      "creative_selection_probability"
    return(selected)
  }

  if (system %in% c("ridge", "two_way") && !is.null(predictions)) {
    selected <- allocate_from_predictions(
      eligible_cells = eligible_cells,
      predictions = predictions,
      epsilon = epsilon,
      policy = allocation_policy
    )
    names(selected)[names(selected) == "was_exploration"] <-
      "was_creative_exploration"
    names(selected)[names(selected) == "selection_probability"] <-
      "creative_selection_probability"
    return(selected)
  }

  selected <- allocate_random(eligible_cells)
  selected$was_creative_exploration <-
    system %in% c("ridge", "two_way")
  selected$creative_selection_probability <- 1 / nrow(eligible_cells)
  selected
}

true_segment_scores <- function(params) {
  scores <- aggregate(
    true_p ~ segment,
    data = params$cells,
    FUN = max
  )
  setNames(scores$true_p, scores$segment)
}

predicted_segment_scores <- function(params, predictions) {
  if (is.null(predictions)) {
    return(NULL)
  }
  joined <- merge(
    params$cells[, c("segment", "key"), drop = FALSE],
    predictions[, c("key", "predicted"), drop = FALSE],
    by = "key",
    all.x = TRUE,
    sort = FALSE
  )
  if (anyNA(joined$predicted) || any(!is.finite(joined$predicted))) {
    return(NULL)
  }
  scores <- aggregate(predicted ~ segment, data = joined, FUN = max)
  setNames(scores$predicted, scores$segment)
}

# Returns a bid probability that accepts approximately the best q proportion
# of opportunities, accounting for the segment supply distribution. Fractional
# acceptance at the threshold handles discrete segment categories.
selective_bid_probability <- function(
  current_score,
  segment_scores,
  segment_probabilities,
  target_acceptance_rate
) {
  q <- min(1, max(0, target_acceptance_rate))
  if (q <= 0) return(0)
  if (q >= 1) return(1)
  if (!is.finite(current_score) ||
      is.null(segment_scores) ||
      any(!is.finite(segment_scores))) {
    return(q)
  }

  probabilities <- segment_probabilities[names(segment_scores)]
  if (anyNA(probabilities)) {
    stop("Segment score names do not match segment probabilities.")
  }
  probabilities <- probabilities / sum(probabilities)

  greater_mass <- sum(probabilities[segment_scores > current_score])
  equal_mass <- sum(probabilities[segment_scores == current_score])

  if (q <= greater_mass) {
    0
  } else if (q >= greater_mass + equal_mass) {
    1
  } else {
    (q - greater_mass) / equal_mass
  }
}

make_clairvoyant_bid_schedule <- function(
  params,
  environment,
  impression_budget
) {
  scores <- true_segment_scores(params)
  auction_scores <- unname(scores[environment$auction_segments])

  # Random tie-breaking prevents systematic preference for early or late
  # auctions when many opportunities have identical segment-level truth.
  tie_breaker <- runif(length(auction_scores))
  selected <- order(-auction_scores, tie_breaker)[
    seq_len(impression_budget)
  ]
  schedule <- rep(FALSE, length(auction_scores))
  schedule[selected] <- TRUE
  schedule
}

# =============================================================================
# 3. BUDGET-CONSTRAINED CAMPAIGN
# =============================================================================

simulate_budget_campaign <- function(
  params,
  environment,
  system = c(
    "random_exhaust",
    "random_paced",
    "ridge",
    "two_way",
    "oracle_online",
    "oracle_clairvoyant"
  ),
  impression_budget = 155000L,
  impression_cost = 1,
  burnin_impressions = 10000L,
  refit_interval = 5000L,
  creative_epsilon = 0.10,
  bid_epsilon = 0.05,
  allocation_policy = c("proportional", "epsilon_greedy"),
  keep_event_log = FALSE,
  verbose = FALSE
) {
  system <- match.arg(system)
  allocation_policy <- match.arg(allocation_policy)
  n_auctions <- environment$n_auctions

  validate_budget_design(
    n_auctions,
    impression_budget,
    impression_cost
  )
  if (burnin_impressions < 0L || refit_interval < 1L) {
    stop("burnin_impressions must be nonnegative and refit_interval positive.")
  }
  if (creative_epsilon < 0 || creative_epsilon > 1 ||
      bid_epsilon < 0 || bid_epsilon > 1) {
    stop("Exploration rates must lie between zero and one.")
  }

  remaining_impressions <- as.integer(impression_budget)
  remaining_budget <- impression_budget * impression_cost
  purchased <- 0L
  total_conversions <- 0L
  expected_conversions <- 0
  cumulative_creative_regret <- 0
  predictions <- NULL
  fit_failures <- 0L
  fit_messages <- character()
  lambda_track <- numeric()
  next_refit <- max(1L, as.integer(burnin_impressions))
  exhausted_at <- NA_integer_

  # Only purchased impressions enter supervised training history.
  history_segment <- character(impression_budget)
  history_creative <- character(impression_budget)
  history_converted <- integer(impression_budget)

  if (keep_event_log) {
    event_segment <- environment$auction_segments
    event_bid <- logical(n_auctions)
    event_creative <- rep(NA_character_, n_auctions)
    event_key <- rep(NA_character_, n_auctions)
    event_predicted <- rep(NA_real_, n_auctions)
    event_bid_probability <- numeric(n_auctions)
    event_creative_probability <- rep(NA_real_, n_auctions)
    event_creative_exploration <- logical(n_auctions)
    event_true_p <- rep(NA_real_, n_auctions)
    event_converted <- rep(NA_integer_, n_auctions)
    event_remaining_impressions <- integer(n_auctions)
  }

  clairvoyant_schedule <- if (system == "oracle_clairvoyant") {
    make_clairvoyant_bid_schedule(
      params,
      environment,
      impression_budget
    )
  } else {
    NULL
  }
  truth_scores <- if (system == "oracle_online") {
    true_segment_scores(params)
  } else {
    NULL
  }

  for (auction in seq_len(n_auctions)) {
    remaining_auctions <- n_auctions - auction + 1L

    if (remaining_impressions <= 0L) {
      if (is.na(exhausted_at)) exhausted_at <- auction - 1L
      if (keep_event_log) {
        event_remaining_impressions[auction:n_auctions] <- 0L
      }
      break
    }

    segment <- environment$auction_segments[auction]
    eligible_cells <- get_segment_cells(params, segment)

    target_rate <- min(
      1,
      remaining_impressions / remaining_auctions
    )

    if (system == "random_exhaust") {
      bid_probability <- 1
      bid <- TRUE
    } else if (system == "random_paced") {
      bid_probability <- target_rate
      bid <- runif(1) < bid_probability
    } else if (system == "oracle_clairvoyant") {
      bid_probability <- as.numeric(clairvoyant_schedule[auction])
      bid <- clairvoyant_schedule[auction]
    } else {
      segment_scores <- if (system == "oracle_online") {
        truth_scores
      } else {
        predicted_segment_scores(params, predictions)
      }

      current_score <- if (is.null(segment_scores)) {
        NA_real_
      } else {
        unname(segment_scores[segment])
      }

      selective_probability <- selective_bid_probability(
        current_score = current_score,
        segment_scores = segment_scores,
        segment_probabilities = environment$segment_probabilities,
        target_acceptance_rate = target_rate
      )

      # Blend selective bidding with random pacing. This preserves exploration
      # and keeps the expected overall acceptance rate close to target_rate.
      bid_probability <- (1 - bid_epsilon) * selective_probability +
        bid_epsilon * target_rate

      # If every remaining auction is needed to spend the budget, force a bid.
      if (remaining_impressions >= remaining_auctions) {
        bid_probability <- 1
      }
      bid <- runif(1) < bid_probability
    }

    if (keep_event_log) {
      event_bid[auction] <- bid
      event_bid_probability[auction] <- bid_probability
      event_remaining_impressions[auction] <- remaining_impressions
    }

    if (!bid) {
      next
    }

    selected <- select_creative_v3(
      eligible_cells = eligible_cells,
      system = system,
      predictions = predictions,
      epsilon = creative_epsilon,
      allocation_policy = allocation_policy
    )

    predicted_p <- lookup_prediction(predictions, selected$key)
    outcome_u <- environment$outcome_uniforms[auction, selected$key]
    converted <- as.integer(outcome_u < selected$true_p)

    purchased <- purchased + 1L
    remaining_impressions <- remaining_impressions - 1L
    remaining_budget <- remaining_budget - impression_cost
    total_conversions <- total_conversions + converted
    expected_conversions <- expected_conversions + selected$true_p
    cumulative_creative_regret <- cumulative_creative_regret +
      selected$oracle_regret

    history_segment[purchased] <- as.character(selected$segment)
    history_creative[purchased] <- as.character(selected$creative)
    history_converted[purchased] <- converted

    if (keep_event_log) {
      event_creative[auction] <- as.character(selected$creative)
      event_key[auction] <- as.character(selected$key)
      event_predicted[auction] <- predicted_p
      event_creative_probability[auction] <-
        selected$creative_selection_probability
      event_creative_exploration[auction] <-
        selected$was_creative_exploration
      event_true_p[auction] <- selected$true_p
      event_converted[auction] <- converted
      event_remaining_impressions[auction] <- remaining_impressions
    }

    should_refit <- system %in% c("ridge", "two_way") &&
      purchased >= next_refit

    if (should_refit) {
      history <- data.frame(
        segment = history_segment[seq_len(purchased)],
        creative = history_creative[seq_len(purchased)],
        converted = history_converted[seq_len(purchased)],
        stringsAsFactors = FALSE
      )

      fit_result <- fit_learner(
        method = system,
        history = history,
        prediction_cells = params$cells,
        structure = params$structure,
        segment_levels = params$segments,
        creative_levels = params$creatives
      )

      if (!is.null(fit_result$predictions)) {
        predictions <- fit_result$predictions
      } else {
        fit_failures <- fit_failures + 1L
      }
      if (is.finite(fit_result$diagnostics$lambda)) {
        lambda_track <- c(
          lambda_track,
          fit_result$diagnostics$lambda
        )
      }
      if (!is.null(fit_result$diagnostics$message)) {
        fit_messages <- c(
          fit_messages,
          fit_result$diagnostics$message
        )
      }

      if (verbose) {
        cat(sprintf(
          "  auction=%7d | bought=%6d | %-8s/%-8s | conversions=%d\n",
          auction,
          purchased,
          params$structure,
          system,
          total_conversions
        ))
      }

      repeat {
        next_refit <- next_refit + refit_interval
        if (next_refit > purchased) break
      }
    }

    if (remaining_impressions == 0L && is.na(exhausted_at)) {
      exhausted_at <- auction
    }
  }

  result <- list(
    market_structure = params$structure,
    system = system,
    allocation_policy = if (system %in% c("ridge", "two_way")) {
      allocation_policy
    } else {
      system
    },
    n_auctions = n_auctions,
    impression_budget = impression_budget,
    impressions_purchased = purchased,
    impressions_rejected = n_auctions - purchased,
    impression_cost = impression_cost,
    starting_budget = impression_budget * impression_cost,
    total_spend = purchased * impression_cost,
    remaining_budget = remaining_budget,
    budget_utilization = purchased / impression_budget,
    auction_acceptance_rate = purchased / n_auctions,
    budget_exhausted_at = exhausted_at,
    total_conversions = total_conversions,
    conversion_rate = if (purchased > 0L) {
      total_conversions / purchased
    } else {
      NA_real_
    },
    conversions_per_auction = total_conversions / n_auctions,
    expected_conversions = expected_conversions,
    expected_conversion_rate = if (purchased > 0L) {
      expected_conversions / purchased
    } else {
      NA_real_
    },
    cost_per_conversion = if (total_conversions > 0L) {
      purchased * impression_cost / total_conversions
    } else {
      Inf
    },
    cumulative_creative_regret = cumulative_creative_regret,
    mean_lambda = if (length(lambda_track)) {
      mean(lambda_track)
    } else {
      NA_real_
    },
    final_lambda = if (length(lambda_track)) {
      tail(lambda_track, 1L)
    } else {
      NA_real_
    },
    fit_failures = fit_failures,
    fit_messages = paste(unique(fit_messages), collapse = " | ")
  )

  if (keep_event_log) {
    result$events <- data.frame(
      auction = seq_len(n_auctions),
      segment = event_segment,
      bid = event_bid,
      bid_probability = event_bid_probability,
      creative = event_creative,
      key = event_key,
      predicted = event_predicted,
      creative_selection_probability = event_creative_probability,
      was_creative_exploration = event_creative_exploration,
      true_p = event_true_p,
      converted = event_converted,
      remaining_impressions = event_remaining_impressions,
      stringsAsFactors = FALSE
    )
  }

  result
}

# =============================================================================
# 4. SQLITE OUTPUT
# =============================================================================

init_v3_results_db <- function(db_path = "pmb_market_gmr_v3.db") {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package 'RSQLite' is required for database output.")
  }
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  RSQLite::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS auction_run_summary (
      replication               INTEGER NOT NULL,
      market_structure          TEXT NOT NULL,
      system                    TEXT NOT NULL,
      allocation_policy         TEXT NOT NULL,
      n_auctions                INTEGER NOT NULL,
      impression_budget         INTEGER NOT NULL,
      impressions_purchased     INTEGER NOT NULL,
      impression_cost           REAL NOT NULL,
      starting_budget           REAL NOT NULL,
      total_spend               REAL NOT NULL,
      remaining_budget          REAL NOT NULL,
      budget_utilization        REAL NOT NULL,
      auction_acceptance_rate   REAL NOT NULL,
      budget_exhausted_at       INTEGER,
      total_conversions         INTEGER NOT NULL,
      conversion_rate           REAL,
      conversions_per_auction   REAL NOT NULL,
      expected_conversions      REAL NOT NULL,
      expected_conversion_rate REAL,
      cost_per_conversion       REAL,
      lift_over_random_exhaust  REAL,
      incremental_conversions   REAL,
      cumulative_creative_regret REAL NOT NULL,
      mean_lambda               REAL,
      final_lambda              REAL,
      fit_failures              INTEGER NOT NULL,
      fit_messages              TEXT,
      timestamp                 TEXT DEFAULT (datetime('now')),
      PRIMARY KEY (
        replication,
        market_structure,
        system,
        allocation_policy
      )
    )")

  # Reuse the corrected V2 cell-truth schema in this separate V3 database.
  RSQLite::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS cell_ground_truth (
      replication       INTEGER NOT NULL,
      market_structure  TEXT NOT NULL,
      cell_key          TEXT NOT NULL,
      segment           TEXT NOT NULL,
      creative          TEXT NOT NULL,
      eligible          INTEGER NOT NULL,
      true_p            REAL NOT NULL,
      true_logit        REAL NOT NULL,
      segment_effect    REAL NOT NULL,
      creative_effect   REAL NOT NULL,
      interaction_effect REAL NOT NULL,
      is_segment_best   INTEGER NOT NULL,
      is_only_available INTEGER NOT NULL,
      rank_within_segment INTEGER NOT NULL,
      oracle_regret     REAL NOT NULL,
      PRIMARY KEY (replication, market_structure, cell_key)
    )")

  invisible(db_path)
}

write_v3_run_summary <- function(db_path, row) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  fields <- c(
    "replication", "market_structure", "system",
    "allocation_policy", "n_auctions", "impression_budget",
    "impressions_purchased", "impression_cost", "starting_budget",
    "total_spend", "remaining_budget", "budget_utilization",
    "auction_acceptance_rate", "budget_exhausted_at",
    "total_conversions", "conversion_rate", "conversions_per_auction",
    "expected_conversions", "expected_conversion_rate",
    "cost_per_conversion", "lift_over_random_exhaust",
    "incremental_conversions", "cumulative_creative_regret",
    "mean_lambda", "final_lambda", "fit_failures", "fit_messages"
  )

  RSQLite::dbWriteTable(
    con,
    "auction_run_summary",
    row[, fields, drop = FALSE],
    append = TRUE,
    row.names = FALSE
  )
}

# =============================================================================
# 5. V3 COMPARISON RUNNER
# =============================================================================

run_budget_market_comparison <- function(
  n_reps = 10L,
  n_auctions = 500000L,
  impression_budget = 155000L,
  impression_cost = 1,
  burnin_impressions = 10000L,
  refit_interval = 5000L,
  creative_epsilon = 0.10,
  bid_epsilon = 0.05,
  allocation_policy = c("proportional", "epsilon_greedy"),
  segment_probabilities = NULL,
  seed_base = 20260728L,
  base_rate = 0.03,
  segment_sd = 0.25,
  creative_sd = 0.20,
  interaction_sd = 0.30,
  systems = c(
    "random_exhaust",
    "random_paced",
    "ridge",
    "two_way",
    "oracle_online",
    "oracle_clairvoyant"
  ),
  db_path = NULL,
  keep_event_log = FALSE,
  verbose = FALSE
) {
  allocation_policy <- match.arg(allocation_policy)
  validate_budget_design(
    n_auctions,
    impression_budget,
    impression_cost
  )

  valid_systems <- c(
    "random_exhaust",
    "random_paced",
    "ridge",
    "two_way",
    "oracle_online",
    "oracle_clairvoyant"
  )
  if (!all(systems %in% valid_systems) || anyDuplicated(systems)) {
    stop("systems contains an unknown or duplicated optimization system.")
  }
  if (!"random_exhaust" %in% systems) {
    stop("systems must include random_exhaust as the primary baseline.")
  }

  if (!is.null(db_path)) {
    init_v3_results_db(db_path)
  }

  result_rows <- list()
  event_logs <- list()
  result_index <- 0L

  for (replication in seq_len(n_reps)) {
    cat(sprintf("Replication %d/%d\n", replication, n_reps))

    set.seed(as.integer(seed_base + replication))
    truth <- make_market_truth(
      base_rate = base_rate,
      segment_sd = segment_sd,
      creative_sd = creative_sd,
      interaction_sd = interaction_sd
    )
    diagonal <- make_diagonal_params(truth)
    crossed <- make_crossed_params(truth)
    validate_market_pair(diagonal, crossed)
    environment <- make_auction_environment(
      truth,
      n_auctions,
      segment_probabilities
    )

    if (!is.null(db_path)) {
      write_cell_ground_truth(db_path, replication, diagonal)
      write_cell_ground_truth(db_path, replication, crossed)
    }

    for (params in list(diagonal, crossed)) {
      structure_rows <- list()

      for (system in systems) {
        system_code <- match(system, valid_systems)
        structure_code <- match(
          params$structure,
          c("diagonal", "crossed")
        )
        set.seed(as.integer(
          seed_base +
            replication * 10000L +
            structure_code * 1000L +
            system_code * 100L
        ))

        sim <- simulate_budget_campaign(
          params = params,
          environment = environment,
          system = system,
          impression_budget = impression_budget,
          impression_cost = impression_cost,
          burnin_impressions = burnin_impressions,
          refit_interval = refit_interval,
          creative_epsilon = creative_epsilon,
          bid_epsilon = bid_epsilon,
          allocation_policy = allocation_policy,
          keep_event_log = keep_event_log,
          verbose = verbose
        )

        row <- as.data.frame(
          sim[setdiff(names(sim), "events")],
          stringsAsFactors = FALSE
        )
        row$replication <- replication
        structure_rows[[system]] <- row

        if (keep_event_log) {
          log_name <- paste(
            replication,
            params$structure,
            system,
            sep = "/"
          )
          events <- sim$events
          events$replication <- replication
          events$market_structure <- params$structure
          events$system <- system
          event_logs[[log_name]] <- events
        }

        cat(sprintf(
          paste0(
            "  %-8s | %-19s: bought=%6d, conv=%6d, ",
            "CVR=%6.3f%%, budget=%6.2f%%\n"
          ),
          params$structure,
          system,
          sim$impressions_purchased,
          sim$total_conversions,
          100 * sim$conversion_rate,
          100 * sim$budget_utilization
        ))
      }

      structure_df <- do.call(rbind, structure_rows)
      baseline <- structure_df$total_conversions[
        structure_df$system == "random_exhaust"
      ]
      structure_df$incremental_conversions <-
        structure_df$total_conversions - baseline
      structure_df$lift_over_random_exhaust <- 100 *
        structure_df$incremental_conversions / baseline

      for (i in seq_len(nrow(structure_df))) {
        result_index <- result_index + 1L
        result_rows[[result_index]] <- structure_df[i, , drop = FALSE]
        if (!is.null(db_path)) {
          write_v3_run_summary(
            db_path,
            structure_df[i, , drop = FALSE]
          )
        }
      }
    }
    cat("\n")
  }

  output <- list(
    results = do.call(rbind, result_rows),
    events = if (keep_event_log) event_logs else NULL,
    design = list(
      n_reps = n_reps,
      n_auctions = n_auctions,
      impression_budget = impression_budget,
      impression_cost = impression_cost,
      burnin_impressions = burnin_impressions,
      refit_interval = refit_interval,
      creative_epsilon = creative_epsilon,
      bid_epsilon = bid_epsilon,
      allocation_policy = allocation_policy,
      seed_base = seed_base
    )
  )
  class(output) <- c("pmb_budget_market_comparison", class(output))
  output
}

# =============================================================================
# 6. REPORTING
# =============================================================================

print_budget_market_summary <- function(x) {
  results <- if (inherits(x, "pmb_budget_market_comparison")) {
    x$results
  } else {
    x
  }

  summary_df <- aggregate(
    cbind(
      total_conversions,
      conversion_rate,
      lift_over_random_exhaust,
      budget_utilization,
      cost_per_conversion,
      auction_acceptance_rate
    ) ~ market_structure + system,
    data = results,
    FUN = mean,
    na.rm = TRUE
  )
  summary_df <- summary_df[
    order(summary_df$market_structure, summary_df$system),
    ,
    drop = FALSE
  ]

  cat("\n")
  cat("====================================================================\n")
  cat(" BUDGET-CONSTRAINED FIXED VERSUS CREATIVE-ROTATION CAMPAIGNS\n")
  cat("====================================================================\n")
  print(summary_df, row.names = FALSE, digits = 4)

  cat("\nPaired incremental-conversion tests versus random_exhaust:\n")
  for (structure_name in c("diagonal", "crossed")) {
    structure_rows <- results[
      results$market_structure == structure_name,
      ,
      drop = FALSE
    ]
    baseline <- structure_rows[
      structure_rows$system == "random_exhaust",
      c("replication", "total_conversions"),
      drop = FALSE
    ]
    names(baseline)[2] <- "baseline_conversions"

    for (system_name in intersect(
      c("random_paced", "ridge", "two_way"),
      structure_rows$system
    )) {
      method_rows <- structure_rows[
        structure_rows$system == system_name,
        c("replication", "total_conversions"),
        drop = FALSE
      ]
      names(method_rows)[2] <- "method_conversions"
      paired <- merge(method_rows, baseline, by = "replication")
      difference <- paired$method_conversions -
        paired$baseline_conversions

      cat(sprintf(
        "  %-8s/%-13s: mean incremental conversions = %.2f",
        structure_name,
        system_name,
        mean(difference)
      ))

      if (length(difference) >= 3L && any(difference != 0)) {
        test <- wilcox.test(
          paired$method_conversions,
          paired$baseline_conversions,
          paired = TRUE,
          alternative = "greater",
          exact = FALSE
        )
        cat(sprintf(", paired Wilcoxon p = %.4f\n", test$p.value))
      } else {
        cat(", insufficient nonzero replications for test\n")
      }
    }
  }

  under_spent <- results[
    results$budget_utilization < 0.999,
    c(
      "replication",
      "market_structure",
      "system",
      "budget_utilization"
    ),
    drop = FALSE
  ]
  cat(sprintf(
    "\nRuns ending with material unspent budget: %d of %d\n",
    nrow(under_spent),
    nrow(results)
  ))
  if (nrow(under_spent)) {
    print(under_spent, row.names = FALSE)
  }

  invisible(summary_df)
}

print.pmb_budget_market_comparison <- function(x, ...) {
  print_budget_market_summary(x)
  invisible(x)
}

# =============================================================================
# 7. VALIDATION
# =============================================================================

validate_v3_program <- function(seed = 4321L) {
  set.seed(seed)
  truth <- make_market_truth()
  diagonal <- make_diagonal_params(truth)
  crossed <- make_crossed_params(truth)
  validate_market_pair(diagonal, crossed)

  n_auctions <- 3000L
  impression_budget <- 1000L
  environment <- make_auction_environment(
    truth,
    n_auctions = n_auctions
  )

  systems <- c(
    "random_exhaust",
    "random_paced",
    "ridge",
    "two_way",
    "oracle_online",
    "oracle_clairvoyant"
  )
  validations <- list()

  for (structure in c("diagonal", "crossed")) {
    params <- if (structure == "diagonal") diagonal else crossed
    for (system in systems) {
      set.seed(seed + match(structure, c("diagonal", "crossed")) * 100L +
                 match(system, systems))
      validations[[paste(structure, system, sep = "/")]] <-
        simulate_budget_campaign(
          params = params,
          environment = environment,
          system = system,
          impression_budget = impression_budget,
          burnin_impressions = 300L,
          refit_interval = 300L,
          keep_event_log = TRUE
        )
    }
  }

  stopifnot(
    validations[["diagonal/random_exhaust"]]$impressions_purchased ==
      impression_budget,
    validations[["crossed/random_exhaust"]]$budget_exhausted_at ==
      impression_budget,
    validations[["diagonal/random_paced"]]$impressions_purchased ==
      impression_budget,
    validations[["crossed/random_paced"]]$impressions_purchased ==
      impression_budget,
    validations[["diagonal/oracle_clairvoyant"]]$impressions_purchased ==
      impression_budget,
    validations[["crossed/oracle_clairvoyant"]]$impressions_purchased ==
      impression_budget,
    validations[["diagonal/oracle_clairvoyant"]]$
      cumulative_creative_regret == 0,
    validations[["crossed/oracle_clairvoyant"]]$
      cumulative_creative_regret == 0,
    all(vapply(
      validations,
      function(z) {
        z$impressions_purchased <= impression_budget &&
          z$total_spend <= z$starting_budget &&
          z$remaining_budget >= 0
      },
      logical(1)
    ))
  )

  # In the diagonal market, every purchased impression must use the one
  # creative assigned to that segment, regardless of bidding system.
  diagonal_keys <- validations[["diagonal/ridge"]]$events
  bought <- diagonal_keys$bid
  expected_key <- make_key(
    diagonal_keys$segment[bought],
    paste0(
      "creative",
      sub("seg", "", diagonal_keys$segment[bought])
    )
  )
  stopifnot(identical(diagonal_keys$key[bought], expected_key))

  cat("V3 validation passed.\n")
  invisible(TRUE)
}

# Example:
# source("C:/R_Code/pmb_market_sim_gmr_v3.R")
# validate_v3_program()
#
# study_v3 <- run_budget_market_comparison(
#   n_reps = 10,
#   n_auctions = 500000,
#   impression_budget = 155000,
#   impression_cost = 1,
#   burnin_impressions = 10000,
#   refit_interval = 5000,
#   creative_epsilon = 0.10,
#   bid_epsilon = 0.05,
#   allocation_policy = "proportional",
#   db_path = "C:/R_Code/pmb_market_gmr_v3.db"
# )
# print(study_v3)
