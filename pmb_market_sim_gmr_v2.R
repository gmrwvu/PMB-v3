# =============================================================================
# pmb_market_sim_gmr_v2.R
# Programmatic advertising simulation:
# fixed creative assignment versus creative rotation
#
# George Ray
#
# Design:
#   * Eight audience segments and eight reusable creative concepts.
#   * One shared latent 8 x 8 segment-by-creative truth market per replication.
#   * Fixed ("diagonal") campaign: one eligible creative per segment.
#   * Rotation ("crossed") campaign: all creatives eligible for every segment.
#   * Audience segments arrive exogenously. Learners choose only the creative.
#   * Logistic Ridge and unregularized Logistic Two-way use the same allocation
#     policy, so their operational performance can be compared with random
#     creative rotation.
#
# Important interpretation:
#   In the fixed campaign there is no within-segment creative choice. Therefore
#   random, Ridge, and Two-way must deliver the same creative and, with matched
#   potential outcomes, produce identical outcomes. Learning lift is possible
#   only when more than one creative is eligible for an arriving segment.
# =============================================================================

suppressPackageStartupMessages({
  library(glmnet)
})

# =============================================================================
# 1. GENERAL HELPERS
# =============================================================================

logit <- function(p) {
  if (any(!is.finite(p)) || any(p <= 0 | p >= 1)) {
    stop("Probabilities supplied to logit() must be strictly between 0 and 1.")
  }
  log(p / (1 - p))
}

validate_probability_vector <- function(x, name) {
  if (length(x) == 0L || any(!is.finite(x)) || any(x < 0) ||
      abs(sum(x) - 1) > 1e-8) {
    stop(sprintf("%s must be a finite, nonnegative vector summing to one.", name))
  }
  invisible(TRUE)
}

make_key <- function(segment, creative) {
  paste0(segment, "/", creative)
}

stable_method_seed <- function(seed_base, replication, structure, method) {
  structure_code <- match(structure, c("diagonal", "crossed"))
  method_code <- match(method, c("random", "ridge", "two_way", "oracle"))

  if (is.na(structure_code) || is.na(method_code)) {
    stop("Unknown structure or method when constructing a method seed.")
  }

  as.integer(
    seed_base +
      replication * 10000L +
      structure_code * 1000L +
      method_code * 100L
  )
}

# =============================================================================
# 2. SHARED LATENT SEGMENT-BY-CREATIVE MARKET
# =============================================================================

make_market_truth <- function(
  base_rate             = 0.03,
  segment_sd            = 0.25,
  creative_sd           = 0.20,
  interaction_sd        = 0.30,
  segments              = paste0("seg", 1:8),
  creatives             = paste0("creative", 1:8)
) {
  if (length(segments) != 8L || length(creatives) != 8L) {
    stop("The current study design requires eight segments and eight creatives.")
  }
  if (anyDuplicated(segments) || anyDuplicated(creatives)) {
    stop("Segment and creative labels must be unique.")
  }
  if (!is.finite(base_rate) || base_rate <= 0 || base_rate >= 1) {
    stop("base_rate must be strictly between zero and one.")
  }

  cells <- expand.grid(
    segment  = segments,
    creative = creatives,
    stringsAsFactors = FALSE
  )
  cells$key <- make_key(cells$segment, cells$creative)
  cells <- cells[order(cells$segment, cells$creative), , drop = FALSE]
  rownames(cells) <- NULL

  segment_effects <- setNames(
    rnorm(length(segments), mean = 0, sd = segment_sd),
    segments
  )
  creative_effects <- setNames(
    rnorm(length(creatives), mean = 0, sd = creative_sd),
    creatives
  )
  interaction_effects <- setNames(
    rnorm(nrow(cells), mean = 0, sd = interaction_sd),
    cells$key
  )

  cells$segment_effect <- unname(segment_effects[cells$segment])
  cells$creative_effect <- unname(creative_effects[cells$creative])
  cells$interaction_effect <- unname(interaction_effects[cells$key])
  cells$true_logit <- logit(base_rate) +
    cells$segment_effect +
    cells$creative_effect +
    cells$interaction_effect
  cells$true_p <- plogis(cells$true_logit)

  if (anyNA(cells$true_p) || any(cells$true_p <= 0 | cells$true_p >= 1)) {
    stop("Truth generation produced invalid conversion probabilities.")
  }

  list(
    cells               = cells,
    segments            = segments,
    creatives           = creatives,
    base_rate           = base_rate,
    base_logit          = logit(base_rate),
    segment_effects     = segment_effects,
    creative_effects    = creative_effects,
    interaction_effects = interaction_effects
  )
}

mark_market_cells <- function(cells, structure) {
  cells <- cells[order(cells$segment, cells$creative), , drop = FALSE]
  rownames(cells) <- NULL

  cells$market_structure <- structure
  cells$eligible <- TRUE
  cells$is_only_available <- ave(
    rep(1L, nrow(cells)),
    cells$segment,
    FUN = function(x) rep(length(x) == 1L, length(x))
  )
  cells$is_segment_best <- ave(
    cells$true_p,
    cells$segment,
    FUN = function(x) x == max(x)
  )
  cells$rank_within_segment <- ave(
    -cells$true_p,
    cells$segment,
    FUN = function(x) rank(x, ties.method = "min")
  )
  best_by_segment <- ave(
    cells$true_p,
    cells$segment,
    FUN = max
  )
  cells$oracle_regret <- best_by_segment - cells$true_p
  cells
}

make_diagonal_params <- function(truth) {
  cells <- truth$cells
  segment_number <- match(cells$segment, truth$segments)
  creative_number <- match(cells$creative, truth$creatives)
  cells <- cells[segment_number == creative_number, , drop = FALSE]
  cells <- mark_market_cells(cells, "diagonal")

  if (nrow(cells) != length(truth$segments) ||
      any(table(cells$segment) != 1L)) {
    stop("Diagonal market must contain exactly one creative per segment.")
  }

  list(
    structure = "diagonal",
    segments = truth$segments,
    creatives = truth$creatives,
    cells = cells,
    n_eligible_cells = nrow(cells),
    creatives_per_segment = 1L
  )
}

make_crossed_params <- function(truth) {
  cells <- mark_market_cells(truth$cells, "crossed")

  expected <- length(truth$segments) * length(truth$creatives)
  if (nrow(cells) != expected ||
      any(table(cells$segment) != length(truth$creatives))) {
    stop("Crossed market must contain every segment-by-creative combination.")
  }

  list(
    structure = "crossed",
    segments = truth$segments,
    creatives = truth$creatives,
    cells = cells,
    n_eligible_cells = nrow(cells),
    creatives_per_segment = length(truth$creatives)
  )
}

validate_market_pair <- function(diagonal, crossed) {
  if (diagonal$structure != "diagonal" || crossed$structure != "crossed") {
    stop("Market objects have incorrect structure labels.")
  }
  shared <- merge(
    diagonal$cells[, c("key", "true_p")],
    crossed$cells[, c("key", "true_p")],
    by = "key",
    suffixes = c("_diagonal", "_crossed")
  )
  if (nrow(shared) != nrow(diagonal$cells) ||
      !isTRUE(all.equal(shared$true_p_diagonal, shared$true_p_crossed))) {
    stop("Diagonal cells do not have identical truth in the crossed market.")
  }
  invisible(TRUE)
}

# =============================================================================
# 3. MATCHED SIMULATION ENVIRONMENT
# =============================================================================

make_replication_environment <- function(
  truth,
  n_impressions,
  segment_probabilities = NULL
) {
  if (is.null(segment_probabilities)) {
    segment_probabilities <- rep(
      1 / length(truth$segments),
      length(truth$segments)
    )
    names(segment_probabilities) <- truth$segments
  } else {
    if (is.null(names(segment_probabilities))) {
      stop("segment_probabilities must be named by segment.")
    }
    segment_probabilities <- segment_probabilities[truth$segments]
    if (anyNA(segment_probabilities)) {
      stop("segment_probabilities is missing one or more segments.")
    }
  }
  validate_probability_vector(segment_probabilities, "segment_probabilities")

  segment_arrivals <- sample(
    truth$segments,
    size = n_impressions,
    replace = TRUE,
    prob = segment_probabilities
  )

  # Potential-outcome uniforms provide common random numbers. Whenever two
  # methods select the same cell on the same impression, they receive the same
  # conversion outcome.
  outcome_uniforms <- matrix(
    runif(n_impressions * nrow(truth$cells)),
    nrow = n_impressions,
    ncol = nrow(truth$cells),
    dimnames = list(NULL, truth$cells$key)
  )

  list(
    segment_arrivals = segment_arrivals,
    outcome_uniforms = outcome_uniforms,
    segment_probabilities = segment_probabilities
  )
}

# =============================================================================
# 4. MODEL-MATRIX AND FITTING HELPERS
# =============================================================================

prepare_model_data <- function(data, segment_levels, creative_levels) {
  data$segment <- factor(data$segment, levels = segment_levels)
  data$creative <- factor(data$creative, levels = creative_levels)
  data
}

ridge_formula_for_structure <- function(structure) {
  if (structure == "diagonal") {
    ~ segment - 1
  } else if (structure == "crossed") {
    ~ segment * creative - 1
  } else {
    stop("Unknown market structure.")
  }
}

twoway_formula_for_structure <- function(structure) {
  if (structure == "diagonal") {
    converted ~ segment
  } else if (structure == "crossed") {
    converted ~ segment * creative
  } else {
    stop("Unknown market structure.")
  }
}

fit_logistic_ridge <- function(
  history,
  prediction_cells,
  structure,
  segment_levels,
  creative_levels,
  min_conversions = 5L,
  nfolds = 5L
) {
  diagnostics <- list(
    method = "ridge",
    converged = FALSE,
    message = NULL,
    lambda = NA_real_
  )

  if (sum(history$converted) < min_conversions ||
      sum(1L - history$converted) < min_conversions) {
    diagnostics$message <- "Insufficient outcome variation."
    return(list(predictions = NULL, diagnostics = diagnostics))
  }

  tryCatch({
    history <- prepare_model_data(
      history,
      segment_levels,
      creative_levels
    )
    prediction_cells <- prepare_model_data(
      prediction_cells,
      segment_levels,
      creative_levels
    )

    model_formula <- ridge_formula_for_structure(structure)
    X <- model.matrix(model_formula, data = history)
    X_pred <- model.matrix(model_formula, data = prediction_cells)

    if (!identical(colnames(X), colnames(X_pred))) {
      stop("Ridge training and prediction matrices have different columns.")
    }

    usable_folds <- min(
      as.integer(nfolds),
      nrow(history),
      sum(history$converted),
      sum(1L - history$converted)
    )
    if (usable_folds < 3L) {
      diagnostics$message <- "Fewer than three usable cross-validation folds."
      return(list(predictions = NULL, diagnostics = diagnostics))
    }

    fit <- cv.glmnet(
      x = X,
      y = as.integer(history$converted),
      family = "binomial",
      alpha = 0,
      nfolds = usable_folds,
      type.measure = "deviance"
    )

    predictions <- data.frame(
      key = prediction_cells$key,
      predicted = as.numeric(
        predict(
          fit,
          newx = X_pred,
          s = "lambda.min",
          type = "response"
        )
      ),
      stringsAsFactors = FALSE
    )

    diagnostics$converged <- TRUE
    diagnostics$lambda <- fit$lambda.min
    list(predictions = predictions, diagnostics = diagnostics)
  }, error = function(e) {
    diagnostics$message <- conditionMessage(e)
    list(predictions = NULL, diagnostics = diagnostics)
  })
}

fit_logistic_twoway <- function(
  history,
  prediction_cells,
  structure,
  segment_levels,
  creative_levels,
  min_conversions = 5L
) {
  diagnostics <- list(
    method = "two_way",
    converged = FALSE,
    message = NULL,
    lambda = NA_real_
  )

  if (sum(history$converted) < min_conversions ||
      sum(1L - history$converted) < min_conversions) {
    diagnostics$message <- "Insufficient outcome variation."
    return(list(predictions = NULL, diagnostics = diagnostics))
  }

  warning_messages <- character()

  tryCatch(
    withCallingHandlers({
      history <- prepare_model_data(
        history,
        segment_levels,
        creative_levels
      )
      prediction_cells <- prepare_model_data(
        prediction_cells,
        segment_levels,
        creative_levels
      )

      fit <- glm(
        formula = twoway_formula_for_structure(structure),
        data = history,
        family = binomial,
        control = glm.control(maxit = 100)
      )

      predicted <- as.numeric(
        predict(fit, newdata = prediction_cells, type = "response")
      )
      if (any(!is.finite(predicted))) {
        stop("Two-way model produced non-finite predictions.")
      }

      predictions <- data.frame(
        key = prediction_cells$key,
        predicted = predicted,
        stringsAsFactors = FALSE
      )

      diagnostics$converged <- isTRUE(fit$converged)
      diagnostics$message <- if (length(warning_messages)) {
        paste(unique(warning_messages), collapse = " | ")
      } else if (!isTRUE(fit$converged)) {
        "glm did not converge."
      } else {
        NULL
      }

      list(predictions = predictions, diagnostics = diagnostics)
    }, warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      diagnostics$message <- conditionMessage(e)
      list(predictions = NULL, diagnostics = diagnostics)
    }
  )
}

fit_learner <- function(
  method,
  history,
  prediction_cells,
  structure,
  segment_levels,
  creative_levels
) {
  if (method == "ridge") {
    fit_logistic_ridge(
      history = history,
      prediction_cells = prediction_cells,
      structure = structure,
      segment_levels = segment_levels,
      creative_levels = creative_levels
    )
  } else if (method == "two_way") {
    fit_logistic_twoway(
      history = history,
      prediction_cells = prediction_cells,
      structure = structure,
      segment_levels = segment_levels,
      creative_levels = creative_levels
    )
  } else {
    stop(sprintf("No fitted learner is defined for method '%s'.", method))
  }
}

# Backward-compatible names for interactive use.
fit_ridge_diagonal <- function(history, prediction_cells,
                               segment_levels, creative_levels) {
  fit_logistic_ridge(
    history, prediction_cells, "diagonal",
    segment_levels, creative_levels
  )
}

fit_ridge_crossed <- function(history, prediction_cells,
                              segment_levels, creative_levels) {
  fit_logistic_ridge(
    history, prediction_cells, "crossed",
    segment_levels, creative_levels
  )
}

fit_twoway_diagonal <- function(history, prediction_cells,
                                segment_levels, creative_levels) {
  fit_logistic_twoway(
    history, prediction_cells, "diagonal",
    segment_levels, creative_levels
  )
}

fit_twoway_crossed <- function(history, prediction_cells,
                               segment_levels, creative_levels) {
  fit_logistic_twoway(
    history, prediction_cells, "crossed",
    segment_levels, creative_levels
  )
}

# =============================================================================
# 5. CREATIVE ALLOCATION
# =============================================================================

allocate_random <- function(eligible_cells) {
  eligible_cells[
    sample.int(nrow(eligible_cells), size = 1L),
    ,
    drop = FALSE
  ]
}

prediction_scores <- function(eligible_cells, predictions) {
  scores <- merge(
    eligible_cells,
    predictions[, c("key", "predicted"), drop = FALSE],
    by = "key",
    all.x = TRUE,
    sort = FALSE
  )
  scores
}

allocate_from_predictions <- function(
  eligible_cells,
  predictions,
  epsilon = 0.10,
  policy = c("proportional", "epsilon_greedy")
) {
  policy <- match.arg(policy)

  if (is.null(predictions) || runif(1) < epsilon) {
    selected <- allocate_random(eligible_cells)
    selected$was_exploration <- TRUE
    selected$selection_probability <- 1 / nrow(eligible_cells)
    return(selected)
  }

  scores <- prediction_scores(eligible_cells, predictions)

  if (anyNA(scores$predicted) || any(!is.finite(scores$predicted))) {
    selected <- allocate_random(eligible_cells)
    selected$was_exploration <- TRUE
    selected$selection_probability <- 1 / nrow(eligible_cells)
    return(selected)
  }

  if (policy == "proportional") {
    weights <- pmax(scores$predicted, 0)
    if (sum(weights) <= 0) {
      selected <- allocate_random(eligible_cells)
      selected$was_exploration <- TRUE
      selected$selection_probability <- 1 / nrow(eligible_cells)
      return(selected)
    }
    weights <- weights / sum(weights)
    selected_index <- sample.int(
      nrow(scores),
      size = 1L,
      prob = weights
    )
    selected <- scores[selected_index, , drop = FALSE]
    selected$selection_probability <- (1 - epsilon) *
      weights[selected_index] + epsilon / nrow(scores)
  } else {
    best <- which(scores$predicted == max(scores$predicted))
    selected_index <- best[sample.int(length(best), size = 1L)]
    selected <- scores[selected_index, , drop = FALSE]
    selected$selection_probability <- (1 - epsilon) / length(best) +
      epsilon / nrow(scores)
  }

  selected$was_exploration <- FALSE
  selected
}

allocate_oracle <- function(eligible_cells) {
  best <- which(eligible_cells$true_p == max(eligible_cells$true_p))
  selected_index <- best[sample.int(length(best), size = 1L)]
  selected <- eligible_cells[selected_index, , drop = FALSE]
  selected$was_exploration <- FALSE
  selected$selection_probability <- 1 / length(best)
  selected
}

# =============================================================================
# 6. CAMPAIGN SIMULATION
# =============================================================================

simulate_campaign <- function(
  params,
  environment,
  method = c("random", "ridge", "two_way", "oracle"),
  n_impressions = length(environment$segment_arrivals),
  burnin_n = 10000L,
  refit_interval = 5000L,
  epsilon = 0.10,
  allocation_policy = c("proportional", "epsilon_greedy"),
  keep_event_log = FALSE,
  verbose = FALSE
) {
  method <- match.arg(method)
  allocation_policy <- match.arg(allocation_policy)

  if (n_impressions > length(environment$segment_arrivals) ||
      n_impressions > nrow(environment$outcome_uniforms)) {
    stop("The matched environment is shorter than n_impressions.")
  }
  if (!all(params$cells$key %in% colnames(environment$outcome_uniforms))) {
    stop("Outcome uniforms are missing one or more eligible cell keys.")
  }
  if (burnin_n < 0L || refit_interval < 1L) {
    stop("burnin_n must be nonnegative and refit_interval must be positive.")
  }

  log_segment <- character(n_impressions)
  log_creative <- character(n_impressions)
  log_key <- character(n_impressions)
  log_converted <- integer(n_impressions)
  log_predicted <- rep(NA_real_, n_impressions)
  log_selection_probability <- rep(NA_real_, n_impressions)
  log_exploration <- logical(n_impressions)
  log_true_p <- numeric(n_impressions)
  log_regret <- numeric(n_impressions)

  predictions <- NULL
  lambda_track <- numeric()
  fit_failures <- 0L
  fit_messages <- character()

  for (i in seq_len(n_impressions)) {
    arriving_segment <- environment$segment_arrivals[i]
    eligible_cells <- params$cells[
      params$cells$segment == arriving_segment,
      ,
      drop = FALSE
    ]

    if (nrow(eligible_cells) == 0L) {
      stop(sprintf(
        "No eligible creative for segment '%s'.",
        arriving_segment
      ))
    }

    if (method == "oracle") {
      cell <- allocate_oracle(eligible_cells)
    } else if (method == "random" ||
               i <= burnin_n ||
               is.null(predictions) ||
               nrow(eligible_cells) == 1L) {
      cell <- allocate_random(eligible_cells)
      cell$was_exploration <- method != "random" && nrow(eligible_cells) > 1L
      cell$selection_probability <- 1 / nrow(eligible_cells)
    } else {
      cell <- allocate_from_predictions(
        eligible_cells = eligible_cells,
        predictions = predictions,
        epsilon = epsilon,
        policy = allocation_policy
      )
    }

    predicted_match <- if (is.null(predictions)) {
      NA_real_
    } else {
      predictions$predicted[match(cell$key, predictions$key)]
    }

    outcome_u <- environment$outcome_uniforms[i, cell$key]
    converted <- as.integer(outcome_u < cell$true_p)

    log_segment[i] <- as.character(cell$segment)
    log_creative[i] <- as.character(cell$creative)
    log_key[i] <- as.character(cell$key)
    log_converted[i] <- converted
    log_predicted[i] <- predicted_match
    log_selection_probability[i] <- cell$selection_probability
    log_exploration[i] <- isTRUE(cell$was_exploration)
    log_true_p[i] <- cell$true_p
    log_regret[i] <- cell$oracle_regret

    should_refit <- method %in% c("ridge", "two_way") &&
      i >= burnin_n &&
      i %% refit_interval == 0L

    if (should_refit) {
      history <- data.frame(
        segment = log_segment[seq_len(i)],
        creative = log_creative[seq_len(i)],
        converted = log_converted[seq_len(i)],
        stringsAsFactors = FALSE
      )

      fit_result <- fit_learner(
        method = method,
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
        status <- if (is.null(fit_result$predictions)) "FAILED" else "OK"
        cat(sprintf(
          "  t=%6d | %-8s/%-8s | refit=%s | conversions=%d\n",
          i,
          params$structure,
          method,
          status,
          sum(log_converted[seq_len(i)])
        ))
      }
    }
  }

  total_conversions <- sum(log_converted)
  result <- list(
    total_conversions = total_conversions,
    conversion_rate = total_conversions / n_impressions,
    expected_conversions = sum(log_true_p),
    expected_conversion_rate = mean(log_true_p),
    cumulative_regret = sum(log_regret),
    mean_regret = mean(log_regret),
    best_creative_share = mean(log_regret == 0),
    exploration_share = mean(log_exploration),
    market_structure = params$structure,
    method = method,
    allocation_policy = if (method %in% c("ridge", "two_way")) {
      allocation_policy
    } else {
      method
    },
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
    fit_messages = paste(unique(fit_messages), collapse = " | "),
    n_impressions = n_impressions
  )

  if (keep_event_log) {
    result$events <- data.frame(
      impression = seq_len(n_impressions),
      segment = log_segment,
      creative = log_creative,
      key = log_key,
      predicted = log_predicted,
      selection_probability = log_selection_probability,
      was_exploration = log_exploration,
      true_p = log_true_p,
      oracle_regret = log_regret,
      converted = log_converted,
      stringsAsFactors = FALSE
    )
  }

  result
}

# =============================================================================
# 7. OPTIONAL SQLITE OUTPUT
# =============================================================================

init_results_db <- function(db_path = "pmb_market_gmr_v2.db") {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package 'RSQLite' is required for database output.")
  }
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  RSQLite::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS run_summary (
      replication             INTEGER NOT NULL,
      market_structure        TEXT NOT NULL,
      method                  TEXT NOT NULL,
      allocation_policy       TEXT NOT NULL,
      total_impressions       INTEGER NOT NULL,
      total_conversions       INTEGER NOT NULL,
      conversion_rate         REAL NOT NULL,
      expected_conversions    REAL NOT NULL,
      expected_conversion_rate REAL NOT NULL,
      lift_over_random        REAL,
      cumulative_regret       REAL NOT NULL,
      best_creative_share     REAL NOT NULL,
      exploration_share       REAL NOT NULL,
      mean_lambda             REAL,
      final_lambda            REAL,
      fit_failures            INTEGER NOT NULL,
      fit_messages            TEXT,
      timestamp               TEXT DEFAULT (datetime('now')),
      PRIMARY KEY (
        replication,
        market_structure,
        method,
        allocation_policy
      )
    )")

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

write_cell_ground_truth <- function(
  db_path,
  replication,
  params
) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  cells <- params$cells
  output <- data.frame(
    replication = replication,
    market_structure = params$structure,
    cell_key = cells$key,
    segment = cells$segment,
    creative = cells$creative,
    eligible = as.integer(cells$eligible),
    true_p = cells$true_p,
    true_logit = cells$true_logit,
    segment_effect = cells$segment_effect,
    creative_effect = cells$creative_effect,
    interaction_effect = cells$interaction_effect,
    is_segment_best = as.integer(cells$is_segment_best),
    is_only_available = as.integer(cells$is_only_available),
    rank_within_segment = as.integer(cells$rank_within_segment),
    oracle_regret = cells$oracle_regret,
    stringsAsFactors = FALSE
  )

  RSQLite::dbWriteTable(
    con,
    "cell_ground_truth",
    output,
    append = TRUE,
    row.names = FALSE
  )
}

write_run_summary <- function(db_path, row) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  fields <- c(
    "replication", "market_structure", "method",
    "allocation_policy", "total_impressions", "total_conversions",
    "conversion_rate", "expected_conversions",
    "expected_conversion_rate", "lift_over_random",
    "cumulative_regret", "best_creative_share",
    "exploration_share", "mean_lambda", "final_lambda",
    "fit_failures", "fit_messages"
  )

  RSQLite::dbWriteTable(
    con,
    "run_summary",
    row[, fields, drop = FALSE],
    append = TRUE,
    row.names = FALSE
  )
}

# =============================================================================
# 8. COMPARISON RUNNER
# =============================================================================

run_market_comparison <- function(
  n_reps = 10L,
  n_impressions = 155000L,
  burnin_n = 10000L,
  refit_interval = 5000L,
  epsilon = 0.10,
  allocation_policy = c("proportional", "epsilon_greedy"),
  segment_probabilities = NULL,
  seed_base = 20260728L,
  base_rate = 0.03,
  segment_sd = 0.25,
  creative_sd = 0.20,
  interaction_sd = 0.30,
  db_path = NULL,
  include_oracle = TRUE,
  keep_event_log = FALSE,
  verbose = FALSE
) {
  allocation_policy <- match.arg(allocation_policy)
  methods <- c("random", "ridge", "two_way")
  if (include_oracle) {
    methods <- c(methods, "oracle")
  }

  if (!is.null(db_path)) {
    init_results_db(db_path)
  }

  results <- list()
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

    environment <- make_replication_environment(
      truth = truth,
      n_impressions = n_impressions,
      segment_probabilities = segment_probabilities
    )

    if (!is.null(db_path)) {
      write_cell_ground_truth(db_path, replication, diagonal)
      write_cell_ground_truth(db_path, replication, crossed)
    }

    replication_rows <- list()

    for (params in list(diagonal, crossed)) {
      structure_rows <- list()

      for (method in methods) {
        set.seed(stable_method_seed(
          seed_base,
          replication,
          params$structure,
          method
        ))

        sim <- simulate_campaign(
          params = params,
          environment = environment,
          method = method,
          n_impressions = n_impressions,
          burnin_n = burnin_n,
          refit_interval = refit_interval,
          epsilon = epsilon,
          allocation_policy = allocation_policy,
          keep_event_log = keep_event_log,
          verbose = verbose
        )

        row <- data.frame(
          replication = replication,
          market_structure = params$structure,
          method = method,
          allocation_policy = sim$allocation_policy,
          total_impressions = sim$n_impressions,
          total_conversions = sim$total_conversions,
          conversion_rate = sim$conversion_rate,
          expected_conversions = sim$expected_conversions,
          expected_conversion_rate = sim$expected_conversion_rate,
          cumulative_regret = sim$cumulative_regret,
          best_creative_share = sim$best_creative_share,
          exploration_share = sim$exploration_share,
          mean_lambda = sim$mean_lambda,
          final_lambda = sim$final_lambda,
          fit_failures = sim$fit_failures,
          fit_messages = sim$fit_messages,
          stringsAsFactors = FALSE
        )
        structure_rows[[method]] <- row

        if (keep_event_log) {
          log_name <- paste(
            replication,
            params$structure,
            method,
            sep = "/"
          )
          event_log <- sim$events
          event_log$replication <- replication
          event_log$market_structure <- params$structure
          event_log$method <- method
          event_logs[[log_name]] <- event_log
        }

        cat(sprintf(
          "  %-8s | %-8s: %6d conversions (%6.3f%%), regret=%8.2f\n",
          params$structure,
          method,
          sim$total_conversions,
          100 * sim$conversion_rate,
          sim$cumulative_regret
        ))
      }

      structure_df <- do.call(rbind, structure_rows)
      random_conversions <- structure_df$total_conversions[
        structure_df$method == "random"
      ]
      structure_df$lift_over_random <- 100 *
        (structure_df$total_conversions - random_conversions) /
        random_conversions

      for (i in seq_len(nrow(structure_df))) {
        result_index <- result_index + 1L
        results[[result_index]] <- structure_df[i, , drop = FALSE]
        if (!is.null(db_path)) {
          write_run_summary(db_path, structure_df[i, , drop = FALSE])
        }
      }
    }
    cat("\n")
  }

  output <- list(
    results = do.call(rbind, results),
    events = if (keep_event_log) event_logs else NULL,
    design = list(
      n_reps = n_reps,
      n_impressions = n_impressions,
      burnin_n = burnin_n,
      refit_interval = refit_interval,
      epsilon = epsilon,
      allocation_policy = allocation_policy,
      seed_base = seed_base
    )
  )
  class(output) <- c("pmb_market_comparison", class(output))
  output
}

# =============================================================================
# 9. SUMMARY AND PAIRED INFERENCE
# =============================================================================

print_market_summary <- function(x) {
  results <- if (inherits(x, "pmb_market_comparison")) {
    x$results
  } else {
    x
  }

  required <- c(
    "replication", "market_structure", "method",
    "total_conversions", "conversion_rate", "lift_over_random"
  )
  if (!all(required %in% names(results))) {
    stop("Results object is missing required columns.")
  }

  summary_df <- aggregate(
    cbind(
      total_conversions,
      conversion_rate,
      lift_over_random,
      cumulative_regret,
      best_creative_share
    ) ~ market_structure + method,
    data = results,
    FUN = mean,
    na.rm = TRUE
  )
  summary_df <- summary_df[
    order(summary_df$market_structure, summary_df$method),
    ,
    drop = FALSE
  ]

  cat("\n")
  cat("====================================================================\n")
  cat(" FIXED CREATIVE VERSUS CREATIVE ROTATION\n")
  cat("====================================================================\n")
  print(summary_df, row.names = FALSE, digits = 4)

  cat("\nPaired crossed-market lift tests against random allocation:\n")
  crossed <- results[results$market_structure == "crossed", , drop = FALSE]

  for (method_name in c("ridge", "two_way")) {
    method_rows <- crossed[
      crossed$method == method_name,
      c("replication", "total_conversions"),
      drop = FALSE
    ]
    random_rows <- crossed[
      crossed$method == "random",
      c("replication", "total_conversions"),
      drop = FALSE
    ]
    names(method_rows)[2] <- "method_conversions"
    names(random_rows)[2] <- "random_conversions"
    paired <- merge(method_rows, random_rows, by = "replication")
    difference <- paired$method_conversions - paired$random_conversions

    cat(sprintf(
      "  %-8s: mean incremental conversions = %.2f",
      method_name,
      mean(difference)
    ))

    if (length(difference) >= 3L && any(difference != 0)) {
      test <- wilcox.test(
        paired$method_conversions,
        paired$random_conversions,
        paired = TRUE,
        alternative = "greater",
        exact = FALSE
      )
      cat(sprintf(", paired Wilcoxon p = %.4f\n", test$p.value))
    } else {
      cat(", insufficient nonzero replications for test\n")
    }
  }

  cat("\nDiagonal validation:\n")
  diagonal <- results[
    results$market_structure == "diagonal" &
      results$method %in% c("random", "ridge", "two_way"),
    ,
    drop = FALSE
  ]
  diagonal_wide <- reshape(
    diagonal[, c("replication", "method", "total_conversions")],
    idvar = "replication",
    timevar = "method",
    direction = "wide"
  )
  conversion_columns <- grep(
    "^total_conversions\\.",
    names(diagonal_wide),
    value = TRUE
  )
  identical_delivery <- all(
    apply(
      diagonal_wide[, conversion_columns, drop = FALSE],
      1,
      function(z) length(unique(z)) == 1L
    )
  )
  cat(sprintf(
    "  Random, Ridge, and Two-way outcomes identical within replication: %s\n",
    if (identical_delivery) "YES" else "NO - investigate implementation"
  ))

  invisible(summary_df)
}

print.pmb_market_comparison <- function(x, ...) {
  print_market_summary(x)
  invisible(x)
}

# =============================================================================
# 10. SMALL VALIDATION RUN
# =============================================================================

validate_program <- function(seed = 1234L) {
  set.seed(seed)
  truth <- make_market_truth()
  diagonal <- make_diagonal_params(truth)
  crossed <- make_crossed_params(truth)
  validate_market_pair(diagonal, crossed)

  stopifnot(
    nrow(diagonal$cells) == 8L,
    nrow(crossed$cells) == 64L,
    all(table(diagonal$cells$segment) == 1L),
    all(table(crossed$cells$segment) == 8L),
    all(diagonal$cells$is_only_available),
    sum(crossed$cells$is_segment_best) >= 8L,
    !anyDuplicated(truth$cells$key),
    all(diagonal$cells$key %in% crossed$cells$key)
  )

  environment <- make_replication_environment(
    truth,
    n_impressions = 500L
  )

  diagonal_results <- lapply(
    c("random", "ridge", "two_way"),
    function(method) {
      set.seed(seed + match(method, c("random", "ridge", "two_way")))
      simulate_campaign(
        params = diagonal,
        environment = environment,
        method = method,
        n_impressions = 500L,
        burnin_n = 100L,
        refit_interval = 100L,
        keep_event_log = TRUE
      )
    }
  )

  stopifnot(
    identical(
      diagonal_results[[1]]$events$key,
      diagonal_results[[2]]$events$key
    ),
    identical(
      diagonal_results[[1]]$events$key,
      diagonal_results[[3]]$events$key
    ),
    identical(
      diagonal_results[[1]]$events$converted,
      diagonal_results[[2]]$events$converted
    ),
    identical(
      diagonal_results[[1]]$events$converted,
      diagonal_results[[3]]$events$converted
    )
  )

  cat("Validation passed.\n")
  invisible(TRUE)
}

# Example:
# validate_program()
#
# study <- run_market_comparison(
#   n_reps = 10,
#   n_impressions = 155000,
#   burnin_n = 10000,
#   refit_interval = 5000,
#   epsilon = 0.10,
#   allocation_policy = "proportional",
#   db_path = "pmb_market_gmr_v2.db",
#   include_oracle = TRUE
# )
# print(study)
