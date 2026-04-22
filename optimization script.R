
if (!requireNamespace("parallel", quietly = TRUE)) {
  stop("The 'parallel' package is required. It ships with base R — please check your installation.")
}
library(parallel)


# -----------------------------------------------------------------------------
# HELPER: Generate all permutations of a vector
# Returns a matrix where each ROW is one permutation
# -----------------------------------------------------------------------------
get_all_permutations <- function(x) {
  n <- length(x)
  if (n == 1) return(matrix(x, nrow = 1))
  
  perms <- matrix(nrow = factorial(n), ncol = n)
  
  # Heap's algorithm (iterative) — efficient for n = 9
  idx <- 1
  a   <- x
  c_  <- integer(n)
  i   <- 1
  
  perms[idx, ] <- a
  idx <- idx + 1
  
  while (i <= n) {
    if (c_[i] < i - 1) {
      if (i %% 2 == 1) {
        tmp  <- a[1]; a[1] <- a[i]; a[i] <- tmp
      } else {
        tmp        <- a[c_[i] + 1]
        a[c_[i] + 1] <- a[i]
        a[i]       <- tmp
      }
      perms[idx, ] <- a
      idx  <- idx + 1
      c_[i] <- c_[i] + 1
      i     <- 1
    } else {
      c_[i] <- 0
      i     <- i + 1
    }
  }
  
  return(perms)
}


# -----------------------------------------------------------------------------
# HELPER: Evaluate one lineup — returns mean runs over n_sims games
# (Wrapper used by parApply for clean parallel dispatch)
# -----------------------------------------------------------------------------
eval_lineup <- function(lineup_vec, pitcher_hand, n_sims, sim_event_probs) {
  runs <- vapply(
    seq_len(n_sims),
    function(i) simulate_one_game(lineup_vec, pitcher_hand, sim_event_probs),
    numeric(1)
  )
  mean(runs)
}


# -----------------------------------------------------------------------------
# MAIN: Optimize lineup over all 9! = 362,880 permutations
#
# Arguments:
#   batters         - character vector of exactly 9 batter names
#   pitcher_hand    - "L" or "R"
#   n_sims          - simulations per lineup (recommended: 500)
#   top_n           - how many top lineups to return (default: 10)
#   sim_event_probs - your probability data frame
#   n_cores         - number of CPU cores to use (default: all available - 1)
#
# Returns:
#   Data frame with columns: Rank, Mean_Runs, Pos1 … Pos9
# -----------------------------------------------------------------------------
optimize_lineup <- function(batters,
                            pitcher_hand,
                            n_sims          = 500,
                            top_n           = 10,
                            sim_event_probs,
                            n_cores         = max(1L, detectCores() - 1L)) {
  
  # --- Input validation ------------------------------------------------------
  if (length(batters) != 9) stop("batters must be exactly 9 names.")
  if (!pitcher_hand %in% c("L", "R")) stop("pitcher_hand must be 'L' or 'R'.")
  if (top_n < 1) stop("top_n must be >= 1.")
  
  # Verify all batters have data in the probability table
  for (b in batters) {
    get_batter_probs(b, pitcher_hand, sim_event_probs)
  }
  
  # --- Generate all permutations ---------------------------------------------
  cat("Generating all 9! = 362,880 lineup permutations...\n")
  perm_matrix <- get_all_permutations(batters)   # 362,880 x 9 matrix
  n_lineups   <- nrow(perm_matrix)
  cat(sprintf("  %d permutations generated.\n\n", n_lineups))
  
  # --- Set up parallel cluster -----------------------------------------------
  n_cores <- min(n_cores, n_lineups)
  cat(sprintf("Launching parallel cluster with %d cores...\n", n_cores))
  cl <- makeCluster(n_cores)
  
  # Export everything the workers need
  clusterExport(cl, varlist = c(
    "simulate_one_game",
    "simulate_half_inning",
    "simulate_pa",
    "resolve_event",
    "resolve_groundout",
    "get_batter_probs",
    "eval_lineup",
    "sim_event_probs",
    "pitcher_hand",
    "n_sims",
    "DP_PROB",
    "TAG_ATTEMPT",
    "TAG_SUCCESS"
  ), envir = environment())
  
  # --- Run simulations in parallel -------------------------------------------
  cat(sprintf(
    "Evaluating %d lineups × %d sims = %s total games simulated...\n",
    n_lineups, n_sims,
    formatC(n_lineups * n_sims, format = "d", big.mark = ",")
  ))
  cat("This may take several minutes. Please wait...\n\n")
  
  start_time <- proc.time()
  
  # parApply over rows of the permutation matrix
  mean_runs_vec <- parApply(
    cl  = cl,
    X   = perm_matrix,
    MARGIN = 1,
    FUN = function(lineup_vec) {
      eval_lineup(lineup_vec, pitcher_hand, n_sims, sim_event_probs)
    }
  )
  
  stopCluster(cl)
  
  elapsed <- (proc.time() - start_time)[["elapsed"]]
  cat(sprintf("Simulation complete in %.1f seconds (%.1f minutes).\n\n",
              elapsed, elapsed / 60))
  
  # --- Rank and format results -----------------------------------------------
  ranked_idx <- order(mean_runs_vec, decreasing = TRUE)
  top_idx    <- ranked_idx[seq_len(min(top_n, n_lineups))]
  
  # Build results data frame
  results_df <- as.data.frame(perm_matrix[top_idx, , drop = FALSE],
                              stringsAsFactors = FALSE)
  colnames(results_df) <- paste0("Pos", 1:9)
  results_df <- cbind(
    Rank       = seq_len(nrow(results_df)),
    Mean_Runs  = round(mean_runs_vec[top_idx], 4),
    results_df
  )
  rownames(results_df) <- NULL
  
  # --- Print results ---------------------------------------------------------
  cat(sprintf("===== TOP %d LINEUPS BY MEAN RUNS (6 innings) =====\n\n", top_n))
  
  for (i in seq_len(nrow(results_df))) {
    cat(sprintf("Rank #%d  |  Mean Runs: %.4f\n", results_df$Rank[i], results_df$Mean_Runs[i]))
    for (pos in 1:9) {
      cat(sprintf("  %d. %s\n", pos, results_df[[paste0("Pos", pos)]][i]))
    }
    cat("\n")
  }
  
  # Print the single best lineup cleanly
  cat("==========================================\n")
  cat("OPTIMAL LINEUP:\n")
  for (pos in 1:9) {
    cat(sprintf("  %d. %s\n", pos, results_df[[paste0("Pos", pos)]][1]))
  }
  cat(sprintf("  Expected Runs (6 inn): %.4f\n", results_df$Mean_Runs[1]))
  cat("==========================================\n")
  
  return(invisible(results_df))
}


# =============================================================================
# EXAMPLE USAGE (uncomment to run)
# =============================================================================

# source("reds_monte_carlo_sim.R")   # load simulation functions first
#
# nine_batters <- c(
#   "Elly De La Cruz",
#   "Eugenio Suarez",
#   "Batter 3",
#   "Batter 4",
#   "Batter 5",
#   "Batter 6",
#   "Batter 7",
#   "Batter 8",
#   "Batter 9"
# )
#
 top_lineups <- optimize_lineup(
   batters         = reds_OD_R,
   pitcher_hand    = "R",
   n_sims          = 1000,
   top_n           = 10,
   sim_event_probs = sim_event_probs
 )