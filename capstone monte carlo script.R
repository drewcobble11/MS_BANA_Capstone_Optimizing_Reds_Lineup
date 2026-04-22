# =============================================================================
# Reds Monte Carlo Baseball Simulation
# Masters Business Analytics Capstone
# =============================================================================
# 
# USAGE:
#   results <- simulate_reds_season(
#     lineup        = c("Elly De La Cruz", "Eugenio Suarez", ...),  # 9 batters
#     pitcher_hand  = "R",       # "L" or "R"
#     n_sims        = 10000,     # number of games to simulate
#     sim_event_probs = sim_event_probs  # your data frame
#   )
#
# RETURNS: A list with:
#   $runs_vector   - numeric vector of runs scored per simulated game
#   $mean_runs     - average runs scored over 6 innings
#   $sd_runs       - standard deviation of runs scored
#   $run_dist      - table of run distribution frequencies
# =============================================================================

# -----------------------------------------------------------------------------
# FIXED PROBABILITY CONSTANTS (adjustable)
# -----------------------------------------------------------------------------
DP_PROB        <- 0.40   # P(double play) on groundout with runner on 1st
TAG_ATTEMPT    <- 0.70   # P(runner on 3rd attempts to tag up on flyout)
TAG_SUCCESS    <- 0.80   # P(tag-up attempt is successful / safe at home)


# -----------------------------------------------------------------------------
# HELPER: Get event probabilities for a single batter vs pitcher hand
# -----------------------------------------------------------------------------
get_batter_probs <- function(batter_name, pitcher_hand, sim_event_probs) {
  df <- sim_event_probs[
    sim_event_probs$batter == batter_name &
      sim_event_probs$pitcher_hand == pitcher_hand,
  ]
  
  if (nrow(df) == 0) {
    stop(paste("No probability data found for batter:", batter_name,
               "vs pitcher hand:", pitcher_hand))
  }
  
  # Return a named vector of probabilities
  probs <- setNames(df$prob, df$sim_event_type)
  
  # Normalize to ensure probabilities sum to 1 (guards against rounding)
  probs <- probs / sum(probs)
  
  return(probs)
}


# -----------------------------------------------------------------------------
# HELPER: Simulate a single plate appearance
# Returns the event type as a character string
# -----------------------------------------------------------------------------
simulate_pa <- function(batter_name, pitcher_hand, sim_event_probs) {
  probs <- get_batter_probs(batter_name, pitcher_hand, sim_event_probs)
  event <- sample(names(probs), size = 1, prob = probs)
  return(event)
}


# -----------------------------------------------------------------------------
# HELPER: Resolve a single plate appearance event given the current base/out state
#
# bases: logical vector of length 3 — c(first, second, third)
# outs:  integer (0, 1, or 2 entering this PA)
#
# Returns a list:
#   $bases  - updated bases (logical vector)
#   $outs   - updated out count
#   $runs   - runs scored on this play (not counting if inning ends)
# -----------------------------------------------------------------------------
resolve_event <- function(event, bases, outs) {
  
  runs  <- 0
  on_first  <- bases[1]
  on_second <- bases[2]
  on_third  <- bases[3]
  
  # ---- STRIKEOUT -----------------------------------------------------------
  if (event == "K") {
    outs <- outs + 1
    # No advancement
  }
  
  # ---- INFIELD OUT ---------------------------------------------------------
  else if (event == "infield_out") {
    outs <- outs + 1
    # No runners advance
  }
  
  # ---- GROUNDOUT -----------------------------------------------------------
  else if (event == "groundout") {
    
    if (on_first) {
      # Possible double play
      if (runif(1) < DP_PROB) {
        # Double play: runner on first and batter both out
        outs    <- outs + 2
        on_first <- FALSE
        # Runner on 2nd advances to 3rd; runner on 3rd scores (if inning not over)
        if (on_third) { runs <- runs + 1; on_third <- FALSE }
        if (on_second) { on_third <- TRUE; on_second <- FALSE }
      } else {
        # Single out (batter); runner on 1st advances to 2nd
        outs     <- outs + 1
        on_second <- TRUE   # runner from 1st
        on_first  <- FALSE
        # Runner on 2nd advances to 3rd
        if (on_second & !on_third) {
          # Note: on_second was just set to TRUE from the runner advancing,
          # so check the *original* on_second state captured above
        }
        # Re-resolve: original runner on 2nd -> 3rd; original runner on 3rd -> scores
        if (on_third) { runs <- runs + 1; on_third <- FALSE }
        # original on_second runner advances to third
        # (on_second already moved to TRUE for the runner from 1st;
        #  need to handle original second-base runner separately)
      }
    } else {
      # No runner on first — just one out, batter is out
      outs <- outs + 1
      # Runner on 2nd advances to 3rd
      if (on_second) {
        on_third  <- TRUE    # may overwrite, but that's fine — runner scores below
        on_second <- FALSE
      }
      # Runner on 3rd scores
      if (on_third & !on_second) {
        # already handled above if originally on second
      }
    }
    
    # Cleaner re-implementation of groundout logic to avoid the above confusion:
    # (Override with a clean block)
    return(resolve_groundout(bases, outs - if(on_first & runif(1) < 0) 2 else 1))
    # ^ placeholder — see dedicated function below
  }
  
  # ---- FLYOUT --------------------------------------------------------------
  else if (event == "flyout") {
    outs <- outs + 1
    # Tag-up opportunity for runner on third (only if not the 3rd out)
    if (on_third && outs <= 3) {
      if (runif(1) < TAG_ATTEMPT) {
        if (runif(1) < TAG_SUCCESS) {
          # Safe — run scores
          runs    <- runs + 1
          on_third <- FALSE
        } else {
          # Thrown out — additional out
          outs    <- outs + 1
          on_third <- FALSE
        }
      }
      # If no attempt, runner stays on third
    }
  }
  
  # ---- SINGLE (1B) ---------------------------------------------------------
  else if (event == "1B") {
    # Runner on 3rd scores
    if (on_third) { runs <- runs + 1; on_third <- FALSE }
    # Runner on 2nd scores
    if (on_second) { runs <- runs + 1; on_second <- FALSE }
    # Runner on 1st advances to 2nd
    if (on_first) { on_second <- TRUE; on_first <- FALSE }
    # Batter to first
    on_first <- TRUE
  }
  
  # ---- DOUBLE (2B) ---------------------------------------------------------
  else if (event == "2B") {
    # All runners score
    if (on_third)  { runs <- runs + 1; on_third  <- FALSE }
    if (on_second) { runs <- runs + 1; on_second <- FALSE }
    if (on_first)  { runs <- runs + 1; on_first  <- FALSE }
    # Batter to second
    on_second <- TRUE
  }
  
  # ---- TRIPLE (3B) ---------------------------------------------------------
  else if (event == "3B") {
    # All runners score
    if (on_third)  { runs <- runs + 1; on_third  <- FALSE }
    if (on_second) { runs <- runs + 1; on_second <- FALSE }
    if (on_first)  { runs <- runs + 1; on_first  <- FALSE }
    # Batter to third
    on_third <- TRUE
  }
  
  # ---- HOME RUN (HR) -------------------------------------------------------
  else if (event == "HR") {
    # Everyone scores, including batter
    runs <- runs + 1 +
      as.integer(on_first) +
      as.integer(on_second) +
      as.integer(on_third)
    on_first  <- FALSE
    on_second <- FALSE
    on_third  <- FALSE
  }
  
  # ---- BB / HBP ------------------------------------------------------------
  else if (event == "BB/HBP") {
    # Batter takes first; force advances only
    if (on_first & on_second & on_third) {
      runs     <- runs + 1   # forced home
      on_third  <- TRUE
      on_second <- TRUE
      on_first  <- TRUE
    } else if (on_first & on_second) {
      on_third  <- TRUE
      on_second <- TRUE
      on_first  <- TRUE
    } else if (on_first) {
      on_second <- TRUE
      on_first  <- TRUE
    } else {
      on_first  <- TRUE
    }
  }
  
  return(list(
    bases = c(on_first, on_second, on_third),
    outs  = outs,
    runs  = runs
  ))
}


# -----------------------------------------------------------------------------
# DEDICATED groundout resolver (clean, replaces the messy inline block above)
# This is the function that is actually called during simulation.
# -----------------------------------------------------------------------------
resolve_groundout <- function(bases, outs_before) {
  
  on_first  <- bases[1]
  on_second <- bases[2]
  on_third  <- bases[3]
  runs      <- 0
  outs      <- outs_before
  
  if (on_first && runif(1) < DP_PROB) {
    # Double play
    outs     <- outs + 2
    on_first <- FALSE
    # Runner on 3rd scores (run only counts if inning not over after this play)
    if (on_third) { runs <- runs + 1; on_third <- FALSE }
    # Runner on 2nd advances to 3rd
    if (on_second) { on_third <- TRUE; on_second <- FALSE }
    
  } else {
    # Single out — batter is out
    outs <- outs + 1
    # Runner on 3rd scores
    if (on_third) { runs <- runs + 1; on_third <- FALSE }
    # Runner on 2nd advances to 3rd
    if (on_second) { on_third <- TRUE; on_second <- FALSE }
    # Runner on 1st advances to 2nd
    if (on_first) { on_second <- TRUE; on_first <- FALSE }
  }
  
  # If the play ended the inning (3 outs), the run on 3rd does NOT count
  if (outs >= 3) {
    runs <- 0
  }
  
  return(list(
    bases = c(on_first, on_second, on_third),
    outs  = min(outs, 3),
    runs  = runs
  ))
}


# -----------------------------------------------------------------------------
# HELPER: Simulate one half-inning, return runs scored
# batter_idx: which batter in the lineup is up first (1-indexed)
# Returns list: $runs, $next_batter_idx
# -----------------------------------------------------------------------------
simulate_half_inning <- function(lineup, pitcher_hand, sim_event_probs,
                                 batter_idx) {
  
  outs  <- 0
  runs  <- 0
  bases <- c(FALSE, FALSE, FALSE)  # first, second, third
  n     <- length(lineup)
  
  while (outs < 3) {
    
    batter <- lineup[batter_idx]
    event  <- simulate_pa(batter, pitcher_hand, sim_event_probs)
    
    # Route to correct resolver
    if (event == "groundout") {
      result <- resolve_groundout(bases, outs)
    } else {
      result <- resolve_event(event, bases, outs)
    }
    
    bases <- result$bases
    outs  <- result$outs
    
    # Only add runs if inning is not over (3-out rule for force plays)
    if (outs < 3) {
      runs <- runs + result$runs
    } else {
      # For non-groundout events, runs scored before the final out DO count
      # (groundout handles this internally); re-add if not a groundout
      if (event != "groundout") {
        runs <- runs + result$runs
      }
    }
    
    # Cycle through batting order
    batter_idx <- (batter_idx %% n) + 1
  }
  
  return(list(runs = runs, next_batter_idx = batter_idx))
}


# -----------------------------------------------------------------------------
# MAIN: Simulate one full game (6 half-innings for the Reds)
# -----------------------------------------------------------------------------
simulate_one_game <- function(lineup, pitcher_hand, sim_event_probs) {
  
  total_runs <- 0
  batter_idx <- 1  # leadoff batter starts
  
  for (inning in 1:6) {
    result     <- simulate_half_inning(lineup, pitcher_hand,
                                       sim_event_probs, batter_idx)
    total_runs <- total_runs + result$runs
    batter_idx <- result$next_batter_idx  # lineup carries over between innings
  }
  
  return(total_runs)
}


# -----------------------------------------------------------------------------
# MAIN EXPORTED FUNCTION: Run the Monte Carlo simulation
#
# Arguments:
#   lineup          - character vector of 9 batter names (batting order)
#   pitcher_hand    - "L" or "R"
#   n_sims          - number of games to simulate
#   sim_event_probs - data frame with columns: batter, pitcher_hand,
#                     sim_event_type, count, prob
#
# Returns a list with summary statistics and the full runs vector
# -----------------------------------------------------------------------------
simulate_reds_season <- function(lineup,
                                 pitcher_hand,
                                 n_sims,
                                 sim_event_probs) {
  
  # Input validation
  if (length(lineup) != 9) {
    stop("lineup must contain exactly 9 batters.")
  }
  if (!pitcher_hand %in% c("L", "R")) {
    stop("pitcher_hand must be 'L' or 'R'.")
  }
  if (n_sims < 1) {
    stop("n_sims must be a positive integer.")
  }
  
  # Check all batters have data
  for (b in lineup) {
    get_batter_probs(b, pitcher_hand, sim_event_probs)  # will error if missing
  }
  
  cat("Running", n_sims, "simulated games...\n")
  
  # Run simulations
  runs_vector <- vapply(
    seq_len(n_sims),
    function(i) simulate_one_game(lineup, pitcher_hand, sim_event_probs),
    numeric(1)
  )
  
  cat("Simulation complete.\n\n")
  
  # Summary output
  mean_runs <- mean(runs_vector)
  sd_runs   <- sd(runs_vector)
  run_dist  <- table(runs_vector)
  
  cat("===== SIMULATION RESULTS (6 Innings) =====\n")
  cat(sprintf("  Games Simulated : %d\n",   n_sims))
  cat(sprintf("  Mean Runs       : %.3f\n", mean_runs))
  cat(sprintf("  Std Dev Runs    : %.3f\n", sd_runs))
  cat(sprintf("  Min Runs        : %d\n",   min(runs_vector)))
  cat(sprintf("  Max Runs        : %d\n",   max(runs_vector)))
  cat(sprintf("  Median Runs     : %.1f\n", median(runs_vector)))
  cat("\nRun Distribution:\n")
  print(run_dist)
  
  return(invisible(list(
    runs_vector = runs_vector,
    mean_runs   = mean_runs,
    sd_runs     = sd_runs,
    run_dist    = run_dist
  )))
}


# =============================================================================
# EXAMPLE USAGE (uncomment to run)
# =============================================================================

Reds_OD_L <- c(
   "TJ Friedl",
   "Matt Mclain",
   "Elly De La Cruz",
   "Sal Stewart",
   "Eugenio Saurez",
   "Spencer Steer",
   "Tyler Stephenson",
   "Noelvi Marte",
   "Ke'Bryan Hayes"
 )

optimal_lefty1 <- c(
  "Tyler Stephenson",
  "Sal Stewart",
  "Eugenio Saurez",
  "Spencer Steer",
  "Matt Mclain",
  "Ke'Bryan Hayes",
  "TJ Friedl",
  "Elly De La Cruz",
  "Noelvi Marte"
)

optimal_lefty2 <- c(
  "Matt Mclain",
  "TJ Friedl",
  "Tyler Stephenson",
  "Sal Stewart",
  "Eugenio Saurez",
  "Ke'Bryan Hayes",
  "Elly De La Cruz",
  "Spencer Steer",
  "Noelvi Marte"
)

optimal_lefty3 <- c(
  "Matt Mclain",
  "TJ Friedl",
  "Tyler Stephenson",
  "Sal Stewart",
  "Eugenio Saurez",
  "Ke'Bryan Hayes",
  "Spencer Steer",
  "Elly De La Cruz",
  "Noelvi Marte"
)

optimal_lefty4 <- c(
  "Eugenio Saurez",
  "Tyler Stephenson",
  "Matt Mclain",
  "Spencer Steer",
  "Sal Stewart",
  "Elly De La Cruz",
  "TJ Friedl",
  "Noelvi Marte",
  "Ke'Bryan Hayes"
)

optimal_lefty1 <- c(
  "Tyler Stephenson",
  "Sal Stewart",
  "Eugenio Saurez",
  "TJ Friedl",
  "Matt Mclain",
  "Spencer Steer",
  "Noelvi Marte",
  "Ke'Bryan Hayes",
  "Elly De La Cruz"
)

results <- simulate_reds_season(
   lineup          = reds_OD_R,
   pitcher_hand    = "R",
   n_sims          = 10000,
   sim_event_probs = sim_event_probs
 )

reds_OD_R <- c(
  "TJ Friedl",
  "Matt Mclain",
  "Elly De La Cruz",
  "Sal Stewart",
  "Eugenio Saurez",
  "Spencer Steer",
  "Will Benson",
  "Jose Trevino",
  "Ke'Bryan Hayes"
)



# # Plot run distribution
 hist(results$runs_vector,
      breaks  = 0:max(results$runs_vector),
      main    = "Reds Simulated Runs Scored (6 Innings)",
      xlab    = "Runs Scored",
      ylab    = "Frequency",
      col     = "red",
      border  = "darkred",
      right   = FALSE)
 abline(v = results$mean_runs, col = "black", lwd = 2, lty = 2)
 legend("topright", legend = paste("Mean =", round(results$mean_runs, 2)),
        lty = 2, lwd = 2)