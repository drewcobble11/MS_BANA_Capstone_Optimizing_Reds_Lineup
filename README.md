# Cincinnati Reds Lineup Optimization  
### Monte Carlo Simulation for Run Production

---

## Project Overview  
This project develops a data-driven framework to evaluate and optimize batting lineups for the Cincinnati Reds using Monte Carlo simulation. The goal is to estimate expected run production over the first six innings of a game and identify lineup configurations that maximize offensive output.

The model uses historical play-by-play data to estimate probabilities of key offensive events (e.g., outs, singles, doubles, home runs) for each hitter, incorporating pitcher handedness to better reflect real game conditions.

---

##  Problem Statement  
Lineup construction in baseball is often based on heuristics (e.g., “best hitter bats third”), but these approaches may not maximize run production.

This project answers:
- How does batting order impact expected runs?
- What is the optimal lineup for maximizing early-game scoring?
- How sensitive is performance to pitcher handedness?

---

## Why Monte Carlo Simulation?  
Traditional analytical approaches struggle to capture the sequential and probabilistic nature of baseball innings. A Monte Carlo simulation allows us to:

- Model inning progression dynamically  
- Capture interaction effects between hitters  
- Estimate expected outcomes over thousands of simulated games  
- Evaluate counterfactual scenarios (e.g., lineup changes)

---

##  Repository Structure  

```
/project
│── main.R
│── 01_capstone data cleaning final.R
│── 02_capstone monte carlo script.R
│── 03_optimization script.R
│── data/
│── output/
│── README.md
```
* not sure if I can put the data but it is from StatHead*
---

##  Methodology  

### 1. Data Preparation  
- Extract play-by-play data  
- Standardize player names  
- Merge pitcher handedness  
- Categorize outcomes into event types  

### 2. Event Classification  
Each plate appearance is mapped to one of the following event types:

| Event Type | Description |
|-----------|------------|
| Type 1 | Outs (strikeouts, popouts, etc.) |
| Type 2 | Ground balls (double play potential) |
| Type 3 | Fly balls (sac fly potential) |
| Type 4 | Singles |
| Type 5 | Doubles |
| Type 6 | Triples |
| Type 7 | Home runs |

### 3. Probability Estimation  
For each hitter:
- Calculate probability of each event type  
- Segment by pitcher handedness (L/R)

### 4. Monte Carlo Simulation  
- Simulate thousands of innings  
- Track base states and scoring  
- Estimate expected runs for each lineup  

---

## Key Outputs  

- Expected runs (first 6 innings)  
- Optimal batting order  
- Player-level contribution to scoring  
- Sensitivity to pitcher handedness  

---

##  How to Run  

### Option 1: Full Pipeline  

```r
source("main.R")
```

### Option 2: Step-by-Step  

```r
source("01_capstone data cleaning final.R")
source("02_capstone monte carlo script.R")
source("03_optimization script.R")

```

---

##  Tools & Technologies  

- R (tidyverse, dplyr, ggplot2)  
- Monte Carlo Simulation  
- Data wrangling and probability modeling  

---

##  Business Value / Impact  

Optimizing lineup construction can lead to measurable improvements in run production. Even marginal gains (e.g., +5–10 runs per season) can translate into additional wins, which have significant financial and competitive implications in professional baseball. Watch the presentation please.

---


