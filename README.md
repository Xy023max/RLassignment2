# Reinforcement Learning Assignment

This project implements a reinforcement learning solution to a modified 5x5 Gridworld problem using R.

## 🧠 Overview

The solution includes:

- Dynamic programming methods:
  - Bellman expectation equations
  - Policy iteration
  - Value iteration
- Monte Carlo methods:
  - On-policy (with Exploring Starts and ε-soft policies)
  - Off-policy with importance sampling

The environment contains four special grid states:  
- Blue (2,5), Red (3,2), Yellow (5,1), Green (5,5),  
each with custom rewards and dynamics.

## 📄 Files

- `rla2.R`: Full R script with all methods and experiments.
- `RL_assignment.pdf`: Final report with explanations, results, and visualizations.

## 📌 Requirements

- R 4.0+
- Recommended packages: `ggplot2`, `reshape2`, `gridExtra` (optional for plots)

## 🔧 How to Run

Open `rla2.R` in RStudio and run it section by section or source the whole file.  
Make sure the working directory is set to the folder containing the script.

---

Feel free to fork or adapt for your own experiments!
