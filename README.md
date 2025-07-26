# RLassignment2

# Reinforcement Learning Assignment

## 🧠 Project Overview

This repository contains the full implementation and results of a reinforcement learning project that solves a modified Gridworld using:

- Bellman Expectation and Optimality Equations
- Iterative Policy Evaluation
- Policy Iteration and Value Iteration
- Monte Carlo methods:
  - On-policy (Exploring Starts and ε-soft)
  - Off-policy with importance sampling

The environment is a **5x5 Gridworld** with four special states: Blue (2,5), Red (3,2), Yellow (5,1), and Green (5,5), each with unique rewards and dynamics.

---

## 📁 Directory Structure

```bash
.
├── code/
│   ├── gridworld_setup.R        # Environment and helper functions
│   ├── part1_dp_methods.R       # Bellman solution, policy/value iteration
│   ├── part2_monte_carlo.R      # MC on-policy and off-policy implementation
│   ├── plotting.R               # Visualization helpers (e.g., heatmap, policy arrows)
│   └── main.R                   # Master script to run all parts
├── figures/
│   ├── q11.png                  # Value function heatmap from Bellman
│   ├── policy_es.png            # Policy from exploring starts
│   ├── policy_eps_soft.png      # Policy from ε-soft
│   ├── policy_off_policy.png    # Off-policy learned policy
│   └── ...                      # Other optional plots
├── results/
│   └── policy_tables.txt        # All policy outputs in matrix form
├── report/
│   └── RL_assignment.pdf        # Full LaTeX report with equations, analysis, results
├── README.md                    # You're here
└── rl_assignment.Rproj          # Optional: RStudio project file
