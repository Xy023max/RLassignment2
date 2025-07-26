# -------------------------
# Part 1-1: Policy Evaluation with Uniform Random Policy
# -------------------------
library(Matrix)

grid_size <- 5
gamma <- 0.95
n_states <- grid_size^2

coord_to_state <- function(x, y) (y - 1) * grid_size + x

blue_state   <- coord_to_state(2, 5)
red_state    <- coord_to_state(3, 2)
yellow_state <- coord_to_state(5, 1)
green_state  <- coord_to_state(5, 5)

actions <- list(up = c(0, 1), down = c(0, -1), left = c(-1, 0), right = c(1, 0))
action_names <- names(actions)
n_actions <- length(actions)

get_transition <- function(state, action) {
  x <- (state - 1) %% grid_size + 1
  y <- (state - 1) %/% grid_size + 1
  
  if (state == blue_state) {
    return(list(states = red_state, rewards = 5, probs = 1))
  }
  if (state == green_state) {
    return(list(states = c(red_state, yellow_state), rewards = c(2.5, 2.5), probs = c(0.5, 0.5)))
  }
  
  dxdy <- actions[[action]]
  new_x <- x + dxdy[1]
  new_y <- y + dxdy[2]
  
  if (new_x < 1 || new_x > grid_size || new_y < 1 || new_y > grid_size) {
    new_state <- state
    reward <- -0.5
  } else {
    new_state <- coord_to_state(new_x, new_y)
    reward <- 0
  }
  
  list(states = new_state, rewards = reward, probs = 1)
}

P <- Matrix(0, nrow = n_states, ncol = n_states, sparse = TRUE)
R <- numeric(n_states)

for (s in 1:n_states) {
  for (a in action_names) {
    trans <- get_transition(s, a)
    for (i in seq_along(trans$states)) {
      s_prime <- trans$states[i]
      reward <- trans$rewards[i]
      prob <- trans$probs[i] / n_actions
      P[s, s_prime] <- P[s, s_prime] + prob
      R[s] <- R[s] + prob * reward
    }
  }
}

I <- Diagonal(n_states)
V_bellman <- solve(I - gamma * P, R)

V_iter <- rep(0, n_states)
threshold <- 1e-4
delta <- 1

while (delta > threshold) {
  delta <- 0
  V_new <- V_iter
  for (s in 1:n_states) {
    v <- 0
    for (a in action_names) {
      trans <- get_transition(s, a)
      for (i in seq_along(trans$states)) {
        s_prime <- trans$states[i]
        reward <- trans$rewards[i]
        prob <- trans$probs[i] / n_actions
        v <- v + prob * (reward + gamma * V_iter[s_prime])
      }
    }
    delta <- max(delta, abs(V_iter[s] - v))
    V_new[s] <- v
  }
  V_iter <- V_new
}

V_matrix <- matrix(V_iter, nrow = grid_size, byrow = TRUE)

image(t(apply(V_matrix, 2, rev)), main = "State Value Function (Iterative Evaluation)",
      col = heat.colors(20), xlab = "X (Column Index)", ylab = "Y (Row Index)", axes = FALSE)
axis(1, at = seq(0, 1, length.out = grid_size), labels = 1:grid_size)
axis(2, at = seq(0, 1, length.out = grid_size), labels = grid_size:1)

for (i in 1:grid_size) {
  for (j in 1:grid_size) {
    text((i - 0.5) / grid_size, (grid_size - j + 0.5) / grid_size,
         labels = sprintf("%.2f", V_matrix[j, i]), cex = 0.6)
  }
}

cat("Value Function Matrix (Y from top to bottom):\n")
print(round(V_matrix[grid_size:1, ], 2))


# -------------------------
# Part 1-2: Bellman Optimality with Value Iteration and Policy Iteration
# -------------------------
value_iteration <- function(threshold = 1e-4, gamma = 0.95) {
  V <- rep(0, n_states)
  policy <- rep(NA, n_states)
  delta <- Inf
  
  while (delta > threshold) {
    delta <- 0
    V_new <- V
    for (s in 1:n_states) {
      max_v <- -Inf
      best_a <- NA
      for (a in action_names) {
        trans <- get_transition(s, a)
        v <- 0
        for (i in seq_along(trans$states)) {
          s_prime <- trans$states[i]
          reward <- trans$rewards[i]
          prob <- trans$probs[i]
          v <- v + prob * (reward + gamma * V[s_prime])
        }
        if (v > max_v) {
          max_v <- v
          best_a <- a
        }
      }
      V_new[s] <- max_v
      policy[s] <- best_a
      delta <- max(delta, abs(V[s] - V_new[s]))
    }
    V <- V_new
  }
  list(V = V, policy = policy)
}

policy_iteration <- function(gamma = 0.95, threshold = 1e-4) {
  policy <- sample(action_names, n_states, replace = TRUE)
  V <- rep(0, n_states)
  is_stable <- FALSE
  
  while (!is_stable) {
    repeat {
      delta <- 0
      V_new <- V
      for (s in 1:n_states) {
        trans <- get_transition(s, policy[s])
        v <- 0
        for (i in seq_along(trans$states)) {
          s_prime <- trans$states[i]
          reward <- trans$rewards[i]
          prob <- trans$probs[i]
          v <- v + prob * (reward + gamma * V[s_prime])
        }
        delta <- max(delta, abs(V[s] - v))
        V_new[s] <- v
      }
      V <- V_new
      if (delta < threshold) break
    }
    
    is_stable <- TRUE
    for (s in 1:n_states) {
      old_action <- policy[s]
      max_v <- -Inf
      best_a <- NA
      for (a in action_names) {
        trans <- get_transition(s, a)
        v <- 0
        for (i in seq_along(trans$states)) {
          s_prime <- trans$states[i]
          reward <- trans$rewards[i]
          prob <- trans$probs[i]
          v <- v + prob * (reward + gamma * V[s_prime])
        }
        if (v > max_v) {
          max_v <- v
          best_a <- a
        }
      }
      policy[s] <- best_a
      if (old_action != best_a) is_stable <- FALSE
    }
  }
  list(V = V, policy = policy)
}

policy_result <- value_iteration()
symbol_map <- list(up = "↑", down = "↓", left = "←", right = "→")
policy_symbols <- matrix(sapply(policy_result$policy, function(a) symbol_map[[a]]),
                         nrow = grid_size, byrow = TRUE)
print(policy_symbols[grid_size:1, ])


# -------------------------
# Part 2-1: Monte Carlo Control with Exploring Starts and Epsilon-soft Policies
# -------------------------
terminal_coords <- list(c(1,2), c(1,4), c(5,4))
terminal_states <- sapply(terminal_coords, function(x) coord_to_state(x[1], x[2]))

blue_state   <- coord_to_state(2, 5)
red_state    <- coord_to_state(3, 1)
yellow_state <- coord_to_state(5, 1)
green_state  <- coord_to_state(5, 5)

get_transition_mod <- function(state, action) {
  if (state %in% terminal_states) {
    return(list(states = state, rewards = 0, probs = 1, done = TRUE))
  }
  
  x <- (state - 1) %% grid_size + 1
  y <- (state - 1) %/% grid_size + 1
  
  dxdy <- actions[[action]]
  new_x <- x + dxdy[1]
  new_y <- y + dxdy[2]
  
  if (new_x < 1 || new_x > grid_size || new_y < 1 || new_y > grid_size) {
    if (state %in% c(red_state, yellow_state) || !(state %in% c(blue_state, green_state))) {
      return(list(states = state, rewards = -0.5, probs = 1, done = FALSE))
    } else {
      return(list(states = state, rewards = 0, probs = 1, done = FALSE))
    }
  } else {
    new_state <- coord_to_state(new_x, new_y)
    if (state %in% c(red_state, yellow_state) || !(state %in% c(blue_state, green_state))) {
      done <- new_state %in% terminal_states
      return(list(states = new_state, rewards = -0.2, probs = 1, done = done))
    } else {
      done <- new_state %in% terminal_states
      return(list(states = new_state, rewards = 0, probs = 1, done = done))
    }
  }
}

init_Q_returns <- function() {
  Q <- matrix(0, nrow = n_states, ncol = n_actions,
              dimnames = list(1:n_states, action_names))
  Returns_count <- matrix(0, nrow = n_states, ncol = n_actions,
                          dimnames = list(1:n_states, action_names))
  list(Q = Q, Returns_count = Returns_count)
}

generate_episode <- function(pi, max_steps = 1000) {
  possible_starts <- setdiff(1:n_states, terminal_states)
  state <- sample(possible_starts, 1)
  
  episode <- list(states = c(), actions = c(), rewards = c())
  
  done <- FALSE
  steps <- 0
  
  while (!done && steps < max_steps) {
    episode$states <- c(episode$states, state)
    action_prob <- pi(state)
    action <- sample(action_names, 1, prob = action_prob)
    episode$actions <- c(episode$actions, action)
    
    trans <- get_transition_mod(state, action)
    episode$rewards <- c(episode$rewards, trans$rewards)
    
    state <- trans$states
    done <- trans$done
    
    steps <- steps + 1
  }
  episode
}

mc_control_es <- function(n_episodes = 10000, gamma = 0.95) {
  init <- init_Q_returns()
  Q <- init$Q
  Returns_count <- init$Returns_count
  policy <- vector("list", n_states)
  for (s in 1:n_states) policy[[s]] <- rep(1/n_actions, n_actions)
  
  for (episode_i in 1:n_episodes) {
    s_start <- sample(setdiff(1:n_states, terminal_states), 1)
    a_start <- sample(action_names, 1)
    
    pi_episode <- function(s) {
      if (s == s_start) {
        probs <- rep(0, n_actions)
        probs[which(action_names == a_start)] <- 1
        return(probs)
      } else {
        return(policy[[s]])
      }
    }
    
    episode <- generate_episode(pi_episode)
    
    G <- 0
    visited_sa <- list()
    for (t in seq(length(episode$states), 1)) {
      s <- episode$states[t]
      a <- episode$actions[t]
      r <- episode$rewards[t]
      G <- gamma * G + r
      sa <- paste(s,a,sep = "-")
      if (!(sa %in% visited_sa)) {
        Returns_count[s,a] <- Returns_count[s,a] + 1
        Q[s,a] <- Q[s,a] + (G - Q[s,a]) / Returns_count[s,a]
        visited_sa <- c(visited_sa, sa)
        
        best_a <- action_names[which.max(Q[s,])]
        policy[[s]] <- rep(0, n_actions)
        policy[[s]][which(action_names == best_a)] <- 1
      }
    }
  }
  list(Q = Q, policy = policy)
}

mc_control_epsilon_soft <- function(n_episodes = 10000, gamma = 0.95, epsilon = 0.1) {
  init <- init_Q_returns()
  Q <- init$Q
  Returns_count <- init$Returns_count
  
  policy <- vector("list", n_states)
  for (s in 1:n_states) policy[[s]] <- rep(1/n_actions, n_actions)
  
  pi_func <- function(s) policy[[s]]
  
  for (episode_i in 1:n_episodes) {
    episode <- generate_episode(pi_func)
    
    G <- 0
    visited_sa <- list()
    for (t in seq(length(episode$states), 1)) {
      s <- episode$states[t]
      a <- episode$actions[t]
      r <- episode$rewards[t]
      G <- gamma * G + r
      sa <- paste(s,a,sep = "-")
      if (!(sa %in% visited_sa)) {
        Returns_count[s,a] <- Returns_count[s,a] + 1
        Q[s,a] <- Q[s,a] + (G - Q[s,a]) / Returns_count[s,a]
        visited_sa <- c(visited_sa, sa)
        
        best_a <- action_names[which.max(Q[s,])]
        new_policy <- rep(epsilon / n_actions, n_actions)
        new_policy[which(action_names == best_a)] <- 1 - epsilon + epsilon / n_actions
        policy[[s]] <- new_policy
      }
    }
  }
  list(Q = Q, policy = policy)
}

set.seed(42)
result_es <- mc_control_es(n_episodes = 5000)
result_eps <- mc_control_epsilon_soft(n_episodes = 5000, epsilon = 0.1)

symbol_map <- list(up = "↑", down = "↓", left = "←", right = "→")

policy_to_matrix <- function(policy) {
  mat <- matrix("", nrow = grid_size, ncol = grid_size)
  for (s in 1:n_states) {
    action_probs <- policy[[s]]
    best_a <- action_names[which.max(action_probs)]
    x <- (s - 1) %% grid_size + 1
    y <- (s - 1) %/% grid_size + 1
    mat[y, x] <- symbol_map[[best_a]]
  }
  mat <- mat[grid_size:1, ]
  mat
}

cat("Policy from Exploring Starts:\n")
print(policy_to_matrix(result_es$policy))

cat("\nPolicy from Epsilon-soft:\n")
print(policy_to_matrix(result_eps$policy))



# -------------------------
# Part 2-2: Off-policy Monte Carlo Control with Importance Sampling
# -------------------------
# Define the environment
create_env <- function() {
  states <- list()
  for (i in 1:5) {
    for (j in 1:5) {
      states <- append(states, list(c(i,j)))
    }
  }
  special_states <- list(
    red = c(3,2),
    blue = c(2,5),
    yellow = c(5,1),
    green = c(5,5)
  )
  terminal_states <- list(special_states$red, special_states$blue, special_states$yellow, special_states$green)
  return(list(states = states, terminal_states = terminal_states, special = special_states))
}

is_terminal <- function(state, env) {
  for (term in env$terminal_states) {
    if (all(term == state)) return(TRUE)
  }
  return(FALSE)
}

step <- function(state, action, env) {
  directions <- list(
    c(-1,0), # up
    c(1,0),  # down
    c(0,-1), # left
    c(0,1)   # right
  )
  next_state <- pmax(pmin(state + directions[[action]], c(5,5)), c(1,1))
  
  # Rewards
  reward <- -1
  if (all(next_state == env$special$red)) reward <- -10
  if (all(next_state == env$special$blue)) reward <- -5
  if (all(next_state == env$special$yellow)) reward <- 10
  if (all(next_state == env$special$green)) reward <- 20
  
  return(list(next_state = next_state, reward = reward))
}

# Policy helpers
get_behavior_policy <- function() {
  policy <- matrix(list(), nrow = 5, ncol = 5)
  for (i in 1:5) {
    for (j in 1:5) {
      policy[[i,j]] <- rep(0.25, 4)  # 每个方向等概率
    }
  }
  return(policy)
}

get_target_policy <- function() {
  policy <- matrix(sample(1:4, 25, replace = TRUE), nrow = 5, ncol = 5)
  return(policy)
}

# Generate episode using behavior policy
generate_episode <- function(env, policy, exploring_starts = FALSE) {
  if (exploring_starts) {
    repeat {
      state <- sample(env$states, 1)[[1]]
      if (!is_terminal(state, env)) break
    }
    action <- sample(1:4, 1)  # exploring start
  } else {
    state <- sample(env$states, 1)[[1]]
    action <- sample(1:4, 1, prob = policy[[state[1], state[2]]])
  }
  
  episode <- list()
  s <- state
  a <- action
  
  while (!is_terminal(s, env)) {
    result <- step(s, a, env)
    episode <- c(episode, list(list(state = s, action = a, reward = result$reward)))
    s <- result$next_state
    a <- sample(1:4, 1, prob = policy[[s[1], s[2]]])
  }
  
  return(episode)
}

# Off-policy MC control with importance sampling
monte_carlo_off_policy <- function(env, n_episodes = 10000, gamma = 0.95) {
  Q <- array(0, dim = c(5,5,4))  # Q(s,a)
  C <- array(0, dim = c(5,5,4))  # Cumulative weights
  target_policy <- get_target_policy()
  behavior_policy <- get_behavior_policy()
  
  for (ep in 1:n_episodes) {
    episode <- generate_episode(env, behavior_policy, exploring_starts = TRUE)
    G <- 0
    W <- 1
    
    for (t in seq_along(episode)[length(episode):1]) {
      s <- episode[[t]]$state
      a <- episode[[t]]$action
      r <- episode[[t]]$reward
      
      i <- s[1]
      j <- s[2]
      G <- gamma * G + r
      
      C[i,j,a] <- C[i,j,a] + W
      Q[i,j,a] <- Q[i,j,a] + (W / C[i,j,a]) * (G - Q[i,j,a])
      
      target_policy[i,j] <- which.max(Q[i,j,])
      
      if (a != target_policy[i,j]) {
        break
      }
      
      W <- W * (1 / behavior_policy[[i,j]][a])
    }
    if (ep %% 1000 == 0) cat("Episode", ep, "done\n")
  }
  
  return(list(Q = Q, policy = target_policy))
}

# Convert policy to readable matrix
policy_to_matrix <- function(policy) {
  direction <- c("↑", "↓", "←", "→")
  matrix(sapply(policy, function(a) direction[a]), nrow = 5, ncol = 5, byrow = FALSE)
}

# Run everything
env <- create_env()
result_off <- monte_carlo_off_policy(env)
cat("\nOff-policy Optimal Policy:\n")
print(policy_to_matrix(result_off$policy))


