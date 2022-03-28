set.seed(5678)

library(cbcTools)
library(idefix)
library(logitr)
library(jph)
library(tidyverse)
library(fastDummies)

# Define the attributes and levels
levels <- list(
  brand = c("GM", "BMW", "Ferrari"),
  price = c("20k", "40k", "100k")
)

profiles <- cbc_profiles(levels)

design <- cbc_design(
  profiles = profiles,
  n_resp   = 3, # Number of respondents
  n_alts   = 3, # Number of alternatives per question
  n_q      = 3  # Number of questions per respondent
)

cbc_balance(design)
cbc_overlap(design)

design <- cbc_design(
  profiles = profiles,
  n_resp   = 30, # Number of respondents
  n_alts   = 3, # Number of alternatives per question
  n_q      = 3  # Number of questions per respondent
)

cbc_balance(design)
cbc_overlap(design)

# Make randomized design ----

design <- cbc_design(
  profiles = profiles,
  n_resp   = 1200, # Number of respondents
  n_alts   = 3, # Number of alternatives per question
  n_q      = 3  # Number of questions per respondent
)

# Make D-efficient design with priors using {idefix} ----
cs <- Profiles(lvls = c(3, 3), coding = c("D", "D"))
mu <- c(1, 2, -1, -4)
sigma <- diag(length(mu))
M <- MASS::mvrnorm(n = 500, mu = mu, Sigma = sigma)
D <- Modfed(
  cand.set = cs, 
  n.sets = 24,
  n.alts = 3, 
  alt.cte = c(0, 0, 0), 
  par.draws = M, 
  max.iter = 25,
  n.start = 5
)
DD <- Decode(des = D$design, lvl.names = levels, coding = c("D", "D"), n.alts = 3)
design_deff <- DD$design
# Repeat rows to match size of randomized design
n <- nrow(design) / nrow(design_deff)
design_deff <- design_deff[rep(seq_len(nrow(design_deff)), n), ]
row.names(design_deff) <- NULL
names(design_deff) <- c("brand", "price")
design_deff$brand <- factor(design_deff$brand, levels$brand)
design_deff$price <- factor(design_deff$price, levels$price)
design_deff$respID <- design$respID
design_deff$qID <- design$qID
design_deff$altID <- design$altID
design_deff$obsID <- design$obsID

# Compare balance

cbc_balance(design)
cbc_balance(design_deff)

# Sim data without interaction ----

data <- cbc_choices(
  design = design,
  obsID = "obsID",
  priors = list(
    price = c(-1, -4),
    brand = c(1, 2)
  )
)

data_deff <- cbc_choices(
  design = design_deff,
  obsID = "obsID",
  priors = list(
    price = c(-1, -4),
    brand = c(1, 2)
  )
)

# Sim data with interaction ----

data_int <- cbc_choices(
  design = design,
  obsID = "obsID",
  priors = list(
    price = c(-1, -4),
    brand = c(1, 2), 
    `price*brand` = c(0.25, 0.5, 0.5, 1)
  )
)

data_int_deff <- cbc_choices(
  design = design_deff,
  obsID = "obsID",
  priors = list(
    price = c(-1, -4),
    brand = c(1, 2), 
    `price*brand` = c(0.25, 0.5, 0.5, 1)
  )
)

# Power analysis on a design ----

# Estimate models with different sample sizes
results <- cbc_power(
  nbreaks = 10,
  n_q     = 3,
  data    = data,
  pars    = c("price", "brand"),
  outcome = "choice",
  obsID   = "obsID"
)

results_deff <- cbc_power(
  nbreaks = 10,
  n_q     = 3,
  data    = data_deff,
  pars    = c("price", "brand"),
  outcome = "choice",
  obsID   = "obsID"
)

results_int <- cbc_power(
  nbreaks = 10,
  n_q     = 3,
  data    = data_int,
  pars    = c("price", "brand", "price*brand"),
  outcome = "choice",
  obsID   = "obsID"
)

results_int_deff <- cbc_power(
  nbreaks = 10,
  n_q     = 3,
  data    = data_int_deff,
  pars    = c("price", "brand", "price*brand"),
  outcome = "choice",
  obsID   = "obsID"
)

# Visualize

results %>% 
  mutate(type = "Random") %>% 
  rbind(results_deff %>% mutate(type = "D-efficient")) %>% 
  ggplot() +
  geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
  geom_point(
    aes(x = sampleSize, y = se, color = type),
    size = 1.8
  ) +
  facet_wrap(vars(coef), nrow = 1) +
  scale_x_continuous(limits = c(0, 1250)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  expand_limits(y = 0) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    color = "Design",
    x = "Sample size",
    y = "Standard error"
  )

ggsave(file.path("images", "design_compare.png"), width = 12, height = 3.5)

results_int %>% 
  mutate(type = "Random") %>% 
  rbind(results_deff %>% mutate(type = "D-efficient")) %>%
  mutate(
    coef = fct_relevel(coef, c(
      "price40k", "price100k", "brandBMW", "brandFerrari",
      "price40k:brandBMW", "price100k:brandBMW",
      "price40k:brandFerrari", "price100k:brandFerrari"))) %>% 
  ggplot() +
  geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
  geom_point(
    aes(x = sampleSize, y = se, color = type),
    size = 1.8
  ) +
  facet_wrap(vars(coef), nrow = 2) +
  scale_x_continuous(limits = c(0, 1250)) +
  scale_y_continuous(limits = c(0, 1)) +
  expand_limits(y = 0) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    color = "Design",
    x = "Sample size",
    y = "Standard error"
  )

ggsave(file.path("images", "design_compare_int.png"), width = 12, height = 6)
