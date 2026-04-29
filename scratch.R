forest_data %>%
  ggplot(aes(x = tree_density, y = nest_success)) +
  geom_smooth(method = "lm", se = T, color = "blue") +
  geom_point(size = 3) +
  theme_bw()

library(tidyverse)

library(cowplot)



# predictor range for plotting

tree_seq <- seq(10, 50, length.out = 200)



prior_pred <- function(n_draws, prior_intercept, prior_slope, prior_sigma) {



  # draw parameters from the prior

  a <- rnorm(n_draws, mean = prior_intercept[1], sd = prior_intercept[2])

  b <- rnorm(n_draws, mean = prior_slope[1], sd = prior_slope[2])

  sigma <- rexp(n_draws, rate = 1 / prior_sigma)  # exponential prior on sigma



  # generate predicted lines

  tibble(draw = rep(1:n_draws, each = length(tree_seq)),

         tree_density = rep(tree_seq, times = n_draws),

         nest_success = a[draw] + b[draw] * tree_density)

}



pp_weak <- prior_pred(

  n_draws = 100,

  prior_intercept = c(0, 5),

  prior_slope     = c(0.4, 0.1),

  prior_sigma     = 5

)



pp_skeptical <- prior_pred(

  n_draws = 100,

  prior_intercept = c(0, 5),

  prior_slope     = c(0, 0.01),

  prior_sigma     = 5

)



pp_flat <- prior_pred(

  n_draws = 100,

  prior_intercept = c(0, 100),

  prior_slope     = c(0, 100),

  prior_sigma     = 20

)



plot_prior <- function(df, title){

  ggplot(df, aes(tree_density, nest_success, group = draw)) +

    geom_line(alpha = 0.2, color = "steelblue") +

    labs(title = title,

         x = "Tree density",

         y = "Predicted nest success (prior only)") +

    theme_minimal()

}



plot_grid(plot_prior(pp_weak,"Weakly Informative Prior"),

          plot_prior(pp_skeptical, "Skeptical Prior"),

          plot_prior(pp_flat,      "Flat Prior"), nrow = 1)

########
bmodel.p.weak <- brm(nest_success ~ tree_density,
                     data = forest_data,
                     prior = pp_weak,
                     iter = 2000, warmup = 1000, chains = 4, cores = 4,
                     seed = 42,
                     silent = 2)

########
library(brms)

## define priors
prior <- c(set_prior("normal(0,2)", class = "b"),
           set_prior("student_t(10,0,1)", class = "sigma"),
           set_prior("student_t(10,0,1)", class = "sd"))

## fit a linear mixed effects models
fit <- brm(time ~ age + sex + disease + (1 + age|patient),
           data = kidney, family = lognormal(),
           prior = prior, sample_prior = "yes",
           control = list(adapt_delta = 0.95))

## perform two-sided hypothesis testing
# test if effect of being female is equal to sum of the age & PKD effects
(hyp1 <- hypothesis(fit, "sexfemale = age + diseasePKD"))
plot(hyp1)
(hyp2 <- hypothesis(fit, "exp(age) - 3 = 0", alpha = 0.01))
plot(hyp2)

## perform one-sided hypothesis testing
# 1-sided test of prob the combined effect of the 2 diseases is less than 3
hypothesis(fit, "diseasePKD + diseaseGN - 3 < 0")

# test if variation btwn patients in their age-slopes is smaller than
# variation btwn patients in their starting points
hypothesis(fit, "age < Intercept",
           class = "sd", group  = "patient")

## test the amount of random intercept variance on all variance
h <- paste("sd_patient__Intercept^2 / (sd_patient__Intercept^2 +",
           "sd_patient__age^2 + sigma^2) = 0")
(hyp2 <- hypothesis(fit, h, class = NULL))
plot(hyp2)

## test more than one hypothesis at once
h <- c("diseaseGN = diseaseAN", "2 * diseaseGN - diseasePKD = 0")
(hyp3 <- hypothesis(fit, h))
plot(hyp3, ignore_prior = TRUE)

## compute hypotheses for all levels of a grouping factor
hypothesis(fit, "age = 0", scope = "coef", group = "patient")

## use the default method
dat <- as.data.frame(fit)
str(dat)
hypothesis(dat, "b_age > 0")
