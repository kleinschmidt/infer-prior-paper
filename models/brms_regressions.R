################################################################################
# bayesian logistic regression analyses

library(tidyverse)
library(forcats)
library(brms)
options(mc.cores = parallel::detectCores())

library(supunsup)

d <- supunsup::supunsup_clean %>%
  filter(labeled == "unlabeled") %>%
  select(subject, supCond, labeled, bvotCond, trial, vot, respP) %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial),
         supervised = fct_recode(supCond, supervised="mixed"))
         


# this is _extremely slow_
b_logit <- brm(respP ~ 1 + bvotCond*supervised*vot_s +
                 (1 + vot_s ~ subject),
               data = d,
               family = bernoulli(),
               algorithm="fullrank")


b_logit_exp1 <- brm(respP ~ 0 + bvotCond*vot_s +
                      (1 + vot_s ~ subject),
                    data = filter(d, supCond == "unsupervised"),
                    family = bernoulli(),
                    chains=8, iter=1000)
