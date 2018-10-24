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
         


# this is _extremely slow. 
b_logit <- brm(respP ~ 0 + bvotCond * supervised * vot_s * trial_s +
                 (1 + vot_s | subject),
               data = d,
               family = bernoulli(),
               chains=4,
               iter=1000)

saveRDS(b_logit, "brm_logistic.rds")


b_logit_exp1 <- brm(respP ~ 0 + bvotCond*vot_s +
                      (1 + vot_s ~ subject),
                    data = filter(d, supCond == "unsupervised"),
                    family = bernoulli(),
                    chains=8, iter=1000)
