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
f_noint <- brmsformula(respP ~ 0 + bvotCond * supervised * vot_s * trial_s +
                         (1 + vot_s | subject),
                       family=bernoulli())

b_logit <- brm(f_noint,
               data = d,
               family = bernoulli(),
               chains=4,
               iter=1000)

saveRDS(b_logit, "brm_logistic.rds")

## this fits v. badly.  I suspect it might have something to do with the minus
## ten condition but I'm really not sure.  the main effects of the conditions
## are sensible except for the -10 one.
##
## I'm looking at the prior and it looks like there's no priors on the betas,
## which leads to _crazy high_ (nonsense) values.  but maybe there's an issue
## with how the priors are set in a non-intercept model?

f_int = brmsformula(respP ~ 1 + bvotCond * supervised * vot_s * trial_s +
                      (1 + vot_s | subject),
                    family=bernoulli())

get_prior(f_int, d)


library(lme4)

g_logit <- glmer(respP ~ 0 + bvotCond * supervised * vot_s * trial_s +
                   (1 + vot_s | subject),
                 data = d,
                 family="bernoulli")



b_logit_exp1 <- brm(respP ~ 0 + bvotCond*vot_s +
                      (1 + vot_s ~ subject),
                    data = filter(d, supCond == "unsupervised"),
                    family = bernoulli(),
                    chains=8, iter=1000)
