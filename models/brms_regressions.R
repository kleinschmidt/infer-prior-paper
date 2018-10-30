################################################################################
# bayesian logistic regression analyses

library(tidyverse)
library(forcats)
library(brms)
library(tidybayes)
options(mc.cores = parallel::detectCores())

library(supunsup)

d <- supunsup::supunsup_clean %>%
  filter(labeled == "unlabeled", bvotCond != "-10") %>%
  select(subject, supCond, labeled, bvotCond, trial, vot, respP) %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial),
         supervised = fct_recode(supCond, supervised="mixed"))
         


# this is _extremely slow.
f_noint <- respP ~ 0 + bvotCond * supervised * vot_s * trial_s +
  (1 + vot_s | subject)


b_logit <- brm(f_noint,
               data = d,
               family = bernoulli(),
               chains=4,
               iter=1000)

saveRDS(b_logit, "brm_logistic.rds")

b_logit <- readRDS("brm_logistic.rds")

## this fits v. badly.  I suspect it might have something to do with the minus
## ten condition but I'm really not sure.  the main effects of the conditions
## are sensible except for the -10 one.
##
## I'm looking at the prior and it looks like there's no priors on the betas,
## which leads to _crazy high_ (nonsense) values.  but maybe there's an issue
## with how the priors are set in a non-intercept model?
##
## oh FUCK the issue that there's no bvotCond=-10 in the supervised group....
## once that's taken care of, all good.

library(lme4)

# doesn't converge so fuck it
g_logit <- glmer(f_noint,
                 data = d,
                 family = "binomial")

