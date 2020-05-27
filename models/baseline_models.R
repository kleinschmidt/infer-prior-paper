library(assertthat)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(purrrlyr)
library(stringr)
library(lme4)
library(ggplot2)
library(forcats)

library(rstan)
library(tidybayes)
options(mc.cores = parallel::detectCores())

library(loo)

## devtools::install_github('kleinschmidt/daver')
library(daver)

## devtools::install_github('kleinschmidt/phonetic-sup-unsup')
library(supunsup)
## devtools::install_github('kleinschmidt/beliefupdatr')
library(beliefupdatr)




data_exp1 <- supunsup::supunsup_clean %>%
  filter(supCond == 'unsupervised') %>%
  mutate(trueCat = respCategory,
         subjNum = as.numeric(factor(subject)),
         trueCatNum = as.numeric(trueCat),
         respCatNum = as.numeric(respCat))

conditions_exp1 <-
  data_exp1 %>%
  group_by(bvotCond, trueCat) %>%
  summarise(mean_vot = mean(vot)) %>%
  spread(trueCat, mean_vot) %>%
  transmute(vot_cond = paste(b, p, sep=', '),
            ideal_boundary = (b+p)/2) %>%
  ungroup() %>%
  mutate(vot_cond = factor(vot_cond, levels=vot_cond),
         cond_num = as.numeric(bvotCond))

data_exp1 %<>% left_join(conditions_exp1)

## subject reducer mat: convert trial log lik to subject log lik
subj_reducer <-
  map(unique(data_exp1_mod$subject), ~ data_exp1_mod$subject == .x) %>%
  lift(cbind)(.)


### comparing with a GLM/logit model:

data_exp1_mod <- data_exp1 %>%
  ungroup() %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial),
         block = as.factor(ntile(trial, 6)))

library(brms)

glm_logit = brm(respP ~ 1 + bvotCond * vot_s * trial_s, data = data_exp1_mod, family=bernoulli(), chains=4, iter=4000)

saveRDS(glm_logit, "expt1_glm_logit.rds")

ll_glm <- log_lik(glm_logit, )

ll_glm_bysub <- ll_glm * subj_reducer

loo_glm_bysub <- loo(ll_glm_bysub)

loo_compare(loo_bernoulli, loo_glm_bysub)

# pareto k param is high for GLM, maybe because of lapsing?  what if I try the
# mixture model for a more robust fit?

glm_logit_lapsing <-
  brm(bf(respP ~ 1,
         mu1 ~ 1 + vot_cond * vot_s * trial_s,
         mu2 ~ 1),
      family = mixture(bernoulli(), bernoulli()),
      data = data_exp1_mod,
      chains = 4, iter = 1000)

saveRDS(glm_logit_lapsing, "expt1_glm_logit_lapsing.rds")

ll_glm_lapsing <- log_lik(glm_logit_lapsing, )

ll_glm_lapsing_bysub <- ll_glm_lapsing %*% subj_reducer

loo_glm_lapsing_bysub <- loo(ll_glm_lapsing_bysub)

print(loo_compare(loo_bernoulli, loo_glm_lapsing_bysub, loo_glm_bysub), simplify=FALSE)

# no even worse: some are over 1 now.  oh well.  the comparison is closer too
# but that's not surprising.
#
# okay but what's the point of all this?  to show that the belief updating model
# fits well...and this is an extremely stringent baseline to compare with.  a
# basic test is just that it fits better than some null model like a single
# classification function with no learning, or even just a "constant
# probability" model.

glm_logit_byblock <-
  brm(respP ~ 1 + bvotCond * vot_s * block,
      data = data_exp1_mod,
      family=bernoulli(),
      chains=4, iter=1000)

saveRDS(glm_logit_byblock, "expt1_glm_logit_byblock.rds")

# Rhats are way high for this model... 
glm_logit_byblock_lapsing <-
  brm(bf(respP ~ 1,
         mu1 ~ 1 + bvotCond * vot_s * block,
         mu2 ~ 1,
         theta2 ~ 1 + block),
      family = mixture(bernoulli(), bernoulli()),
      data = data_exp1_mod,
      chains=4, iter=1000)

saveRDS(glm_logit_byblock_lapsing, "expt1_glm_logit_byblock_lapsing.rds")

# let's try linear trial + lapsing by block
glm_logit_lapsing_variable <-
  brm(bf(respP ~ 1,
         mu1 ~ 1 + bvotCond * vot_s * trial_s,
         mu2 ~ 1,
         theta2 ~ 1 + block),
      family = mixture(bernoulli(), bernoulli()),
      data = data_exp1_mod,
      chains=4, iter=1000)
saveRDS(glm_logit_lapsing_variable, "expt1_glm_logit_lapsing_variable.rds")
## much better

ll_glm_lapsing_var <- log_lik(glm_logit_lapsing_variable, )

ll_glm_lapsing_var_bysub <- ll_glm_lapsing_var %*% subj_reducer

loo_glm_lapsing_var_bysub <- loo(ll_glm_lapsing_var_bysub)

# intercept-only (null model) baseline
glm_intercept_only <-
  brm(respP ~ 1, family=bernoulli(), data = data_exp1_mod, chains=4, iter=1000)

saveRDS(glm_intercept_only, "expt1_glm_intercept_only.rds")

ll_glm_intercept_only <- log_lik(glm_intercept_only, )

ll_glm_intercept_only_mat <- ll_glm_intercept_only %*% subj_reducer

(loo_glm_intercept_only <- loo(ll_glm_intercept_only_mat))


# compare LL for subjects across 
ll_by_sub_draw %>%
  select(.draw, loglik = loglik_bernoulli) %>%
  spread(.draw, loglik) %>%
  right_join(tibble(subject = unique(data_exp1_mod$subject))) %>%
  ungroup() %>%
  select(-subject) %>%
  as.matrix() %>%
  t() %>%
  apply(2, mean) %>%
  plot(apply(ll_glm_bysub,2,mean))

