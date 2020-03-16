################################################################################
# paths are relative to this script (e.g., models/ subdir of repo)

################################################################################
# bayesian logistic regression analyses

library(tidyverse)
library(glue)
library(magrittr)
library(forcats)
library(brms)
library(tidybayes)
options(mc.cores = parallel::detectCores())

library(supunsup)

library(ggbeeswarm)

## library(Cairo)
## CairoX11(dpi=216)

################################################################################
# expt 1

d1 <- supunsup::supunsup_clean %>%
  filter(supCond == "unsupervised") %>%
  select(subject, bvotCond, trial, vot, respP) %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial))

f <- respP ~ 1 + bvotCond * vot_s * trial_s + (1 + vot_s | subject)

# b_logit_exp1 <- brm(f, data=d1, family=bernoulli(), chains=4, iter=1000)
# saveRDS(b_logit_exp1, "brm_logistic_exp1.rds")
b_logit_exp1 <- readRDS("brm_logistic_exp1.rds")

## extract boundaries

fits <- fitted(b_logit_exp1) %>% as.tibble() %>% bind_cols(d1)

fits_avg <- fitted(b_logit_exp1, re_formula = NA) %>% as.tibble() %>% bind_cols(d1)

ggplot(fits, aes(x=vot_s, y=Estimate, group=subject)) +
  geom_line(stat="summary", fun.y=mean) +
  facet_grid(ntile(trial, 6) ~ bvotCond)

d1_blocks <-
  d1 %>%
  group_by(block=ntile(trial, 3)) %>%
  summarise_at(vars(trial, trial_s), mean)

data_pred <-
  cross_df(list(vot_s = seq(min(d1$vot_s), max(d1$vot_s), length.out=100),
                bvotCond = unique(d1$bvotCond),
                block = 1:3)) %>%
  left_join(d1_blocks) %>%
  mutate(vot = vot_s * sd(d1$vot) + mean(d1$vot))

data_pred_subj <-
  d1 %>%
  group_by(subject, bvotCond) %>%
  summarise() %>%
  left_join(data_pred, by="bvotCond")

expt1_bounds <-
  fitted(b_logit_exp1, newdata=data_pred, re_formula=NA) %>%
  as_tibble() %>%
  bind_cols(data_pred)

expt1_bounds_bysub <-
  fitted(b_logit_exp1, newdata=data_pred_subj) %>%
  as_tibble() %>%
  bind_cols(data_pred_subj)
  
  
ggplot(expt1_bounds,
       aes(x=vot, y=Estimate, ymin=`Q2.5`, ymax=`Q97.5`,
           color=bvotCond, fill=bvotCond)) +
  geom_ribbon(alpha=0.1, color=NA) +
  geom_line() +
  facet_grid(.~trial)


ggplot(mapping=aes(x=vot, y=Estimate, color=bvotCond, fill=bvotCond)) +
  geom_line(data=expt1_bounds_bysub %>% filter(block==3),
            aes(group=subject), alpha=0.2) +
  facet_grid(.~bvotCond)

expt1_bounds %>%
  filter(block == 3) %>%
  ggplot(aes(x=vot, y=Estimate, color=bvotCond, fill=bvotCond)) +
  geom_line(data=expt1_bounds_bysub %>% filter(block==3),
            aes(group=subject), alpha=0.2) +
  geom_ribbon(aes(fill=bvotCond, ymin=`Q2.5`, ymax=`Q97.5`), alpha=0, linetype=2,
              show.legend=FALSE) +
  geom_line(size=1) +
  facet_grid(.~bvotCond)


expt1_bounds_bysub %>%
  group_by(subject, block) %>%
  filter(abs(Estimate - 0.5) == min(abs(Estimate - 0.5))) %>%
  ggplot(aes(x=bvotCond, y=vot, fill=bvotCond)) +
  geom_violin() +
  facet_grid(.~block)

expt1_bounds_bysub %>%
  group_by(subject, block) %>%
  filter(abs(Estimate - 0.5) == min(abs(Estimate - 0.5))) %>%
  ggplot(aes(x=bvotCond, y=vot, fill=bvotCond, color=bvotCond)) +
  geom_violin(alpha=0.2) +
  geom_beeswarm(cex=2) +
  facet_grid(.~block)

# estimate uncertainty of fixed effects boundaries:
exp1_bounds_fixef <-
  tidybayes::linpred_draws(b_logit_exp1, data_pred, re_formula=NA) %>%
  group_by(bvotCond, block, trial, .draw) %>%
  arrange((.value - 0.5)^2) %>%         # sort by distance from 0.5
  filter(row_number() == 1) %>%         # take the closest to 0.5
  group_by(bvotCond, block, trial) %>%
  summarise(low=quantile(vot, 0.025), high=quantile(vot, 0.975), mean=mean(vot))

# sorta re-creates the original figure
expt1_bounds_bysub %>%
  group_by(subject, block) %>%
  filter(abs(Estimate - 0.5) == min(abs(Estimate - 0.5))) %>%
  ## filter(block==3) %>%
  ggplot(aes(x=bvotCond, y=vot, fill=bvotCond, color=bvotCond)) +
  geom_violin(alpha=0.5, color=NA) +
  ## geom_beeswarm(cex=2) +
  geom_pointrange(data = exp1_bounds_fixef,
                  aes(y=mean, ymin=low, ymax=high),
                  color="white", show.legend=FALSE) +
  facet_grid(block ~ .) +
  coord_flip()



################################################################################
# expt 1 vs 2: effect of supervision

d2 <- supunsup::supunsup_clean %>%
  filter(labeled == "unlabeled", bvotCond != "-10") %>%
  select(subject, supCond, labeled, bvotCond, trial, vot, respP) %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial),
         supervised = fct_recode(supCond, supervised="mixed"))

f_noint <- respP ~ 0 + bvotCond * supervised * vot_s * trial_s +
  (1 + vot_s | subject)

## b_logit_sup_v_unsup <- brm(f_noint,
##                data = d2,
##                family = bernoulli(),
##                chains=4,
##                iter=1000)

## saveRDS(b_logit_sup_v_unsup, "brm_logistic_sup_v_unsup.rds")

b_logit_sup_v_unsup <- readRDS("brm_logistic_sup_v_unsup.rds")


################################################################################
# expt 1 vs 2: with intercept, and prior so we can do bayes factors

f2_int <- respP ~ bvotCond * supervised * vot_s * trial_s +
  (1 + vot_s | subject)

beta_prior <- set_prior("student_t(3, 0, 1)", class="b")

make_stancode(f2_int, d2, family=bernoulli(), prior=beta_prior)

## b_logit_sup_v_unsup_w_prior <- brm(f2_int,
##                                    data = d2,
##                                    family = bernoulli(),
##                                    chains = 4,
##                                    prior = beta_prior,
##                                    iter = 1000,
##                                    sample_prior = "yes")

## saveRDS(b_logit_sup_v_unsup_w_prior, "b_logit_sup_v_unsup_w_prior.rds")

b_logit_sup_v_unsup_w_prior <- readRDS("b_logit_sup_v_unsup_w_prior.rds")

hyps_sup <- b_logit_sup_v_unsup_w_prior %>%
  fixef() %>%
  rownames() %>%
  purrr::keep(~ str_detect(., "supervised")) %>%
  glue("{x} = 0", x=.)

hyps_sup_test <- hypothesis(b_logit_sup_v_unsup_w_prior, hypothesis=hyps_sup)

hyps_rest <- b_logit_sup_v_unsup_w_prior %>%
  fixef() %>%
  rownames() %>%
  purrr::keep(~ !str_detect(., "supervised")) %>%
  glue("{x} = 0", x=.)

hypothesis(b_logit_sup_v_unsup_w_prior, hypothesis=hyps_rest)

################################################################################
# non-linear trial effects

data_exp1_mod <-
  data_exp1 %>%
  select(subject, bvotCond, trial, vot, respP) %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial))


b_logit_s_exp1 <- brm(
  bf(respP ~ 1 + bvotCond * vot_s + s(trial_s, by=bvotCond) + (1 + vot_s | subject)),
  data = data_exp1_mod, family=bernoulli(),
  chains=4, iter=100
)

saveRDS(b_logit_s_exp1, "models/brm_logistic_smooth_exp1.rds")


plot(marginal_smooths(b_logit_s_exp1))



# from manuscript:

exp1_blocks <-
  data_exp1_mod %>%
  group_by(block=ntile(trial, 6)) %>%
  summarise_at(vars(trial, trial_s), mean)
# overall fit (fixed effects):
data_pred <-
  cross_df(list(vot_s = seq(min(data_exp1_mod$vot_s),
                            max(data_exp1_mod$vot_s),
                            length.out=100),
                bvotCond = unique(data_exp1_mod$bvotCond),
                block = 1:6)) %>%
  left_join(exp1_blocks, by="block") %>%
  mutate(vot = vot_s * sd(data_exp1_mod$vot) + mean(data_exp1_mod$vot)) %>%
  left_join(conditions_exp1, by="bvotCond")

expt1_s_class <-
  fitted(b_logit_s_exp1, newdata=data_pred, re_formula=NA) %>%
  as_tibble() %>%
  bind_cols(data_pred)

# by-subject fit (fixed+random effects):
data_pred_subj <-
  data_exp1_mod %>%
  group_by(subject, bvotCond) %>%
  summarise() %>%
  left_join(data_pred, by="bvotCond")

expt1_s_class_bysub <-
  fitted(b_logit_s_exp1, newdata=data_pred_subj) %>%
  as_tibble() %>%
  bind_cols(data_pred_subj)


# plot class function by subject:
ggplot(expt1_s_class_bysub, aes(x=vot, y=Estimate, color=vot_cond, group=subject)) +
  geom_line(alpha=0.2) +
  facet_grid(block ~ vot_cond)

# there's some CRAZY variance in the slopes by subject.  I think this might be a
# consequence of not having any interaction between the trial splines and the
# slope...


expt1_bounds_s <- expt1_s_class %>% group_by(vot_cond, trial, block) %>% find_bound(x=vot, y=Estimate)

expt1_bounds_bysub_s <-
  expt1_s_class_bysub %>%
  group_by(vot_cond, subject, trial, block) %>%
  find_bound(x=vot, y=Estimate)

# plot boundaries by condition, vs. boundaries from linear effect of trial
ggplot(expt1_bounds_s, aes(x=trial, y=vot, color=vot_cond)) +
  geom_point() +
  geom_line() +
  geom_line(data = expt1_bounds, linetype=2)

# these look quite similar, except for the beginning?  which might have
# something to do with the lapse rate and/or shallower boundaries not being
# accomodated by the trial effect.  and the trajectory of the boundaries from
# the linear fits look like a reasonable linear interpolation of the boundaries
# from the spline so...not unreasonable.

################################################################################
# expt. 4: separate means

sepmeans <- supunsup::separatemeans_clean

sepmeans_conds <-
  sepmeans %>%
  group_by(bvotCond, pvotCond) %>%
  summarise() %>%
  ungroup() %>%
  arrange(bvotCond, pvotCond) %>%
  mutate(vot_cond = paste(bvotCond, pvotCond, sep=', '),
         vot_cond = factor(vot_cond, levels=vot_cond),
         ideal_boundary = (pvotCond + bvotCond)/2)

sepmeans %<>% inner_join(sepmeans_conds)

sepmeans_test <- sepmeans %>%
  filter(is_test) %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial))


f4 <- respP ~ 1 + vot_cond * vot_s * trial_s + (1 + vot_s | subject)

## b_logit_exp4 <- brm(f4, data=sepmeans_test, family=bernoulli(), chains=4, iter=1000)
## saveRDS(b_logit_exp4, "models/brm_logistic_expt4.rds")
b_logit_exp4 <- readRDS("models/brm_logistic_expt4.rds")


# mixture model for lapsing:
mix = mixture(bernoulli(), bernoulli())
b_logit_lapse_exp4 <-
  brm(bf(respP ~ 1,
         mu1 ~ 1 + vot_cond * vot_s * trial_s + (1 + vot_s | subject),
         mu2 ~ 1),
      family = mix,
      data = sepmeans_test)


exp4_blocks <-
  sepmeans_test %>%
  group_by(block=ntile(trial, 6)) %>%
  summarise_at(vars(trial, trial_s), mean)

# overall fit (fixed effects):
sepmeans_pred <-
  cross_df(list(vot_s = seq(min(sepmeans_test$vot_s),
                            max(sepmeans_test$vot_s),
                            length.out=100),
                vot_cond = unique(sepmeans_test$vot_cond),
                block = 1:6)) %>%
  left_join(exp4_blocks, by="block") %>%
  mutate(vot = vot_s * sd(sepmeans_test$vot) + mean(sepmeans_test$vot)) %>%
  left_join(sepmeans_conds, by="vot_cond")

expt4_bounds <-
  fitted(b_logit_exp4, newdata=sepmeans_pred, re_formula=NA) %>%
  as_tibble() %>%
  bind_cols(sepmeans_pred)

expt4_bounds %>%
  ggplot(aes(x=vot, y=Estimate)) +
  geom_line(aes(color=vot_cond)) +
  geom_ribbon(aes(ymin=`Q2.5`, ymax=`Q97.5`, fill=vot_cond), alpha=0.1) +
  facet_grid(~block)

expt4_bounds_lapse <-
  fitted(b_logit_lapse_exp4, newdata=sepmeans_pred, re_formula=NA) %>%
  as_tibble() %>%
  bind_cols(sepmeans_pred)
  

expt4_bounds_lapse %>%
  ggplot(aes(x=vot, y=Estimate)) +
  geom_line(aes(color=vot_cond)) +
  geom_ribbon(aes(ymin=`Q2.5`, ymax=`Q97.5`, fill=vot_cond), alpha=0.1) +
  geom_line(data = expt4_bounds, aes(color=vot_cond), linetype=3) +
  facet_grid(~block)
