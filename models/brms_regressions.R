################################################################################
# bayesian logistic regression analyses

library(tidyverse)
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


