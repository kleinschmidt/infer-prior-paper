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

vot_colors = hcl(h = seq(0,330, length.out=6)-15,
                 c = 100,
                 l = 65)

scale_color_exp1 <- scale_color_manual('/b/, /p/\nmean VOT', values=vot_colors[2:6])
scale_fill_exp1 <- scale_fill_manual('/b/, /p/\nmean VOT', values=vot_colors[2:6])


fit_inc <- infer_prior_beliefs(data_exp1,
                               cue = "vot",
                               category = "trueCat",
                               response = "respCat",
                               condition = "vot_cond",
                               ranefs = "subject",
                               n_blocks = 6,
                               chains = 4,
                               iter = 2000)

saveRDS(fit_inc, "fit_inc.rds")

fit_inc <- readRDS("fit_inc.rds")

categories <-
  data_frame(cat_num = 1:2,
             category = c('b', 'p'))

prior_samples_df <-
  fit_inc %>%
  spread_draws(nu_0, kappa_0, mu_0[cat_num], sigma_0[cat_num]) %>%
  left_join(categories)

## create a data_frame with samples for updated parameters
updated_samples_df <-
  fit_inc %>%
  spread_draws(c(kappa_n, nu_n, mu_n, sigma_n)[block_num, cat_num, cond_num],
               lapse_rate[block_num]) %>%
  left_join(categories) %>%
  left_join(conditions_exp1)

## create a data_frame for lapsing rate samples
lapse_rate_samples <-
  fit_inc %>%
  spread_draws(lapse_rate[block_num])


## loo

## okay it DOES work but I made a big error and was using ALL VOTs .......

class_fun_samples <-
  updated_samples_df %>%
  group_by(.draw) %>%
  nest() %>%
  sample_n(200) %>%
  unnest() %>%
  mutate(mean=mu_n, sd=sigma_n) %>%
  select(.draw, block_num, bvotCond, category, mean, sd) %>%
  group_by(.draw, bvotCond, block_num) %>%
  do(stats_to_lhood(., noise_sd=0)) %>%
  lhood_to_classification() %>%
  left_join(lapse_rate_samples) %>%
  mutate(prob_p = (1-lapse_rate)*prob_p + lapse_rate/2)

ll_by_sub_draw <- data_exp1 %>%
  group_by(subject, vot, bvotCond, block_num=ntile(trial, 6)) %>%
  summarise(n_p = sum(respP), n_resp = n()) %>%
  left_join(class_fun_samples) %>%
  group_by(subject, .draw) %>%
  summarise(loglik = sum(dbinom(n_p, size=n_resp, prob=prob_p, log=TRUE)))

loooo <- ll_by_sub_draw %>%
  spread(.draw, loglik) %>%
  ungroup() %>%
  select(-subject) %>%
  as.matrix() %>%
  t() %>%
  loo()

### comparing with a GLM/logit model:

# needs data_exp1_mod from infer-prior.Rmd but you get the idea.....
glm_logit = brm(respP ~ 1 + bvotCond * vot_s * trial_s, data = data_exp1_mod, family=bernoulli(), chains=4, iter=1000)
ll_glm <- log_lik(glm_logit, )
ll_glm_bysub <- map(unique(data_exp1_mod$subject), ~ data_exp1_mod$subject == .x) %>% lift(cbind)(.) %>% {ll_glm %*% .};

## some visualizations

data_exp1 %>%
  mutate(block_num = ntile(trial, 6)) %>%
  group_by(subject, bvotCond, block_num, vot) %>%
  summarise(respP = mean(respP)) %>%
  ggplot(aes(x=vot, y=respP, color=bvotCond)) +
  geom_line(aes(group=subject), alpha=0.2) +
  geom_pointrange(stat="summary", fun.data=mean_cl_boot) +
  facet_grid(block_num ~ bvotCond)
  


mod_class_funs <- 
  updated_samples_df %>%
  group_by(.draw) %>%
  nest() %>%
  sample_n(200) %>%
  unnest() %>%
  mutate(mean=mu_n, sd=sigma_n) %>%
  select(.draw, block_num, bvotCond, category, mean, sd) %>%
  group_by(.draw, bvotCond, block_num) %>%
  do(stats_to_lhood(., noise_sd=0)) %>%
  lhood_to_classification() %>%
  left_join(lapse_rate_samples) %>%
  mutate(prob_p = (1-lapse_rate)*prob_p + lapse_rate/2) %>%
  group_by(bvotCond, block_num, vot) %>%
  select(bvotCond, block_num, vot, prob_p) %>%
  summarise(prob_p_low = quantile(prob_p, 0.025),
            prob_p_high = quantile(prob_p, 0.975),
            prob_p = mean(prob_p))

prior_class_funs <-
  prior_samples_df %>%
  group_by(.draw) %>%
  nest() %>%
  sample_n(200) %>%
  unnest() %>%
  mutate(mean=mu_0, sd=sigma_0) %>%
  select(.draw, category, mean, sd) %>%
  group_by(.draw, category) %>%
  do(stats_to_lhood(., noise_sd=0)) %>%
  lhood_to_classification() %>%
  group_by(vot) %>%
  summarise(prob_p_low = quantile(prob_p, 0.025),
            prob_p_high = quantile(prob_p, 0.975),
            prob_p = mean(prob_p))



data_exp1 %>%
  mutate(block_num = ntile(trial, 6)) %>%
  group_by(subject, bvotCond, block_num, vot) %>%
  summarise(respP = mean(respP)) %>%
  ggplot(aes(x=vot, y=respP, color=bvotCond)) +
  geom_line(aes(group=subject), alpha=0.2) +
  geom_pointrange(stat="summary", fun.data=mean_cl_boot) +
  geom_ribbon(data=mod_class_funs, aes(y=prob_p, ymin=prob_p_low, ymax=prob_p_high, fill=bvotCond))+
  geom_line(data=prior_class_funs, aes(y=prob_p), color="black", linetype=2) +
  facet_grid(block_num ~ bvotCond)

################################################################################
# don't include -10 ms shift

fit_inc_posshift <- infer_prior_beliefs(filter(data_exp1, bvotCond != "-10"),
                                        cue = "vot",
                                        category = "trueCat",
                                        response = "respCat",
                                        condition = "vot_cond",
                                        ranefs = "subject",
                                        n_blocks = 6,
                                        chains = 4,
                                        iter = 2000)


scale_shift <- prior_samples_df %>%
  group_by(.draw, nu_0, kappa_0) %>%
  summarise() %>%
  mutate(solution = ifelse(nu_0 > kappa_0, "shift", "scale")) %>%
  ungroup() %>%
  select(.draw, solution)

prior_samples_long <-
  fit_inc %>%
  gather_draws(nu_0, kappa_0, mu_0[cat_num], sigma_0[cat_num]) %>%
  group_by(.draw) %>%
  left_join(categories) %>%
  left_join(scale_shift)

prior_samples_posshift_long <-
  fit_inc_posshift %>%
  gather_draws(nu_0, kappa_0, mu_0[cat_num], sigma_0[cat_num]) %>%
  left_join(categories)
  
prior_samples_long_all <-
  bind_rows(prior_samples_long %>% mutate(model="all conds"),
            prior_samples_posshift_long %>% mutate(model="pos shift"))

ggplot(prior_samples_long_all,
       aes(x=.value, color=category)) +
  geom_density(aes(y=..scaled..)) +
  facet_grid(model ~ .variable, scales="free")

prior_samples_long_all %>%
  ggplot(aes(x=.value, color=category)) +
  geom_density(aes(y=..scaled..)) +
  facet_grid(paste(model, solution) ~ .variable, scales="free")

prior_samples_posshift_wide <-
  fit_inc_posshift %>%
  spread_draws(nu_0, kappa_0, mu_0[cat_num], sigma_0[cat_num]) %>%
  left_join(categories)

bind_rows(prior_samples_posshift_wide %>% mutate(model="pos shift"),
          prior_samples_df %>% mutate(model="all shift")) %>%
  ggplot( aes(x=kappa_0, y=nu_0)) +
  stat_density_2d() +
  facet_grid(.~model) +
  scale_x_log10() +
  scale_y_log10()

#' Okay what am I seeing here?  the fit looks _more_ like a "scale" solution
#' when you drop the -10ms condition.
#'
#' I kinda wish I didn't know this because it makes writing it up a little more
#' complicated.  I "like" the shift solution better from a theoretical
#' perspective, but then again why should that be preferable??  I mean what
#' theoretical reason is there to prefer that solution??  I think it has more to
#' do with the fact that the stimuli were constructed with a shift design,
#' right?
#' 


################################################################################
# all un-labeled trials

data_exp12 <- supunsup::supunsup_clean %>%
  filter(labeled == 'unlabeled') %>%
  mutate(trueCat = respCategory,
         subjNum = as.numeric(factor(subject)),
         trueCatNum = as.numeric(trueCat),
         respCatNum = as.numeric(respCat)) %>%
  left_join(conditions_exp1)

fit_12_inc <- infer_prior_beliefs(data_exp12,
                                  cue = "vot",
                                  category = "trueCat",
                                  response = "respCat",
                                  condition = "vot_cond",
                                  ranefs = "subject",
                                  n_blocks = 6,
                                  chains = 4,
                                  iter = 2000)

prior_samples_12_long <-
  fit_12_inc %>%
  gather_draws(nu_0, kappa_0, mu_0[cat_num], sigma_0[cat_num]) %>%
  left_join(categories)




fit_12_posshift_inc <- infer_prior_beliefs(filter(data_exp12, bvotCond != "-10"),
                                           cue = "vot",
                                           category = "trueCat",
                                           response = "respCat",
                                           condition = "vot_cond",
                                           ranefs = "subject",
                                           n_blocks = 6,
                                           chains = 4,
                                           iter = 2000)

prior_samples_12_posshift_long <-
  fit_12_posshift_inc %>%
  gather_draws(nu_0, kappa_0, mu_0[cat_num], sigma_0[cat_num]) %>%
  left_join(categories)


prior_samples_long_all %>%
  bind_rows(prior_samples_12_long %>% mutate(model="all shifts (+sup)"),
            prior_samples_12_posshift_long %>% mutate(model="pos shifts (+sup)")) %>%
  ggplot(aes(x=.value, color=category)) +
  geom_density(aes(y=..scaled..)) +
  facet_grid(model ~ .variable, scales="free")


#' See a very similar thing when we include all the data from the supervised
#' conditions too: dropping the -10 shift leads to 
#' 

################################################################################
# all from experiment 2 (both labeled and un-labeled)

data_exp2 <-
  supunsup::supunsup_clean %>%
  filter(supCond %in% c("supervised", "mixed")) %>%
  mutate(trueCat = respCategory,
         trueCatNum = as.numeric(trueCat),
         respCatNum = as.numeric(respCat))

conditions_exp2 <-
  data_exp2 %>%
  group_by(bvotCond, supCond, trueCat) %>%
  summarise(mean_vot = mean(vot)) %>%
  spread(trueCat, mean_vot) %>%
  transmute(vot_cond = paste(b, p, sep=', '),
            ideal_boundary = (b+p)/2) %>%
  ungroup() %>%
  mutate(vot_cond = factor(vot_cond),
         supervised = fct_recode(supCond, supervised="mixed"))

data_exp2 %<>% inner_join(conditions_exp2)

fit_inc_exp2 <- infer_prior_beliefs(data_exp2,
                                    cue = "vot",
                                    category = "trueCat",
                                    response = "respCat",
                                    condition = "vot_cond",
                                    ranefs = "subject",
                                    test_df = filter(data_exp2, labeled == "unlabeled"),
                                    n_blocks = 6,
                                    chains = 4,
                                    iter = 2000)

saveRDS(fit_inc_exp2, "fit_inc_exp2.rds")

confidence_exp2 <- tidybayes::spread_draws(fit_inc_exp2, nu_0, kappa_0, lp__)

ggplot(confidence_exp2, aes(x=kappa_0, y=nu_0)) +
  geom_point(alpha=0.05) +
  coord_equal() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline()

confidence_exp2 %>%
  summarise(mean(kappa_0 < nu_0))

#' Looks like the fit is a slightly faster learning rate (a bit more than 1× the
#' number of trials.  Still shift but more equivocal... (p = 0.725)


################################################################################
# both expt 1 and

data_exp12_all <-
  supunsup::supunsup_clean %>%
  mutate(trueCat = respCategory,
         subjNum = as.numeric(factor(subject)),
         trueCatNum = as.numeric(trueCat),
         respCatNum = as.numeric(respCat))

conditions_exp12_all <-
  data_exp12_all %>%
  group_by(bvotCond, supCond, trueCat) %>%
  summarise(mean_vot = mean(vot)) %>%
  spread(trueCat, mean_vot) %>%
  transmute(vot_cond = paste(b, p, sep=', '),
            ideal_boundary = (b+p)/2) %>%
  ungroup() %>%
  mutate(vot_cond = factor(vot_cond),
         cond = paste(vot_cond, supCond))

data_exp12_all %<>% inner_join(conditions_exp12_all)

fit_exp12_all <-
  infer_prior_beliefs(data_exp12_all,
                      cue = "vot",
                      category = "trueCat",
                      response = "respCat",
                      condition = "cond",
                      ranefs = "subject",
                      test_df = filter(data_exp12_all, labeled == "unlabeled"),
                      n_blocks = 6,
                      chains = 4,
                      iter = 2000)

saveRDS(fit_exp12_all, "fit_exp12_all.rds")

confidence_exp12 <- tidybayes::spread_draws(fit_exp12_all, nu_0, kappa_0, lp__)

ggplot(confidence_exp12, aes(x=kappa_0, y=nu_0)) +
  geom_point(alpha=0.05) +
  coord_equal() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline()

confidence_exp12 %>%
  summarise(mean(kappa_0 < nu_0))
