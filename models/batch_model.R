library(assertthat)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(purrrlyr)
library(stringr)
library(lme4)
library(ggplot2)

library(tidybayes)
library(rstan)
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


fit_batch <- infer_prior_beliefs(data_exp1,
                                 cue = "vot",
                                 category = "trueCat",
                                 response = "respCat",
                                 condition = "vot_cond",
                                 ranefs = "subject",
                                 chains = 4,
                                 iter = 2000)

saveRDS(fit_batch, "fit_batch.rds")

mod_samples <- rstan::extract(fit_batch)
mod_summary <- summary(fit_batch)$summary

max_Rhat <- max(mod_summary[, 'Rhat'])
lapse_rate <- mean(mod_samples$lapse_rate)

categories <-
  data_frame(cat_num = 1:2,
             category = c('b', 'p'))


## create a data_frame with samples for prior parameters
prior_samples_df <-
  fit_batch %>%
  spread_draws(nu_0, kappa_0, mu_0[cat_num], sigma_0[cat_num]) %>%
  filter(.iteration > 500) %>%
  left_join(categories)

## create a data_frame with samples for updated parameters
updated_samples_df <-
  fit_batch %>%
  spread_draws(c(kappa_n, nu_n, mu_n, sigma_n)[cat_num, cond_num]) %>%
  filter(.iteration > 500) %>%
  left_join(categories) %>%
  left_join(conditions_exp1)

## create a data_frame for lapsing rate samples
lapse_rate_samples <-
  fit_batch %>%
  spread_draws(lapse_rate) %>%
  filter(.iteration > 500)
