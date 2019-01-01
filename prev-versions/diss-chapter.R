## ----preamble, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE, results='hide'----

library(assertthat)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(purrrlyr)
library(stringr)
library(lme4)
library(ggplot2)

## devtools::install_github('kleinschmidt/daver')
library(daver)

## devtools::install_github('kleinschmidt/phonetic-sup-unsup')
library(supunsup)
## devtools::install_github('kleinschmidt/beliefupdatr')
library(beliefupdatr)

library(knitr)
opts_chunk$set(warning = FALSE,
               message = FALSE,
               error = FALSE,
               cache=TRUE,
               echo=opts_knit$get("rmarkdown.pandoc.to") != 'latex')

options(digits=2)

## Produce markdown-formatted figures so that pandoc knows what to do with
## the captions. requires pandoc-fignos to parse the IDs. refer to figures
## in text with {@fig:label} or just @fig:label
## 
## (see https://github.com/tomduck/pandoc-fignos)
knit_hooks$set(plot = function(x, options) {
  paste0('![', options$fig.cap, ']',
         '(', opts_knit$get('base.url'), paste(x, collapse='.'), ')',
         '{#fig:', options$label, '}')
})
## Produce markdown-formatted table captions with anchors for cross-refs.
## Requires pandoc-tablenos to parse the IDs. Refer to tables
## in text with {@tbl:label} or @tbl:label.
## Based partly on http://stackoverflow.com/a/18672268
##
## (see https://github.com/tomduck/pandoc-tablenos)
knit_hooks$set(tbl.cap = function(before, options, envir) {
  if(!before){
    paste0('\n\nTable: ', options$tbl.cap,
           ' {#tbl:', options$label, '}', sep = '')
  }
})


## ----typical-talker------------------------------------------------------

prior_stats_by_talker <-
  votcorpora::vot %>%
  filter(source %in% c('gva13', 'bbg09', 'buckeye'),
         place == 'lab') %>%
  mutate(source = ifelse(source %in% c('gva13', 'bbg09'),
                         'goldricketal',
                         source)) %>%
  group_by(source, prevoiced, subject, phoneme) %>%
  summarise(mu = mean(vot),
            sigma2 = var(vot),
            sigma = sd(vot),
            n = n()) %>%
  rename(category = phoneme)

## plot single-talker distributions to get a sense of talker variability
## prior_stats_by_talker %>%
##   rename(mean=mu, sd=sigma) %>%
##   group_by(source, prevoiced, subject) %>%
##   by_slice(stats_to_lhood, xlim=c(-100, 100), noise_sd=0, .collate='rows') %>%
##   left_join(votcorpora::vot %>%
##               group_by(subject, prevoiced) %>%
##               tally() %>%
##               mutate(prop_prevoiced = n / sum(n))) %>% 
##   group_by(source, subject, category, vot) %>%
##   summarise(lhood=sum(lhood * prop_prevoiced)) %>%
##   ggplot(aes(x=vot, y=lhood, color=source, group=paste(subject, category))) +
##   geom_line()

prior_stats <-
  prior_stats_by_talker %>%
  filter(source == 'goldricketal', category == 'b') %>%
  group_by(source, prevoiced, category) %>%
  summarise_at(vars(mu, sigma2, sigma, n), funs(mean, var, sum)) %>%
  transmute(category,
            mean = mu_mean,
            sd = sqrt(sigma2_mean),
            n = n_sum) %>%
  bind_rows(supunsup::prior_stats %>%
              filter(source=='kronrod2012') %>%
              mutate(n = 1))

prior_lhood <- 
  prior_stats %>%
  filter(source == 'kronrod2012') %>%
  supunsup::stats_to_lhood()

prior_class <- prior_lhood %>% lhood_to_classification()


## ----expt1-data----------------------------------------------------------

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
  mutate(vot_cond = factor(vot_cond, levels=vot_cond))

data_exp1 %<>% left_join(conditions_exp1)

vot_colors = hcl(h = seq(0,330, length.out=6)-15,
                 c = 100,
                 l = 65)

scale_color_exp1 <- scale_color_manual('/b/, /p/\nmean VOT', values=vot_colors[2:6])
scale_fill_exp1 <- scale_fill_manual('/b/, /p/\nmean VOT', values=vot_colors[2:6])


## ----vot-dists-exp1, fig.width=8, fig.height=2, fig.cap="Each subject heard one of these five synthetic accents, which differ only in the distribution of VOTs of the word-initial stops. Black dashed lines show VOT distributions from a hypothetical typical talker [as estimated by @Kronrod2012]. Note that the 0 and 10ms shifted accents are reasonably close to this typical talker, while the -10, 20, and 30ms shifted accents deviate substantially."----

exposure_stats <- data_exp1 %>%
  group_by(vot_cond, category=trueCat) %>%
  summarise(mean=mean(vot), sd=sd(vot))

sd_noise = sqrt(82)

exposure_lhood <- exposure_stats %>%
  group_by(vot_cond) %>%
  do(supunsup::stats_to_lhood(., sd_noise))

data_exp1 %>%
  group_by(vot_cond, vot) %>%
  filter(subject == first(subject)) %>%
  tally() %>%
  ggplot(aes(x=vot)) +
  geom_bar(stat='identity', aes(y=n, fill=vot_cond)) +
  geom_line(data=prior_lhood, aes(y=lhood*1600, group=category),
            color="black", linetype=2) +
  geom_text(data=data.frame(vot_cond='-10, 30'), x = 10, y = 60,
            label = 'Typical Talker',
            color='black', hjust=0, vjust=0.3, size=3) +
  geom_text(data=data.frame(vot_cond='-10, 30'), x = 40, y = 50,
            label = 'Exposure\nTalker',
            color=vot_colors[2], hjust=0, vjust=0.8, size=3,
            lineheight=1) + 
  facet_grid(.~vot_cond) +
  scale_x_continuous('VOT (ms)') +
  scale_y_continuous('Frequency') +
  ## scale_fill_discrete('/b/, /p/\nmean VOT') ## +
  scale_fill_exp1
  ## theme(legend.position='none')


## ----participants-exp1---------------------------------------------------

n_subj <- data_exp1 %>% group_by(subject) %>% summarise() %>% tally()

excluded <- supunsup::supunsup_excluded %>%
  filter(supCond == 'unsupervised') %>%
  group_by(subject) %>% 
  summarise() %>% 
  right_join(supunsup::excludes) %>%
  select(subject, exclude80PercentAcc, rank)

n_excluded <- nrow(excluded)
n_subj_repeat <- sum(!is.na(excluded$rank))
n_subj_bad <- sum(!is.na(excluded$exclude80PercentAcc))
n_both <- n_subj_repeat + n_subj_bad - n_excluded 

n_total <- n_subj + n_excluded


## ----class-curves, fig.height=2, fig.width=8, fig.cap="Listeners' responses, smoothed with logistic functions (thin lines), compared with the classification functions expected based on a typical talker (no learning; dashed black lines) and complete (but not necessarily optimal) adaptation to the exposure distributions (thick dashed colored lines). Listeners' actual category boundaries lie between the typical talker and exposure talker boundaries (see Table {@tbl:boundary-shift})."----

## generate predicted classification functions assuming Bayes-optimal classifier
## + noise

perfect_learning <- exposure_stats %>%
  group_by(vot_cond) %>%
  do(stats_to_lhood(.)) %>%
  lhood_to_classification()

no_learning <- prior_lhood %>%
  lhood_to_classification()

prior_bound <- no_learning %>%
  arrange(abs(prob_p - 0.5)) %>%
  filter(row_number() ==1) %$%
  vot

vot_limits <- data_exp1 %$% vot %>% range()

ggplot(data_exp1, aes(x=vot, y=respP, color=vot_cond)) +
  geom_line(aes(group=subject), stat='smooth', method='glm', 
            method.args=list(family='binomial'), alpha=0.2) +
  facet_grid(.~vot_cond) +
  geom_line(data=perfect_learning, aes(y=prob_p), group=1, linetype=2, size=1) +
  geom_line(data=no_learning, aes(y=prob_p), group=1, linetype=2, color='black') +
  geom_text(data=data.frame(vot_cond='-10, 30'),
            x = 30, y = 0, label = 'Typical\ntalker',
            size = 3.5, hjust=0, vjust = 0, color='black',
            lineheight=1) + 
  geom_text(data=data.frame(vot_cond='-10, 30'),
            x = 12, y = 1, label = 'Expo-\nsure',
            size = 3.5, hjust=1, vjust=1, color=vot_colors[2],
            lineheight=1, fontface='bold') + 
  geom_text(data=data.frame(vot_cond='-10, 30'),
            x = 90, y = 0.75, label = 'Actual\nlisteners',
            size = 3.5, hjust=1, vjust=1, color=vot_colors[2],
            lineheight=1) + 
  scale_x_continuous('VOT (ms)', limits = vot_limits) +
  scale_y_continuous('Probability /p/ response') + 
  scale_color_exp1


## ----boundaries-exp1, cache=TRUE-----------------------------------------

boundaries_exp1 <- data_exp1 %>%
  group_by(bvotCond, vot_cond, subject) %>%
  do({ glm(respP ~ vot, family='binomial', data=.) %>%
         broom::tidy() %>%
         select(term, estimate)
  }) %>%
  ungroup() %>%
  spread(term, estimate) %>%
  mutate(boundary = -`(Intercept)` / vot,
         ideal_boundary = as.numeric(as.character(bvotCond)) + 20,
         prior_boundary = prior_bound,
         prop_shift = (boundary-prior_boundary)/(ideal_boundary-prior_boundary))

boundary_summary_exp1 <- boundaries_exp1 %>%
  group_by(vot_cond) %>%
  summarise(median_shift_perc = round(100*median(prop_shift)),
            shift_text = paste(median_shift_perc, '%', sep=''),
            prop_between = mean(xor(boundary > ideal_boundary,
                                    boundary > prior_boundary))
            )


## ----boundary-violin-plots-exp1, fig.width=6.5, fig.height=4.5, fig.cap="Most listeners' individual boundaries fall between the boundaries implied by cue distributions from a typical talker and exposure talker. Violin plots (shaded regions) show the density of individual subjects' category boundaries.  White points and CIs show mean and bootstrapped 95% CIs for mean boundary in each condition."----

ggplot(boundaries_exp1, aes(y = boundary, x = vot_cond, fill = vot_cond)) +
  ## this is an awful hack: plot the violins first to force continuous x
  geom_violin(color=NA,draw_quantiles=c(0.25, 0.5, 0.75), show.legend=FALSE,
              alpha = 0) +
  geom_segment(data = boundaries_exp1 %>%    # prior boundary
                 summarise(xmin = min(as.numeric(vot_cond))-0.5,
                           xmax = max(as.numeric(vot_cond))+0.5,
                           y = unique(prior_bound)),
               aes(x=xmin, xend=xmax, y=y, yend=y, fill=NA),
               color='black', linetype=2) + 
  geom_segment(aes(x=as.numeric(vot_cond)-.5, # exposure boundaries_exp1
                   xend=as.numeric(vot_cond)+.5,
                   y=ideal_boundary,
                   yend=ideal_boundary,
                   color=vot_cond),
               linetype = 2, size = 1,
               data = boundaries_exp1 %>% group_by(vot_cond, ideal_boundary) %>% summarise()) +
  geom_violin(color=NA,
              ## draw_quantiles=c(0.25, 0.5, 0.75),
              alpha = 0.5,
              show.legend=FALSE) +
  coord_flip() +
  geom_text(data=(boundaries_exp1 %>% filter(vot_cond=='20, 60') %>% head(n=1)),
            aes(y=ideal_boundary, color=vot_cond),
            ## x=3.5, y=41, 
            label='Exposure talker-\nspecific boundary',
            hjust = 0, vjust = 0,
            nudge_x = -0.5, nudge_y = 1) +
  geom_text(data=(boundaries_exp1 %>% filter(vot_cond=='20, 60') %>% head(n=1)),
            aes(y=prior_boundary), color='black',
            ## x=3.5, y=41, 
            label='Typical talker\'s boundary\n(based on Kronrod et al.)',
            hjust = 1, vjust = 0,
            nudge_x = -0.5, nudge_y = -1) +
  geom_pointrange(stat='summary', fun.data='mean_cl_boot', color='white') +
  theme(legend.position='none') +
  labs(x = 'Condition (/b/, /p/ mean VOT)',
       y = 'Category boundary (ms VOT)') +
  scale_color_exp1 +
  scale_fill_exp1


## ----boundary-shift, results='asis', tbl.cap="Percentage of boundary shift from typical talker to each exposure talker (see Figure {@fig:class-curves}), averaged over subjects with 95% bootstrapped confidence intervals.  0% shift corresponds to no adaptation at all, while 100% corresponds to perfect adaptation, ignoring any prior  beliefs. Typical and exposure talker boundaries were too close together to reliably determine boundary shift percentage in the 0ms condition."----

## bootstrapped boundary summary
boundaries_exp1 %>% 
  group_by(vot_cond, bvotCond) %>% 
  do(daver::boot_ci(., function(d,i) {mean(d$prop_shift[i])})) %>%
  mutate(observed = round(observed * 100),
         ci_lo = round(ci_lo * 100),
         ci_high = round(ci_high * 100)) %>%
  ungroup() %>%
  transmute(`/b/, /p/ mean VOT` = vot_cond,
            `Mean shift` = ifelse(bvotCond == 0,
                                  '---',
                                  sprintf('%d%%', observed)),
            `95% CI` = ifelse(bvotCond == 0,
                              '---',
                              sprintf('%d--%d%%', ci_lo, ci_high))) %>%
  knitr::kable(escape = FALSE)


## ----prepare-data-model--------------------------------------------------

## run the belief-updating model for inferring prior. the model source is in
## the beliefupdatr package, or will be soon :)
##
## devtools::install_github('kleinschmidt/beliefupdatr')
  
data_exp1_stan_conj <-
  prepare_data_conj_suff_stats_infer_prior(data_exp1,
                                           cue = "vot",
                                           category = "trueCat",
                                           response = "respCat",
                                           condition = "vot_cond",
                                           ranefs = "subject")

data_exp1_stan_conj_inc6 <-
  prepare_data_incremental_suff_stats(data_exp1,
                                      cue = "vot",
                                      category = "trueCat",
                                      response = "respCat",
                                      condition = "vot_cond",
                                      ranefs = "subject",
                                      n_blocks=6)


## ----run-model, eval=FALSE-----------------------------------------------
## 
## library(rstan)
## fit_lapsing <- sampling(beliefupdatr:::stanmodels$conj_id_lapsing_sufficient_stats_fit,
##                         data = data_exp1_stan_conj,
##                         chains = 4,
##                         iter = 2000)
## 
## fit_lapsing_inc6 <- sampling(beliefupdatr:::stanmodels$conj_id_lapsing_sufficient_stats_incremental_fit,
##                              data = data_exp1_stan_conj_inc6,
##                              chains = 4,
##                              iter = 1000)
## 
## head(summary(fit_lapsing)$summary, n=10)
## head(summary(fit_lapsing_inc6)$summary, n=10)
## 
## mod_summary <- summary(fit_lapsing)$summary
## mod_samples <- rstan::extract(fit_lapsing)
## 
## saveRDS(mod_samples, file='models/samples_lapsing.rds')
## saveRDS(mod_summary, file='models/summary_lapsing.rds')
## 
## mod_inc_summary <- summary(fit_lapsing_inc6)$summary
## mod_inc_samples <- rstan::extract(fit_lapsing_inc6)
## 
## saveRDS(mod_inc_samples, file='models/samples_inc_lapsing.rds')
## saveRDS(mod_inc_summary, file='models/summary_inc_lapsing.rds')
## 

## ----load-samples--------------------------------------------------------

mod_samples <- readRDS('models/samples_lapsing.rds')


## ----load-summary--------------------------------------------------------
mod_summary <- readRDS('models/summary_lapsing.rds')

## ----model-analysis-exp1, cache=TRUE-------------------------------------


## rename dimensions to make melting easier
rename_dims <- function(x, var, new_names) {
  names(dimnames(x[[var]])) <- new_names
  return(x)
}

mod_samples %<>%
  rename_dims('mu_0', c('iterations', 'cat_num')) %>%
  rename_dims('sigma_0', c('iterations', 'cat_num')) %>%
  rename_dims('mu_n', c('iterations', 'cat_num', 'cond_num')) %>%
  rename_dims('sigma_n', c('iterations', 'cat_num', 'cond_num')) %>%
  rename_dims('kappa_n', c('iterations', 'cat_num', 'cond_num')) %>%
  rename_dims('nu_n', c('iterations', 'cat_num', 'cond_num'))
  

max_Rhat <- max(mod_summary[, 'Rhat'])
lapse_rate <- mean(mod_samples$lapse_rate)

categories <-
  data_frame(cat_num = 1:2,
             category = c('b', 'p'))

# helper function to melt a mult-dimensional array of samples into a df
melt_samples <- function(samples, varname) {
  reshape2::melt(samples[[varname]], value.name=varname) %>%
    tbl_df
}

## create a data_frame with samples for prior parameters
prior_samples_df <- 
  c('mu_0', 'sigma_0', 'kappa_0', 'nu_0') %>%
  map( ~ melt_samples(mod_samples, .x)) %>%
  reduce(inner_join) %>%
  tbl_df %>%
  left_join(categories)

## create a data_frame with samples for updated parameters
updated_samples_df <- 
  c('mu_n', 'sigma_n', 'kappa_n', 'nu_n') %>%
  map( ~ melt_samples(mod_samples, .x)) %>%
  reduce(inner_join) %>%
  tbl_df %>%
  left_join(categories) %>%
  left_join(conditions_exp1 %>% mutate(cond_num=as.numeric(bvotCond))) %>%
  group_by(bvotCond) %>%
  select(-cat_num, -cond_num)

## create a data_frame for lapsing rate samples
lapse_rate_samples <- melt_samples(mod_samples, 'lapse_rate')



## ----model-goodness-of-fit-----------------------------------------------

mod_fitted <-
  data_exp1_stan_conj %$%
  z_test_counts %>%
  data.frame() %>%
  tbl_df() %>%
  mutate(prob_p = apply(mod_samples$p_test_conj[, , 2], 2, mean),
         prob_p_lapse = prob_p * (1-lapse_rate) + lapse_rate/2)

mod_goodness_of_fit <- 
  mod_fitted %>%
  mutate(LL_mod = dbinom(x = p, size = b+p, prob = prob_p_lapse, log = TRUE),
         LL_null = dbinom(x = p, size = b+p, prob = mean(p/(b+p)), log = TRUE)) %>%
  summarise(LL_mod = sum(LL_mod),
            LL_null = sum(LL_null),
            rho = cor(p/(b+p), prob_p_lapse, method='spearman'),
            n = n()) %>%
  mutate(LL_ratio = LL_mod - LL_null,
         pseudo_R2_mcfadden = 1 - LL_mod/LL_null,
         pseudo_R2_nagelkerke = (1 - exp(2/n * -LL_ratio)) / (1-exp(2/n*LL_null)))


## ----model-fit-classification, fig.width=8, fig.height=2, fig.cap="The classification functions (shaded ribbons, 95% posterior predictive intervals) predicted by the belief updating model fit listeners' responses well (dots with lines showing bootstrapped 95% confidence intervals)."----


## pick a random subset of iterations to do the MCMC integration for posterior
## predictive checks
some_iterations <- 
  updated_samples_df %>%
  group_by(iterations) %>%
  summarise() %>%
  sample_n(200)

## convert samples into distributions and then classification functions for each
## condition
mod_class_funs <- 
  updated_samples_df %>%
  right_join(some_iterations) %>%
  mutate(mean=mu_n, sd=sigma_n) %>%
  select(iterations, bvotCond, category, mean, sd) %>%
  group_by(iterations, bvotCond) %>%
  do(stats_to_lhood(., noise_sd=0)) %>%
  lhood_to_classification() %>%
  left_join(lapse_rate_samples) %>%
  mutate(prob_p = (1-lapse_rate)*prob_p + lapse_rate/2) %>%
  select(bvotCond, vot, prob_p) %>%
  group_by(bvotCond, vot) %>%
  summarise(prob_p_low = quantile(prob_p, 0.025),
            prob_p_high = quantile(prob_p, 0.975),
            prob_p = mean(prob_p))

data_by_subject <- data_exp1 %>%
  group_by(subject, vot_cond, vot) %>%
  summarise(prob_p = mean(respP))



## plot observed and model-predicted classification functions
mod_class_funs %>%
  left_join(conditions_exp1) %>%
  ggplot(aes(x=vot, y=prob_p, color=vot_cond, fill=vot_cond)) +
  geom_ribbon(aes(ymin=prob_p_low, ymax=prob_p_high), size=0, alpha=0.5) + 
  geom_point(data = data_by_subject, stat='summary', fun.y='mean') + 
  geom_linerange(data=data_by_subject, stat='summary', fun.data='mean_cl_boot') + 
  facet_grid(.~vot_cond) +
  scale_x_continuous('VOT (ms)') +
  scale_y_continuous('Probability /p/ response') + 
  scale_color_exp1 +
  scale_fill_exp1



## ----inferred-prior, fig.width=6, fig.height=2.5, fig.cap="Expected cue distributions based on the prior beliefs inferred here from behavioral adaptation data. Plotted with VOT distributions measured by @Kronrod2012 based on a combination of classification and discrimination behavior, and from production data by @Goldrick2013 for /b/, including pre-voicing."----

prior_summary <- 
  prior_samples_df %>% 
  gather('stat', 'val', mu_0:sigma_0) %>% 
  unite(stat_cat, stat, category) %>% 
  select(-cat_num) %>% spread(stat_cat, val) %>%
  gather('stat', 'value', kappa_0:sigma_0_p) %>% 
  group_by(stat) %>%
  summarise(mean=mean(value), 
            low=quantile(value, 0.025), 
            high=quantile(value, 0.975)) %>%
  mutate(units = ifelse(str_detect(stat, '(kappa|nu)'), 
                        'observations', 
                        'ms VOT'))

prior_expected <-
  prior_summary %>%
  select(stat, mean) %>%
  spread(stat, mean) %>%
  map(round) %>%
  as_vector()


typical_talker_lhoods <- 
  prior_stats %>%
  group_by(source, prevoiced, n) %>%
  by_slice(stats_to_lhood, xlim=c(-100, 100), .collate='rows') %>%
  group_by(source, category, vot) %>%
  summarise(lhood = sum(lhood * n) / sum(n)) # combine prevoiced and non, weighted

## Plot prior vs. typical talker from various sources
prior_samples_df %>% 
  group_by(category) %>% 
  summarise(mean = mean(mu_0), sd = mean(sigma_0)) %>%
  stats_to_lhood(xlim=c(-100,100), noise_sd = 0) %>%
  ggplot(aes(x=vot, y=lhood, group=category, linetype=category)) +
  geom_line(data = typical_talker_lhoods %>%
              filter(source %in% c('goldricketal', 'kronrod2012')),
            aes(color=source, group=paste(source, category)),
            size = 1) +
  geom_line(aes(color='Inferred prior'), size=2) +
  scale_color_discrete('Source') +
  scale_x_continuous('VOT (ms)') +
  scale_y_continuous('Likelihood')

## TODO: clearer legend


## ----inferred-prior-params, results='asis', tbl.cap="Expected values and 95% highest posterior density intervals for the prior parameters, given the adaptation data."----

format_params <- function(s) {
  s %>%
    str_replace('_0_([bp])$', '_{0,\\\\mathrm{\\1}}') %>%
    str_c('$\\', ., '$')
}

prior_summary %>% 
  transmute(Parameter = format_params(stat),
            Expected = mean,
            `95% HPD Int.` = sprintf('%.0f--%.0f', low, high),
            Units = units) %>%
  as.data.frame() %>%
  knitr::kable(escape=FALSE, digits=0)


## ----prior-variance, eval=FALSE------------------------------------------
## 
## ## Compare with actual variance across talkers
## 
## ## devtools::install_bitbucket('hlplab/votcorpora')
## 
## votcorpora::vot %>%
##   filter(place == 'lab') %>%
##   group_by(source, prevoiced, voicing, phoneme, subject) %>%
##   summarise(mean_vot = mean(vot)) %>%
##   summarise_each(funs(mean, sd), mean_vot)
## 
## prior_samples_df %>%
##   mutate(mu_var = sigma_0^2/kappa_0,
##          mu_sd = sqrt(mu_var)) %>%
##   select(iterations, category, starts_with('mu')) %>%
##   gather(parameter, value, starts_with('mu')) %>%
##   group_by(category, parameter) %>%
##   summarise_each(funs(mean, low=quantile(., 0.025), high=quantile(., 0.975)),
##                  value)
## 

## ----separatemeans-data--------------------------------------------------

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

sepmeans_stats <-
  sepmeans %>%
  filter(!is_test) %>%
  left_join(sepmeans_conds) %>%
  group_by(bvotCond, pvotCond, vot_cond, trueCat) %>%
  summarise(mean = mean(vot), sd = sd(vot)) %>%
  rename(category = trueCat)

sepmeans_exposure_lhood <-
  sepmeans_stats %>%
  group_by(bvotCond, pvotCond, vot_cond) %>%
  do({stats_to_lhood(.)})

sepmeans_exposure_class <-
  sepmeans_exposure_lhood %>%
  do({lhood_to_classification(.)})

sepmeans_test <-
  sepmeans %>%
  filter(is_test) %>%
  left_join(sepmeans_conds)

scale_color_exp2 <- scale_color_manual('/b/, /p/\nmean VOT', values=vot_colors[1:5])
scale_fill_exp2 <- scale_fill_manual('/b/, /p/\nmean VOT', values=vot_colors[1:5])




## ----exposure-dists-exp2, fig.width=10, fig.height=2, fig.cap="In Experiment 2, each subject heard a talker that produced one of these five VOT distributions. The variance of each category was constant across conditions, but the means varied semi-independently."----


sepmeans %>%
  left_join(sepmeans_conds) %>%
  filter(!is_test) %>%
  group_by(vot_cond, vot) %>%
  filter(subject == first(subject)) %>%
  tally() %>%
  ggplot(aes(x=vot)) +
  geom_bar(stat='identity', aes(y=n, fill=vot_cond)) +
  geom_line(data=prior_lhood, aes(y=lhood*1600, group=category),
            color="black", linetype=2) +
  facet_grid(.~vot_cond) +
  scale_x_continuous('VOT (ms)') +
  scale_y_continuous('Frequency') +
  scale_fill_exp2


## ----exp2-subjects-------------------------------------------------------

n_excl2 <- supunsup::separatemeans_excluded %>%
  left_join(sepmeans_conds) %>%
  group_by(vot_cond, subject) %>%
  summarise() %>%
  tally()

n_clean2 <- sepmeans %>%
  left_join(sepmeans_conds) %>%
  group_by(vot_cond, subject) %>%
  summarise() %>%
  tally()

n_total2 <- bind_rows(n_excl2, n_clean2) %$% sum(n)

accept_to_submit_time <-
  supunsup::separatemeans_assignments %>%
  do(daver::boot_ci(., function(d,i) with(d[i, ], mean(submittime-accepttime))))

active_time <- supunsup::separatemeans %>%
  group_by(subject) %>%
  summarise(time = max(tend) - min(tstart)) %>%
  mutate(time_min = time / 60000) %>%
  do(daver::boot_ci(.$time_min, function(d,i) mean(d[i])))

post_test_len <-
  sepmeans_test %>%
  filter(subject==first(subject)) %>%
  nrow()


## ----fit-cat-bounds------------------------------------------------------

bound_at_trial <- post_test_len / 6

## estimate category boundares at 1/6 of post test trials (bound_at_trial)
boundaries_exp2 <- sepmeans %>%
  filter(is_test) %>%
  group_by(bvotCond, pvotCond, subject) %>%
  mutate(trial = trial - min(trial) - bound_at_trial) %>% 
  do({ glm(respP ~ vot + trial, family='binomial', data=.) %>%
         broom::tidy() %>%
         select(term, estimate)
  }) %>%
  ungroup() %>%
  spread(term, estimate) %>%
  mutate(boundary = -`(Intercept)` / vot,
         ideal_boundary = (bvotCond + pvotCond)/2,
         prior_boundary = prior_bound,
         prop_shift = (boundary-prior_boundary)/(ideal_boundary-prior_boundary)) %>%
  left_join(sepmeans_conds) %>%
  filter(boundary < 100, boundary > -100)


## ----boundary-exposure-vs-test-exp2, fig.width=5, fig.height=2.5, fig.cap="Category boundaries estimated during post-test are correlated with estimates from exposure (in the 10, 50 condition where such an estimate is possible), but more variable. Blue line shows best linear fit, and black line shows where the two estimates are equal."----

boundaries_exp2_exposure <-
  sepmeans %>%
  filter(!is_test, bvotCond == 10, pvotCond == 50) %>%
  group_by(subject) %>%
  do({ glm(respP ~ vot, family='binomial', data=.) %>%
         broom::tidy() %>%
         select(term,estimate) }) %>%
  spread(term, estimate) %>%
  mutate(boundary_exposure = -`(Intercept)` / vot)

boundaries_exp2_comparison <- 
  boundaries_exp2_exposure %>%
  left_join(boundaries_exp2 %>% select(subject, boundary)) %>%
  ungroup()

exp2_cor <-
  boundaries_exp2_comparison %>%
  cor.test(formula=~ boundary_exposure + boundary, data=.) %>%
  broom::tidy()

exp2_var <-
  boundaries_exp2_comparison %>%
  summarise_each(funs(sd, se=sd(.)/sqrt(length(.))), boundary_exposure, boundary)

boundaries_exp2_exp_v_test_summary <- 
  boundaries_exp2_comparison %>%
  do(daver::boot_ci(., function(d,i) with(d[i, ], mean(boundary_exposure - boundary))))



boundaries_exp2_comparison %>%
  ggplot(aes(x=boundary, y=boundary_exposure)) +
  geom_abline() +
  geom_point() +
  geom_text(data = boundaries_exp2_comparison %>%
              filter(boundary == min(boundary)),
            label = 'One subject', hjust=0, vjust=0,
            nudge_x = .5, nudge_y = .5) +
  stat_smooth(method='lm') +
  coord_equal(ylim=c(20,36)) +
  labs(x='Category boundary estimated from post-test',
       y='Boundary estimated\nfrom exposure')


## ----fig.width=10, fig.height=2, fig.cap="Listeners' categorization functions in Experiment 2 (during post-test) reflect partial adaptation to the exposure talker, especially for more extreme conditions extreme distributions. Adaptation was even less complete than in Experiment 1. Classification functions are estimated with a logistic GLM including trial; curves show predictions for the beginning of the post-test phase to minimize impact of unlearning during test."----

## generate and plot predictions halfway through first third (1/6 of 70 = 
exp2_predict_at <- data_frame(trial = bound_at_trial,
                              vot = seq(min(sepmeans_test$vot),
                                        max(sepmeans_test$vot)))

sepmeans_test %>%
  group_by(subject, vot_cond) %>%
  mutate(trial = trial - min(trial)) %>%
  nest() %>%
  mutate(mod = map(data, glm, formula=respP ~ vot+trial, family='binomial'),
         pred = map(mod, predict, exp2_predict_at, type='response')) %>%
  unnest(map(pred, ~ mutate(exp2_predict_at, prob_p = .))) %>%
  ggplot(aes(x=vot, y=prob_p, color=vot_cond)) +
  geom_line(aes(group=subject), alpha=0.2) +
  geom_line(data = prior_class,
            linetype=2, color='black') +
  geom_line(data = sepmeans_exposure_class,
            linetype=2, size=1) +
  facet_grid(.~vot_cond) +
  scale_x_continuous('VOT (ms)', limits = vot_limits) +
  scale_y_continuous('Probability /p/ response') + 
  scale_color_exp2



## ----boundary-violin-plots-exp2, fig.width=6.5, fig.height=4.5, fig.cap="The distribution of listeners' individual category boundaries in Experiment 2 reflects partial adaptation to the exposure talker's VOT distributions. Extreme shifts have minimal additional effect on listeners' boundaries, corroborating the strong prior biases observed in Experiment 1. The un-filled distribution shows the boundaries from listeners in the corresponding 10ms VOT /b/ mean condition from Experiment 1. These conditions had the same exposure distributions, but differed in whether category boundaries were estimated based on the exposure (Experiment 1) or post-test data (Experiment 2)."----

ggplot(boundaries_exp2, aes(y = boundary, x = vot_cond, fill = vot_cond)) +
  ## this is an awful hack: plot the violins first to force continuous x
  geom_violin(color=NA,draw_quantiles=c(0.25, 0.5, 0.75), show.legend=FALSE,
              alpha = 0) +
  geom_segment(data = boundaries_exp2 %>%    # prior boundary
                 summarise(xmin = min(as.numeric(vot_cond))-0.5,
                           xmax = max(as.numeric(vot_cond))+0.5,
                           y = unique(prior_bound)),
               aes(x=xmin, xend=xmax, y=y, yend=y, fill=NA),
               color='black', linetype=2) + 
  geom_segment(aes(x=as.numeric(vot_cond)-.5, # exposure boundaries_exp2
                   xend=as.numeric(vot_cond)+.5,
                   y=ideal_boundary,
                   yend=ideal_boundary,
                   color=vot_cond),
               linetype = 2, size = 1,
               data = boundaries_exp2 %>% group_by(vot_cond, ideal_boundary) %>% summarise()) +
  geom_violin(color=NA,
              ## draw_quantiles=c(0.25, 0.5, 0.75),
              alpha = 0.5,
              show.legend=FALSE) +
  coord_flip() +
  geom_pointrange(stat='summary', fun.data='mean_cl_boot', color='white') +
  theme(legend.position='none') +
  labs(x = 'Condition (Mean /b/ and /p/ VOT)',
       y = 'Category boundary (ms VOT)') +
  scale_color_exp2 +
  scale_fill_exp2 +
  geom_violin(data = boundaries_exp1 %>%
                filter(bvotCond == 10) %>%
                mutate(vot_cond = '10, 50'),
              fill=NA, aes(color=vot_cond))


## ----boot-diff-pvot------------------------------------------------------

boot_50_ne_80 <-
  boundaries_exp2 %>%
  filter(bvotCond=='10') %>%
  boot_ci(function(d,i) d[i, ] %>%
                          group_by(vot_cond) %>%
                          summarise(boundary=mean(boundary)) %$%
                          diff(boundary),
          h0=0)


## ----prop-prevoicing-----------------------------------------------------
prop_pre_by_talker <- 
  votcorpora::vot %>%
  filter(source == 'gva13') %>%
  group_by(subject) %>%
  summarise(prop_pre = mean(prevoiced)) %>%
  arrange(prop_pre)

## ----predict-expt2-from-inferred, cache=TRUE-----------------------------

#' Convert from stan parametrization to beliefupdatr::nix2
stan_conj_to_nix2 <- function(stan_p) {
  with(stan_p, list(nu = nu_0,
                    kappa = kappa_0,
                    mu = mu_0,
                    sigma2 = sigma_0 ^ 2))
}

# convert samples of prior params in array form to list of samples in nix2
# parameter list form.
#
# e.g., mod_nix2_samples[[1]][[1]] is the first 
mod_nix2_samples <- 
  mod_samples[c('nu_0', 'kappa_0', 'mu_0', 'sigma_0')] %>%
  ## repeate these since there's just one for both categories
  map_at(c('nu_0', 'kappa_0'), ~ cbind(.x, .x)) %>%
  ## turn arrays into nested lists
  map(array_tree) %>%
  ## zip list of variables into list of samples
  transpose() %>%
  ## zip each sample's list of variables into list of categories
  map(transpose) %>%
  ## ...and zip list of samples into a list of categories
  transpose() %>%
  set_names(c('b', 'p')) %>%
  ## rename and convert expected sd to var
  at_depth(2, stan_conj_to_nix2)

## confirm that we have nix2 params at depth 2
invisible(mod_nix2_samples %>% at_depth(2, ~ assert_that(is_nix2_params(.))))

## get summary statistics for each condition
updated_nix2_samples <- 
  sepmeans %>%
  left_join(sepmeans_conds) %>%
  group_by(bvotCond, pvotCond, trueCat) %>%
  filter(subject == first(subject),
         is_test == FALSE) %>%
  nest() %>%
  mutate(prior_samples = map(trueCat, ~ mod_nix2_samples[[.x]]),
         updated_samples = map2(data, prior_samples,
                                function(d, s) map(s, nix2_update, x=d$vot)))

sample_to_lhood <- function(p)
  data_frame(vot = seq(-10,50),
             lhood = d_nix2_predict(vot, p))

predicted_lhood <- updated_nix2_samples %>%
  mutate(test_lhood = map(updated_samples,
                          . %>%
                            map(sample_to_lhood) %>%
                            do.call(what=rbind))) %>%
  unnest(test_lhood)

lapse_rate_samples <-
  mod_samples[['lapse_rate']] %>%
  as.numeric() %>%
  data_frame(lapse_rate=.) %>%
  mutate(sample = row_number())

predicted_prob_p <-
  predicted_lhood %>%
  group_by(bvotCond, pvotCond, trueCat, vot) %>%
  mutate(sample = row_number()) %>%
  spread(trueCat, lhood) %>%
  left_join(lapse_rate_samples) %>%
  mutate(prob_p = p / (b+p),
         prob_p_lapse = prob_p * (1-lapse_rate) + lapse_rate*0.5) %>%
  group_by(bvotCond, pvotCond, vot) %>%
  summarise_each(funs(mean=mean, lo=quantile(., 0.025), hi=quantile(., 0.975)),
                 prob_p, prob_p_lapse)


## ----predict-exp2-from-exp1-inc, cache=TRUE------------------------------

# convert samples of prior params in array form to list of samples in nix2
# parameter list form.
#
# e.g., mod_nix2_samples[[1]][[1]] is the first 
mod_inc_nix2_samples <- 
  mod_inc_samples[c('nu_0', 'kappa_0', 'mu_0', 'sigma_0')] %>%
  ## repeate these since there's just one for both categories
  map_at(c('nu_0', 'kappa_0'), ~ cbind(.x, .x)) %>%
  ## turn arrays into nested lists
  map(array_tree) %>%
  ## zip list of variables into list of samples
  transpose() %>%
  ## zip each sample's list of variables into list of categories
  map(transpose) %>%
  ## ...and zip list of samples into a list of categories
  transpose() %>%
  set_names(c('b', 'p')) %>%
  ## rename and convert expected sd to var
  at_depth(2, stan_conj_to_nix2)


## confirm that we have nix2 params at depth 2
invisible(mod_inc_nix2_samples %>% at_depth(2, ~ assert_that(is_nix2_params(.))))

## get summary statistics for each condition
updated_inc_nix2_samples <- 
  sepmeans %>%
  left_join(sepmeans_conds) %>%
  group_by(bvotCond, pvotCond, trueCat) %>%
  filter(subject == first(subject),
         is_test == FALSE) %>%
  nest() %>%
  mutate(prior_samples = map(trueCat, ~ mod_inc_nix2_samples[[.x]]),
         updated_samples = map2(data, prior_samples,
                                function(d, s) map(s, nix2_update, x=d$vot)))

predicted_lhood_inc <- updated_inc_nix2_samples %>%
  mutate(test_lhood = map(updated_samples,
                          . %>%
                            map(sample_to_lhood) %>%
                            do.call(what=rbind))) %>%
  unnest(test_lhood)

lapse_rate_inc_samples <-
  mod_inc_samples[['lapse_rate']][ , 6] %>%
  as.numeric() %>%
  data_frame(lapse_rate=.) %>%
  mutate(sample = row_number())

predicted_prob_p_inc <-
  predicted_lhood_inc %>%
  group_by(bvotCond, pvotCond, trueCat, vot) %>%
  mutate(sample = row_number()) %>%
  spread(trueCat, lhood) %>%
  left_join(lapse_rate_inc_samples) %>%
  mutate(prob_p = p / (b+p),
         prob_p_lapse = prob_p * (1-lapse_rate) + lapse_rate*0.5) %>%
  group_by(bvotCond, pvotCond, vot) %>%
  summarise_each(funs(mean=mean, lo=quantile(., 0.025), hi=quantile(., 0.975)),
                 prob_p, prob_p_lapse)



## ----plot-exp2-predictions-first-third, fig.width=8, fig.height=2, fig.cap="The prior beliefs inferred from Experiment 1 predict how much listeners adapt to each of the input distributions of Experiment 2. Shaded regions show the 95% posterior predictive intervals for the belief updating model, based on prior beliefs inferred from Experiment 1 and exposure distributions from Experiment 2.  Dots and errorbars show mean and 95% bootstrapped CIs for the mean probability of /p/ response over subjects. Note the additional uncertainty in predictions relative to the posterior predictive distribution given Experiment 1's conditions (Figure {@fig:model-fit-classification})."----

predicted_prob_p %>%
  left_join(sepmeans_conds) %>%
  ggplot(aes(x=vot, y=prob_p_lapse_mean, color=vot_cond, fill=vot_cond)) +
  ## geom_line() +
  geom_ribbon(aes(ymin=prob_p_lapse_lo, ymax=prob_p_lapse_hi),
              alpha=0.2, color=NA) +
  geom_pointrange(data = sepmeans_test %>%
                    filter(ntile(trial,3) == 1) %>%
                    group_by(vot_cond, vot, subject) %>%
                    summarise(resp_p = mean(respP)),
                  aes(y=resp_p),
                  stat='summary', fun.data=mean_cl_boot) +
  facet_grid(.~vot_cond) +
  labs(x = 'VOT (ms)',
       y = 'Probability /p/ response') +
  scale_color_exp2 +
  scale_fill_exp2


## ----plot-exp2-inc-predictions-------------------------------------------


predicted_prob_p_inc %>%
  left_join(sepmeans_conds) %>%
  ggplot(aes(x=vot, y=prob_p_lapse_mean, color=vot_cond, fill=vot_cond)) +
  ## geom_line() +
  geom_ribbon(aes(ymin=prob_p_lapse_lo, ymax=prob_p_lapse_hi),
              alpha=0.2, color=NA) +
  geom_pointrange(data = sepmeans_test %>%
                    filter(ntile(trial,3) == 1) %>%
                    group_by(vot_cond, vot, subject) %>%
                    summarise(resp_p = mean(respP)),
                  aes(y=resp_p),
                  stat='summary', fun.data=mean_cl_boot) +
  facet_grid(.~vot_cond) +
  labs(x = 'VOT (ms)',
       y = 'Probability /p/ response') +
  scale_color_exp2 +
  scale_fill_exp2



## ----exp2-mod-goodness-of-fit, cache=TRUE--------------------------------

#' @param d_test test data
#' @param pred model predictions (i.e., predicted_prob_p)
#' @param ... additional arguments passed to group_by for output summary
exp2_gof <- function(d_test, pred, ...) {
  d_test %>%
    group_by(subject, vot, bvotCond, pvotCond, vot_cond, respCat, ...) %>%
    tally() %>%
    spread(respCat, n, fill=0) %>%
    left_join(pred) %>%
    group_by(...) %>%
    mutate(LL_mod = dbinom(x = p, size = b+p, prob = prob_p_lapse_mean,
                           log = TRUE),
           LL_null = dbinom(x = p, size = b+p, prob = mean(p/(b+p)),
                            log = TRUE)) %>%
    summarise(LL_mod = sum(LL_mod),
              LL_null = sum(LL_null),
              rho = cor(p/(b+p), prob_p_lapse_mean, method='spearman'),
              n = n()) %>%
    mutate(LL_ratio = LL_mod - LL_null,
           pseudo_R2_mcfadden = 1 - LL_mod/LL_null,
           pseudo_R2_nagelkerke = (1 - exp(2/n * -LL_ratio)) / (1-exp(2/n*LL_null)))
}

mod_pred_gof_exp2 <- sepmeans_test %>% exp2_gof(predicted_prob_p)

## by condition
mod_pred_gof_exp2_by_cond <-
  sepmeans_test %>%
  filter(ntile(trial,3) == 1) %>%
  exp2_gof(predicted_prob_p, vot_cond) %>%
  mutate(LL_mod_each = LL_mod/n, LL_ratio_each = LL_ratio/n)

mod_pred_gof_exp2_by_third <- 
  sepmeans_test %>%
  mutate(third=ntile(trial, 3)) %>%
  exp2_gof(predicted_prob_p, third)

## compare boundaries
set.seed(102)
exp2_pred_vs_actual_boundaries <- 
  predicted_prob_p %>%
  group_by(bvotCond, pvotCond) %>%
  mutate(dev = abs(prob_p_lapse_mean - 0.5)) %>%
  filter(dev==min(dev)) %>%
  rename(boundary_pred=vot) %>%
  right_join(boundaries_exp2) %>%
  group_by(boundary_pred, add=TRUE) %>%
  do(daver::boot_ci(.,
                    function(d,i) with(d[i, ], mean(boundary-boundary_pred)),
                    h0=0,
                    R=10000)) 


