library(tidyverse)
library(brms)

options(mc.cores = parallel::detectCores())

dat <- cross_df(list(x = seq(0, 10),
                     rep = seq(1, 5),
                     subj = seq(1, 10))) %>%
  mutate(theta = 0.02 + 0.95*plogis(1 * (x-5)),
         y = rbernoulli(n=length(theta), p=theta))


ggplot(dat, aes(x=x, y=theta)) + geom_point()
ggplot(dat, aes(x=x, y=y)) + geom_point(position=position_jitter(h=0.1))


fit1 = brm(y ~ 1 + x, data=dat, family=bernoulli(), )

f = bf(y ~ 1 / (1+exp(-(eta))),
       eta ~ 1+x,
       family=bernoulli(link="identity"),
       nl=TRUE)

set_prior("student_t(3,0,10)", class="b", nlpar="eta")

get_prior(f, data=dat)

fit2 = brm(bf(y ~ 1 / (1+exp(-(eta))),
              eta ~ 1+x,
              family=bernoulli(link="identity"),
              nl=TRUE),
           data=dat,
           prior = set_prior("student_t(3,0,10)", class="b", nlpar="eta"))



fit3 = brm(bf(y ~ lapselow + (1-lapsehigh-lapselow) * 1 / (1+exp(-(eta))),
              eta ~ 1+x,
              lapselow ~ 1,
              lapsehigh ~ 1, 
              family=bernoulli(link="identity"),
              nl=TRUE),
           data=dat,
           inits="0",
           control=list(adapt_delta=0.99),
           prior = c(set_prior("student_t(3,0,10)", class="b", nlpar="eta"),
                     set_prior("uniform(0,0.5)", class="b", nlpar="lapselow"),
                     set_prior("uniform(0,0.5)", class="b", nlpar="lapsehigh")))




fit4 = brm(bf(y ~ lapselow + (1-lapsehigh-lapselow) * inv_logit(eta),
              eta ~ 1+x,
              lapselow ~ 1,
              lapsehigh ~ 1, 
              family=bernoulli(link="identity"),
              nl=TRUE),
           data=dat,
           inits="0",
           control=list(adapt_delta=0.99),
           prior = c(prior(student_t(7, 0, 10), class="b", nlpar="eta"),
                     prior(beta(1, 1), nlpar="lapselow", lb=0.0, ub = 0.1),
                     prior(beta(1, 1), nlpar="lapsehigh", lb=0.0, ub = 0.1)))




################################################################################
# run on real data!

library(supunsup)

d <- supunsup::supunsup_clean %>%
  filter(supCond == "unsupervised") %>%
  select(subject, bvotCond, trial, vot, respP) %>%
  mutate(vot_s = (vot - mean(vot)) / sd(vot),
         trial_s = (trial - mean(trial)) / sd(trial))

f <- bf(respP ~ lapselow + (1-lapsehigh-lapselow) * inv_logit(eta),
        eta ~ 1 + vot_s*trial_s*bvotCond + (1 + vot_s*trial_s | subject),
        lapselow ~ 1,
        lapsehigh ~ 1,
        family = bernoulli(link="identity"),
        nl=TRUE)

priors = c(prior(student_t(7, 0, 10), class="b", nlpar="eta"),
           prior(beta(1, 1), nlpar="lapselow", lb=0.0, ub = 0.1),
           prior(beta(1, 1), nlpar="lapsehigh", lb=0.0, ub = 0.1))

fit_supunsup = brm(f,
                   data = d,
                   prior = priors)



fit_supunsup_logistic = brm(respP ~ 1 + vot_s*trial_s*bvotCond +
                              (1 + vot_s*trial_s | subject),
                            family=bernoulli(),
                            data = d)

saveRDS(fit_supunsup_logistic, file="models/fit_supunsup_logistic.rds")
