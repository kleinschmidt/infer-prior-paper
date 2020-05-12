# set up dependencies in docker



## deps <- c(
##   "rmarkdown",
##   "knitr",
##   "tidyverse",
##   "dplyr",
##   "glue",
##   "forcats",
##   "magrittr",
##   "purrr",
##   "purrrlyr",
##   "stringr",
##   "beliefupdatr",
##   "supunsup",
##   "rstan",
##   "brms",
##   "tidybayes",
##   "cowplot",
##   "ggbeeswarm",
##   "latex2exp",
##   "kleinschmidt/daver",
##   "kleinschmidt/phonetic-sup-unsup",
##   "kleinschmidt/beliefupdatr"
## )

deps <- c("rmarkdown", "kleinschmidt/votcorpora")

renv::install(deps, prompt=FALSE)
renv::snapshot(type="all")
