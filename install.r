# set up dependencies in docker

deps <- c(
  "rmarkdown",
  "knitr",
  "tidyverse",
  "dplyr",
  "glue",
  "forcats",
  "magrittr",
  "purrr",
  "purrrlyr",
  "stringr",
  "beliefupdatr",
  "supunsup",
  "rstan",
  "brms",
  "tidybayes",
  "cowplot",
  "ggbeeswarm",
  "latex2exp",
  "kleinschmidt/daver",
  "kleinschmidt/phonetic-sup-unsup",
  "kleinschmidt/beliefupdatr"
)

renv::install(deps, prompt=FALSE)
renv::snapshot(type="all")
