# What constrains distributional learning in adults?

[https://osf.io/3wdp2/](https://osf.io/3wdp2/)

## Reproducing

### Paper

The R code to generate this paper is in the RMarkdown file [infer-prior.Rmd]().
Dependencies are tracked with [`packrat`](https://rstudio.github.io/packrat/).
Launching R in this directory should be all that's necessary to install the
correct versions of the necessary packages in a local library for this project,
but be aware that this may take some time since they're compiled from source and
there are quite a few dependencies.

### Bayesian models

Because of their long compile and run-times, the Bayesian regressions and
belief-updating model are not run in the RMarkdown document.  The code to run
them is there but commented out, and standalone scripts are also included in the
`models/` subdirectory.  The [OSF repository](https://osf.io/3wdp2/) has the RDS
files generated if you don't want to run them yourself.
