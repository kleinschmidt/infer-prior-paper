FROM rocker/verse:3.6.3

COPY ./packrat/packrat.lock packrat/

RUN Rscript -e "install.packages(\"packrat\", repos = \"https://cran.rstudio.com/\")" && \
        Rscript -e "library(packrat); packrat::init(infer.dependencies=FALSE)" && \
        Rscript -e "packrat::restore()"
