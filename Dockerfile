FROM rocker/verse:3.6.3

WORKDIR /paper
RUN Rscript -e "install.packages(\"renv\"); renv::consent(TRUE); renv::init(bare = TRUE)"

COPY install.r install.r
RUN Rscript install.r

VOLUME /paper/renv
