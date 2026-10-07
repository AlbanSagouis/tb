FROM rocker/r-base
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_cran("data.table")'
CMD R -e 'library(dockerfiler)'
CMD R -e 'data.table::setDTthreads(6)'
