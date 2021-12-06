FROM rocker/shiny:4.1.2

ADD ./shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD ./ /srv/transfer.server
WORKDIR /srv/transfer.server

RUN rm .Rprofile

RUN apt-get update -y \
    && apt-get install -y \
    libcurl4-openssl-dev \
    libmagick++-dev \
    git \
    && Rscript -e 'install.packages("devtools")' \
    && Rscript -e 'devtools::install_github("rstudio/renv")' \
    && Rscript -e "renv::install()" \
    && Rscript -e 'renv::install("./")' \
    && Rscript -e 'renv::install("ddionrails/soep-plots")' \
    && rm -rf /tmp/* \
    && rm -rf /var/lib/apt/lists/*

USER shiny:shiny


