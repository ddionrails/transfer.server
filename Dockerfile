FROM rocker/shiny:4.0.5

ADD ./shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD ./ /srv/transfer.server
WORKDIR /srv/transfer.server

RUN rm .Rprofile

RUN apt-get update -y \
    && apt-get install -y \
    libcurl4-openssl-dev \
    libmagick++-dev \
    git \
    && Rscript -e 'install.packages("remotes")' \
    && Rscript -e 'remotes::install_github("rstudio/renv")' \
    && Rscript -e "renv::install()" \
    && Rscript -e 'renv::install("./")' \
    && Rscript -e 'renv::install("ddionrails/soep-plots")' \
    && rm -rf /tmp/* \
    && rm -rf /var/lib/apt/lists/*

USER shiny:shiny


