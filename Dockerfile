FROM rocker/r-ver:3.3.2

MAINTAINER Oliver Pelz "o.pelz@dkfz.de"

#### things we need for the crispranalyzer package
#### and for the crispr reannotator
#### and another deb pkgs we later need for the R libraries to compile or run
RUN apt-get update && apt-get install -y  \
    wget \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    build-essential \
    libgd-dev \
    libexpat1-dev \
    libxml2-dev \
    git \
    libssl-dev \
    curl \
    libssl-dev \
    libtiff5-dev \
    htop
    
RUN apt-get update && apt-get install -y ghostscript

# fix for RGL package
RUN apt-get update && apt-get install -y libx11-dev mesa-common-dev libglu1-mesa-dev

# again some more things we need to run the crispranalyzer package
RUN apt-get update && apt-get -y --no-install-recommends \
   install texlive texlive-xetex

# install the shiny server debian package from r-studio
COPY ./shiny-server-1.5.2.837-amd64.deb /tmp/ss.deb
RUN gdebi -n /tmp/ss.deb && \
    rm -f /tmp/ss.deb

COPY ./shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

# now to the R part...


# first we need devtools for all the installation of all further packages
RUN R -e 'install.packages("devtools", repos = "http://cloud.r-project.org/")'

# install all the packages we need from cran, bioconductor and github

RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite()'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("HTqPCR")'
RUN R -e 'devtools::install_version("dplyr", version = "0.5.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("readr", version = "1.0.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinydashboard", version = "0.5.3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("jsonlite", version = "1.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinyBS", version = "0.61", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_github("jbkunst/highcharter", ref = "cc5f1e0157c50fc67233bc1bad6c3ad906d418c8")'
RUN R -e 'devtools::install_version("openxlsx", version = "4.0.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("caTools", version = "1.17.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("reshape2", version = "1.4.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("MESS", version = "0.4-3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("data.table", version = "1.10.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("httr", version = "1.2.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("plyr", version = "1.8.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("tidyverse", version = "1.0.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("htmltools", version = "0.3.5", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("DT", version = "0.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("tidyr", version = "0.6.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("sm", version = "2.2-5.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinyjs", version = "0.9", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("ggplot2", version = "2.2.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("markdown", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("knitr", version = "1.15.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shiny", version = "1.0.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("bookdown", version = "0.3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("R.utils", version = "2.5.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("htmltools", version = "0.3.5", repos = "http://cloud.r-project.org/")'
RUN R -e 'install.packages("rgl")'
RUN R -e 'install.packages("qpcR")'




# cleaning up downloaded deb packages for keeping clean our docker image
RUN apt-get -qq clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# NOTE: since we dont have a public repos yet, clone the car repo in this local docker dir and copy latest development onto the docker image
COPY ./VAMBE/ /srv/shiny-server/VAMBE

# we will run VAMBE as user 
RUN chown -R shiny:shiny /srv/shiny-server/VAMBE


# add R profile options

RUN echo 'setwd("/srv/shiny-server/VAMBE")' >> /usr/local/lib/R/etc/Rprofile.site
RUN echo 'options(download.file.method = "libcurl")' >> /usr/local/lib/R/etc/Rprofile.site


COPY docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh


# Add ENV for KiteMatic
ENV verbose_logfiles=TRUE

EXPOSE 3838

ENTRYPOINT ["/docker-entrypoint.sh"]
# finally run
CMD ["start-app"]