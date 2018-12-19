FROM rocker/rstudio:3.5.1

# Install system requirements
RUN apt-get update
RUN apt-get -y install zlib1g-dev
RUN apt-get -y install libproj-dev libgdal-dev

# Install R Requirements
WORKDIR /tmp
ADD R-reqs.txt .
RUN while read in; do R -e "install.packages('$in', repos = 'http://cran.us.r-project.org')"; done < R-reqs.txt

# Specific install for INLA
RUN R -e "install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)"

# Specific versions of Haven (1.1.2) and labelled (1.1.0)
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/haven/haven_1.1.2.tar.gz', repos=NULL, type='source')"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/labelled/labelled_1.1.0.tar.gz', repos=NULL, type='source')"

# Copy the start script
COPY start.sh .
CMD ["/bin/bash","start.sh"]