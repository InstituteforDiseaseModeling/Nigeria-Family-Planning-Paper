FROM rocker/rstudio:3.4.0

# Install system requirements
RUN apt-get update
RUN apt-get -y install zlib1g-dev
RUN apt-get -y install libproj-dev libgdal-dev

# Install R Requirements
WORKDIR /tmp
ADD R-reqs.txt .
RUN while read in; do R -e "install.packages('$in', repos = 'http://cran.us.r-project.org')"; done < R-reqs.txt

