# Use an official R base image
FROM r-base:latest

# Install required packages (tidyverse as an example)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
