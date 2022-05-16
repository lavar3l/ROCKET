FROM rocker/r-ubuntu:22.04

RUN mkdir /home/rocket

WORKDIR /home/rocket

# Install dependencies
RUN apt update -y && \
    apt install python3 pip wget libcurl4-openssl-dev -y

# Install virtualenv
RUN pip3 install virtualenv

# Install r packages
RUN R -e "install.packages(c('stringr', 'reticulate', 'forecast', 'foreign', 'geometry', 'foreach', 'doParallel', 'rlang', 'snow'))"