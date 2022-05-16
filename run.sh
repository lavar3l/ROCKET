#!/bin/bash

# How to run
# docker exec -it r.docker chmod +x /home/rocker/run.sh && ./home/rocket/run.sh

# Install python requirements for the project
cd /home/rocket/python && pip install -r requirements.txt

# Run R script
cd /home/rocket/rocket && Rscript main.R