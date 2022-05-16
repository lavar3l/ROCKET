#!/bin/bash

# Install python requirements for the project
cd /home/rocket/python && pip install -r requirements.txt

# Run R script
cd /home/rocket/rocket && Rscript main.R