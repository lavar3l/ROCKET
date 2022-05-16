FROM rocker/r-ubuntu:22.04

RUN mkdir /home/rocket

WORKDIR /home/rocket

RUN apt update -y && \
    apt install python3 pip wget -y && \
    pip3 install virtualenv