FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \ 
    unzip \
    pwgen \
    postgresql-client \
 && rm -rf /var/lib/apt/lists/*

