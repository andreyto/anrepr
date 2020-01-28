#!/bin/bash
#echo -n "Set desired password for RStudio Web interface and press Enter: "
#read -s MGSAT_DOCKER_PASSWD
#  -e PASSWORD="$MGSAT_DOCKER_PASSWD" \
docker run --rm --name rstudio -p 8787:8787 \
	-d \
	-v $HOME/work:/home/rstudio/work \
    rtex-full:1.0
#	rocker/verse:latest
#	-e USERID=$UID \
#    -e DISABLE_AUTH=true \

