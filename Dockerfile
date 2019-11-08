# start from the rocker/r-ver:3.6.1 image
FROM rocker/r-ver:3.6.1

ENV PLUMBER_PORT=5555

# install plumber
RUN R -e "install.packages(c('plumber','remotes'))"

WORKDIR /opt/openeo-r-udf

# copy RScripts from the current directory into the container
COPY /*.R ./

# open port 5555 to traffic
EXPOSE ${PLUMBER_PORT}

# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "server_start.R"]