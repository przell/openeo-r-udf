FROM r-udf-plumber:0.1

ENV PLUMBER_PORT=5555

# copy RScripts from the current directory into the container
WORKDIR /opt/openeo-r-udf
COPY /*.R .
COPY /libraries.json .

RUN apt-get update && apt-get install libudunits2-dev libgdal-dev -y

RUN ["Rscript", "install_libraries.R"]


# open port 5555 to traffic
EXPOSE ${PLUMBER_PORT}
