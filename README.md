# openeo-r-udf

## Introduction
This repository contains a R package for implementing the concept of User-Defined Functions for processing Earth Observation (EO) data in cloud backends. This will allow users to run their custom code written in R to be executed on EO data such as satellite imageries with the help of processing backends conforming to the openEO API. The openEO API is being developed as part of the project ["openEO"](https://github.com/Open-EO).

### Background
This repository is meant to be part of the H2020 funded project [openEO](http://openeo.org). The objective of this project is to develop an uniform API to allow processing of Earth Observation (EO) data in cloud-based processing backends from various client nodes. In this API framework, User-Defined Functions (UDFs) is a concept that would allow users to run their own scripts on EO data in these cloud backends.

### User-Defined Functions
The UDFs are implemented by developing an UDF API which work hand-in-hand with the openEO core API. The main idea is that there are UDF (web-) services which could be used by the backends as required. The typical workflow is:

1. The user uploads his/her script from the client nodes to the backends along with the process graph
2. The backend executes the process graph and encounters the UDF in the process graph
3. The backend seeks the services of the UDF service to execute the user's script and sends the script and intermediate data to the service through appropriate means
4. The UDF service executes the script on the data and sends the result back to the backend.
5. The backend receives the data and continues executing the process graph until the final result is obtained.
6. The backend sends the completed result to the user's client node.

These UDF service is being developed for two different languages - Python and R. This repository concerns with the implementation using R.

### Architecture

![openEO UDF Architecture](https://github.com/Open-EO/openeo-r-udf/blob/master/data/openeo_github.png)

In the openEO API, the different clients interact with the different backends through the openEO API which acts as a common language understood by both the clients and the backends. The UDF service is not accessible to the clients directly but only through the backends and hence the UDF service's internal operations are abstracted to the user.

## Installation

### Dependencies
To run the API you need to have the following packages installed:
 * stars (>= 0.4-0)
 * jsonlite
 * plumber
 * lubridate
 * sf
 
Depending on what analysis a potential UDF user can run this list needs to be extended.

These can be installed by running the following:

```r
install.packages(c("stars", "plumber", "lubridate","sf"), dependencies = TRUE)
```

Additionally on Linux systems you need to install the following libraries to allow the "stars" package to function properly:
```r
sudo apt-get install libudunits2-dev libgdal-dev -y
```

### Running the API locally
Clone this repository and use RStudio (>= 1.2) to "plumb" `api.R`. Since version 1.2 RStudio allows to run plumbers "plumb" natively. But you can run the plumb also manually by executing the code in `server_start.R`. It will require the file `api.R` and `data_transformation.R`.

### Using Docker
Docker provides a virtual containerized environment for running software. In order to install this R package in a Docker environment, please follow these steps:

1. Install Docker on your machine. The installation instructions vary according to the Operating System. Detailed instructions for all common Operating Systems may be found here: <https://docs.docker.com/install/>.
2. Make sure that Docker has been installed correctly using the following command. Details on containers and Docker version will be shown.
```bash
docker info
```
3. Test whether installation using Docker is working correctly. A hello message should be printed on screen. 
```bash
docker run hello-world
```
4. Build the docker image by using the preconfigured settings of the `docker-compose.yml`.
```bash
docker-compose build r-udf-service
```
This will build the local image `r-udf-plumber:0.0.1` and it will take some time. In the future there will also be an image available on Dockerhub.

5. Run it using the following command.
```bash
docker-compose run up -d
```
As a default the container runs on port 5555. You can change this behavior by stating another port in the Docker environments section by `PLUMBER_PORT=5555`. If you change this, please make sure that you also adapt the port mapping for the container.

## Usage
This package is intended to be used as part of the openEO API. The package works along with the different backends and are not supposed to accessible directly by the client. However, for testing, please refer to the the Wiki pages of this repository [here](https://github.com/Open-EO/openeo-r-udf/wiki).
You can either run this service as a standalone service to debug your code for the usage in openeo infrastructure of a back-end that supports R UDF execution or run it in a productive environment within a openeo back-end. We are working on a way to support most of the data types that are described in the [UDF API](https://open-eo.github.io/openeo-udf/api_docs/)
