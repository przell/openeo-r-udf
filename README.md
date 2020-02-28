# openeo-r-udf

## Introduction
This repository contains a R package for implementing the concept of User-Defined Functions for processing Earth Observation (EO) data in cloud backends. This will allow users to run their custom code written in R to be executed on EO data such as satellite imageries with the help of processing backends conforming to the openEO API. The openEO API is being developed as part of the project ["openEO"](https://github.com/Open-EO).

### Background
This repository is meant to be part of the H2020 funded project [openEO](http://openeo.org). The objective of this project is to develop an uniform API to allow processing of Earth Observation (EO) data in cloud-based processing backends from various client nodes. In this API framework, User-Defined Functions (UDFs) is a concept that would allow users to run their own scripts on EO data in these cloud backends.

### User-Defined Functions
The UDFs are implemented by developing an UDF API which work hand-in-hand with the openEO core API. The main idea is that there are UDF (web-) services which could be used by the backends as required. The typical workflow is:

1. Users upload their script from an openEO client into their workspace of a selected back-end.
2. Users execute their process graphs on the back-end that contains a UDF.
3. When executing the `run_udf` process the backend sends the user's script and the intermediate data to the UDF service that the user requires and is registered at the back-end.
4. The UDF service runs the script on the data and sends the result back to the backend.
5. The backend receives the data and continues executing the process graph until the final result is obtained.
6. The backend sends the completed result to the user's client node.

These UDF service is being developed for two different languages - Python and R. This repository concerns with the implementation using R.

### Architecture

![openEO UDF Architecture](/img/udf_architecture.png)

In the openEO API, the different clients interact with the different backends through the openEO API which acts as a common language understood by both the clients and the backends. The UDF service is not accessible to the clients directly but only through the backends and hence the UDF service's internal operations are abstracted to the user.

## Installation

### Dependencies
To run the API you need to have the following packages installed:
 * stars (>= 0.4-1)
 * jsonlite
 * plumber
 * lubridate
 * sf
 
Depending on what analysis a potential UDF user can run this list needs to be extended.

These can be installed by running the following:

```r
install.packages(c("plumber", "lubridate","sf"), dependencies = TRUE)
remotes::install.github("r-spatial/stars")
```

Additionally on Linux systems you need to install the following libraries to allow the "stars" package to function properly:
```r
sudo apt-get install libudunits2-dev libgdal-dev -y
```

### Running the API locally
Clone this repository and use RStudio (>= 1.2) to *plumb* `api.R`. Since version 1.2 RStudio allows to run plumbers *plumb* natively. But you can run the plumb also manually by executing the code in `server_start.R`. It will require the file `api.R` and `data_transformation.R`.

### Using Docker
Docker provides a virtual containerized environment for running software. In order to install this R package in a Docker environment, please follow these steps:

1. Install Docker on your machine. The installation instructions vary according to the Operating System. Detailed instructions for all common Operating Systems may be found here: <https://docs.docker.com/install/>.
2. Make sure that Docker has been installed correctly using the following command. Details on containers and Docker version will be shown.
```bash
docker info
```
3. Run the provided `docker-compose.yml`. In the shell / console change directory to this cloned GitHub repository. Run `docker-compose up -d`.

### Custom Docker Images

If you find yourself lacking some packages then you might want to have a look at the docker files and configurations in the `/customizer` folder of this repository. By adding more required libraries in the `libraries.json` with their respective version or *latest* version you can install those libraries into the image.

If the R package requires system libraries you have to install them via the *Dockerfile*. There is a comment block telling you want to uncomment and configure.

## Usage
For first code examples, please have a look at `/examples/getting_started.Rmd` or [its HTML version](/examples/getting_started.html). With the test data sets `/examples/data/hypercube.json`, `/examples/data/raster_collection_tile.json` and other examples with simple structured data you can start experimenting with OpenEO UDFs. In the markdown file there is a reference to the `send_udf` function of the [`openeo` package](https://github.com/Open-EO/openeo-r-client) that combines the code and the serialized data and sends it to the service for processing. The request is build according to the [API description](https://open-eo.github.io/openeo-udf/api_docs/). Currently the services offers the functionality for `POST /udf` with `HyperCubes`, `RasterCollectionTiles` and `StructuredData` as supported input data. The results are provided as a `Hypercube` or if the results are simple, then `StructuredData` is returned.
