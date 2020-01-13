# Changelog

## [Unreleased]

### Added
- added a `getting_started` markdown describing the design choices and the UDF scripting
- added support for UDF data translation from stars to xts and back
- annotation to require a certain called variable with a specific type
- getting_started.Rmd
- endpoint for versions of the service (api version and implementation version)

### Changed
- folder structure in /examples
- endpoint `/libs` renamed into `/packages`
- dimension name from "t" into "time" in /examples/data/hypercube.json

### Removed
- introduction.Rmd, which was integrated into the getting_started.Rmd


## [0.1]

### Changed
- modified the repository
- changed experimental data models to the UDF API (RasterCollectionTile and HyperCube)
- data translation from lists to stars into arrays to stars
- dissolved package structure to plumber 'plumbs' with data_translation scripts

### Removed
- command line tool
- large examples
