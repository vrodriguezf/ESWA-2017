# ESWA-2017

This is the R code associated to the article: 

Rodríguez-Fernández, V., Menéndez, H. D., & Camacho, D. (2017). Analysing temporal performance profiles of UAV operators using time series clustering. Expert Systems with Applications, 70, 103-118.

You can find a link to the article here:
http://www.sciencedirect.com/science/article/pii/S0957417416305851

## Requirements

1. Download *R 3.4.0* and *RStudio*
2. Open the file *ESWA.RProj* with RStudio
3. Type `install.packages("packrat")` to install package *packrat*, for managing dependencies
4. Type `packrat::restore()` to load all the project dependencies

## Usage

The project structure has been created using the R package *ProjectTemplate*. For more details about ProjectTemplate, see http://projecttemplate.net

* The input data can be found in the `cache` folder
* The output data can be found in the `output` folder
* Every analysis script can be found in the folder `src`.
* Do not execute file `src/ESWA_1-TimeSeriesMeasures.R` without previous permission to access the database.

## Contact

For any questions about the use of the code (which is currently in a bit of a mess) please contact me by email or add a new issue in this repository.

