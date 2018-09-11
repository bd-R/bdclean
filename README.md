[![Build Status](https://travis-ci.org/thiloshon/bdclean.svg?branch=master)](https://travis-ci.org/thiloshon/bdclean)
[![codecov](https://codecov.io/gh/thiloshon/bdclean/branch/master/graph/badge.svg)](https://codecov.io/gh/thiloshon/bdclean)

# bdclean
### User-friendly biodiversity data cleaning pipeline

`bdclean` will provide features to manage complete pipeline for biodiversity data cleaning. 
A modular approach to plug in data checking and cleaning functions would make this package
one stop shop for many biodiversity data cleaning needs.

![](https://github.com/bd-R/bdclean/blob/master/img/bdclean2.png)



## Install

Development version, from GitHub

```r
library("devtools")
devtools::install_github("bd-R/bdclean")
```

```r
library("bdclean")
```

## Download some data from GBIF

```r
library(rgbif)
```

```r
occdat <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 359,      # Class code for mammalia
  limit = 5000,         # Get only 5000 records
)
```

Taking just the species occurrence dataframe

```r
myData <- occdat$data
```



## Using bdclean

### Getting your input: generating input questions and saving your answers 

```r
myConfig <- get_config()
```


### Cleaning your data based on your answers

```r
cleanData <- clean_data(myData, myConfig)
```
Cleaning reports should be created in `Your-Working-Directory\CleaningReports`


## Package release
 

`bdclean` **is still under development**, a CRAN release with many package upgrades is expected in July 2018.



## We need your feedback :innocent:

Please submit your feedback useing this **[link](https://github.com/bd-R/Feedback-bdclean/issues/new)**

   :deciduous_tree: :mushroom: :shell: :fish: :frog: :honeybee: :turtle: :rooster: :whale2: :monkey: :octocat: 
