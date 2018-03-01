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

Getting your input: generating input questions and saving your answers 

```r
myConfig <- get_config()
```


Cleaning your data based on your answers

```r
cleanData <- clean_data(myData, myConfig)
```
Cleaning reports should be created in `Your-Working-Directory...\CleaningReports`


## Package release
 

`bdclean` **is still under development**, first version will be released shortly.



## bd-R organization motto
```{r}
diff.type.of.audience <-  diff.solution
```

   :deciduous_tree: :mushroom: :shell: :fish: :frog: :honeybee: :turtle: :rooster: :whale2: :monkey: :octocat: 
