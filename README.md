# Useful Functions in R

##### A set of functios which may be useful.

##### Common instructions:
```
- Import and save script files
- Source the file inside R with "source(filename.R)"
- Start using =)
```


## Functions included:



### Dataframe Subsetting 
This function quickly subsets a dataframe or matrix based on a vector of IDs.
###### # df.sub()

```r
df.sub(data, axis = 2, index = 1, list.names)
```

| Parameter | Arguments |
| ------ | ------ |
| data| A dataframe or matrix. |
| axis | You must specify in which axis is the target-vector of names/IDs. If they are arranged in line, axis must be set to 1, otherwise (vector arranged in a column) it must be set to 2.  |
| index | It specifies in which line or column there is the target-vector for matching (like an ordinal number). |
| list.names | A vector of names for matching the target-vector in the dataframe/matrix. |



### Packages library
Verify if target-packages are installed, install them (if they are not), and load/atach them into namespace.
###### # pkg.lib()

```r
required.pkgs <- c("devtools", "ggplot2", "tidyverse", "stringr", "WriteXLS")
pkg.lib(required.pkgs)
```
Obs: It accepts only packages avaialable on CRAN repository.



### Remove all objects in current environment
Alias/shorcut for "rm(list = ls())" and "gc(reset = TRUE)".
###### # rm.all()

```r
rm.all()
```
