rSCBapi
=======

rSCBapi is a package to interface with the API of Statistics Sweden, a.k.a. SCB.

Please note that this package is still in its infancy and might not function as expected. Version 0.1 contains functions to inspect metadata, construct url:s, list nodes and subnodes in the API data tree, and download real data.

## Installation
Use the `devtools` package for easy installation:
```r
install_github("rSCBapi", "prenumerant", "v0.1")
```

## Explore data
Data in the SCB API is structured in a data tree. The URL to the top node is stored in the `baseURL()` function. To explore the top node of the data tree, use `scbGetMetadata()`:
```r
url <- baseURL()
topNode <- scbGetMetadata(url)
```

This will return a list of data nodes at the top level of the API, including node id:s. The latter are contained in `topNode$id`, and can be used to ascend the node tree using `buildPath()`:

```r
url <- buildPath(baseURL(), as.character(topNode$id[15]))
topNode <- scbGetMetadata(url)
```