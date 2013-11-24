rSCB
=======

# Please note - development version
This is the development version of the rSCB package. The package is provided as-is and comes without any warranty or guarantee. If things aren't working in this version you are encouraged to file a bug in the GitHub [issue tracker](https://github.com/LCHansson/rSCB/issues), but please do not be surprised if a function should not work as expected.

If you want to use a stable and tested version of rSCB, please use the version found in the `master` branch of the [rSCB repository](https://github.com/LCHansson/rSCB/). It can be installed by doing the following:

```s
devtools::install_github("rSCB","LCHansson",ref="master")
```



## Introduction
rSCB is a package to interface with the API of Statistics Sweden, a.k.a. SCB.

Please note that this package is still in its infancy and might not function as expected. Version 0.1 contains functions to inspect metadata, construct url:s, list nodes and subnodes in the API data tree, and download real data.

## A brief note on using the SCB API
The SCB API is a RESTful API. The data consists of a metadata part and a data part. The metadata part is structured in a hierarchical node tree, where each node contains information about any (sub-)nodes that are below it in the tree structure or, if the nodes is at the bottom of the tree structure, the data referenced by the node as well as what dimensions are available for the data at that subnode.


## Installation
Use the `devtools` package for easy installation:
```r
install.packages("devtools")
devtools::install_github("rSCBapi", "prenumerant", "v0.1")
```

## Exploring the top node of the API data tree
Data in the SCB API is structured in a data tree. The URL to the top node is stored in the `baseURL()` function. To explore the top node of the data tree, use `scbGetMetadata()`:
```r
url <- baseURL()
topNode <- scbGetMetadata(url)
View(topNode)
```

## Traversing the node tree
The node tree can be ascended by adding the id of the next subnode to the URL of the base URL. This id is stored in the "id" column of the topNode object created above.

By using the buildPath() function, this can be easily appended as such:
```r
id <- as.character(topNode$id[16])

url <- buildPath(baseURL(), id)

nextNode <- scbGetMetadata(url)
View(nextNode)
```
This can be repeated until we reach a node that references data instead of subnodes.

## Getting data dimensions
Next, we want to find the dimensions of the data at a particular bottom node, e.g. the node "KPIFastM" which is the bottom node in the following tree branch:

PR -> PR0101 -> PR0301B -> HMPIM07

The following code constructs the URL and fetches dimension metadata:

```r
url <- buildPath(baseURL(), "PR", "PR0101", "PR0301B", "HMPIM07")

dims <- scbGetDims(url)
```

The function scbGetDims() prints out a friendly message stating that the dimensions for the data at this node are "ContentsCode" and "Tid". We can now either pass on a wildcard ("*") to these dimensions, or a value, or a range of values on vector form.

To see what values are allowed for each dimension, have a look at the `dims` object using `print(dims)`.

## Getting the data
This information can now be used to get the actual data:
```r
sdata <- scbGetData(dataURL, list(SPIN2007 = "*", ContentsCode = "PR0301I4", Tid = c("2010M02","2011M03")))

View(sdata)
```

## Further examples
Further examples of package usage are contained in the "examples" folder installed with this package. To locate this folder, run `system.file(package = "rSCBapi")` from the R terminal.

## Development information
This package is still in its early development stages. The package can already be used in its present form to construct a simple menu system, to mine the SCB API for data, or to discover new data. However, work is needed to improve usability and widen the range of possible applications. You are invited to contribute to package development in any way you can and want to.

## Open source license
Please note that all source code contained in this project is open source licensed under the Affero Gnu Public License v3. This means that you are allowed to modify, use, and spread the source code freely withoug any permission from the author. HOWEVER, this source code and ANY derivatives thereof MUST be licensed with the same open source license. For further information about the AGPLv3, see LICENSE included with the source code of this package.
