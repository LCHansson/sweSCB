## EXAMPLE: USING rSCBapi TO MINE DATA FROM THE SCB API

## 1. Get metadata from the base node
a <- scbGetMetadata(buildPath(baseURL()))

# View the data. We will recycle the values in the "id" column to delve deeper into the API node structure.
View(a)

# Go down one level into the node tree.
b <- scbGetMetadata(buildPath(baseURL(),
                              as.character(a$id[9])

))
# Do this for consecutive steps...
cc <- scbGetMetadata(buildPath(baseURL(),
                               as.character(a$id[9]),
                               as.character(b$id[4])
))

# ...until we reach a node with no nodes below it, only data
d <- scbGetMetadata(buildPath(baseURL(),
                              as.character(a$id[9]),
                              as.character(b$id[4]),
                              as.character(cc$id[1])
))

# Construct the URL to the data
dataURL <- buildPath(baseURL(),
                     as.character(a$id[9]),
                     as.character(b$id[4]),
                     as.character(cc$id[1]),
                     as.character(d$id[1])
)

# Get the dimensions of the data
e <- scbGetDims(dataURL)

# View dimension names
for(i in 1:length(e)) {
   print(e[[i]]$code)
}

# View dimension value space
for(i in 1:length(e)) {
   print(e[[i]]$code)
   print(e[[i]]$values)
}

# Submit a query containing one or several values for each of the named dimensions
sdata <- scbGetData(dataURL, list(ContentsCode = "*", Tid = "1980M01"))
