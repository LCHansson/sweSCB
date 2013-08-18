# Build url
url <- (buildPath(baseURL(), "BefProgFoddaMedel10"))

# Get dimensions (names of dimensions are printed in the terminal)
dims <- scbGetDims(url)

# Get data
test <- scbGetData(url, dims=list(
	Fodelseland = "010",
	Alder="*",
	ContentsCode = "*",
	Tid="*"
))