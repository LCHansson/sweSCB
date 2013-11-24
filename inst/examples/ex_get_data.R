# Build url
varname <- "BefProgFoddaMedel10"

# Get metadata
metadata <- scbGetMetadata(varname)

# Get dimensions (names of dimensions are printed in the terminal)
dims <- scbGetDims(metadata)

# Get data
test <- scbGetData(metadata$URL, dims=list(
	Fodelseland = "010",
	Alder="*",
	ContentsCode = "*",
	Tid="*"
))

# Examine data
View(test)

# Make sure the data is actually numeric (it often isn't)
if(is.character(test$Födda.2010)) {
	for(i in 1:nrow(test)) {
		test$Födda.2010[i] <- paste0(str_extract_all(test$Födda.2010[i],"[[:digit:]]")[[1]],collapse="")
	}
	test$Födda.2010 <- as.numeric(test$Födda.2010)
}
	
# Plot the data
ggplot(test,aes(x=ålder,y=Födda.2010,fill=födelseland)) + geom_histogram()
