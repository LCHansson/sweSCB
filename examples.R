require(httr)
require(RJSONIO)



##### EXAMPLE: HIERARCHY MINING #####

# Define the query
scbQueryData <- list(
	base_url = "http://api.scb.se",
	api_name = "OV0104", # Are there any other names?
	api_version = "v1",
	database_id = "doris",
	language = "sv",
	levels = "ssd",
# 	table_id = "AM/AM0114"
		table_id = "AM/AM0106/AM0106B/Kommun15g"
)

# Build the base URL
baseUrl <- buildPath(scbQueryData)

# Get sublevels at the current node
getLevels(baseUrl)

# Then get the data
# This won't work... yet
getScbData(baseUrl)

# We probably also want to be able to recursively describe all subnodes at
# a certain node.
# (This also won't work yet)
getAllLevelsBelow(baseUrl)



##### EXAMPLE: FETCHING ACTUAL DATA #####
scbQueryData <- list(
	base_url = "http://api.scb.se",
	api_name = "OV0104", # Are there any other names?
	api_version = "v1",
	database_id = "doris",
	language = "sv",
	levels = "ssd",
	table_id = "BE/BE0401/BE0401B/BefProgFoddaMedel10"
)

baseUrl <- buildPath(scbQueryData)

POST(
	baseUrl,
	body = toJSON(
		list(query = list(
			list(code = "Fodelseland",
				 selection = list(filter = "item",
				 				 values = list("010","020")
				 )),
			list(code = "Alder",
				 selection = list(filter = "all",
				 				 values = list("*")
				 )),
			list(code = "ContentsCode",
				 selection = list(filter = "all",
				 				 values = list("*")
				 )),			
			list(code = "Tid",
				 selection = list(filter = "top",
				 				 values = list("3")
				 ))),
			 response = list(format = "csv"
			 )))
)


## Parse the CSV output to a data.frame and print it
a <- content(response, as="text")
b <- read.table(textConnection(a), sep=',', header=T, stringsAsFactors=F)
print(b)




##### EXAMPLE: INSTALLING THE rSCBapi PACKAGE #####
require(devtools)

install_github("rSCBapi", "prenumerant")

