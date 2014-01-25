

##### SANDBOX #####
baseUrl <- parse_url(baseUrl)

baseUrl$query = NULL
baseUrl$params = NULL

build_url(baseUrl)

response <- POST(
	url=basePath,
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

## När output är ett JSON-objekt
response$url
response$content
a <- content(response, as="text")
# b <- iconv(a, "UTF-8", "ISO8859-1")
fromJSON(b)
RJSONIO::fromJSON(a)
rjson::fromJSON(a, unexpected.escape="skip")


parse_auto(response$content, type="json", encoding="ISO8859-1")






setnames(subLevelData, "id", paste0("subId",subLevel))
subLevelData[,id := topLevelId]

hierarchy[id==topLevelId,subId := subLevelData]

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

baseUrl2 <- buildPath(scbQueryData)

response <- POST(
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




##### EXAMPLE: INSTALLING THE sweSCB PACKAGE FROM GITHUB #####
require(devtools)

install_github("sweSCB", "LCHansson")
