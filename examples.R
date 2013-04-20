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


## Parsa output till en data.frame när output är ett CSV-objekt
a <- content(response, as="text")
b <- read.table(textConnection(a), sep=',', header=T, stringsAsFactors=F)
