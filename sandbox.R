

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