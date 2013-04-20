POST(
	"http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgFoddaMedel10",
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
