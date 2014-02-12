require(httr)
require(RJSONIO)
require(stringr)
require(data.table)
require(ggplot2)

##### EXAMPLE: HIERARCHY MINING #####
kMaxLevels = 4

kTakeSample = TRUE
kSamplesize = 10



# RUN --------------------------------------------------------------------------
## INIT: Define the data containers
hierarchy <- data.table(id_lv1 = scbGetLevels(descriptions=TRUE)$id, desc_lv1 = scbGetLevels(descriptions=TRUE)$description)
emptyRows <- list()
tableAtLevel <- list()
timeSeries <- data.table(obs=integer(), level=integer(), id=character(), time=numeric())
count <- 0

## Warn if kTakeSample is FALSE
if(!kTakeSample) {
	cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
	cat("WARNING: Fetching data ALL NODES in the SCB API.\n")
	cat("This might take a VERY LONG TIME!\n")
	cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n\n")
	
}

## Run loop
for(j in 2:kMaxLevels) {
	
	# Take a sample of Hierarchy to make it manageable
	if (kTakeSample) {
	hierarchy <- hierarchy[
		sample(
			nrow(hierarchy),
			min(nrow(hierarchy),kSamplesize*(j-1)))
		]
	}
	
	# Define the sublevel at which we will be looking
	idLevel <- j
	
	cat("*******************************************\n")
	cat("Getting data for level", idLevel,"\n")
	cat("*******************************************\n\n")
	
	# Clear input data
	subLevelData <- data.table(topId=character(),bottomId = character(),desc=character())
	
	topLevelName <- paste0("id_lv", idLevel-1)
	topURLName <- paste0("URL_lv", idLevel-1)
	descName <- paste0("description_lv", idLevel)
	
	for(i in 1:nrow(hierarchy)) {
		count <- count+1
		topLevelId <- str_trim(as.character(hierarchy[i,topLevelName,with=FALSE][[1]]))
		if(topLevelId != "") {
			tid <- system.time({
				
				## QUICK FIX FOR BUGS IN THE SCB API:
				if (!topLevelId %in% c("UF0502C")) {
					
					# Trim levels (some levels are returned with trailing whitespace)
					queryLevels <- scbGetLevels(topLevelId,descriptions=TRUE,quiet=FALSE)
				} else {
					queryLevels <- FALSE
				}
				
				if (queryLevels[[1]][1] != FALSE) {
					queryData <- data.table(
						topId = topLevelId,
						bottomId = as.character(queryLevels$id),
						desc = as.character(queryLevels$description)
					)
				} else {
					queryData <- data.table(
						topId = topLevelId,
						bottomId = "",
						desc = ""
					)
				}
				
				cat("Query",i,":    ",queryUrl,"\n")
				cat("Level",j-1, "value: ",topLevelId,"\n")
				cat("Level",j  , "values:",queryData$bottomId,"\n")
				
				subLevelData <- rbind(subLevelData,queryData)
			})
		} else {
			# Make sure the timestamp from the previous query doesn't cause us
			# to post the next query too soon
			tid <- c(0,0,0)
		}
		
		# Create time series object for analysis purposes
		timeSeries <- rbind(timeSeries,
							list(obs=count,
								 level=j,
								 id=topLevelId,
								 time=tid[[3]]))
		cat("Time taken for query:", tid[[3]],"\n\n")
		
		# Ensure we don't overdo our query limit (1/sec) by adding
		# 0.1-1.2 seconds sleep time, depending on past query processing time 
		if(tid[[3]] != 0) { Sys.sleep(1.2-min(tid[[3]],1.1)) }
	}
	
	setnames(subLevelData,
			 names(subLevelData),
			 c(topLevelName,bottomLevelName,descName))
	
	hierarchy <- merge(hierarchy,subLevelData,
	                   by = paste0(topLevelName),
	                   all.x = TRUE)
	
# 	emptyRows[[j]] <- hierarchy[get(bottomLevelName) == ""]
	tableAtLevel[[j]] <- hierarchy
	
	if(all(hierarchy[,bottomLevelName,with=FALSE] == "") | nrow(hierarchy) == 0) {
		stop("Reached bottom node in all data tree paths. Stopping.")
	}
	
# 	hierarchy <- hierarchy[get(bottomLevelName) != ""]
}

# Order columns
hierarchy <- hierarchy[,list(
	id_lv1,
	desc_lv1,
	id_lv2,
	desc_lv2,
	id_lv3,
	desc_lv3,
	id_lv4,
	desc_lv4)]

hierarchy_bk <- copy(hierarchy)
save(hierarchy,file="hierarchy.RData")

View(hierarchy)