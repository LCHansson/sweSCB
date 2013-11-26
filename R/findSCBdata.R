# To do: 
# Lägg in att välja variabler
# Lägg in assertions
# Lägg in att clean and convert med reshape/plyr



#' Wrapper function to simply find and download data from SCB to the current R session.
#' 
#' @param history keep the history when the function is running.
#' 
#' @examples
#' 
#' @export

findSCBdata<-function(history=FALSE){
  Node <- rSCB::scbGetMetadata() # Get top node
  allNodes <- list() # List to store nodes
  quit<-FALSE # Parameter indicating when to jump out of while loop

  while(!quit){ # The main program
    # Generate header
    if(!history){cat("\014")}
    cat("CONTENT OF SCB API AT CURRENT (",length(allNodes)+1,") LEVEL:\n",sep="") 
    cat(rep("=",getOption("width")),"\n",sep="") 
    
    # Print information in node and ask for choice
    .findScbData.printNode(Node)
    inputValue<-.findScbData.input(type="node",input=Node)

    if(inputValue=="q"){quit<-TRUE;next()}

    if(inputValue=="b"){
      if(length(allNodes)==0){next()}
      Node <- allNodes[[length(allNodes)]]
      allNodes[[length(allNodes)]] <- NULL
    }
    
    if(str_detect(inputValue,pattern="[0-9]+")){
      if(Node$type[as.numeric(inputValue)]=="t"){
        .findScbData.Download(scbGetMetadata(Node$id[as.numeric(inputValue)]))
        
        inputDownMore<-.findScbData.input(type="yesno",
            input="\nDo you want to download more data from SCB?")
        quit <- inputDownMore == "n"
        next()
      }
      allNodes[[length(allNodes)+1]]<-Node
      Node<-rSCB::scbGetMetadata(Node$id[as.numeric(inputValue)])
    }
  }
}


.findScbData.Download <- function(dataNode){
  inputDown<-.findScbData.input(type="yesno",
      input="\nDo you want to download this file?")
  if(inputDown=="n"){return()}

  inputClean<-.findScbData.input(type="yesno",
      input="\nDo you want to clean this file (to R format)?")
  clean <- inputClean == "y"

  inputName<-.findScbData.input(type="text",
      input="\nLoad data.frame into R as (ex. myData):")
  
  # The rest is to do
    varList<-list()
    varListText<-c("")
    for(listElem in dataNode$variables$variables){
      varList[[listElem$code]]<-"*"
      varListText<-c(varListText,str_c(listElem$code," = \"*\", ",sep=""))
    }
    varListText[length(varListText)]<-str_sub(varListText[length(varListText)],1,
                         str_length(varListText[length(varListText)])-2)
    varListText <- varListText[-1]

    tempData<-scbGetData(dataNode$URL, varList)
    if(clean){tempData<-scbCleanData(tempData)}
    assign(inputName,value=scbGetData(dataNode$URL, varList),envir=.GlobalEnv)
    
    cat("The data has been downloaded and loaded into R with the following code:\n\n")
    cat(inputName," <- \n  scbGetData(\"", dataNode$URL,"\",\n",
        rep(" ",13), "list(",sep="")
    for (i in 1:length(varListText)){
      if(i!=1){cat(rep(" ",18),sep="")}
      cat(varListText[i],sep="")
      if(i!=length(varListText)){cat("\n",sep="")}
    }
    cat("))\n")
    if(clean){
      cat(inputName," <- scbCleanData(",inputName,")\n",sep="")
    } 
    cat("\n",sep="")
}

.findScbData.inputBaseCat <- function(alt){
  output<-"\n(" 
  for (i in 1:length(alt)){
    if(i!=1){output <- str_join(output,", ",sep="")}
    output <- str_join(output,"'",codedAlt[alt[i],1],"' = ",
                       codedAlt[alt[i],2],sep="")
  }
  return(str_join(output,")",sep=""))
}


.findScbData.input <- function(type=c("node","alt","yesno","text"),input=NULL){
  codedAlt<-data.frame(abbr=c("q","b","*","y","n"),
                       name=c("Quit","Back","All","Yes","No"),
                       stringsAsFactors=FALSE)
  
  if(type=="node"){
    baseCat<-1:2
    alt<-as.character(1:nrow(input))
    textHead<-str_join("\nEnter the data (number) you want to explore:",
                       .findScbData.inputBaseCat(baseCat),sep="")
  }
  
  if(type=="yesno"){
    baseCat<-4:5
    textHead<-str_join(input,.findScbData.inputBaseCat(baseCat),sep="")
  }
  
  if(type=="text"){
    textHead<-input
  }
  
  if(type=="alt"){
    baseCat<-1:3
    alt<-input
    altText <- .findScbData.printAltText(alt)
    textHead <-
      str_join("\nChoose your alternative(s), separate multiple choices by ','",
      "\nAlternatives: ", altText,.findScbData.inputBaseCat(baseCat),sep="")
  }

  inputOK <- FALSE
  
  while(!inputOK){
    cat(textHead)
    input<-scan(what=character(),nmax=1,multi.line = FALSE,quiet=TRUE)    
    input<-tolower(str_trim(unlist(str_split(string=input,pattern=","))))
    inputOK<-all(input%in%tolower(c(alt,codedAlt$abbr[baseCat]))) | type == "text"
    if(!inputOK){cat("Sorry, no such entry allowed. Try again!")}
  }
  return(input)
}

.findScbData.printAltText<-function(alt){  
  alt <- as.character(alt)
  noAlt <- length(alt)
  noPos <- getOption("width") - 30
  if (noAlt==1){
    altText<-alt
  }else if(noAlt==2){
    altText<-str_join(alt,collapse=", ")
  }else{
    altTextFirst <- altTextLast <- c("")
    first <- TRUE
    while(sum(str_length(c(altTextFirst,altTextLast))) < noPos & length(alt) > 0){
      if(first){
        altTextFirst <- str_join(c(altTextFirst,alt[1]),collapse=", ")
        alt <- alt[-1]
      }else{
        altTextLast <- str_join(c(alt[length(alt)],altTextLast),collapse=", ")
        alt <- alt[-length(alt)]
      }
      first<-!first
    }
    altTextFirst <- str_sub(altTextFirst,start=3)
    altTextLast <- str_sub(altTextLast,end=str_length(altTextLast)-2)
    if(length(alt)==0){diff<-", "}else{diff<-" ... "}
    altText<-str_join(c(altTextFirst,altTextLast),collapse=diff)
  }
  return(altText)
}
  
.findScbData.printNode<-function(xscb){
  xscb$text<-as.character(xscb$text)
  nSCBidlen<-max(str_length(as.character(xscb$id)))
  nSCBpos<-floor(log(x=nrow(xscb),10))+1
  nSCBconsole<-getOption("width")
  
  startPos<-nSCBpos+nSCBidlen+5
  scbTextSpace<-nSCBconsole-startPos
  
  for (i in 1:nrow(xscb)){
    cat(i,".",rep(" ",nSCBpos-str_length(as.character(i))),
        " [",xscb$id[i],"]",rep(" ",nSCBidlen-str_length(as.character(xscb$id[i]))),
        " ",
        sep="")
    
    first<-rerun<-TRUE
    tempText<-xscb$text[i]
    while(first | rerun){
      tempTextSpaces<-str_locate_all(tempText,pattern=" ")[[1]][,1]
      if(str_length(tempText)>scbTextSpace){
        tempTextCut<-max(tempTextSpaces[tempTextSpaces < scbTextSpace])-1
      }else{
        tempTextCut<-str_length(tempText)
        rerun<-FALSE
      }
            
      cat(rep(" ",startPos*(1-as.numeric(first))),
          str_sub(tempText,1,tempTextCut),"\n",sep="")
      
      if(rerun){
        tempText<-str_sub(tempText,tempTextCut+2)}

      first<-FALSE      
    }
  }
}


  



