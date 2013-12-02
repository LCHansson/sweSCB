#' Wrapper function to simply find and download data from SCB to the current R session. 
#' 
#' @param history keep the history when the function is running.
#' 
#' @seealso
#' \code{\link{scbGetMetadata}}, \code{\link{scbGetData}}
#' 
#' @export

findSCBdata<-function(history=FALSE){
  Node <- rSCB::scbGetMetadata() # Get top node
  allNodes <- list() # List to store nodes
  quit<-FALSE # Parameter indicating when to jump out of while loop

  # The main program
  while(!quit){ 
    # Generate header
    if(!history){cat("\014")}
    cat("CONTENT OF SCB API AT CURRENT (",length(allNodes)+1,") NODE LEVEL:\n",sep="") 
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
        .findScbData.Download(list(rSCB::scbGetMetadata(Node$id[as.numeric(inputValue)]),
                                   Node$id[as.numeric(inputValue)]))
        
        inputDownMore<-.findScbData.input(type="yesno",
            input="Do you want to download more data from SCB?")
        quit <- inputDownMore == "n"
        next()
      }
      allNodes[[length(allNodes)+1]]<-Node
      Node<-rSCB::scbGetMetadata(Node$id[as.numeric(inputValue)])
    }
  }
}

.findScbData.Download <- function(dataNode,...){
  dataNodeName<-dataNode[[2]]
  dataNode <- dataNode[[1]] 
  
  inputDown<-.findScbData.input(type="yesno",
      input=str_c("Do you want to download '",dataNodeName,"'?",sep=""))
  if(inputDown=="n"){return()}

  inputName<-.findScbData.input(type="text",
                                input="Load data.frame into R as (ex. myData):")  
  
  inputClean<-.findScbData.input(type="yesno",
      input="Do you want to clean and melt this file (to wide R format)?")
  cleanBool <- inputClean == "y"
  
  inputCode<-.findScbData.input(type="yesno",
                                 input="Do you want to print the code for downloading this data?")
  
  # Choose variables values
  varList<-list()
  varListText<-character(0)
  
  for(i in 1:length(dataNode$variables$variables)){
    listElem <- dataNode$variables$variables[[i]]
    varDF <- data.frame(id=listElem$values,text=listElem$valueTexts,stringsAsFactors=FALSE)
    
    varAlt <- .findScbData.input(type="alt", 
                                 input=list(varDF,
                                            listElem$text))
    
    if(varAlt[1] != "*"){
      tempAlt <- character(0)
      for (i in 1:length(varAlt)){ 
        index <- as.numeric(unlist(str_split(varAlt[i],pattern=":")))
        if(length(index) == 2){
          tempAlt <- c(tempAlt, listElem$values[index[1]:index[2]])
        }else{
          tempAlt <- c(tempAlt, listElem$values[index])
        }  
      }
    }else{
      tempAlt <- "*"
    }
    
    varList[[listElem$code]] <- tempAlt    
    varListText<-c(varListText,
                   str_c(listElem$code," = c('",
                         str_c(tempAlt,collapse="', '"),
                         "')",collapse=""))
  }
  
  cat("Downloading... ")
  tempData<-rSCB::scbGetData(dataNode$URL, varList, clean = cleanBool)
  cat("Done.\n")
  assign(inputName,value=tempData,envir=.GlobalEnv)
    
  if(inputCode == "y") {
    .findScbData.printCode(dataNode$URL,varListText,inputName,clean=cleanBool)
  }
}

.findScbData.inputBaseCat <- function(alt,codedAlt){
  output<-"\n(" 
  for (i in 1:length(alt)){
    if(i!=1){output <- str_join(output, ", ", sep="")}
    output <- str_join(output, "'", codedAlt[alt[i],1], "' = ",
                       codedAlt[alt[i],2], sep="")
  }
  return(str_join(output,")", sep=""))
}

.findScbData.input <- function(type=c("node","alt","yesno","text"),input=NULL){
  codedAlt <- data.frame(abbr=c("esc","b","*","y","n","a"),
                         name=c("Quit","Back","Select all","Yes","No","Show all"),
                         stringsAsFactors=FALSE)
  textTitle<-alt<-character(0)
  baseCat<-numeric(0)
  
  if(type=="node"){
    baseCat<-1:2
    alt<-rownames(input)
    textHead<-"\nEnter the data (number) you want to explore:"
  }
  
  if(type=="yesno"){
    baseCat<-c(1,4:5)
    textHead<-input
  }
  
  if(type=="text"){
    textHead<-input
  }
  
  if(type=="alt"){
    baseCat<-c(1,3,6)
    varDF<-input[[1]]
    alt<-rownames(varDF)
    # Calculate a short list of alternatives
    if(nrow(varDF)>11){
      varDFshort<-varDF[c(1:6,(nrow(varDF)-4):nrow(varDF)),]
      rownames(varDFshort)[6]<-"."
    }else{
      varDFshort<-varDF}

    textTitle <-str_join("\nALTERNATIVES FOR VARIABLE: ",toupper(input[[2]])," \n",
        str_join(rep("=",getOption("width")),collapse=""),"\n", sep="")    
    textHead <-
      str_join("Choose your alternative(s) by number:",
               "\nSeparate multiple choices by ',' and intervals by ':'",
               sep="")
  }

  inputOK <- FALSE
  inputScan <- ""
  
  while(!inputOK){
    cat(textTitle)
    if(type=="alt"){
      if(inputScan=="a"){toprint <- varDF}else{toprint <- varDFshort}
      .findScbData.printNode(xscb=toprint,print=TRUE)
    }
    cat(textHead)
    if(type!="text"){cat(.findScbData.inputBaseCat(baseCat,codedAlt),"\n")}
    
    inputScanRaw <- scan(what=character(), nmax=1, multi.line = FALSE, quiet=TRUE)
    if(length(inputScanRaw)==0) {next()}
    inputScan <- tolower(str_trim(unlist(str_split(string=inputScanRaw,pattern=","))))
    if(inputScan[1]=="a"){next()}
    if(type == "text") inputScan <- inputScanRaw
    
    inputOK <- 
      (length(inputScan) == 1 && inputScan %in% tolower(codedAlt$abbr[baseCat])) |
      all(inputScan %in% tolower(alt) | 
          str_detect(string=inputScan,pattern="[0-9]+:[0-9]+")) |
      type == "text"
        
    if(!inputOK){cat("Sorry, no such entry allowed. Please try again!\n")}
  } 
  return(inputScan)
}

.findScbData.printNode<-function(xscb,print=TRUE){
  xscb$text<-as.character(xscb$text)
  nSCBidlen<-max(str_length(as.character(xscb$id)))
  nSCBpos<-max(str_length(rownames(xscb)))
  nSCBconsole<-getOption("width")
  
  startPos<-nSCBpos+nSCBidlen+5
  scbTextSpace<-nSCBconsole-startPos
  finalText<-character(0) 
  for (i in 1:nrow(xscb)){
    if(rownames(xscb)[i]=="."){
      finalText <- str_join(finalText,"\n")
      next()
    }
    
    finalText <- str_join(
      finalText,
      rownames(xscb)[i],".",
      str_join(rep(" ",nSCBpos-str_length(rownames(xscb)[i])),collapse=""),
      " [",xscb$id[i],"]",
      str_join(rep(" ",nSCBidlen-str_length(as.character(xscb$id[i]))),collapse=""),
      " ",collapse="")
    
    first<-rerun<-TRUE
    tempText<-xscb$text[i]
    while(first | rerun){
      tempTextSpaces<-str_locate_all(tempText,pattern=" ")[[1]][,1]
      if(str_length(tempText) > scbTextSpace){
        tempTextCut<-max(tempTextSpaces[tempTextSpaces < scbTextSpace])-1
      }else{
        tempTextCut<-str_length(tempText)
        rerun<-FALSE
      }
            
      finalText <-
        str_join(finalText,
                 str_join(rep(" ",startPos*(1-as.numeric(first))),collapse=""),
                 str_sub(tempText,1,tempTextCut),"\n",collapse="")
      
      if(rerun){
        tempText<-str_sub(tempText,tempTextCut+2)
      }

      first<-FALSE      
    }
  }
  if(print){cat(finalText)}else{return(finalText)}
}

.findScbData.printCode <- function(url,varListText,inputName,clean){
  cat("To download the same data from SCB again, use the following code:\n\n")
  cat(inputName," <- \n  scbGetData(\"", url,"\",\n",
      rep(" ",13), "list(",sep="")

  for (i in 1:length(varListText)){
    if(i!=1){cat(rep(" ",18),sep="")}
    cat(varListText[i],sep="")
    if(i!=length(varListText)){cat(",\n",sep="")}
  }

  cat("),\n")
  cat(rep(" ",13), "clean = ",as.character(clean),sep="")
  cat(")\n\n")
}

