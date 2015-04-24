library('RCurl')
library("jsonlite")

loadApiKey <- function(){
  if(! exists("apiKey") ){
    apiKey <<- as.character(  read.table("football-data_apikey.txt")[1,1])
  }
  return(apiKey)
}

createRequestUrl <- function( request ){
  return(paste( "http://api.football-data.org/alpha/", request, sep = ""))  
}

executeRequest <-function(requestUrl){
  response = getURL(requestUrl, httpheader = paste("\"X-Auth-Token: ",loadApiKey(), "\"") )
  return(response)
}

createAndExecuteRequest <- function(request, pretty = TRUE){
   requestUrl = createRequestUrl(request)
   response = executeRequest(requestUrl)
   if(pretty){
     response = prettify(response)
   }
   return(response)
}

