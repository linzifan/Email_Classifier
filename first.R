# RESULTS - Used to check
load("/Users/linzifan/Documents/R practice/Email Classifier/TrainingMessages.rda")
# example without attachment: 2096.8aecfec50aa2ec00803e8200e0d91399
trainMessages[[1]]
# example with attachment: 01336.82adb611b4bea7ae97c57911d3152cee
trainMessages[[15]]
length(trainMessages[[15]]$header)
class(trainMessages[[15]]$attachment$`1`)


# Explore step by step
setwd("~/Documents/R practice/Email Classifier/SpamAssassinTraining/easy_ham")

filename <- "2096.8aecfec50aa2ec00803e8200e0d91399" # without attachment
filename <- "01336.82adb611b4bea7ae97c57911d3152cee" # with attachment
txt <- readLines(filename, warn = FALSE)

splitHeader <- function(txt){
  # body is all the text after the first blank line following header and up to any attachment
  isblank <- txt == ""
  ind <- which(isblank)[1] # index number of the first blank line
  # check whether there is a "From Line" at the beginning
  if(grepl("^From ", txt[1])) {
    fromLine <- txt[1]
    start <- 2
  } else {
    fromLine <- ""
    start <- 1
  }
  list(header = txt[start:(ind - 1)],
       body = txt[-(1:ind)],
       fromLine = fromLine)
}

# splitHeader(txt)



makeHeader <- function(txt, asVector = TRUE){
  con <- textConnection(txt)
  on.exit(close(con))    
  h <- read.dcf(con, all = TRUE)  # a data frame with one row
  # turn it into a character vector
  if (asVector){
    structure(unlist(h),
              names = rep(names(h), sapply(h, function(x)
                if(is.list(x))
                  length(x[[1]])
                else
                  1L)))
  } else {h}
}

# makeHeader(splitHeader(txt)$header)



getContentType <- function(header){
  i <- match("content-type", tolower(names(header)))
  if (is.na(i)){
    return(character())
  } else return(header[[i]])
}

# ContentType <- getContentType(header)



getBoundaryMarker <- function(ContentType){
  rx <- "(boundary|BOUNDARY)="
  els <- strsplit(ContentType, ";[[:space:]]*")[[1]]
  val <- grep(rx, els, value = TRUE)
  gsub("(^[\"']|[\"']$)", "", gsub(rx, "",  val))
}

# getBoundaryMarker(ContentType)


splitBody <- function(body, header){
  ct <- getContentType(header)
  boundary <- getBoundaryMarker(ct)
  
  if (length(ct) == 0 || !grepl("boundary", tolower(ct)))
    return(list(body = body))
 
  isStart <- (body %in% c(sprintf("--%s", boundary), sprintf("--%s--", boundary)))
  
  if(!any(isStart)) {
    i <- agrep(paste0("--", boundary), body)
    if(length(i))
      isStart[i] <- TRUE
    else
      return(list(body = body))
  }
  
  textbody <- character()
  endMarker <- which(body == sprintf("--%s--", boundary))
  
  pieces <- split(body, cumsum(isStart))
  if(!isStart[1]) {
    textbody <-  pieces[[1]]
    pieces <- pieces[-1]
  }
  
  if(length(endMarker)) {
    textbody <- c(textbody, pieces[[length(pieces)]][-1])
    pieces <- pieces[ - length(pieces) ]
  }
  
  atts <- lapply(pieces, makeAttachment, boundary)  
  return(list(body = textbody, attachments = atts))
}

# body <- splitHeader(txt)$body
# header <- makeHeader(splitHeader(txt)$header)
# splitBody(body, header)


makeAttachment <- function(pieces, boundary){
  if(paste0("--", boundary) == pieces[1] || length(agrep(paste0("--", boundary), pieces[1])))
    pieces = pieces[-1]
  i <- which(pieces != "")
  if(length(i) == 0 || i[1] > 1) 
    return(list(header = character(), body = pieces))
  
  parts <- splitHeader(pieces)
  list(header = makeHeader(parts$header),
       body = parts$body)
}


readMessage <- function(filename){
  txt <- readLines(filename, warn = FALSE)
  parts <- splitHeader(txt)
  header <- makeHeader(parts$header)
  result <- splitBody(parts$body, header)
  result$header <- header
  result
}

message1 <- readMessage("2096.8aecfec50aa2ec00803e8200e0d91399")
message2 <- readMessage("01336.82adb611b4bea7ae97c57911d3152cee")
