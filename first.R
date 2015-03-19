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

# filename <- "2096.8aecfec50aa2ec00803e8200e0d91399" # without attachment
# filename <- "01336.82adb611b4bea7ae97c57911d3152cee" # with attachment
# txt <- readLines(filename, warn = FALSE)

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
  if (length(ct) != 0)
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
  if(grepl("^mv ", txt[1]))
    return(NULL)
  parts <- splitHeader(txt)
  header <- makeHeader(parts$header)
  result <- splitBody(parts$body, header)
  result$header <- header
  result
}

# message1 <- readMessage("2096.8aecfec50aa2ec00803e8200e0d91399")
# message2 <- readMessage("01336.82adb611b4bea7ae97c57911d3152cee")

setwd("~/Documents/R practice/Email Classifier/SpamAssassinTraining/easy_ham")
filenames <- list.files(getwd())
train_easy_ham<- vector("list", length(filenames))
for (i in 1:length(filenames)){
  train_easy_ham[i] <- list(readMessage(filenames[i]))
}
names(train_easy_ham) <- paste0("ham/", filenames)

setwd("~/Documents/R practice/Email Classifier/SpamAssassinTraining/easy_ham_2")
filenames <- list.files(getwd())
train_easy_ham_2<- vector("list", length(filenames))
for (i in 1:length(filenames)){
  train_easy_ham_2[i] <- list(readMessage(filenames[i]))
}
names(train_easy_ham_2) <- paste0("ham/", filenames)

setwd("~/Documents/R practice/Email Classifier/SpamAssassinTraining/hard_ham")
filenames <- list.files(getwd())
train_hard_ham<- vector("list", length(filenames))
for (i in 1:length(filenames)){
  train_hard_ham[i] <- list(readMessage(filenames[i]))
}
names(train_hard_ham) <- paste0("ham/", filenames)

setwd("~/Documents/R practice/Email Classifier/SpamAssassinTraining/spam")
filenames <- list.files(getwd())
train_spam<- vector("list", length(filenames))
for (i in 1:length(filenames)){
  train_spam[i] <- list(readMessage(filenames[i]))
}
names(train_spam) <- paste0("spam/", filenames)

setwd("~/Documents/R practice/Email Classifier/SpamAssassinTraining/spam_2")
filenames <- list.files(getwd())
train_spam_2<- vector("list", length(filenames))
for (i in 1:length(filenames)){
  train_spam_2[i] <- list(readMessage(filenames[i]))
}
names(train_spam_2) <- paste0("spam/", filenames)

training <- c(train_easy_ham, train_easy_ham_2, train_hard_ham, train_spam, train_spam_2)
