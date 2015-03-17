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
# example 2096.8aecfec50aa2ec00803e8200e0d91399.txt
filename <- "2096.8aecfec50aa2ec00803e8200e0d91399"
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

