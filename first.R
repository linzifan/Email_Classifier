load("/Users/linzifan/Documents/R practice/Email Classifier/TrainingMessages.rda")
# example without attachment: 2096.8aecfec50aa2ec00803e8200e0d91399
trainMessages[[1]]$header
# example with attachment: 01336.82adb611b4bea7ae97c57911d3152cee
trainMessages[[15]]
length(trainMessages[[15]]$header)


setwd("~/Documents/R practice/Email Classifier")

list.files()[2] # extract name from a folder
file.info(list.files()[2])
test <- readLines(list.files()[2]) # read each line, form a character vector
# body is all the text after the first blank line following header and up to any attachment
ind <- which.max(test=="") # index number of the first blank line
# check without attachment
body <- test[-(1:ind)]




strsplit(test[2:(ind-1)],":")
write.dcf(test[2:(ind-1)])
class(read.dcf(list.files()[2]))
class(trainMessages[[1]]$header)
