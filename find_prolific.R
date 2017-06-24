load("email_data.Rdata")
library(plyr)
count <- count(newdf$sender)
names(count) <- c("sender", "freq")
count <- count[with(count, order(freq, decreasing = T)),]
prolific <- count[1:100,]
save(prolific, file = "prolific.RData")
