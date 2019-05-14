"mylu.params"
bat.params <- read.csv("data-raw/paramUpdate_April2019.csv", stringsAsFactors = F, header = T,
                         row.names = 1)
mylu.params <- bat.params["MYLU",]
devtools::use_data(mylu.params, overwrite = T)

