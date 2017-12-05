"mylu.params"
if("mylu.params.rda" %!in% list.files("data/")){
  bat.params <- read.csv("data-raw/batParamsFilled.csv", stringsAsFactors = F, header = T,
                         row.names = 1)
  mylu.params <- bat.params["mylu",]
  devtools::use_data(mylu.params, overwrite = T)
}
