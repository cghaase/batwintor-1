"myca.mod"
if("myca.mod.rda" %!in% list.files("data/")){
  myca.mod <- data.table::fread("raw_data/mycaMod.csv")
  devtools::use_data(myca.mod, overwrite = T)
}
