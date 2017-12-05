"bat.params"
if("bat.params.rda" %!in% list.files("data/")){
 #coolated data from study and literature. Some missing values were filled where needed
  bat.params <- read.csv("data-raw/batParamsFilled.csv", stringsAsFactors = F, header = T,
                  row.names = 1)

  devtools::use_data(bat.params, overwrite = T)
}
