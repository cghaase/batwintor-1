f.path <- file.path("C:","Users","crhranac","Dropbox","ShareDocs")

n.mylu <- read.csv(paste0(f.path,"/Update","MYLU",".csv"))
n.mymy <- read.csv(paste0(f.path,"/Update","MYMY",".csv"))
n.epfu <- read.csv(paste0(f.path,"/Update","EPFU",".csv"))

f.mylu <- read.csv(paste0(f.path,"/Former","MYLU",".csv"))
f.mymy <- read.csv(paste0(f.path,"/Former","MYMY",".csv"))
f.epfu <- read.csv(paste0(f.path,"/Former","EPFU",".csv"))

diff.mylu <- as.data.frame(cbind(time = f.mylu$time, diff = f.mylu$g.fat.consumed - n.mylu$g.fat.consumed))
diff.mymy <- cbind(time =as.data.frame(f.mymy$time,diff =  f.mymy$g.fat.consumed - n.mymy$g.fat.consumed))
diff.epfu <- as.data.frame(cbind(time =f.epfu$time,diff =  f.epfu$g.fat.consumed - n.epfu$g.fat.consumed))

plot(diff.mylu$time,diff.mylu$diff)
