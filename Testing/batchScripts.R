# Batch Script functions
library(batwintor)
WNS_run <- function(species, out.name){
  data("bat.params")
  bat.p <- BatLoad(x = bat.params, species)

  data("fung.params")
  fung.ch <- FungSelect(choose =  "Chaturvedi")

  env <- BuildEnv(temp = c(-5,20),
                  pct.rh = c(50,100),
                  range.res = .5,
                  twinter = 6,
                  winter.res = 24)
  z <- DynamicEnergyPd(env = env,
                  bat.params = bat.p,
                  fung.params = fung.ch)
  write.csv(z, file = out.name, row.names = F)

}
WNS_batch <- function(species.l, out.name){

  out.name <- file.path(out.name,
                        paste0(species.l, "DNEpd.csv"))
  library(snowfall)
  sfInit(parallel=TRUE, cpus = 3)
  sfLibrary('batwintor', character.only = T)
  sfExportAll()
  sfLapply(species.l, WNS_run,
           out.name = out.name)
  sfStop(nostop = FALSE)

}

species.l <- row.names(bat.params)
out.name <- file.path("D:", "Dropbox",
                      "batwintor_aux", "Run_10_12")
WNS_batch(species.l = species.l,
          out.name = out.name)
spec <- species.l[[1]]
WNS_run(spec, out.name)
