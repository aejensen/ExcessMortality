library(brms)

models <- vector("list", 20)
for(k in 1:20) {
  load(paste0("m", k, ".RData"))
  models[[k]] <- m
}

ppavg <- pp_average(models[[1]], models[[2]], models[[3]], models[[4]],
                    models[[5]], models[[6]], models[[7]], models[[8]],
                    models[[9]], models[[10]], models[[11]], models[[12]],
                    models[[13]], models[[14]], models[[15]], models[[16]],
                    models[[17]], models[[18]], models[[19]], models[[20]],
                    summary = FALSE)

save(ppavg, file="postavg.RData")
