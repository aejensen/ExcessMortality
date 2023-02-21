library(brms)
rm(list=ls())
d <- read.csv("../DeathPop.txt", sep=";")
d$year <- as.numeric(sapply(stringr::str_split(d$ISOweek, "-"), \(q) q[1]))
d$week <- sapply(stringr::str_split(d$ISOweek, "-"), \(q) q[2])
d$week <- as.numeric(stringr::str_sub(d$week, 2))
d$time <- d$year + d$week / 53
d$prop <- (d$observed / d$N) * 1000

dTotal <- subset(d, d$group == "Total")
dTotal$N <- as.integer(dTotal$N)

order <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

m <- brm(bf(prop ~ s(time) + s(week, bs="cc"),
            sigma ~ s(time) + s(week, bs="cc"),
            autocor = substitute(~ar(p = order), list(order = order))),
         data = dTotal, 
         family = gaussian(),
         cores = 4, 
         iter = 2 * 10^4,
         seed = 12345,
         control = list(adapt_delta = 0.99))

save(m, file = paste0("m", order, ".RData"))

#sbatch --partition=long -J "AR" -o AR-%a.txt --array=1-20 --wrap='Rscript fit.R'
