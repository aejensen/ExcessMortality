library(brms)
rm(list=ls())
d <- read.csv("DeathPop.txt", sep=";")
d$year <- as.numeric(sapply(stringr::str_split(d$ISOweek, "-"), \(q) q[1]))
d$week <- sapply(stringr::str_split(d$ISOweek, "-"), \(q) q[2])
d$week <- as.numeric(stringr::str_sub(d$week, 2))
d$time <- d$year + d$week / 53
d$prop <- (d$observed / d$N) * 1000

dTotal <- subset(d, d$group == "Total")
dTotal$N <- as.integer(dTotal$N)

m_global1 <- brm(bf(observed | trials(N) ~ s(time) + s(week, bs="cc")), 
                 data = dTotal, 
                 family = binomial(),
                 cores = 4, 
                 iter = 2 * 10^4,
                 seed = 12345)
m_global1 <- add_criterion(m_global1, "loo")

m_global1.5 <- brm(bf(observed | trials(N) ~ s(time) + s(week, bs="cc")), 
                 data = dTotal, 
                 family = beta_binomial(),
                 cores = 4, 
                 iter = 2 * 10^4,
                 seed = 12345)

m_global2 <- brm(bf(prop ~ s(time) + s(week, bs="cc")), 
                 data = dTotal, 
                 family = gaussian(),
                 cores = 4, 
                 iter = 2 * 10^4,
                 seed = 12345)
m_global2 <- add_criterion(m_global2, "loo")

m_global3<- brm(bf(prop ~ s(time) + s(week, bs="cc"),
                    sigma ~ s(time) + s(week, bs="cc")), 
                 data = dTotal, 
                 family = gaussian(),
                 cores = 4, 
                 iter = 2 * 10^4,
                 seed = 12345)
m_global3 <- add_criterion(m_global3, "loo")

m_global4 <- brm(bf(prop ~ s(time) + s(week, bs="cc"),
                   sigma ~ s(time) + s(week, bs="cc"),
                   autocor = ~ar(p = 1)), 
                data = dTotal, 
                family = gaussian(),
                cores = 4, 
                iter = 2 * 10^4,
                seed = 12345,
                control = list(adapt_delta = 0.99))



loo_compare(m_global2, m_global3)

m_local1 <- brm(bf(observed | trials(N) ~ s(week, bs="cc")), 
                data = dTotal, 
                family = binomial(),
                cores = 4, 
                iter = 2 * 10^4,
                seed = 12345)
m_local1 <- add_criterion(m_local1, "loo")

m_local1.5 <- brm(bf(observed | trials(N) ~ s(week, bs="cc")), 
                   data = dTotal, 
                   family = beta_binomial(),
                   cores = 4, 
                   iter = 2 * 10^4,
                   seed = 12345)

m_local2 <- brm(bf(prop ~ s(week, bs="cc")), 
                data = dTotal, 
                family = gaussian(),
                cores = 4, 
                iter = 2 * 10^4,
                seed = 12345)
m_local2 <- add_criterion(m_local2, "loo")

m_local3 <- brm(bf(prop ~ s(week, bs="cc"),
                   sigma ~ s(week, bs="cc")), 
                data = dTotal, 
                family = gaussian(),
                cores = 4, 
                iter = 2 * 10^4,
                seed = 12345)
m_local3 <- add_criterion(m_local3, "loo")

loo_compare(m_local2, m_local3)

save.image(file = "fits.RData")


######################

ci <- \(x, lower, upper, col = "gray") {
  polygon(c(x, rev(x)), c(lower, rev(upper)), col = col, border = NA)
}

post_pred <- predict(m_global1, summary = FALSE)
plot(observed ~ time, dTotal, pch=19, cex=0.4, type="n",
     ylim=c(900,1600), xlim=c(2017, 2023), xlab="Time", ylab="#deaths")
ci(dTotal$time, apply(post_pred, 2, quantile, 0.025), apply(post_pred, 2, quantile, 0.975))
lines(dTotal$time, apply(post_pred, 2, mean), lwd = 2)
points(observed ~ time, data = dTotal, pch=19, cex=0.5)


post_pred <- predict(m_local3, summary = FALSE)
plot(observed ~ time, dTotal, pch=19, cex=0.4, type="n",
     ylim=c(900,1600), xlim=c(2017, 2023), xlab="Time", ylab="#deaths")
ci(dTotal$time, apply(post_pred, 2, quantile, 0.025), apply(post_pred, 2, quantile, 0.975))
lines(dTotal$time, apply(post_pred, 2, mean), lwd = 2)
points(observed ~ time, data = dTotal, pch=19, cex=0.5)
