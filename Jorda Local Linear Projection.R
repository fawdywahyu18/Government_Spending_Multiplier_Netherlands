library(lpirfs)
library(vars)
library(ggplot2)
library(readxl)
library(dplyr)
library(zoo)
library(gridExtra)
library(ggpubr)

setwd("D:/Kuliah/Master/Research Seminar/Data Nederland")
jorda.raw <- read_excel("r_data.xlsx", sheet = "seasonally adjusted")
jorda.growth <- read_excel("r_data.xlsx", sheet = "growth")

# Generating shock variables
rgdp_pot6 <- jorda.raw[c("rgdp_pot6")]
bb <- (jorda.raw[c("rgov_exp")] - jorda.raw[c("rgov_rev")])/jorda.raw[c("rgdp_pot6")]
bp <-  jorda.raw[c("rgov")] / jorda.raw[c("rgdp_pot6")] * jorda.raw[c("pop")]
rgdp_growth <- jorda.raw[c("rgdp_growth")]

rgdp_growth_app <- approx(rgdp_growth, n=104)
for(i in 1:4) {
  rgdp_growth[i,1] <- rgdp_growth_app$y[i]
}

rgdpg_7ma_mod <- data.frame(rollmean(rgdp_growth, k=7))
add1 <- data.frame(rep(0, 6))
names(add1) <- "rgdp_growth"
rgdpg_7ma_ <- rbind(add1, rgdpg_7ma_mod)

rgdpg_4ma <- data.frame(rollmean(rgdp_growth, k=4))
add2 <- data.frame(rep(0, 3))
names(add2) <- "rgdp_growth"
rgdpg_4ma_ <- rbind(add2, rgdpg_4ma)

rgdpg_5ma <- jorda.raw[c("rgdpg_5ma")]
rgdpg_7ma <- jorda.raw[c("rgdpg_7ma")]
# bb <- lag(bb, n=4)
rec_date <- jorda.raw[c("fdum_reces")]
rec_date1 <- jorda.raw[c("rec_date1")]
rec_date2 <- jorda.raw[c("rec_date2")]
rec_date3 <- jorda.raw[c("rec_date3")]

bb.approx <- approx(bb, n=104)
bp.approx <- approx(bp, n=104)
rgdpg_5ma.app <- approx(rgdpg_5ma, n=104)
rgdpg_7ma.app <- approx(rgdpg_7ma, n=104)

for(i in 1:4) {
  bp[i,1] <- bp.approx$y[i]
  rgdpg_5ma[i,1] <- rgdpg_5ma.app$y[i]
  rgdpg_7ma[i,1] <- rgdpg_7ma.app$y[i]
  rec_date[i,1] <- 1 
  bb[i,1] <- bb.approx$y[i]
}

# for(i in 1:8) {
# bb[i,1] <- bb.approx$y[i]
# }

# Shock variables are : 1) bp (Blanchard-Perotti)
#                       2) bb (Balance budget lag 4)
#                       3) rgdpg_5ma
#                       4) rgdpg_7ma

# Generating state variables
unmp <- as.matrix(jorda.raw[c("unmp")])
unmp_hp <- hp_filter(unmp, lambda = 1000000)
unmp_trend <- unmp_hp[[2]]

jorda.raw <- jorda.raw %>%
  mutate(if_else(unmp>6.5,1,0)) %>%
  mutate(if_else(unmp>unmp_trend,1,0))

unmp_dum1 <- jorda.raw[,18]
colnames(unmp_dum1) <- c("dummy_6.5")
unmp_dum2 <- jorda.raw[,19]
colnames(unmp_dum2) <- c("dummy_hp")

sum(unmp_dum1==1)
sum(unmp_dum1==0)
sum(unmp_dum2==1)
sum(unmp_dum2==0)
sum(rec_date==1)
sum(rec_date==0)

# State variables: 1) unmp_dum1
#                  2) unmp_dum2
#                  3) rec_date
#                  4) rgdpg_5ma - 0.8

# Generating endogenous variables
rgdp <- log(jorda.raw[c("rgdp")]/jorda.raw[c("pop")])
rgov <- log(jorda.raw[c("rgov")]/jorda.raw[c("pop")])
rtax <- log(jorda.raw[c("rtax")]/jorda.raw[c("pop")])

rgdp.as <- rgdp/rgdp_pot6
rgov.as <- rgov/rgdp_pot6
rtax.as <- rtax/rgdp_pot6

rgdp.g1 <- diff(log(jorda.raw[,"rgdp"]))
rgov.g1 <- diff(log(jorda.raw[,"rgov"]))
rtax.g1 <- diff(log(jorda.raw[,"rtax"]))

rgdp.g <- jorda.growth[,"gdp"]/100
rgov.g <- jorda.growth[,"gov"]/100
rtax.g <- jorda.growth[,"tax"]/100

# rgdp.approx <- approx(rgdp, n=104)
# rgov.approx <- approx(rgov, n=104)
# rtax.approx <- approx(rtax, n=104)

# for(i in 1:4) {
  #rgdp[i,1] <- rgdp.approx$y[i]
  #rgov[i,1] <- rgov.approx$y[i]
  #rtax[i,1] <- rtax.approx$y[i]
#}

endog_data <- cbind(rgov, rtax, rgdp)
endog_data2 <- cbind(rgov.g, rtax.g, rgdp.g)
endog_data3 <- cbind(rgov.g1, rtax.g1, rgdp.g1)
endog_data4 <- cbind(rgov.as, rtax.as, rgdp.as)
endog_data.ts <- ts(endog_data, star=c(1995,1), end=c(2020,4), frequency = 4)

# Interbank rate and unmp rate
int <- jorda.raw[c("int")]
unmp <- jorda.raw[c("unmp")]

endog_data4 <- endog_data4[5:104,]
rgdpg_4ma_ <- data.frame(rgdpg_4ma_[5:104,])
rgdpg_7ma_ <- data.frame(rgdpg_7ma_[5:104,])
int <- data.frame(int[5:104,])
unmp <- data.frame(unmp[5:104,])
rec_date <- data.frame(rec_date1[5:104,])
rgdpg_7ma <- data.frame(rgdpg_7ma[5:104,])

# Shock linear
shockl <- data.frame(endog_data4[,1])

# Estimate linear model
results_lin_iv <- lp_lin_iv(endog_data = endog_data4, lags_endog_lin = 4,
                            shock = shockl, trend = 0,
                            confint = 1.96, hor = 25)
summary(results_lin_iv)

multipliers <- function(x) {
  response.df <- data.frame(x)
  gov_sum_4 <- rowSums( response.df[1,1:4] )
  gov_sum_8 <- rowSums( response.df[1,1:8] )
  gov_sum_16 <- rowSums( response.df[1,1:16] )
  gdp_sum_4 <- rowSums( response.df[3,1:4] )
  gdp_sum_8 <- rowSums( response.df[3,1:8] )
  gdp_sum_16 <- rowSums( response.df[3,1:16] )
  
  multiplier_4 <- gdp_sum_4/gov_sum_4
  multiplier_8 <- gdp_sum_8/gov_sum_8
  multiplier_16 <- gdp_sum_16/gov_sum_16
  multipliers <- data.frame(multiplier_4, multiplier_8, multiplier_16)
  return(multipliers)
}

# Table function
table_func <- function(x, y, z, x1, y1, z1, x2, y2, z2) {
  tabel.mul <- matrix(nrow = 9, ncol = 3)
  tabel.mul[1,1] <- x$multiplier_4
  tabel.mul[2,1] <- y$multiplier_4
  tabel.mul[3,1] <- z$multiplier_4
  tabel.mul[4,1] <- x$multiplier_8
  tabel.mul[5,1] <- y$multiplier_8
  tabel.mul[6,1] <- z$multiplier_8
  tabel.mul[7,1] <- x$multiplier_16
  tabel.mul[8,1] <- y$multiplier_16
  tabel.mul[9,1] <- z$multiplier_16
  
  tabel.mul[1,2] <- x1$multiplier_4
  tabel.mul[2,2] <- y1$multiplier_4
  tabel.mul[3,2] <- z1$multiplier_4
  tabel.mul[4,2] <- x1$multiplier_8
  tabel.mul[5,2] <- y1$multiplier_8
  tabel.mul[6,2] <- z1$multiplier_8
  tabel.mul[7,2] <- x1$multiplier_16
  tabel.mul[8,2] <- y1$multiplier_16
  tabel.mul[9,2] <- z1$multiplier_16
  
  tabel.mul[1,3] <- x2$multiplier_4
  tabel.mul[2,3] <- y2$multiplier_4
  tabel.mul[3,3] <- z2$multiplier_4
  tabel.mul[4,3] <- x2$multiplier_8
  tabel.mul[5,3] <- y2$multiplier_8
  tabel.mul[6,3] <- z2$multiplier_8
  tabel.mul[7,3] <- x2$multiplier_16
  tabel.mul[8,3] <- y2$multiplier_16
  tabel.mul[9,3] <- z2$multiplier_16
  tabel.mul <- data.frame(tabel.mul)
  return(tabel.mul)
}

# Multipliers (Linear)
multipliers_mean <- multipliers(results_lin_iv$irf_lin_mean)
multipliers_up <- multipliers(results_lin_iv$irf_lin_up)
multipliers_low <- multipliers(results_lin_iv$irf_lin_low)

# Standard error (Linear)
se_lin <- (multipliers_up - multipliers_mean)/1.96

# Make and save linear plots
iv_lin_plots <- plot_lin(results_lin_iv)

# Nonlinear shock using Blanchard - Perotti Shock
shocknl <- shockl
#               endog_data2 <- cbind(endog_data["rgov"], endog_data["rgdp"])
# Use moving average growth rate of GDP as exogenous variable
exog_data <- rgdpg_7ma
# Use OECD recession date as switching variable
switching_variable <- if_else(rec_date >= 1, 1, 0)
# Estimate nonlinear model
results_nl_iv <- lp_nl_iv(endog_data = endog_data4, lags_endog_nl = 4,
                          shock = shocknl, 
                          # exog_data = exog_data, lags_exog = 4, 
                          trend = 0,
                          confint = 1.96, hor = 25,
                          switching = switching_variable, use_hp = FALSE, use_logistic = FALSE,
                          lag_switching = FALSE)

# Multipliers in non linear (recession dates)
multipliers_s1_up_rec <- multipliers(results_nl_iv$irf_s1_up)
multipliers_s1_mean_rec <- multipliers(results_nl_iv$irf_s1_mean)
multipliers_s1_low_rec <- multipliers(results_nl_iv$irf_s1_low)

multipliers_s2_up_rec <- multipliers(results_nl_iv$irf_s2_up)
multipliers_s2_mean_rec <- multipliers(results_nl_iv$irf_s2_mean)
multipliers_s2_low_rec <- multipliers(results_nl_iv$irf_s2_low)

multipliers_s1_mean_rec
multipliers_s2_mean_rec

# Standard error (rec_date)
se_s1_rec <- abs((multipliers_s1_up_rec - multipliers_s1_mean_rec)/1.96)
se_s2_rec <- abs((multipliers_s2_up_rec - multipliers_s2_mean_rec)/1.96)

# Creating table
table1 <- table_func(multipliers_mean, multipliers_up, multipliers_low,
                     multipliers_s1_mean_rec, multipliers_s1_up_rec, multipliers_s1_low_rec,
                     multipliers_s2_mean_rec, multipliers_s2_up_rec, multipliers_s2_low_rec)

table1 <- cbind(c("mean", "upper", "lower", "mean", "upper", "lower", "mean", "upper", "lower"), table1)
colnames(table1) <- c("Confidence Interval", "Linear", "Expansion periods", "Recession periods")

# Creating IRF graph (one IRF graph)
# Response of rgov and rgdp
rata2 <- results_lin_iv$irf_lin_mean[1,]
lower <- results_lin_iv$irf_lin_low[1,]
upper <- results_lin_iv$irf_lin_up[1,]
period <- seq(1:length(rata2))
kategori1 <- rep(1, length(rata2))
part1 <- data.frame(cbind(period, rata2, lower, upper, kategori1))
names(part1) <- c("period", "mean", "lower", "upper", "group")
part1$group <- "linear"
part1_mean <- part1 %>%
  select(period, mean, group)

rata2 <- results_nl_iv$irf_s1_mean[1,]
lower <- results_nl_iv$irf_s1_low[1,]
upper <- results_nl_iv$irf_s1_up[1,]
period <- seq(1:length(rata2))
kategori2 <- rep(2, length(rata2))
part2 <- data.frame(cbind(period, rata2, lower, upper, kategori2))
names(part2) <- c("period", "mean", "lower", "upper", "group")
part2$group <- "expansion"
part2_mean <- part2 %>%
  select(period, mean, group)

rata2 <- results_nl_iv$irf_s2_mean[1,]
lower <- results_nl_iv$irf_s2_low[1,]
upper <- results_nl_iv$irf_s2_up[1,]
period <- seq(1:length(rata2))
kategori3 <- rep(3, length(rata2))
part3 <- data.frame(cbind(period, rata2, lower, upper, kategori3))
names(part3) <- c("period", "mean", "lower", "upper", "group")
part3$group <- "recession"
part3_mean <- part3 %>%
  select(period, mean, group)

library(ggplot2)
responses <- rbind(part2, part3)
responses_mean <- rbind(part1_mean, part2_mean, part3_mean)

ggplot(responses_mean, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_line()

ggplot(responses, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()

ggplot(part1, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()




# Make and save nonlinear plots
plots_nl_iv <- plot_nl(results_nl_iv)
# Make list to save all plots
combine_plots <- list()
# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]
combine_plots[[2]] <- iv_lin_plots[[3]]
# Save nonlinear plots for expansion period
combine_plots[[3]] <- plots_nl_iv$gg_s1[[1]]
combine_plots[[4]] <- plots_nl_iv$gg_s1[[2]]
# Save nonlinear plots for recession period
combine_plots[[5]] <- plots_nl_iv$gg_s2[[1]]
combine_plots[[6]] <- plots_nl_iv$gg_s2[[2]]
# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 2, ncol = 3, top = NULL)

# Switching Variable : Unemployment Rate

# Nonlinear shock using Blanchard - Perotti Shock
shocknl2 <- shockl

# Use unemployment rate as exogenous variable
exog_data2 <- unmp/100
# Use unemployment rate as switching variable
switching_variable2 <- unmp/100
# Estimate nonlinear model
results_nl_iv2 <- lp_nl_iv(endog_data = endog_data4, lags_endog_nl = 4,
                           shock = shocknl2, 
                           # exog_data = exog_data2, lags_exog = 4, 
                           trend = 0,
                           confint = 1.96, hor = 25,
                           switching = switching_variable2, use_hp = TRUE, gamma = 3,
                           lambda = 1600, use_logistic = TRUE)

# Multipliers in non linear (unemployment rate)
multipliers_s1_up_unmp <- multipliers(results_nl_iv2$irf_s1_up)
multipliers_s1_mean_unmp <- multipliers(results_nl_iv2$irf_s1_mean)
multipliers_s1_low_unmp <- multipliers(results_nl_iv2$irf_s1_low)

multipliers_s2_up_unmp <- multipliers(results_nl_iv2$irf_s2_up)
multipliers_s2_mean_unmp <- multipliers(results_nl_iv2$irf_s2_mean)
multipliers_s2_low_unmp <- multipliers(results_nl_iv2$irf_s2_low)

multipliers_s1_mean_unmp
multipliers_s2_mean_unmp

# Standard error (unmp_hp)
se_s1_unmp <- (multipliers_s1_up_unmp - multipliers_s1_mean_unmp)/1.96
se_s2_unmp <- (multipliers_s2_up_unmp - multipliers_s2_mean_unmp)/1.96

# Creating a table for unemployment rate (HP-Filter)
table2 <- table_func(multipliers_mean, multipliers_up, multipliers_low,
                     multipliers_s1_mean_unmp, multipliers_s1_up_unmp, multipliers_s1_low_unmp,
                     multipliers_s2_mean_unmp, multipliers_s2_up_unmp, multipliers_s2_low_unmp)

table2 <- cbind(c("mean", "upper", "lower", "mean", "upper", "lower", "mean", "upper", "lower"), table2)
colnames(table2) <- c("Confidence Interval", "Linear", "Expansion periods", "Recession periods")

# Creating IRF graph (one IRF graph)
# Response of rgov and rgdp
rata2 <- results_lin_iv$irf_lin_mean[1,]
lower <- results_lin_iv$irf_lin_low[1,]
upper <- results_lin_iv$irf_lin_up[1,]
period <- seq(1:length(rata2))
kategori1 <- rep(1, length(rata2))
part1 <- data.frame(cbind(period, rata2, lower, upper, kategori1))
names(part1) <- c("period", "mean", "lower", "upper", "group")
part1$group <- "linear"
part1_mean <- part1 %>%
  select(period, mean, group)

rata2 <- results_nl_iv2$irf_s1_mean[3,]
lower <- results_nl_iv2$irf_s1_low[3,]
upper <- results_nl_iv2$irf_s1_up[3,]
period <- seq(1:length(rata2))
kategori2 <- rep(2, length(rata2))
part2 <- data.frame(cbind(period, rata2, lower, upper, kategori2))
names(part2) <- c("period", "mean", "lower", "upper", "group")
part2$group <- "expansion"
part2_mean <- part2 %>%
  select(period, mean, group)

rata2 <- results_nl_iv2$irf_s2_mean[3,]
lower <- results_nl_iv2$irf_s2_low[3,]
upper <- results_nl_iv2$irf_s2_up[3,]
period <- seq(1:length(rata2))
kategori3 <- rep(3, length(rata2))
part3 <- data.frame(cbind(period, rata2, lower, upper, kategori3))
names(part3) <- c("period", "mean", "lower", "upper", "group")
part3$group <- "recession"
part3_mean <- part3 %>%
  select(period, mean, group)

library(ggplot2)
responses <- rbind(part2, part3)
responses_mean <- rbind(part1_mean, part2_mean, part3_mean)

ggplot(responses_mean, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_line()

ggplot(responses, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()

ggplot(part1, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()



# Make and save nonlinear plots
plots_nl_iv2 <- plot_nl(results_nl_iv2)
# Make list to save all plots
combine_plots <- list()
# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]
combine_plots[[2]] <- iv_lin_plots[[3]]
# Save nonlinear plots for expansion period
combine_plots[[3]] <- plots_nl_iv2$gg_s1[[1]]
combine_plots[[4]] <- plots_nl_iv2$gg_s1[[3]]
# Save nonlinear plots for recession period
combine_plots[[5]] <- plots_nl_iv2$gg_s2[[1]]
combine_plots[[6]] <- plots_nl_iv2$gg_s2[[3]]
# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 2, ncol = 3, top = NULL)


# Switching variable is unemployment rate (6.5%) ganti jadi 6%

# Nonlinear shock using Blanchard - Perotti Shock
shocknl3 <- shockl

# Use unemployment rate as exogenous variable
exog_data3 <- unmp/100
# Use unemployment rate as switching variable
unmp_num <- as.numeric(unmp$unmp)
med_unmp <- median(unmp_num)
switching_variable3 <- if_else(unmp>6,1,0)

# Estimate nonlinear model
results_nl2_iv3 <- lp_nl_iv(endog_data = endog_data4, lags_endog_nl = 4,
                            shock = shocknl3, 
                            # exog_data = exog_data3, lags_exog = 0, 
                            trend = 0,
                            confint = 1.96, hor = 20,
                            switching = switching_variable3, use_hp = FALSE, use_logistic = FALSE,
                            lag_switching = TRUE)

# Multipliers in non linear (unemployment rate 6.5%)
multipliers_s1_up_unmp1 <- multipliers(results_nl2_iv3$irf_s1_up)
multipliers_s1_mean_unmp1 <- multipliers(results_nl2_iv3$irf_s1_mean)
multipliers_s1_low_unmp1 <- multipliers(results_nl2_iv3$irf_s1_low)

multipliers_s2_up_unmp1 <- multipliers(results_nl2_iv3$irf_s2_up)
multipliers_s2_mean_unmp1 <- multipliers(results_nl2_iv3$irf_s2_mean)
multipliers_s2_low_unmp1 <- multipliers(results_nl2_iv3$irf_s2_low)

multipliers_s1_mean_unmp1
multipliers_s2_mean_unmp1

# Standard error (unmp 6.5%)
se_s1_unmp1 <- (multipliers_s1_up_unmp1 - multipliers_s1_mean_unmp1)/1.96
se_s2_unmp1 <- (multipliers_s2_up_unmp1 - multipliers_s2_mean_unmp1)/1.96

# Creating a table for unemployment rate (threshold = 6.5%)
table3 <- table_func(multipliers_mean, multipliers_up, multipliers_low,
                     multipliers_s1_mean_unmp1, multipliers_s1_up_unmp1, multipliers_s1_low_unmp1,
                     multipliers_s2_mean_unmp1, multipliers_s2_up_unmp1, multipliers_s2_low_unmp1)

table3 <- cbind(c("mean", "upper", "lower", "mean", "upper", "lower", "mean", "upper", "lower"), table3)
colnames(table3) <- c("Confidence Interval", "Linear", "Expansion periods", "Recession periods")

# Creating IRF graph (one IRF graph)
# Response of rgov and rgdp
rata2 <- results_lin_iv$irf_lin_mean[1,]
lower <- results_lin_iv$irf_lin_low[1,]
upper <- results_lin_iv$irf_lin_up[1,]
period <- seq(1:length(rata2))
kategori1 <- rep(1, length(rata2))
part1 <- data.frame(cbind(period, rata2, lower, upper, kategori1))
names(part1) <- c("period", "mean", "lower", "upper", "group")
part1$group <- "linear"
part1_mean <- part1 %>%
  select(period, mean, group)

rata2 <- results_nl2_iv3$irf_s1_mean[1,]
lower <- results_nl2_iv3$irf_s1_low[1,]
upper <- results_nl2_iv3$irf_s1_up[1,]
period <- seq(1:length(rata2))
kategori2 <- rep(2, length(rata2))
part2 <- data.frame(cbind(period, rata2, lower, upper, kategori2))
names(part2) <- c("period", "mean", "lower", "upper", "group")
part2$group <- "expansion"
part2_mean <- part2 %>%
  select(period, mean, group)

rata2 <- results_nl2_iv3$irf_s2_mean[1,]
lower <- results_nl2_iv3$irf_s2_low[1,]
upper <- results_nl2_iv3$irf_s2_up[1,]
period <- seq(1:length(rata2))
kategori3 <- rep(3, length(rata2))
part3 <- data.frame(cbind(period, rata2, lower, upper, kategori3))
names(part3) <- c("period", "mean", "lower", "upper", "group")
part3$group <- "recession"
part3_mean <- part3 %>%
  select(period, mean, group)

library(ggplot2)
responses <- rbind(part2, part3)
responses_mean <- rbind(part1_mean, part2_mean, part3_mean)

ggplot(responses_mean, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_line()

ggplot(responses, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()

ggplot(part1, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()



# Make and save nonlinear plots
plots_nl_iv3 <- plot_nl(results_nl2_iv3)
# Make list to save all plots
combine_plots <- list()
# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]
combine_plots[[2]] <- iv_lin_plots[[3]]
# Save nonlinear plots for expansion period
combine_plots[[3]] <- plots_nl2_iv3$gg_s1[[1]]
combine_plots[[4]] <- plots_nl2_iv3$gg_s1[[3]]
# Save nonlinear plots for recession period
combine_plots[[5]] <- plots_nl2_iv3$gg_s2[[1]]
combine_plots[[6]] <- plots_nl2_iv3$gg_s2[[3]]
# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 2, ncol = 3, top = NULL)


# Multipliers of government spending in the ZLB

# Use zero lower bound as switching variable
zlb_dummy <- if_else(int<=1.5, 1, 0)
switching_variable4 <- zlb_dummy
# Estimate nonlinear model
results_nl <- lp_nl(endog_data4,
                    lags_endog_lin = 4, 
                    lags_endog_nl = 4,
                    trend = 0, shock_type = 0,
                    confint = 1.96, hor = 20,
                    switching = switching_variable4, lag_switching = FALSE, use_logistic = FALSE)

results_nl_iv4 <- lp_nl_iv(endog_data = endog_data4, lags_endog_nl = 4,
                          shock = shocknl, 
                          trend = 0,
                          confint = 1.96, hor = 25,
                          switching = switching_variable4, use_hp = FALSE, use_logistic = FALSE,
                          lag_switching = FALSE)

# Multipliers in non linear (ZLB)
multipliers_s1_up_zlb <- multipliers(results_nl_iv4$irf_s1_up)
multipliers_s1_mean_zlb <- multipliers(results_nl_iv4$irf_s1_mean)
multipliers_s1_low_zlb <- multipliers(results_nl_iv4$irf_s1_low)

multipliers_s2_up_zlb <- multipliers(results_nl_iv4$irf_s2_up)
multipliers_s2_mean_zlb <- multipliers(results_nl_iv4$irf_s2_mean)
multipliers_s2_low_zlb <- multipliers(results_nl_iv4$irf_s2_low)

# Standard error (ZLB)
se_s1_zlb <- (multipliers_s1_up_zlb - multipliers_s1_mean_zlb)/1.96
se_s2_zlb <- (multipliers_s2_up_zlb - multipliers_s2_mean_zlb)/1.96

# Creating a table for zero lower bound (zlb)
table4 <- table_func(multipliers_mean, multipliers_up, multipliers_low,
                     multipliers_s1_mean_zlb, multipliers_s1_up_zlb, multipliers_s1_low_zlb,
                     multipliers_s2_mean_zlb, multipliers_s2_up_zlb, multipliers_s2_low_zlb)

table4 <- cbind(c("mean", "upper", "lower", "mean", "upper", "lower", "mean", "upper", "lower"), table4)
colnames(table4) <- c("Confidence Interval", "Linear", "Expansion periods", "Recession periods")

# Response of rgov and rgdp
rata2 <- results_lin_iv$irf_lin_mean[3,]
lower <- results_lin_iv$irf_lin_low[3,]
upper <- results_lin_iv$irf_lin_up[3,]
period <- seq(1:length(rata2))
kategori1 <- rep(1, length(rata2))
part1 <- data.frame(cbind(period, rata2, lower, upper, kategori1))
names(part1) <- c("period", "mean", "lower", "upper", "group")
part1$group <- "linear"
part1_mean <- part1 %>%
  select(period, mean, group)

rata2 <- results_nl_iv4$irf_s1_mean[1,]
lower <- results_nl_iv4$irf_s1_low[1,]
upper <- results_nl_iv4$irf_s1_up[1,]
period <- seq(1:length(rata2))
kategori2 <- rep(2, length(rata2))
part2 <- data.frame(cbind(period, rata2, lower, upper, kategori2))
names(part2) <- c("period", "mean", "lower", "upper", "group")
part2$group <- "normal"
part2_mean <- part2 %>%
  select(period, mean, group)

rata2 <- results_nl_iv4$irf_s2_mean[1,]
lower <- results_nl_iv4$irf_s2_low[1,]
upper <- results_nl_iv4$irf_s2_up[1,]
period <- seq(1:length(rata2))
kategori3 <- rep(3, length(rata2))
part3 <- data.frame(cbind(period, rata2, lower, upper, kategori3))
names(part3) <- c("period", "mean", "lower", "upper", "group")
part3$group <- "near zero lower bound"
part3_mean <- part3 %>%
  select(period, mean, group)

library(ggplot2)
responses <- rbind(part2, part3)
responses_mean <- rbind(part1_mean, part2_mean, part3_mean)

ggplot(responses_mean, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_line()

ggplot(responses, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()

ggplot(part1, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()

# Make and save nonlinear plots
plots_nl <- plot_nl(results_nl)
# Make list to save all plots
combine_plots <- list()
# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]
combine_plots[[2]] <- iv_lin_plots[[3]]
# Save nonlinear plots for expansion period
combine_plots[[3]] <- plots_nl$gg_s1[[1]]
combine_plots[[4]] <- plots_nl$gg_s1[[3]]
# Save nonlinear plots for recession period
combine_plots[[5]] <- plots_nl$gg_s2[[1]]
combine_plots[[6]] <- plots_nl$gg_s2[[3]]
# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 2, ncol = 3, top = NULL)

# All multipliers table
table_mult <- cbind(table1, table2[,3:4], table3[,3:4], table4[,3:4])
table_se <- rbind(se_lin, se_s1_rec, se_s2_rec, se_s1_unmp, se_s2_unmp, 
                  se_s1_unmp1, se_s2_unmp1, se_s1_zlb, se_s2_zlb)
table_se <- cbind( c("linear", "recession date 1", "recession date 2",
                     "unmp 1", "unmp 2", "unmp1 1", "unmp1 2",
                     "zlb 1", "zlb 2"), table_se)
table_all <- list(table_mult, table_se)
library(writexl)
write_xlsx(table_all, "multipliers and se.xlsx")


#_______________________CONTROLLING EXPECTATIONS_________________________#
library(forecast)
library(stats)

jorda.fcst <- read_excel("r_data.xlsx", sheet = "forecast reg")
rgov.fcst <- jorda.fcst[,c("diff_rgov_fcst")]
rgov.fcst.diff <- rgov.fcst
rgov.diff <- jorda.fcst$diff_rgov
rgdp.diff <- jorda.fcst$diff_gdp
rtax.diff <- jorda.fcst$diff_tax
unmp.ts <- ts(unmp/100, start=c(1995, 1), end=c(2020, 4), frequency=4)
int.ts <- ts(int/100, start=c(1995, 1), end=c(2020, 4), frequency=4)

# Lead and Lag
lag.func <- function(x, n) {
  lag.list <- list()
  for (i in 1:n) {
    lag.val <- lag(as.vector(x), n=i)
    lag.val.ts <- ts(lag.val, start=c(1995, 1), end=c(2020, 4), frequency=4)
    lag.list[[i]] <- lag.val.ts
  }
  return(lag.list)
}

lead.func <- function(x, n) {
  lead.list <- list()
  for (i in 1:n) {
    lead.val <- lead(as.vector(x), n=i)
    lead.val.ts <- ts(lead.val, start=c(1995, 1), end=c(2020, 4), frequency=4)
    lead.list[[i]] <- lead.val.ts
  }
  return(lead.list)
}

rgdp.lag <- lag.func(rgdp.diff, 4)
rtax.lag <- lag.func(rtax.diff, 4)
unmp.lag <- lag.func(unmp/100, 4)
int.lag <- lag.func(int/100, 4)
rgov.fcst.lead <- lead.func(rgov.fcst.diff, 1)
rgov.fcst.lag <- lag.func(rgov.fcst.diff, 4)

data.reg <- cbind(rgov.diff, rgdp.diff, rtax.diff, rgov.fcst.diff,
                  rgdp.lag[[1]], rgdp.lag[[2]], rgdp.lag[[3]], rgdp.lag[[4]],
                  rtax.lag[[1]], rtax.lag[[2]], rtax.lag[[3]], rtax.lag[[4]],
                  unmp.lag[[1]], unmp.lag[[2]], unmp.lag[[3]], unmp.lag[[4]])
# int.lag[[1]], int.lag[[2]], int.lag[[3]], int.lag[[4]])

reg1 <- lm(rgov.diff ~ ., data = data.reg[ ,-2:-3])
summary(reg1)
shock.exp <- reg1$residuals
ts.plot(shock.exp)
vec.shock.exp <- numeric(104)
vec.shock.exp[5:104] <- shock.exp
shock.approx <- approx(shock.exp, n=104)
vec.shock.exp[1:4] <- shock.approx$y[1:4] 

rgov.diff1 <- diff(endog_data$rgov, lag=1)
ts.plot(rgov.diff1)
vec.rgov.diff1 <- numeric(104)
vec.rgov.diff1[2:104] <- rgov.diff1
rgov.approx <- approx(rgov.diff1, n=104)
vec.rgov.diff1[1] <- rgov.approx$y[1]

rtax.diff1 <- diff(endog_data$rtax, lag=1)
vec.rtax.diff1 <- numeric(104)
vec.rtax.diff1[2:104] <- rtax.diff1
rtax.approx <- approx(rtax.diff1, n=104)
vec.rtax.diff1[1] <- rtax.approx$y[1]

rgdp.diff1 <- diff(endog_data$rgdp, lag=1)
vec.rgdp.diff1 <- numeric(104)
vec.rgdp.diff1[2:104] <- rgdp.diff1
rgdp.approx <- approx(rgdp.diff1, n=104)
vec.rgdp.diff1[1] <- rgdp.approx$y[1]

norm.rgov <- vec.rgov.diff1
rgdp.miya <- vec.rgdp.diff1
endog.new <- cbind(norm.rgov, rgdp.miya)
endog.new <- data.frame(endog.new)
std.controls <- cbind(vec.rgdp.diff1, unmp.ts)
std.controls2 <- cbind(unmp.ts)
std.controls <- data.frame(std.controls)
std.controls2 <- data.frame(rtax.diff)
endog.new2 <- cbind(vec.shock.exp, endog_data)

# Shock linear
shockl2 <- data.frame(vec.shock.exp[5:104])
ts.plot(shockl2)

# Estimate linear model
results_lin2_iv <- lp_lin_iv(endog_data = endog_data4, lags_endog_lin = 4,
                             # exog_data = std.controls, lags_exog = 4,
                             shock = shockl2, trend = 0,
                             confint = 1.96, hor = 25)
plot(results_lin2_iv)

# Multipliers (Linear)
multipliers2 <- function(x) {
  response.df <- data.frame(x)
  gov_sum_4 <- rowSums( response.df[2,1:4] )
  gov_sum_8 <- rowSums( response.df[2,1:8] )
  gov_sum_16 <- rowSums( response.df[2,1:16] )
  gdp_sum_4 <- rowSums( response.df[4,1:4] )
  gdp_sum_8 <- rowSums( response.df[4,1:8] )
  gdp_sum_16 <- rowSums( response.df[4,1:16] )
  
  multiplier_4 <- gdp_sum_4/gov_sum_4
  multiplier_8 <- gdp_sum_8/gov_sum_8
  multiplier_16 <- gdp_sum_16/gov_sum_16
  multipliers <- data.frame(multiplier_4, multiplier_8, multiplier_16)
  return(multipliers)
}

multipliers2_mean <- multipliers(results_lin2_iv$irf_lin_mean)
multipliers2_up <- multipliers(results_lin2_iv$irf_lin_up)
multipliers2_low <- multipliers(results_lin2_iv$irf_lin_low)

# Standard error (Linear)
se_lin2 <- abs((multipliers2_up - multipliers2_mean)/1.96)

# Make and save linear plots
iv_lin2_plots <- plot_lin(results_lin2_iv)

# Use OECD recession date as switching variable
switching_variable <- if_else(rec_date >= 1, 1, 0)
exog_data <- rgdpg_7ma_
# switching_variable <- if_else(rec_date >= 1, 1, 0)

# Estimate nonlinear model
results_nl2_iv <- lp_nl_iv(endog_data = endog_data4, lags_endog_nl = 4,
                           shock = shockl2,  
                           # exog_data = exog_data, lags_exog = 3, 
                           trend = 0,
                           confint = 1.96, hor = 25,
                           switching = switching_variable, use_hp = FALSE, use_logistic = FALSE,
                           lag_switching = FALSE)

# plot(results_nl2_iv)
# Multipliers in non linear (recession dates)
multipliers2_s1_up_rec <- multipliers(results_nl2_iv$irf_s1_up)
multipliers2_s1_mean_rec <- multipliers(results_nl2_iv$irf_s1_mean)
multipliers2_s1_low_rec <- multipliers(results_nl2_iv$irf_s1_low)

multipliers2_s2_up_rec <- multipliers(results_nl2_iv$irf_s2_up)
multipliers2_s2_mean_rec <- multipliers(results_nl2_iv$irf_s2_mean)
multipliers2_s2_low_rec <- multipliers(results_nl2_iv$irf_s2_low)
multipliers2_s1_mean_rec
multipliers2_s2_mean_rec

# Standard error (OECD based recession dates)
se2_s1_rec <- abs((multipliers2_s1_up_rec - multipliers2_s1_mean_rec)/1.96)
se2_s2_rec <- abs((multipliers2_s2_up_rec - multipliers2_s2_mean_rec)/1.96)

# Creating a table for OECD based recession dates (unexpected shock)
table5 <- table_func(multipliers2_mean, multipliers2_up, multipliers2_low,
                     multipliers2_s1_mean_rec, multipliers2_s1_up_rec, multipliers2_s1_low_rec,
                     multipliers2_s2_mean_rec, multipliers2_s2_up_rec, multipliers2_s2_low_rec)

table5 <- cbind(c("mean", "upper", "lower", "mean", "upper", "lower", "mean", "upper", "lower"), table5)
colnames(table5) <- c("Confidence Interval", "Linear", "Expansion periods", "Recession periods")

# Response of rgov and rgdp
rata2 <- results_lin2_iv$irf_lin_mean[1,]
lower <- results_lin2_iv$irf_lin_low[1,]
upper <- results_lin2_iv$irf_lin_up[1,]
period <- seq(1:length(rata2))
kategori1 <- rep(1, length(rata2))
part1 <- data.frame(cbind(period, rata2, lower, upper, kategori1))
names(part1) <- c("period", "mean", "lower", "upper", "group")
part1$group <- "linear"
part1_mean <- part1 %>%
  select(period, mean, group)

rata2 <- results_nl2_iv$irf_s1_mean[1,]
lower <- results_nl2_iv$irf_s1_low[1,]
upper <- results_nl2_iv$irf_s1_up[1,]
period <- seq(1:length(rata2))
kategori2 <- rep(2, length(rata2))
part2 <- data.frame(cbind(period, rata2, lower, upper, kategori2))
names(part2) <- c("period", "mean", "lower", "upper", "group")
part2$group <- "expansion"
part2_mean <- part2 %>%
  select(period, mean, group)

rata2 <- results_nl2_iv$irf_s2_mean[1,]
lower <- results_nl2_iv$irf_s2_low[1,]
upper <- results_nl2_iv$irf_s2_up[1,]
period <- seq(1:length(rata2))
kategori3 <- rep(3, length(rata2))
part3 <- data.frame(cbind(period, rata2, lower, upper, kategori3))
names(part3) <- c("period", "mean", "lower", "upper", "group")
part3$group <- "recession"
part3_mean <- part3 %>%
  select(period, mean, group)

library(ggplot2)
responses <- rbind(part2, part3)
responses_mean <- rbind(part1_mean, part2_mean, part3_mean)

ggplot(responses_mean, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_line()

ggplot(responses, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()

ggplot(part1, aes(x = period, y = mean, group=group, col=group, fill=group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2) +
  geom_line()



# Make and save nonlinear plots
plots_nl2_iv <- plot_nl(results_nl2_iv)
# Make list to save all plots
combine_plots <- list()
# Save linear plots in list
combine_plots[[1]] <- iv_lin2_plots[[2]]
combine_plots[[2]] <- iv_lin2_plots[[4]]
# Save nonlinear plots for expansion period
combine_plots[[3]] <- plots_nl2_iv$gg_s1[[2]]
combine_plots[[4]] <- plots_nl2_iv$gg_s1[[4]]
# Save nonlinear plots for recession period
combine_plots[[5]] <- plots_nl2_iv$gg_s2[[2]]
combine_plots[[6]] <- plots_nl2_iv$gg_s2[[4]]
# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 2, ncol = 3, top = NULL)

# All multipliers table for unexpected shocks
table_mult2 <- table5
table_se2 <- rbind(se_lin2, se2_s1_rec, se2_s2_rec)
table_se2 <- cbind( c("linear", "recession date 1", "recession date 2"), table_se2)
table_all2 <- list(table_mult2, table_se2)
library(writexl)
write_xlsx(table_all2, "multipliers and se unexpected shocks.xlsx")
