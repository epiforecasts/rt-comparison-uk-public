# Exploring time-series features

# Time series viz
forecast::ggtsdisplay(eng$median_cases_blend)

# Straight correlation: spurious
eng[,4:6] %>%
  as.data.frame() %>%
  GGally::ggpairs()

# Rolling correlations
rollcor <- zoo::rollapply(eng$median_cases_blend, length(eng), function(y) c(cor(case_count$ma,
                                                                                        eng$median_cases_hosp),
                                                                eng$median_cases_hosp))

rollapply(list2$V1, w, cor, x)

summary_wide <- readRDS("rt-estimate/summary_wide.rds")

# Testing on England as region
eng <- summary_wide[summary_wide$region == "England",]
# Complete pairwise observations
eng <- eng[55:180,]

cases_hosp_medcor <- zoocat::rollcor(diff(log(eng$median_cases_blend)), diff(log(eng$median_cases_hosp)), 
                                     width = 7, method = "kendall")
cases_deaths_medcor <- zoocat::rollcor(diff(diff(log(eng$median_cases_blend))), 
                                       diff(diff(log(eng$median_deaths_blend))), 
                                     width = 7, method = "kendall")
deaths_hosp_medcor <- zoocat::rollcor(diff(diff(diff(log(eng$median_deaths_blend)))),
                                      diff(diff(diff(log(eng$median_cases_hosp)))), 
                                     width = 7, method = "kendall")

c(min(deaths_hosp_medcor, na.rm=T), max(deaths_hosp_medcor, na.rm=T))


# ACF & PACF
par(mfrow=c(3,2))
acf(eng$median_cases_blend, lag.max = length(eng$median_cases_blend-1), main = "")
pacf(eng$median_cases_blend, lag.max = length(eng$median_cases_blend-1), main = "")
acf(eng$median_cases_hosp, lag.max = length(eng$median_cases_hosp-1), main = "")
pacf(eng$median_cases_hosp, lag.max = length(eng$median_cases_hosp-1), main = "")
acf(eng$median_deaths_blend, lag.max = length(eng$median_deaths_blend-1), main = "")
pacf(eng$median_deaths_blend, lag.max = length(eng$median_deaths_blend-1), main = "")


# Arima -------------------------------------------------------------------

arima_cases <- names(forecast::auto.arima(eng$median_cases_blend)$coef)
    # 3 AR differencing + 2 MA
arima_hosp <- names(forecast::auto.arima(eng$median_cases_hosp)$coef)
    # 1 AR differencing + 5 MA
arima_death <- names(forecast::auto.arima(eng$median_deaths_blend)$coef)
    # 3 AR differencing

forecast::Acf(forecast::auto.arima(eng$median_cases_blend)$residuals)



# Cross correlation -------------------------------------------------------
# Median Rt from hospital cases and deaths
forecast::Ccf(x = diff(diff(log(eng$median_cases_blend))), 
              y = diff(diff(log(eng$median_deaths_blend))), 
              lag.max = length(eng$median_cases_blend-1),
              main = "Rt(Deaths) on Rt(cases) - 2xdifferenced", xlab = "Lag (days)")

forecast::Ccf(x = eng$median_cases_blend, 
              y = eng$median_deaths_blend, 
              lag.max = length(eng$median_cases_blend-1),
              main = "Rt(Deaths) on Rt(cases)", xlab = "Lag (days)")


# Correlate Rt estimate with level of  case count data
case_count <- data_ma %>%
  dplyr::filter(variable == "cases_blend" & region == "England") %>%
  left_join(eng, by = "date")

cor(case_count$ma, case_count$median_cases_blend, use = "pairwise")
ccf(case_count$ma, case_count$median_cases_blend, na.action = na.pass)




