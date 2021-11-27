library(tidyverse)

voo_data <- read_csv(file = "data/VOO.csv")

glimpse(voo_data)

min(voo_data$Date)
max(voo_data$Date)

voo_ts <- voo_data %>%
  pull(Open) %>%
  ts()

glimpse(voo_ts)

plot.ts(voo_ts)
hist(voo_ts)
mean(voo_ts)
sd(voo_ts)

plot.ts(diff(voo_ts))
hist(diff(voo_ts))
diff_mean <- mean(diff(voo_ts))
diff_sd <- sd(diff(voo_ts))
qqnorm(diff(voo_ts))

diff_mean - diff_sd*0.5
diff_mean + diff_sd*0.5

diff_mean - diff_sd
diff_mean + diff_sd

diff_mean - diff_sd*2
diff_mean + diff_sd*2

#FITTING THE MODEL
rw <- arima(x = voo_ts, order = c(0,1,0))

ts.plot(voo_ts)
RW_fitted <- voo_ts - residuals(rw)
points(RW_fitted, type = "l", col = 2, lty = 2)


RW_forecast <- predict(rw, n.ahead = 10)$pred
RW_forecast_se <- predict(rw, n.ahead = 10)$se
points(RW_forecast, type = "l", col = 2)
points(RW_forecast - 2*RW_forecast_se, type = "l", col = 2, lty = 2)
points(RW_forecast + 2*RW_forecast_se, type = "l", col = 2, lty = 2)

#PERCENT CHANGE
voo_pc_ts <- voo_data %>%
  mutate(pct_change = (Open - lag(Open))/lag(Open)*100) %>%
  pull(pct_change) %>%
  ts()

plot.ts(voo_pc_ts)
hist(voo_pc_ts)
pc_mean <- mean(voo_pc_ts, na.rm = TRUE)
pc_sd <- sd(voo_pc_ts, na.rm = TRUE)
qqnorm(diff(voo_pc_ts))

pc_mean - pc_sd
pc_mean + pc_sd

pc_mean - pc_sd*2
pc_mean + pc_sd*2
