


  


data<- read.csv("data/oilmnth.csv")

data$Date <- as.Date(data$Date)
data <- data %>% arrange(Date)

cpt_res <- cpt.mean(data$Value1,Q = 12,minseglen = 10,penalty = "MBIC",method = "BinSeg")


segment_id <- rep(1:length(cpt_res@cpts), times = c(cpt_res@cpts[1], diff(cpt_res@cpts)))
data$segment_id <- segment_id


segment_mean_mid_value <- data %>%
  group_by(segment_id) %>%
  summarise(mean_mid_value = mean(Value1)) %>%
  merge(data,.,by = "segment_id")

print(segment_mean_mid_value)

some_data <- segment_mean_mid_value %>%
  group_by(segment_id) %>%
  mutate(diff = Value1 - mean_mid_value,
         cumsum_diff = cumsum(diff),
         cumsum_lag =lag(cumsum_diff),
         cumsum_mean_mid = cumsum_lag + mean_mid_value,
         cumsum_mean_mid = case_when(is.na(cumsum_mean_mid) ~ mean_mid_value,
                                     TRUE ~ cumsum_mean_mid)) %>% ungroup()




colnames(some_data)

model_arima <-auto.arima(some_data[,"cumsum_mean_mid"],approximation = FALSE,allowdrift = T,allowmean = T)
some_data$manhattan_arima <- model_arima$fitted
some_data$manhattan_arima_residuals <- residuals(model_arima)

ggplot(some_data) +
  geom_point(aes(x = Date,y = Value1, colour = "data")) +
  geom_line(aes(x = Date,y = manhattan_arima, color = "Arima_sim"))


xt <- arima.sim(n=50, list(order=c(input$p,input$d,input$q), ar = c(.9), ma = -.2,))



arimaorder(model_arima)

model_arima$coef['ar1']


arima.sim(list(ar=fit$coef["ar1"],ma=fit$coef["ma1"]),sd=fit$sigma,10)


model_arima$coef
model_arima$mask
model_arima$loglik
model_arima$aic
model_arima$arma
model_arima$residuals
model_arima$call
model_arima$n.cond
model_arima$aicc
model_arima$bic
model_arima$nobs


model_arima$arma
