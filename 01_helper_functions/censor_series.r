censor_series <- function(ser, outperiod) {
  ser[(ser$Date %% outperiod) == 0, ]
}