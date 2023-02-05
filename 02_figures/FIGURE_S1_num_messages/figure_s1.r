### figure s1
# show nummber of times each unique message received

# libraries
library(sattagutils)

# load in series tags
load("../../00_data/tags/series_tags_w_meta.rdata")

tagorder <- c(1, 2, 6, 7, 5, 4, 3, 8)
tags <- tags_allmsg[tagorder]

# pdf("figure_s1.pdf", width = 6, height = 8)
par(mfrow = c(4, 2), oma = c(4.1, 4.1, 1.1, 0), mar = c(4.1, 2.1, 0, 1.1))

for(i in 1:length(tags)) {
if(sattagutils::DeployID(tags[[i]]) != 'Tag10') {
  # ser <- sattagutils::getstream(tags[[i]], 'series', squash = TRUE)
  msg <- sattagutils::getstream(tags[[i]], 'seriesrange', squash = TRUE)
  msg <- msg[msg$original == "portal", ]
  
  # ser <- ser[order(ser$Date), ]
  msg <- msg[order(msg$Start), ]
  
  # hand fix corrupt message
  if(sattagutils::DeployID(tags[[i]]) == 'Tag11')
    msg <- msg[-13, ]
  
  dates <- (c(msg$Start, msg$End) - min(msg$Start)) / 60 / 60 / 24
  st <- (msg$Start - min(msg$Start)) / 60 / 60 / 24
  en <- (msg$End - min(msg$Start)) / 60 / 60 / 24
  
  # st[st >= 14] <- NA
  # en[st >= 14] <- NA
  
  
  plot(dates, rep(0, length(dates)), type = 'n', axes = FALSE, xlab = "", ylab = "", ylim = c(0, 25))#, xlim = c(0, 14))
  axis(3, at = c(st, en), labels = FALSE, tcl = 0.5)
  # labs <- format(num2date(dseq), '%d-%b', las = 2)
  axis(1, at = 0:100)
  axis(2, las = 1)
  box()
  
  diffs <- st[2:length(st)] - en[1:length(st) - 1]
  gaps <- which(diffs != 0)
  
  rect(st, 0, en, msg$Count)
  rect(en[gaps], rep(-2000, length(gaps)), st[gaps+1], rep(2000, length(gaps)), col = rgb(1, 0, 1, .25), border = NA)
  
} else {
  plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
}
  box()
  legend('topright', legend = sattagutils::DeployID(tags[[i]]), bty = 'n')
  
  # put in legends
  if(i == 3) {
    mtext("time-series message received count", side = 2, line = 3.1, at = -10)
  }
  
  if(i == 7)
    mtext("days since deployment", side = 1, line = 4.1, at = 15)
}
# dev.off()
