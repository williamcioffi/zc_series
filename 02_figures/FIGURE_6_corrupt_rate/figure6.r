### figure 6
# calculate gonio efficiency

# libraries
library(sattagutils)
library(reshape2)
library(ggplot2)

# tiny helper function
getstretchid <- function(stretches) {
  c(1, 1 + cumsum(stretches[1:(length(stretches) - 1)] != stretches[2:length(stretches)]))
}

# get raw dat
datbarber <- sattagutils::rcsv("../../00_data/gonio/strength/barber_gonio_2018_2019.csv")
datcharter <- sattagutils::rcsv("../../00_data/gonio/strength/charter_gonio_2018fall_2019.csv")
dat <- rbind(datbarber, datcharter)
deploytimes <- sattagutils::rcsv("../../00_data/gonio/deploylengths2018.csv")
datdf <- sattagutils::rcsv("../../00_data/gonio/gonio_corrupt_2018.csv")

# merge
datdf <- merge(deploytimes, datdf)

keypath  <- "../../00_data/gonio/tags/key"
knames <- list.files(keypath)

tdiff <- vector(length = length(knames))
ndays <- vector(length = length(knames))

for(i in 1:length(knames)) {
  key <- sattagutils::rcsv(file.path(keypath, knames[i]))
  curdat <- dat[grep(key$HEX, dat$V9), ]
  curdat <- curdat[order(date2num(curdat$V1)), ]
  
  # get dates
  dates_utc <- as.POSIXct(curdat$V1, tz = "UTC")
  # convert to EST (local time) to account for actual field days
  dates_est <- as.POSIXct(format(dates_utc, tz = "EST"), tz = "EST")
  dates_num <- date2num(dates_est)
  
  # calculate efficiency per day
  day <- format(dates_est, "%Y%m%d")
  dstretch <- getstretchid(day)
  udstretch <- unique(dstretch)
  nday <- length(udstretch)
  ndays[i] <- nday
  
  # calculate the time difference between first and last each day
  
  for(s in 1:nday) {
    curdates <- dates_num[dstretch == udstretch[s]]
    tdiff[i] <- tdiff[i] + curdates[length(curdates)] - curdates[1]
  }
}

# convert tdiff into days
tdiff_days <- tdiff/60/60/24

# make a labeled df
pttkey <- sapply(strsplit(knames, "_"), '[[', 1)
tdiff_df <- data.frame(PTT = pttkey, dur_days = tdiff_days, ndays_gonio_effort = ndays)

# filter out low effort
# this only actually influences Tag11 for which 99% of gonio 
# hits were recieved a short time after deployment
tdiff_df$dur_days[tdiff_df$dur_days < .1] <- 0

# merge together
datdf2 <- merge(datdf, tdiff_df)
datdf2[datdf2$source == "portal", c('dur_days', 'ndays_gonio_effort')] <- NA
datdf2$dur_days[is.na(datdf2$dur_days)] <- datdf2$dur[is.na(datdf2$dur_days)]

# calc
datdf2[, 'totalrate'] <- datdf2$total / datdf2$dur_days
datdf2[, 'corrupt rate'] <- datdf2$corrupt / datdf2$dur_days
datdf2[, 'decoded rate'] <- datdf2[, 'totalrate'] - datdf2[, 'corrupt rate']

datdf4 <- datdf2[, c('DEPLOYID', 'decoded rate', 'corrupt rate', 'source')]

# melt it
datdf5 <- reshape2::melt(datdf4, id.vars = c('DEPLOYID', 'source'))

# make a dummy order variable for ggplot
datdf5$varorder <- NA
datdf5$varorder[datdf5$variable == "decoded rate"] <- 2
datdf5$varorder[datdf5$variable == "corrupt rate"] <- 1

datdf5$source[datdf5$source == "gonio"] <- "field"
datdf5$source[datdf5$source == "portal"] <- "sat"

# set non values to NA
datdf5$value[is.nan(datdf5$value)] <- NA
datdf5$value[is.infinite(datdf5$value)] <- NA

### start plot
pdf("figure6.pdf", width = 5, height = 3)
###

# colors
fill <- c("#AAAAAA", "#444444")

p <- ggplot2::ggplot()
p <- p + geom_bar(data = datdf5, aes(y = value, x = source, fill = reorder(variable, varorder)), stat = "identity", position = "stack") 
p <- p + facet_grid(~ DEPLOYID)
p <- p + scale_fill_manual(values = fill)
p <- p + xlab("") + ylab("messages per day")
p <- p + theme_bw()
p <- p + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)
p <- p + theme(legend.position = "bottom", legend.box = "horizontal")
p <- p + guides(fill = guide_legend(title = ""))
p

### end plot
dev.off()
###
