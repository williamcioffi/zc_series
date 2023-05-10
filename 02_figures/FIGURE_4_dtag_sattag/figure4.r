### figure 4
# compare DTAG to SPLASH10 (sattag) in same group

# libraries and helper functions
library("R.matlab")
source("../../01_helper_functions/matchtimes.r")

# constants
ERRORBARCOLOR <- rgb(48/255, 102/255, 190/255)
SPLASH10COLOR <- rgb(178/255, 76/255, 99/255)

# load dtag prh provided by J. Shearer 2019-11-06
prh <- R.matlab::readMat("../../00_data/tags/Tag18_depth.mat")
dtag_dep <- prh$p

# on time
dtagontime <- as.numeric(as.POSIXct("2019-08-06 15:38:20 UTC", tz = "UTC"))

# calculate the time axis
# prh$fs is actually a 1x1 array and R has depreciated recycling it (incr/prh$fs) as.vector fixes the warning
incr <- 1:(length(prh$p) - 1)
incr <- incr/as.vector(prh$fs)
tt <- c(dtagontime, dtagontime + incr)

# load sat tag Zc_Tag093_DUML
sattag <- read.table("../../00_data/tags/Tag17.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
sattag <- sattag[order(sattag$Date), ]

# make end bound based on tag off
tagoff_cutoff <- 1565122600

# apply cutoff
sattag <- sattag[sattag$Date < tagoff_cutoff & sattag$Date > dtagontime, ]
dtag_dep <- dtag_dep[tt < tagoff_cutoff]
tt <- tt[tt < tagoff_cutoff]

# matchtimes.r
matches <- matchtimes(sattag$Date, tt)


### start figure
pdf(file = "figure4.pdf")
###

par(mfrow = c(2, 1), mar = c(5.1, 5.1, 1.1, 1.1))

# make a plotting area
plot(0, 0, 
  xlim = c(0, max(sattag$Depth) + max(sattag$DRange)), 
  ylim = c(0, max(sattag$Depth) + max(sattag$DRange)), 
  type = 'n', 
  las = 1,
  xlab = "SPLASH10 depth (meters)",
  ylab = ""
)

# write in y axis label
mtext("DTAG depth (meters)", line = 3.4, side = 2)


# I like these kinds of axes
axis(1, tcl = 0.3, lab = NA)
axis(2, tcl = 0.3, lab = NA)


# add in the 1:1 line
f1 <- function(x) x
curve(f1, from = 0, max(sattag$Depth), add = TRUE, lty = 2)

# add in error segments.
# this is a little confusing because I've drawn them on the y axis
# but centered at the x axis (sattag position), but think about it 
# on the line because that's the one to one line. We're comparing 
# it to the actual measurements of the dtag which is y axis.
segments(
  sattag$Depth, 
  sattag$Depth + sattag$DRange, 
  sattag$Depth, 
  sattag$Depth - sattag$DRange, 
  col = ERRORBARCOLOR, lwd = 2
)

# add the points
points(sattag$Depth, dtag_dep[matches])

# legend
legend("bottomright",
  c('depth', 'SPLASH10 depth range', '1:1 line'),
  col = c("black", ERRORBARCOLOR, "black"),
  pch = c(1, NA, NA),
  lty = c(NA, 1, 2),
  lwd = c(NA, 2, 1),
  bty = 'n'
)

# plot profiles
# normalize the tt to minute since tag on
tt_norm <- (tt - min(tt))/60

# decimate just for plotting
decseq <- seq(1, length(tt_norm), by = 1000)

plot(tt_norm[decseq], -dtag_dep[decseq], 
  type = 'l', 
  las = 1, 
  ylim = c(-max(sattag$Depth) - max(sattag$DRange), 0),
  xlab = "time (min since tag on)", 
  ylab = "",
  axes = FALSE
)

# write in ylab
mtext("depth (meters)", side = 2, line = 3.4)

# draw in more axis ticks
axis(1, tcl = -0.3, at = seq(0, max(tt_norm), by = 10), lab = NA)
axis(1, tcl = 0.3, at = seq(0, max(tt_norm), by = 10), lab = NA)
axis(1, tcl = 0, at = seq(0, max(tt_norm), by = 30))
axis(2, tcl = -0.3, at = seq(0, -1400, by = -200), lab = abs(seq(0, -1400, by = -200)),las = 1)
axis(2, tcl = 0.3, lab = NA)
box()

# make the sattag date to match up
sattagdate_norm <- (sattag$Date - min(tt))/60

segments(
  sattagdate_norm, 
  -sattag$Depth + sattag$DRange, 
  sattagdate_norm, 
  -sattag$Depth - sattag$DRange, 
  col = ERRORBARCOLOR, 
  lwd = 2
)

points(sattagdate_norm, -sattag$Depth, 
  col = SPLASH10COLOR,
  pch = 16, 
  cex = .5
)

legend("bottomright",
  legend = c("DTAG", "SPLASH10", "SPLASH10 depth range"),
  col = c("black", SPLASH10COLOR, ERRORBARCOLOR), 
  pch = c(NA, 16, NA), 
  lty = c(1, NA, 1), 
  lwd = c(1, NA, 2), 
  bty = 'n'
 )

### figure end
dev.off()
###
