### Figure 2
# Concurrently recorded time-series and dive summary ('behavior')

# required libraries and functions
library(sattagutils) # github.com/williamcioffi/sattagutils
library(colorspace)
source("../../01_helper_functions/plot_dives.r")
source("../../01_helper_functions/findgaps.r")

# constants
ERRORBARCOLOR <- rgb(48/255, 102/255, 190/255)

# import tag data
tags <- readRDS("../../00_data/tags/baseline_tags.rds")

# use the first tag as an example
extag <- tags[[1]]
ser <- sattagutils::getstream(extag, "series", squash = TRUE)
beh <- sattagutils::getstream(extag, "behavior", squash = TRUE)

# grab a snippet from the beginning
ser <- ser[1:(48*4), ]

# calculate overlap
overlap <- beh$Start >= ser$Date[1] & beh$End <= ser$Date[nrow(ser)]

# clip again
ser <- ser[c(-(1:7), -((nrow(ser)-2):nrow(ser))), ]
origser <- ser # make a copy of the original for resampling

### make plot
pdf("figure2.pdf", width = 6, height = 6)
###

par(mfrow = c(3, 1), mar = c(4.1, 4.1, 1.1, 1.1), oma = c(1.1, 1.1, 0, 0))
periods <- c(2.5, 5, 7.5)

for(i in 1:length(periods)) {

if(periods[i] != 2.5) {
  ser <- sattagutils::resample_ser(origser, periods[i])
}

# helper_functions.RData
plot_dives2(beh[overlap, ],
  show_gaps = FALSE,
  legend = FALSE,
  start_time = (ser$Date[1]),
  end_time = (ser$Date[nrow(ser)]), 
  pch = NA, col = "grey45", lwd = 2,
  yaxt = FALSE
)

axis(2, at = seq(0, -1400, by = -200), lab = NA, tcl = -0.3)
axis(2, at = seq(0, -1400, by = -200), lab = abs(seq(0, -1400, by = -200)), tcl = 0.3, las = 1)
axis(1, at = ser$Date, lab = NA, tcl = 0.3)

segments(
  ser$Date, -ser$Depth - ser$DRange,
  ser$Date, -ser$Depth + ser$DRange,
  col = ERRORBARCOLOR
)

points(ser$Date, -ser$Depth, pch = 16, cex = .5)
lines(ser$Date, -ser$Depth)

hours_from_start <- (ser$Date - min(ser$Date)) / 60 / 60
atdese <- ser$Date[hours_from_start %in% 0:7] 

axis(1, at = atdese, labels = 0:7)

if(i == 3) mtext("running time (hours)", side = 1, line = 3.1)
box()

legend("bottomright", 
  legend = paste0("sampling period = ", periods[i], " min"), 
  bty = 'n'
)
}

### end plot
dev.off()
###
