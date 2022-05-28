### Figure 3
# large comparison of metrics
# time-series derrived dives vs. dive-summary data
# at 2.5, 5, and 7.5 minute sampling periods

# libraries
library(sattagutils) # github.com/williamcioffi/sattagutils

# constants
ERRORBARCOLOR <- rgb(48/255, 102/255, 190/255)

# load tag data
tags <- readRDS("../../00_data/tags/baseline_tags.rds")

# calculate depth ranges
abeh <- sattagutils::getstream(tags, "behavior", squash = TRUE)

drange <- abeh$DepthMax - abeh$DepthMin
depdep <- (abeh$DepthMax + abeh$DepthMin) / 2
drange_hi <- drange/2
drange_lo <- -drange/2

drangedf <- data.frame(depdep, drange_hi, drange_lo)
drangedf <- drangedf[!is.na(drangedf[, 1]), ]
drangedf <- drangedf[order(drangedf$depdep), ]
drangedf <- drangedf[!duplicated(drangedf), ]

# load in metric data for each sampling period
periods <- c(2.5, 5, 7.5)
allouts <- list()
for(n in 1:length(periods)) {
  allouts_tmp <- readRDS(file.path(paste0("../../00_data/working/outs_alltags_", periods[n], "_period_brs_censored.rds")))
  allouts_tmp <- allouts_tmp[!is.na(allouts_tmp)]
  
  allouts[[n]] <- allouts_tmp
}

# helper list of letters for figure labels
letterlabs <- list(
  c('a', 'd', 'g', 'j', 'm'),
  c('b', 'e', 'h', 'k', 'n'),
  c('c', 'f', 'i', 'l', 'o')
)

### make figure
pdf('figure3.pdf', width = 5.5*1.65, height = 9.5*1.4)
###

par(mfcol = c(5, 3), mar = c(4.1, 4.1, 1.1, 2.1), oma = c(0, 4.1, 4.1, 0))

for(i in 1:length(allouts)) {
outs <- allouts[[i]]

# concat some useful metrics
what <- do.call('c', lapply(outs, "[[", "what"))
conv_dur <- do.call('c', lapply(outs, "[[", "conv_dur"))
orig_dur <- do.call('c', lapply(outs, "[[", "orig_dur"))

conv_dep <- do.call('c', lapply(outs, "[[", "conv_dep"))
orig_dep <- do.call('c', lapply(outs, "[[", "orig_dep"))

conv_shp <- do.call('c', lapply(outs, "[[", "conv_shp"))
orig_shp <- do.call('c', lapply(outs, "[[", "orig_shp"))
		
# duration hist
hist(conv_dur - orig_dur, nclass = 20, col = "grey", main = "", xlab = "duration difference (s)", las = 1, xlim = c(-1500, 1500), ylim = c(0, 400))
box()
axis(1, tcl = 0.3, labels = NA)
axis(2, tcl = 0.3, labels = NA)
abline(v = c(-periods[i]*60, periods[i]*60), lty = 2, lwd = 2, col = ERRORBARCOLOR)

### letter label
mtext(paste0('(', letterlabs[[i]][1], ')'), side = 2, line = 3.1, at = par()$usr[4], las = 1)

### COL LABEL
mtext(paste0(periods[i], '-minute sampling'), 3, line = 1.1, font = 2)

### ROW LABEL
if(i == 1) mtext("estimated vs. recorded duration", 2, line = 6.1, font = 2, at = -100)


# duration scatterplot
plot(jitter(orig_dur), jitter(conv_dur - orig_dur),
  pch = 16, 
  cex = .75, 
  las = 1, 
  xlab = "dive summary duration (s)", 
  ylab = "duration difference (s)", 
  main = "", 
  ylim = c(-1500, 1500)
)

axis(1, tcl = 0.3, labels = NA)
axis(2, tcl = 0.3, labels = NA)
abline(h = c(-periods[i]*60 , periods[i]*60), lty = 2 , lwd = 2, col = ERRORBARCOLOR)

### letter label
mtext(paste0('(', letterlabs[[i]][2], ')'), side = 2, line = 3.1, at = par()$usr[4], las = 1)


# depth hist
hist(conv_dep - orig_dep, 
  nclass = 20, 
  xlab = "depth difference (meters)", 
  las = 1, 
  col = "grey", 
  main = "", 
  xlim = c(-250, 250), 
  ylim = c(0, 90)
)

axis(1, tcl = 0.3, labels = NA)
axis(2, tcl = 0.3, labels = NA)
box()

### letter label
mtext(paste0('(', letterlabs[[i]][3], ')'), side = 2, line = 3.1, at = par()$usr[4], las = 1)

### ROW LABEL
if(i == 1) mtext("estimated vs. recorded max depth", 2, line = 6.1, font = 2, at = -20)

# depth scatterplot
plot(jitter(orig_dep), jitter(conv_dep - orig_dep),
  pch = 16, 
  cex = .75, 
  las = 1, 
  xlab = "dive summary max depth (meters)", 
  ylab = "depth difference (meters)", 
  main = "", 
  ylim = c(-250, 250)
)

axis(1, tcl = 0.3, labels = NA)
axis(2, tcl = 0.3, labels = NA)
lines(drangedf$depdep, drangedf$drange_hi, col = ERRORBARCOLOR, lty = 2, lwd = 2)
lines(drangedf$depdep, drangedf$drange_lo, col = ERRORBARCOLOR, lty = 2, lwd = 2)


### letter label
mtext(paste0('(', letterlabs[[i]][4], ')'), side = 2, line = 3.1, at = par()$usr[4], las = 1)


# make a heat map of confusion matrix
shapemat <- matrix(NA, 3, 3)
colnames(shapemat) <- c("Square", "U", "V")
rownames(shapemat) <- c("Square", "U", "V")

shapemat[1, 1] <- length(which(conv_shp == "Square" & orig_shp == "Square"))
shapemat[2, 2] <- length(which(conv_shp == "U" & orig_shp == "U"))
shapemat[3, 3] <- length(which(conv_shp == "V" & orig_shp == "V"))

shapemat[1, 2] <- length(which(conv_shp == "Square" & orig_shp == "U"))
shapemat[1, 3] <- length(which(conv_shp == "Square" & orig_shp == "V"))

shapemat[2, 1] <- length(which(conv_shp == "U" & orig_shp == "Square"))
shapemat[2, 3] <- length(which(conv_shp == "U" & orig_shp == "V"))

shapemat[3, 1] <- length(which(conv_shp == "V" & orig_shp == "Square"))
shapemat[3, 2] <- length(which(conv_shp == "V" & orig_shp == "U"))

shapemat_norm <- shapemat / matrix(colSums(shapemat), 3, 3, byrow = TRUE)

# plot confusion matrix
image(t(shapemat_norm)[, 3:1],
  axes = FALSE,
  xlab = "dive summary shape",
  ylab = "time-series estimated shape",
  col = grey.colors(25)[25:1]
)

box()

### ROW LABEL
if(i == 1) mtext("estimated vs. recorded shape", 2, line = 6.1, font = 2)

### letter label
mtext(paste0('(', letterlabs[[i]][5], ')'), side = 2, line = 3.1, at = par()$usr[4], las = 1)


coords <- c(0, .5, 1)
shapelabels <- c("Sq.", "U", "V")
axis(1, at = coords, lab = shapelabels)
axis(1, at = coords, lab = NA, tcl = 0.3)
axis(2, at = coords[3:1], lab = shapelabels, las = 1)
axis(2, at = coords, lab = NA, tcl = 0.3)
# axis(3, at = coords, lab = colSums(shapemat), tcl = 0)

xmat <- matrix(coords, 3, 3, byrow = TRUE)
ymat1 <- matrix(coords[3:1], 3, 3) + 0.075
ymat2 <- matrix(coords[3:1], 3, 3) - 0.075

text(xmat, ymat1, sprintf("%.2f", round(shapemat_norm, 2)), cex = 2)
text(xmat, ymat2, paste0("(", shapemat, ")"), cex = 1.5)
abline(v = c(0.25, 0.75))
abline(h = c(0.25, 0.75))
}

### end figure
dev.off()
###