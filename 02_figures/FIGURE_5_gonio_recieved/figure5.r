### figure 5
# show gonio and satellite reception message by message

# library
library(sattagutils) # github.com/williamcioffi/sattagutils

# load data
load("../../00_data/tags/series_tags_w_meta.rdata")

### start plot
pdf("figure5.pdf", width = 6, height = 4)
###

# messages download source
par(mar = c(4.1, 4.1, 1.1, 1.1), oma = rep(0, 4))
plot(0, 0, ylim = c(0.5, 10), xlim = c(0, 13.6), xlab = "days of data collection", ylab = "", type = 'n', axes = FALSE)
axis(1, at = 0:14)
axis(1, at = 0:14, lab = NA, tcl = 0.3)
axis(2, at = 1:8, lab = sort(sattagutils::DeployID(tags_allmsg)), las = 1)
axis(2, at = 1:8, lab = NA, tcl = 0.3)
box()

k <- 0
for(i in order(sattagutils::DeployID(tags_allmsg))) {
  curtag <- tags_allmsg[[i]]
  k <- k + 1
	
  if("seriesrange" %in% sattagutils::streamtype(curtag)) {
    smsg <- getstream(curtag, "seriesrange", squash = TRUE)
    
    # make running time
    smsg$End <- smsg$End - min(smsg$Start)
    smsg$Start <- smsg$Start - min(smsg$Start)
    
    # rescale to hours
    smsg$End <- smsg$End / 60 / 60 / 24
    smsg$Start <- smsg$Start / 60 / 60 / 24
    
    if(!is.null(smsg$original)) {
      rect(smsg$Start[smsg$original == "portal"], k - 0.45, smsg$End[smsg$original == "portal"], k + 0.45, col = rgb(1, 0, 0, .5))
      rect(smsg$Start[smsg$original == "gonio"],  k - 0.45, smsg$End[smsg$original == "gonio"], k + 0.45, col = rgb(0, 0, 1, .5))
    } else {
      rect(smsg$Start, 0, smsg$End, 1, col = rgb(1, 0, 0, .5))
    }
  }
}

legend("top", legend = c("satellite", "field only", "both"), pt.bg = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5), rgb(.5, 0, .5)), pch = 22, bty = 'n', ncol = 3)

### end plot
dev.off()
###
