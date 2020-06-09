# Round-Trip.R 
# Horace W. Tso (c)
# Jun 5, 2020

library(goodies)
library(quantmod)

home = "/mnt/WanChai/Dropbox/GITHUB_REPO/Monumental_Day"
data_dir = "/data/"
plot_dir = "/plots/"
setwd(home)
source(paste(home, "/utils/fun.R", sep=""))

full_path = paste( home, data_dir, "YF_stock_indices.RData",sep="")
stock_mkt_indices = c("GSPC", "NDX", "RUT", "FTSE", "GDAXI", "FCHI", "N225", "HSI", "STI", 
                      "AORD", "JKSE", "KS11", "TWII", "BVSP", "MXX")

if (file.exists(full_path)) {
  load(full_path)  
} else {
  getYF(stock_mkt_indices)   
  save.image(full_path)
}

x = NDX
x = GSPC
x = RUT
x = FTSE
x = GDAXI
x = FCHI
x = N225
x = HSI
x = STI
x = AORD

tkr = strsplit(colnames(x), "\\.")[[1]][[1]]
# deal with NAs in the yf time series
xx = x[,6]
if ( sum(is.na(xx)) > 0 ) {
  rl = rle(as.integer(is.na(xx)))
  maxgap = max(rl$lengths[which(rl$values==1)])
  cat("maxgap : ", maxgap, "\n")
  cat("# NA segments : ", length(which(is.na(xx))), "\n")
  x1 = na.spline(xx, maxgap=20, na.rm=TRUE)
  cat("aft NA impute, :", length(which(is.na(x1))), "\n\n")
  x[,6] = x1
}

x1 = x["2020"]
t1 = which.max(x1["::2020-06-04",6])
t2 = which.min(x1[,6])
t3 = nrow(x1)
# How long did it take to go from record high in Feb to the low in March ?
(dt1 = t2 - t1 + 1) # 23
# How long did it take to recover from March low?
(dt2 = t3 - t2 ) # 53
# Observation : Speed of decline is faster than the speed of recovery. This is still correct.
# In fact, it took more than twice as long to go back to previous peak. 

# Roll a window of 53 trading days over the history of index.
forw = as.data.frame(rollapply(zoo(x[,6]), width=53, look_inside, by=1, align="left"))
forw[,"date"] = as.Date(row.names(forw))
colnames(forw)[1] = "Forw"
backw = as.data.frame(rollapply(zoo(x[,6]), width=23, look_inside, by=1, align="right"))
backw[,"date"] = as.Date(row.names(backw))
colnames(backw)[1] = "Backw"
df = na.omit(merge(forw, backw, by="date", all=TRUE))
df1 = df[which(df[,"Backw"] < -0.2),]

# How many rolling windows of more than 20% decline?
# Many are overlapped, so exclude those that are less than one week apart
sum(as.integer(diff(df1[,"date"])) > 5)
# 17

# How many have completely recovered loss in the forward window
df1[,"r"] = sapply(df1[,"Backw"], recov)
ix = which(df1[,"Forw"] > df1[,"r"])
df1[ix,]
# Two : 2001-09-20, 2020-03-17


# :::: VIZ ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
png("NDX_jun2020.png", width=640, height=480)
chartSeries(NDX[,1:4], subset="2020"); title(main="Coronavirus Crash -- June 2020", col.main="white", font.main=4, cex.main=1.5)
dev.off()

ix = which(index(NDX) == as.Date("2001-09-21"))
x1 = x[(ix-23-40):(ix+dt2+1),1:4]
png("NDX_sep2001.png", width=640, height=480)
chartSeries(x1); title(main="Internet Bubble -- Sep 2001", col.main="white", font.main=4, cex.main=1.5)
dev.off()

x2 = NDX[(ix-23-40):(ix+200),1:4]
png("NDX_sep2001_yr_later.png", width=640, height=480)
chartSeries(x2); title(main="... A year later", col.main="white", font.main=4, cex.main=1.5)
dev.off()

png("Forward_in_roll_window.png", width=640, height=480)
plot(forw, type="l", xlab="Date", ylab="Forward return %", main="Forward Return in rolling window");abline(h=0)
dev.off()

