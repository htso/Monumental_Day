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
ix = which(diff(df1[,"date"]) > 5)
len(ix)
# 17
df1[ix,]

         date        Forw      Backw         r
540  1987-11-17  0.10726108 -0.2166572 0.2765802
1239 1990-08-23  0.01102104 -0.2127735 0.2702824
3676 2000-04-14  0.14272937 -0.2232561 0.2874256
3684 2000-04-27  0.05647829 -0.2151367 0.2741072
3693 2000-05-10  0.19128245 -0.2439223 0.3226154
3830 2000-11-22 -0.11714797 -0.2202659 0.2824885
3837 2000-12-04 -0.17980340 -0.2080043 0.2626332
3850 2000-12-21 -0.18510097 -0.2032545 0.2551059
3860 2001-01-08 -0.25268898 -0.2002650 0.2504142
3910 2001-03-21  0.19866170 -0.2745615 0.3784765
3923 2001-04-09  0.16624810 -0.2357558 0.3084822
4028 2001-09-07  0.18873635 -0.2016659 0.2526084
4042 2001-10-03  0.31289162 -0.2083974 0.2632602
4224 2002-06-25 -0.08849758 -0.2051882 0.2581595
4229 2002-07-02 -0.05726085 -0.2024927 0.2539070
5801 2008-09-29 -0.21100161 -0.2126770 0.2701267
5822 2008-10-28 -0.07315207 -0.2239600 0.2885934




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

