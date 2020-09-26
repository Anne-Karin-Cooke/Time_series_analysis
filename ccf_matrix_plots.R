
library(tseries)
library(ggfortify)

### Acf and Cross corr
ts_vgg_p1 <- ts(all$p1, frequency = 1)
ts_vgg_p2 <- ts(all$p2, frequency = 1)
ts_vgg_p3 <- ts(all$p3, frequency = 1)

acf(ts_vgg_p1, na.action=na.pass)
acf(ts_vgg_p2, na.action=na.pass)
acf(ts_vgg_p3, na.action=na.pass)

ts_Bdiff <- ts(Boug_diff, frequency = 1)
ts_iGdiff <- ts(Boug_iG$V12-Boug_iG$V12[1], frequency = 1)
ts_sim_grav_diff <- ts(sim_grav_diff, frequency = 1)

ts_sd2 <- ts(gek_ctd_wl_sd2$gek_ctd_wl_sd2_pub..m., frequency = 1)
ts_sd1 <- ts(gek_ctd_wl_sd1$gek_ctd_wl_sd1_pub..m., frequency = 1)
ts_sc1 <- ts(gek_ctd_wl_sc1$gek_ctd_wl_sc1_pub..m., frequency = 1)
ts_prec <- ts(Boug_iG$prec, frequency = 1)



library(ggfortify)

# par(mar=c(2,4,1,3), xpd=F)
# par(mfrow=c(3,3))
par(mar=(c(3,5,3,1)), mfrow=c(3,3))


#sd1, p2
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sd1,ts_vgg_p1,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="p1", xlab="", main="SD1", cex.main=2, cex.lab=2, font.lab=2, cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

#sd2, p1
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sd2,ts_vgg_p1,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", main="SD2", cex.main=2, cex.lab=2, cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

#sc1, p1
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sc1,ts_vgg_p1,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", main="SC1", cex.main=2, cex.lab=2, cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

#sd1, p2
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sd1,ts_vgg_p2,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="p2", xlab="", cex.lab=2, font.lab=2, cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

#sd2, p2
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sd2,ts_vgg_p2,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

#sc1, p2
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sc1,ts_vgg_p2,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))





 #sd1, p2
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sd1,ts_vgg_p3,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="p3", xlab="", cex.lab=2, font.lab=2, cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

#sd2, p1
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sd2,ts_vgg_p3,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

#sc1, p1
ccf1 <- as.data.frame(fortify(stats::ccf(ts_sc1,ts_vgg_p3,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))




par(mar=(c(3,5,3,1)), mfrow=c(2,3))

ccf1 <- as.data.frame(fortify(stats::ccf(ts_prec,ts_vgg_p1,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="effective precipitation", xlab="", main="p1", cex.axis=1.5, font.lab=2, cex.lab=1.5, cex.main=1.5 )
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))



ccf1 <- as.data.frame(fortify(stats::ccf(ts_prec,ts_vgg_p2,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", main="p2", cex.axis=1.5, cex.axis=1.5, font.lab=2, cex.lab=1.5, cex.main=1.5 )
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))



ccf1 <- as.data.frame(fortify(stats::ccf(ts_prec,ts_vgg_p3,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", cex.axis=1.5, main="p3", cex.axis=1.5, font.lab=2, cex.lab=1.5, cex.main=1.5 )
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))

# igrav

ccf1 <- as.data.frame(fortify(stats::ccf(ts_iGdiff,ts_vgg_p1,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="iGrav residuals", xlab="", cex.axis=1.5, cex.axis=1.5, font.lab=2, cex.lab=1.5, cex.main=1.5 )
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))



ccf1 <- as.data.frame(fortify(stats::ccf(ts_iGdiff,ts_vgg_p2,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))



ccf1 <- as.data.frame(fortify(stats::ccf(ts_iGdiff,ts_vgg_p3,lag.max=100,plot=FALSE, na.action = na.pass)))
plot(ccf1$Lag, ccf1$ACF, type="h", ylab="", xlab="", cex.axis=1.5)
abline(h=0, col="red")
abline(h=(ccf1$upper[1]), col="blue", lty=c(3), lwd=2)
abline(h=(ccf1$lower[1]), col="blue", lty=c(3), lwd=2)
order = ccf1[order(-(abs(ccf1$ACF))),]
sign_lags <- order[which(abs(order$ACF) > order$upper),]
sign_lags <- order[which(abs(order$Lag) > 1),]
grid(col="grey2")
print(head(sign_lags))


ccf(ts(Boug_iG$V12),ts(grads02_p3), lag.max = 365)
ccf(ts(Boug_iG$V12),ts(all$p3), lag.max = 365, na.action =na.pass)
