
if(!require(mice)){install.packages("mice")}
if(!require(Kendall)){install.packages("Kendall")}
if(!require(trend)){install.packages("trend")}

# dataframe with estimated VGG, daily_rainfall etc
# p1, p2, 3: pillar number on which VGG were estimated from measurements
df_all_vgg_bh <-read.table("/home/df_all_vgg_bh.txt", sep=",", header=T)

library(tseries)

p3 <- (ts(df_all_vgg_bh$p3, frequency = 1))
p2 <- (ts(df_all_vgg_bh$p2, frequency = 1))
p1 <- (ts(df_all_vgg_bh$p1, frequency = 1))


### Trend test MK

library(Kendall)


MKp3 = MannKendall(p3)
MKp2 = MannKendall(p2)
MKp1 = MannKendall(p1)

summary(MKp3) ## significant trend in time
summary(MKp2)  ## not sign
summary(MKp1) ## barely signicant

### Trend test Seasonal MK

library(Kendall)

SMK = SeasonalMannKendall(p3)

summary(SMK)

### Sen slope
library(trend)

func_senslope <- function(x,na.rm=T) {
  if(sum(is.na(x)) == length(x)) return(NA)
  if(na.rm) x <- x[!is.na(x)]
  (sens.slope(x)) 
}
func_senslope(p3)
### Pettitt’s test for change in value

library(trend)
func_pettitt <- function(x,na.rm=T) {
  if(sum(is.na(x)) == length(x)) return(NA)
  if(na.rm) x <- x[!is.na(x)]
  (pettitt.test(x)) 
}

func_pettitt(p3)





