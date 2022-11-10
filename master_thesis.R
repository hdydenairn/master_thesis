### Harrison Dyde-Nairn
### TFM - Master of Economics 2020/21
### Universitat Jaume I

# required packages
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(panelvar)
library(plm)
library(astsa)

### attach data ----

### Zivot & Andrews - Structural Break Tests -----
## migration

BEL <- ts(na.omit(tfm.netmigration$Belgium), start=1990, end=2020,
          frequency=1)

summary(BEL)

za.BEL.t <- ur.za(BEL, model = "trend", lag = 3)
summary(za.BEL.t)
plot(za.BEL.t)

GER <- ts(na.omit(tfm.netmigration$Germany), start=1990, end=2020,
          frequency=1)

summary(GER)

za.GER.t <- ur.za(GER, model = "intercept", lag = 3)
summary(za.GER.t)
plot(za.GER.t)


IRE <- ts(na.omit(tfm.netmigration$Ireland), start=1990, end=2020,
          frequency=1)

summary(IRE)

za.IRE.t <- ur.za(IRE, model = "intercept", lag = 3)
summary(za.IRE.t)
plot(za.IRE.t)


GRE <- ts(na.omit(tfm.netmigration$Greece), start=1990, end=2020,
          frequency=1)

summary(GRE)

za.GRE.t <- ur.za(GRE, model = "both", lag = 3)
summary(za.GRE.t)
plot(za.GRE.t)


ESP <- ts(na.omit(tfm.netmigration$Spain), start=1990, end=2020,
          frequency=1)

plot(ESP)

summary(ESP)

za.ESP.t <- ur.za(ESP, model = "intercept", lag = 3)
summary(za.ESP.t)
plot(za.ESP.t)


FRA <- ts(na.omit(tfm.netmigration$France), start=1998, end=2020,
          frequency=1)

summary(FRA)

za.FRA.t <- ur.za(FRA, model = "trend", lag = 3)
summary(za.FRA.t)
plot(za.FRA.t)


ITA <- ts(na.omit(tfm.netmigration$Italy), start=1990, end=2020,
          frequency=1)

summary(ITA)

za.ITA.t <- ur.za(ITA, model = "intercept", lag = 3)
summary(za.ITA.t)
plot(za.ITA.t)

LUX <- ts(na.omit(tfm.netmigration$Luxembourg), start=1990, end=2020,
          frequency=1)

summary(LUX)

za.LUX.t <- ur.za(LUX, model = "intercept", lag = 3)
summary(za.LUX.t)
plot(za.LUX.t)

NED <- ts(na.omit(tfm.netmigration$Netherlands), start=1990, end=2020,
          frequency=1)

summary(NED)

za.NED.t <- ur.za(NED, model = "intercept", lag = 3)
summary(za.NED.t)
plot(za.NED.t)


AUT <- ts(na.omit(tfm.netmigration$Austria), start=1990, end=2020,
          frequency=1)

summary(AUT)

za.AUT.t <- ur.za(AUT, model = "intercept", lag = 3)
summary(za.AUT.t)
plot(za.AUT.t)


POR <- ts(na.omit(tfm.netmigration$Portugal), start=1990, end=2020,
          frequency=1)

summary(POR)

za.POR.t <- ur.za(POR, model = "intercept", lag = 3)
summary(za.POR.t)
plot(za.POR.t)

FIN <- ts(na.omit(tfm.netmigration$Finland), start=1990, end=2020,
          frequency=1)

summary(FIN)

za.FIN.t <- ur.za(FIN, model = "intercept", lag = 3)
summary(za.FIN.t)
plot(za.FIN.t)

EMU <- ts(na.omit(tfm.netmigration$EMU.12.agg.), start=1990, end=2020,
          frequency=1)

summary(EMU)

za.EMU.t <- ur.za(EMU, model = "trend", lag = 3)
summary(za.EMU.t)
plot(za.EMU.t)

## employment growth

BEL <- ts(na.omit(tfm.employment$Belgium), start=1996, end=2020,
          frequency=1)

summary(BEL)

za.BEL.t <- ur.za(BEL, model = "trend", lag = 3)
summary(za.BEL.t)
plot(za.BEL.t)

GER <- ts(na.omit(tfm.employment$Germany), start=1996, end=2020,
          frequency=1)

summary(GER)

za.GER.t <- ur.za(GER, model = "intercept", lag = 3)
summary(za.GER.t)
plot(za.GER.t)


IRE <- ts(na.omit(tfm.employment$Ireland), start=1996, end=2020,
          frequency=1)

summary(IRE)

za.IRE.t <- ur.za(IRE, model = "both", lag = 3)
summary(za.IRE.t)
plot(za.IRE.t)


GRE <- ts(na.omit(tfm.employment$Greece), start=1996, end=2020,
          frequency=1)

summary(GRE)

za.GRE.t <- ur.za(GRE, model = "intercept", lag = 3)
summary(za.GRE.t)
plot(za.GRE.t)


ESP <- ts(na.omit(tfm.employment$Spain), start=1990, end=2020,
          frequency=1)

plot(ESP)

summary(ESP)

za.ESP.t <- ur.za(ESP, model = "both", lag = 3)
summary(za.ESP.t)
plot(za.ESP.t)


FRA <- ts(na.omit(tfm.employment$France), start=1996, end=2020,
          frequency=1)

summary(FRA)

za.FRA.t <- ur.za(FRA, model = "both", lag = 3)
summary(za.FRA.t)
plot(za.FRA.t)


ITA <- ts(na.omit(tfm.employment$Italy), start=1996, end=2020,
          frequency=1)

summary(ITA)

za.ITA.t <- ur.za(ITA, model = "both", lag = 3)
summary(za.ITA.t)
plot(za.ITA.t)

LUX <- ts(na.omit(tfm.employment$Luxembourg), start=1996, end=2020,
          frequency=1)

summary(LUX)

za.LUX.t <- ur.za(LUX, model = "both", lag = 3)
summary(za.LUX.t)
plot(za.LUX.t)

NED <- ts(na.omit(tfm.employment$Netherlands), start=1996, end=2020,
          frequency=1)

summary(NED)

za.NED.t <- ur.za(NED, model = "both", lag = 3)
summary(za.NED.t)
plot(za.NED.t)


AUT <- ts(na.omit(tfm.employment$Austria), start=1996, end=2020,
          frequency=1)

summary(AUT)

za.AUT.t <- ur.za(AUT, model = "intercept", lag = 3)
summary(za.AUT.t)
plot(za.AUT.t)


POR <- ts(na.omit(tfm.employment$Portugal), start=1996, end=2020,
          frequency=1)

summary(POR)

za.POR.t <- ur.za(POR, model = "trend", lag = 3)
summary(za.POR.t)
plot(za.POR.t)

FIN <- ts(na.omit(tfm.employment$Finland), start=1996, end=2020,
          frequency=1)

summary(FIN)

za.FIN.t <- ur.za(FIN, model = "both", lag = 3)
summary(za.FIN.t)
plot(za.FIN.t)

## participation rate

BEL <- ts(na.omit(tfm.participation$Belgium), start=1995, end=2019,
          frequency=1)

summary(BEL)

za.BEL.t <- ur.za(BEL, model = "intercept", lag = 3)
summary(za.BEL.t)
plot(za.BEL.t)

GER <- ts(na.omit(tfm.participation$Germany), start=1995, end=2019,
          frequency=1)

summary(GER)

za.GER.t <- ur.za(GER, model = "both", lag = 3)
summary(za.GER.t)
plot(za.GER.t)


IRE <- ts(na.omit(tfm.participation$Ireland), start=1995, end=2019,
          frequency=1)

summary(IRE)

za.IRE.t <- ur.za(IRE, model = "intercept", lag = 3)
summary(za.IRE.t)
plot(za.IRE.t)


GRE <- ts(na.omit(tfm.participation$Greece), start=1995, end=2019,
          frequency=1)

summary(GRE)

za.GRE.t <- ur.za(GRE, model = "intercept", lag = 3)
summary(za.GRE.t)
plot(za.GRE.t)


ESP <- ts(na.omit(tfm.participation$Spain), start=1996, end=2019,
          frequency=1)

plot(ESP)

summary(ESP)

za.ESP.t <- ur.za(ESP, model = "intercept", lag = 3)
summary(za.ESP.t)
plot(za.ESP.t)


FRA <- ts(na.omit(tfm.participation$France), start=1996, end=2019,
          frequency=1)

summary(FRA)

za.FRA.t <- ur.za(FRA, model = "intercept", lag = 3)
summary(za.FRA.t)
plot(za.FRA.t)


ITA <- ts(na.omit(tfm.participation$Italy), start=1996, end=2019,
          frequency=1)

summary(ITA)

za.ITA.t <- ur.za(ITA, model = "intercept", lag = 3)
summary(za.ITA.t)
plot(za.ITA.t)

LUX <- ts(na.omit(tfm.participation$Luxembourg), start=1995, end=2019,
          frequency=1)

summary(LUX)

za.LUX.t <- ur.za(LUX, model = "intercept", lag = 3)
summary(za.LUX.t)
plot(za.LUX.t)

NED <- ts(na.omit(tfm.participation$Netherlands), start=1995, end=2019,
          frequency=1)

summary(NED)

za.NED.t <- ur.za(NED, model = "intercept", lag = 3)
summary(za.NED.t)
plot(za.NED.t)


AUT <- ts(na.omit(tfm.participation$Austria), start=1995, end=2019,
          frequency=1)

summary(AUT)

za.AUT.t <- ur.za(AUT, model = "trend", lag = 3)
summary(za.AUT.t)
plot(za.AUT.t)


POR <- ts(na.omit(tfm.participation$Portugal), start=1995, end=2019,
          frequency=1)

summary(POR)

za.POR.t <- ur.za(POR, model = "intercept", lag = 3)
summary(za.POR.t)
plot(za.POR.t)

FIN <- ts(na.omit(tfm.participation$Finland), start=1995, end=2019,
          frequency=1)

summary(FIN)

za.FIN.t <- ur.za(FIN, model = "both", lag = 3)
summary(za.FIN.t)
plot(za.FIN.t)

## employment rate

BEL <- ts(na.omit(tfm.employmentrate$Belgium), start=1991, end=2019,
          frequency=1)

summary(BEL)

za.BEL.t <- ur.za(BEL, model = "both", lag = 3)
summary(za.BEL.t)
plot(za.BEL.t)

GER <- ts(na.omit(tfm.employmentrate$Germany), start=1991, end=2019,
          frequency=1)

summary(GER)

za.GER.t <- ur.za(GER, model = "intercept", lag = 3)
summary(za.GER.t)
plot(za.GER.t)


IRE <- ts(na.omit(tfm.employmentrate$Ireland), start=1991, end=2019,
          frequency=1)

summary(IRE)

za.IRE.t <- ur.za(IRE, model = "both", lag = 3)
summary(za.IRE.t)
plot(za.IRE.t)


GRE <- ts(na.omit(tfm.employmentrate$Greece), start=1991, end=2019,
          frequency=1)

summary(GRE)

za.GRE.t <- ur.za(GRE, model = "both", lag = 3)
summary(za.GRE.t)
plot(za.GRE.t)


ESP <- ts(na.omit(tfm.employmentrate$Spain), start=1991, end=2019,
          frequency=1)

plot(ESP)

summary(ESP)

za.ESP.t <- ur.za(ESP, model = "both", lag = 3)
summary(za.ESP.t)
plot(za.ESP.t)


FRA <- ts(na.omit(tfm.employmentrate$France), start=1991, end=2019,
          frequency=1)

summary(FRA)

za.FRA.t <- ur.za(FRA, model = "both", lag = 3)
summary(za.FRA.t)
plot(za.FRA.t)


ITA <- ts(na.omit(tfm.employmentrate$Italy), start=1991, end=2019,
          frequency=1)

summary(ITA)

za.ITA.t <- ur.za(ITA, model = "intercept", lag = 3)
summary(za.ITA.t)
plot(za.ITA.t)

LUX <- ts(na.omit(tfm.employmentrate$Luxembourg), start=1991, end=2019,
          frequency=1)

summary(LUX)

za.LUX.t <- ur.za(LUX, model = "trend", lag = 3)
summary(za.LUX.t)
plot(za.LUX.t)

NED <- ts(na.omit(tfm.employmentrate$Netherlands), start=1991, end=2019,
          frequency=1)

summary(NED)

za.NED.t <- ur.za(NED, model = "intercept", lag = 3)
summary(za.NED.t)
plot(za.NED.t)


AUT <- ts(na.omit(tfm.employmentrate$Austria), start=1991, end=2019,
          frequency=1)

summary(AUT)

za.AUT.t <- ur.za(AUT, model = "intercept", lag = 3)
summary(za.AUT.t)
plot(za.AUT.t)


POR <- ts(na.omit(tfm.employmentrate$Portugal), start=1991, end=2019,
          frequency=1)

summary(POR)

za.POR.t <- ur.za(POR, model = "both", lag = 3)
summary(za.POR.t)
plot(za.POR.t)

FIN <- ts(na.omit(tfm.employmentrate$Finland), start=1991, end=2019,
          frequency=1)

summary(FIN)

za.FIN.t <- ur.za(FIN, model = "intercept", lag = 3)
summary(za.FIN.t)
plot(za.FIN.t)

### employment rate ~ employment correlation ----

FOLS <- plm(employmentrate ~ employment, data=tfm.1996_2019, index=c("country", "year"), model="within")
summary(FOLS)
plot(FOLS)

### PVAR Analysis ----
# code run with each seperate sample, i.e., tfm.1996_2019, tfm. postcrisis. csv

varone <- pvarfeols(dependent_vars = c("employment","wages","participationrate","employmentrate"),
                    lags = 2,
                    transformation = c("demean"),
                    data = tfm.1996_2008,
                    panel_identifier= c("country", "year")
)

varone <- pvarfeols(dependent_vars = c("employment","wages","participationrate","employmentrate"),
                    lags = 2,
                    transformation = c("demean"),
                    data = tfm.1996_2008,
                    panel_identifier= c("country", "year"))



summary(varone)

stability(varone)

### IRF ----

varone_oirf <- oirf (varone, n.ahead = 10)
varone_oirf
plot(varone_oirf)

## IRF collated graph ----

a<- varone_oirf[["employment"]]

employment_i <- data.frame(a)

employment_levels <- cumsum(employment_i$employment)
wages_levels <- cumsum(employment_i$wages)

period <- c(1,2,3,4,5,6,7,8,9,10)
mobilty <- employment_levels - employment_i$employmentrate - employment_i$participationrate
mobilty
employment <- employment_levels
unemploymentrate <-  -1* employment_i$employmentrate
unemploymentrate
participationrate <- employment_i$participationrate
participationrate
wages <- wages_levels

data <- data.frame(period,mobilty,employment,unemploymentrate,participationrate,wages)

plot(data$period,                              # Draw first time series
     data$mobilty,
     type = "l",
     col = 2,
     ylim = c(- 0.015, 0.02),
     xlab = "years after shock",
     ylab = "log points => 0.01 = 1% ")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
lines(data$period,                            
      data$employment,
      type = "l",
      col = 3)
lines(data$period,                            
      data$unemploymentrate,
      type = "l",
      col = 4)
lines(data$period,                             
      data$participationrate,
      type = "l",
      col = 5)
lines(data$period,                             
      data$wages,
      type = "l",
      col = 6)
legend("bottomright",                           
       c("Mobilty", "Employment", "Unemployment Rate" , "Participation Rate", "Wages"),
       lty = 1,
       col = 2:6)
