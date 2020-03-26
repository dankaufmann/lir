# ------------------------------------------------------------------------
# R Code for NIR Note
# TODO: Calculate probability of an appreciation
# ------------------------------------------------------------------------
#   by Daniel Kaufmann, 2019 (daniel.kaufmann@unine.ch)
# ------------------------------------------------------------------------
library(tsbox)
library(forecast)
library(ggplot2)
library(quantmod)
library(forecast)
library(xts)
library(xlsx)
library(ggpubr)
ts_fred <- function(..., class = "data.frame") {
  symb <- c(...)
  dta.env <- new.env()
  suppressMessages(getSymbols(symb, env = dta.env, src = "FRED"))
  z <- data.table::rbindlist(lapply(as.list(dta.env), ts_dt), idcol = "id")
  tsbox:::as_class(class)(z)
}
normalize <- function(x){
  x_norm = (x-mean(x, na.rm =TRUE))/sd(x, na.rm =TRUE)
  return(x_norm)
}
ts_cumsum <- function(x){
  # Function to calculate cumulative sums of a time series
  return(ts_(cumsum)(x))
}
ts_cumprod <- function(x){
  # Function to calculate cumulative sums of a time series
  return(ts_(cumprod)(x))
}
#-------------------------------------------------------------------------------------
# Application with CHF data
#-------------------------------------------------------------------------------------
myStart <- "1974-01-01"
myEnd   <- "2019-12-05"
H = 24

# 1) Get Data
#SREUR = ts_xts(ts_frequency(ts_fred("EUR3MTD156N"), to="month", aggregate="last", na.rm = TRUE))
#SRCHF = ts_xts(ts_frequency(ts_fred("CHF3MTD156N"), to="month", aggregate="last", na.rm = TRUE))
#SRUSD = ts_xts(ts_frequency(ts_fred("USD3MTD156N"), to="month", aggregate="last", na.rm = TRUE))
SRData = read.xlsx("C:\\Users\\kaufmannd\\switchdrive\\Public\\Blog\\NegativeIR\\SRData.xlsx", sheetName = "Sheet1", startRow = 1)
XRData = read.xlsx("C:\\Users\\kaufmannd\\switchdrive\\Public\\Blog\\NegativeIR\\XRData.xlsx", sheetName = "Report", startRow = 19)
EURData = read.xlsx("C:\\Users\\kaufmannd\\switchdrive\\Public\\Blog\\NegativeIR\\EPU_EUR.xlsx", sheetName = "European News-Based Index", startRow = 1)
USDData = read.xlsx("C:\\Users\\kaufmannd\\switchdrive\\Public\\Blog\\NegativeIR\\EPU_US.xlsx", sheetName = "Main Index", startRow = 1)

UNC_US = xts(USDData[,3], order.by = as.Date(paste(USDData[,1],"-",USDData[,2],"-1", sep = "")))
UNC_EU = xts(EURData[,3], order.by = as.Date(paste(EURData[,1],"-",EURData[,2],"-1", sep = "")))

SRUSD = xts(SRData[,2], order.by = as.Date(paste(SRData[,1], "-01", sep = "")))
SRGER = xts(SRData[,3], order.by = as.Date(paste(SRData[,1], "-01", sep = "")))
SRCHF = xts(SRData[,4], order.by = as.Date(paste(SRData[,1], "-01", sep = "")))

LRCHF = ts_xts(ts_fred("IRLTLT01CHM156N"))
LRGER = ts_xts(ts_fred("IRLTLT01DEM156N"))
LRUSD = ts_xts(ts_fred("IRLTLT01USM156N"))

EURUSD = ts_xts(ts_fred("CCUSSP01DEM650N"))
dEURUSD = ts_pc(EURUSD)

XRCHF = xts(XRData[,2], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))
dXRCHF = ts_pc(XRCHF)
XRCHFr = xts(XRData[,4], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))
dXRCHFr = ts_pc(XRCHFr)
XRCHFEUR = xts(XRData[,3], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))
dXRCHFEUR = ts_pc(XRCHFEUR)
XRCHFEURr = xts(XRData[,5], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))
dXRCHFEURr = ts_pc(XRCHFEURr)

MinXR1 = xts(index(XRCHF) == "2011-09-01", order.by = index(XRCHF))
MinXR2 = xts(index(XRCHF) == "2015-01-01", order.by = index(XRCHF))
Break1 = xts(index(XRCHF) > "2010-01-01", order.by = index(XRCHF))

l.SRGER = lag(SRGER, 1)
l.SRCHF = lag(SRCHF, 1)
l.SRUSD = lag(SRUSD, 1)
l.LRCHF = lag(LRCHF, 1)
l.LRGER = lag(LRGER, 1)
l.LRUSD = lag(LRUSD, 1)
l.dEURUSD = lag(dEURUSD, 1)
l2.dEURUSD = lag(dEURUSD, 2)


l.Inter = l.SRGER*l.SRCHF

l.UNC_US = lag(UNC_US, 1)
l.UNC_EU = lag(UNC_EU, 1)


MyData = ts_c(SRGER, SRCHF, SRUSD, LRCHF, LRGER, LRUSD, l.SRGER, l.SRCHF, l.SRUSD, l.Inter, l.LRCHF, l.LRGER, l.LRUSD, l.UNC_US, l.UNC_EU, l2.dEURUSD,l.dEURUSD, dEURUSD, dXRCHF, dXRCHFr, dXRCHFEUR, dXRCHFEURr,MinXR1, MinXR2, Break1)
MyData = ts_span(MyData, myStart, myEnd)

# 2) Estimate ARIMA Baseline
ModelMan = Arima(ts_ts(MyData$dXRCHF), order = c(1, 0, 1), include.constant= TRUE)
summary(ModelMan)
ForecastMan = forecast(ModelMan, h = H)

# Again, tranform back to levels
# Step 1: Merge the history and the forecast
temp1 = ts_bind(ForecastMan$x, ForecastMan$mean)
temp1[1] = 1

# Step 2: Calculate the cumulative sum of the log-differences and take the exponent to get level
temp2 = ts_cumprod(1+temp1/100)

# Step 4: Split series in history and forecast for the plot
fcstStart = ts_summary(ForecastMan$mean)$start
histEnd   = ts_summary(ForecastMan$x)$end
AutoHist = ts_span(temp2, NULL, histEnd)
AutoFcst = ts_span(temp2, fcstStart, NULL)

# 3) Estimate ARIMAX
ModelExo = Arima(ts_ts(MyData$dXRCHF), order = c(0, 0, 1), include.constant= TRUE, 
        xreg = ts_c(MyData$l.SRCHF, MyData$l.SRGER, MyData$dEURUSD, MyData$l.dEURUSD, MyData$MinXR1, MyData$MinXR2))
summary(ModelExo)
checkresiduals(ModelExo)+theme_minimal()

# 4) Do a baseline
Baseline = ts_c(ts_xts(ForecastMan$mean), ts_xts(ForecastMan$mean),ts_xts(ForecastMan$mean),ts_xts(ForecastMan$mean),ts_xts(ForecastMan$mean),ts_xts(ForecastMan$mean))
Baseline[] = 0
Baseline[,1] = -0.75
Baseline[,2] = MyData$l.SRGER[length(MyData$l.SRGER)]
#[,3] = Baseline[,1]*Baseline[,2]


# Step 2: Create a forecast setting the exogenous variables to the values in the scenario
ForecastBase = forecast(ModelExo, xreg = Baseline)

# Again, tranform back to levels
# Step 1: Merge the history and the forecast
temp1 = ts_bind(ForecastBase$x, ForecastBase$mean)
temp1[1] = 1

# Step 2: Calculate the cumulative sum of the log-differences and take the exponent to get level
temp2 = ts_cumprod(1+temp1/100)

# Step 4: Split series in history and forecast for the plot
BaseFcst = ts_span(temp2, fcstStart, NULL)

# 4) Do a scenario 1
Scenario1 = Baseline
Scenario1[] = 0
Scenario1[,1] = -2
Scenario1[,2] = MyData$l.SRGER[length(MyData$l.SRGER)]
#Scenario1[,3] = Scenario1[,1]*Scenario1[,2]

# Step 2: Create a forecast setting the exogenous variables to the values in the scenario
ForecastScen1 = forecast(ModelExo, xreg = Scenario1)

# Again, tranform back to levels
# Step 1: Merge the history and the forecast
temp1 = ts_bind(ForecastScen1$x, ForecastScen1$mean)
temp1[1] = 1

# Step 2: Calculate the cumulative sum of the log-differences and take the exponent to get level
temp2 = ts_cumprod(1+temp1/100)

# Step 4: Split series in history and forecast for the plot
Scen1Fcst = ts_span(temp2, fcstStart, NULL)

# 4) Do a scenario 2
Scenario2 = Baseline
Scenario2[] = 0
Scenario2[,1] = 0
Scenario2[,2] = MyData$l.SRGER[length(MyData$l.SRGER)]

# Step 2: Create a forecast setting the exogenous variables to the values in the scenario
ForecastScen2 = forecast(ModelExo, xreg = Scenario2)

# Again, tranform back to levels
# Step 1: Merge the history and the forecast
temp1 = ts_bind(ForecastScen2$x, ForecastScen2$mean)
temp1[1] = 1

# Step 2: Calculate the cumulative sum of the log-differences and take the exponent to get level
temp2 = ts_cumprod(1+temp1/100)

# Step 4: Split series in history and forecast for the plot
Scen2Fcst = ts_span(temp2, fcstStart, NULL)

# 4) Do a scenario 3
Scenario3 = Baseline
Scenario3[] = 0
Scenario3[,1] = 1
Scenario3[1:24,1] = seq(-0.75, 2, length.out = 24)
Scenario3[,2] = MyData$l.SRGER[length(MyData$l.SRGER)]
#Scenario3[,3] = Scenario3[,1]*Scenario3[,2]

# Step 2: Create a forecast setting the exogenous variables to the values in the scenario
ForecastScen3 = forecast(ModelExo, xreg = Scenario3)

# Again, tranform back to levels
# Step 1: Merge the history and the forecast
temp1 = ts_bind(ForecastScen3$x, ForecastScen3$mean)
temp1[1] = 1

# Step 2: Calculate the cumulative sum of the log-differences and take the exponent to get level
temp2 = ts_cumprod(1+temp1/100)

# Step 4: Split series in history and forecast for the plot
Scen3Fcst = ts_span(temp2, fcstStart, NULL)

# Redo a plot with the automatically selected and manual forecast
ts_plot(
  `History`= ts_span(AutoHist, "2012-01-01"),
  `Forecast (Baseline)` = BaseFcst,
  `Forecast (Scenario 1: Expansion)`   = Scen1Fcst,
  `Forecast (Scenario 2: Removal NIR)` = Scen2Fcst,
  `Forecast (Scenario 3: Contraction)` = Scen3Fcst,
  title = "Exchange rate forecast",
  subtitle = "Index, increase is an appreciation"
)
ts_save(filename = "AutoForecast.pdf", width = 8, height = 5, open = FALSE)

sdf



sdf


# Step 3: Transform the forecast to year-on-year growth rates
temp1 = ts_bind(ForecastScen1$x, ForecastScen1$mean)
temp1[1] = 0
temp2 = exp(ts_cumsum(temp1))
temp3 = ts_pcy(temp2)
FcstScen1YoY = ts_span(temp3, fcstStart, NULL)

# b) Scenario 2: We assume that the oil price increases by 5% each month from January 2020 until December 2023
Scenario2 = Scenario1
Scenario2["2020-01-01/2023-12-01", 1] = 0.05

ForecastScen2 = forecast(ModelX2, xreg = Scenario2)

temp1 = ts_bind(ForecastScen2$x, ForecastScen2$mean)
temp1[1] = 0
temp2 = exp(ts_cumsum(temp1))
temp3 = ts_pcy(temp2)
FcstScen2YoY = ts_span(temp3, fcstStart, NULL)

# c) Scenario 3: We assume that the oil price drops by 40% in November 2019 and december 2019
Scenario3 = Scenario1
Scenario3["2019-11-01/2019-12-01", 1] = -0.4

ForecastScen3 = forecast(ModelX2, xreg = Scenario3)

temp1 = ts_bind(ForecastScen3$x, ForecastScen3$mean)
temp1[1] = 0
temp2 = exp(ts_cumsum(temp1))
temp3 = ts_pcy(temp2)
FcstScen3YoY = ts_span(temp3, fcstStart, NULL)

# Redo a plot for the various scenarios
ts_plot(
  `History`= ts_span(HistYoY, "2000-01-01"),
  `Forecast (Scenario 1)` = FcstScen1YoY,
  `Forecast (Scenario 2)` = FcstScen2YoY,
  `Forecast (Scenario 3)` = FcstScen3YoY,
  title = "UK CPI and forecast",
  subtitle = "Percentage growth to same period previous year (year-on-year)"
)
ts_save(filename = paste(outDir, "/CPIFcstyoy4.pdf", sep = ""), width = 8, height = 5, open = FALSE)



# 3) Calculate scenarios



# 2) Do for term spread, bond spread, initial claims, VIX, TED spread (all from FRED)
myIdents <- c("T10Y2Y",                "CSINFT02USM460S",     "UMCSENT",                    "WLEMUINDXD",     "USEPUINDXD", "BSPRTE02USM460S",          "BSOITE02USM460S",          "HOUST",         "AAA10Y",               "BAA10Y",               "ICSA",           "VIXCLS", "TEDRATE", "MICH",                      "TCU")
myLabels <- c("Term spread (10Y-2Y)", "Inflation exp. (EC)",  "Consumer sent. (Michigan)", "EPU (Equity)",   "EPU",        "Manufacturing prod. (EC)", "Manufacturing ord. (EC)",  "Housing starts", "Bond spread (AAA-10Y)","Bond spread (BAA-10Y)","Initial claims", "VIX",    "TED",     "Inflation exp. (Michigan)",  "Capacity utilization")
for (i in 1:length(myIdents)){

  ind <- myIdents[i]
  lab <- myLabels[i]
  
  # 3) Load the data
  LI = ts_fred(ind)
  LI = xts(LI[,3], order.by=as.Date(LI[,2]))
  
  # 4) Aggregate to quarterly frequency to compare to GDP, and shorten time series
  LIq = ts_frequency(LI, to = "quarter", aggregate= "mean", na.rm = TRUE)
  LIq <- ts_span(LIq, start = myStart)
  LI <- ts_span(LI, start = myStart)
  
  # 5) Do plots original data
  p1 <- ts_ggplot(
    `GDP growth` = GDP, 
    `X`   = LI,
    title = lab
  )+theme_minimal()+ylab("")+xlab("")
  p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
  p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
  p1 <- p1 + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
  p1 <- p1 + theme(panel.grid.major =element_blank()) + theme(panel.grid.minor = element_blank()) 
  p1 <- p1 + theme(legend.justification=c(1,0), legend.position=c(.90,.75))+theme(legend.title=element_blank())
  
  # Do CCF
  p2 <- ggAcf(ts_ts(LIq))+theme_minimal()+ggtitle("")+ylab("Auto-correlation, X(t+s)")+xlab("")
  p3 <- ggCcf(ts_ts(GDP), ts_ts(LIq))+theme_minimal()+ggtitle("")+ylab("Cross-correlation, GDP(t+s), X(t)")+xlab("")
  g1 <- ggarrange(p1,                                                 # First row with scatter plot
                  ggarrange(p2, p3, ncol = 2), # Second row with box and dot plots
                  nrow = 2 
  ) 
  ggsave(paste("./Figures/Estimation_", ind, ".png", sep =""), plot = g1, width = 17, height = 15, units = c("cm"))
  
  # 6) Compute ACF pre-whitened data
  ModelLI <- auto.arima(LIq, max.d = 0, max.p = 5, max.q = 5, ic = c("aic"))
  ModelGDP <- auto.arima(GDP, max.d = 0, max.p = 5, max.q = 5, ic = c("aic"))
  
  rLI <- xts(resid(ModelLI), order.by = index(LIq))
  rGDP <- xts(resid(ModelGDP), order.by = index(GDP))
  
  p1 <- ts_ggplot(
    `GDP growth` = rGDP, 
    `X` = rLI,
    title = lab
  )+theme_minimal()+ylab("")+xlab("")
  p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
  p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
  p1 <- p1 + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
  p1 <- p1 + theme(panel.grid.major =element_blank()) + theme(panel.grid.minor = element_blank()) 
  p1 <- p1 + theme(legend.justification=c(1,0), legend.position=c(.90,.75))+theme(legend.title=element_blank())
  
  # Do CCF
  p2 <- ggAcf(ts_ts(rLI))+theme_minimal()+ggtitle("")+ylab("Auto-correlation, X(t+s)")+xlab("")
  p3 <- ggCcf(ts_ts(rGDP), ts_ts(rLI))+theme_minimal()+ggtitle("")+ylab("Cross-correlation, GDP(t+s), X(t)")+xlab("")
  g1 <- ggarrange(p1,                                                 # First row with scatter plot
                  ggarrange(p2, p3, ncol = 2), # Second row with box and dot plots
                  nrow = 2 
  ) 
  ggsave(paste("./Figures/EstimationWhite_", ind, ".png", sep =""), plot = g1, width = 17, height = 15, units = c("cm"))
  
  print(paste(lab, "Start - End", ts_summary(LI)$start, ts_summary(LI)$end, sep = ", "))
}

# 7) create a simple leading indicator for the US using indicators that worked well (case study)
# Lead in the order of above indicators (in quarters). 0 means it is not included
myLeads   <- c(2, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1)
mySigns   <- c(1, 0, 1, 0, 0, 1, 1, 1,-1,-1,-1, 0, 0, 0, 1)
for (i in 1:length(myIdents)){
  
  leads <- myLeads[i]
  ind  <- myIdents[i]
  sig  <- mySigns[i]
  
  # Only use those with a lead
  if (leads > 0){
    
    LI = ts_fred(ind)
    LI = xts(LI[,3], order.by=as.Date(LI[,2]))
    LI <- ts_span(LI, start = myStart)
    
    # 0) Aggregate to monthly
    LI <- ts_frequency(LI, to = "month", aggregate = "mean", na.rm = TRUE)
    
    # 1) Normalize indicator
    LI = sig*normalize(LI)
    
    # 2) Lead so that *concident* with GDP growth (note that measured in quarters and we have 
    #    to move variable forward in time (so lag the variable)). This just makes sure that all
    #    indicators have the same lead on GDP growth (1q)
    LI <- lag(LI, leads*3-3)
    
    # 3) Add to data frame for later use
    if (i>1){
      Collect <- ts_c(Collect, LI)  
    }else{
      Collect <- ts_c(LI)
    }
  }
}
# Aggregate and shorten series so that no missing at end
CLI   <- xts(rowMeans(Collect, na.rm = TRUE), order.by = index(Collect))*as.numeric(sqrt(var((GDP))))
CLInm <- xts(rowMeans(Collect, na.rm = FALSE), order.by = index(Collect))*as.numeric(sqrt(var((GDP))))

# Shorten because many missing values at the end!
CLI <- ts_span(CLI, end = myEnd)
CLInm <- ts_span(CLInm, end = myEnd)
CLIPhilly = ts_fred("USSLIND")
CLIPhilly = xts(CLIPhilly[, 3], order.by = CLIPhilly[, 2])
#CLIPhilly <- normalize(CLIPhilly)*as.numeric(sqrt(var((GDP))))*0.9

# Plot with GDP growth

p1 <- ts_ggplot(
  `GDP growth` = rGDP, 
  `CLI` = CLI,
  title = "Comparison with GDP growth"
)+theme_minimal()+ylab("")+xlab("")
p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
p1 <- p1 + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
p1 <- p1 + theme(panel.grid.major =element_blank()) + theme(panel.grid.minor = element_blank()) 
p1 <- p1 + theme(legend.justification=c(1,0), legend.position=c(.90,.75))+theme(legend.title=element_blank())
p1
ggsave(paste("./Figures/CLIandGDP.png", sep =""), plot = p1, width = 13, height = 10, units = c("cm"))

p1 <- ts_ggplot(
  `CLI Philly Fed` = CLIPhilly, 
  `CLI` = CLI,
  title = "Comparison with Philly Fed CLI"
)+theme_minimal()+ylab("")+xlab("")
p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
p1 <- p1 + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
p1 <- p1 + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
p1 <- p1 + theme(panel.grid.major =element_blank()) + theme(panel.grid.minor = element_blank()) 
p1 <- p1 + theme(legend.justification=c(1,0), legend.position=c(.90,.75))+theme(legend.title=element_blank())
p1
ggsave(paste("./Figures/CLIandPhilly.png", sep =""), plot = p1, width = 13, height = 10, units = c("cm"))


# Do plot with NBER recessions


# From http://www.nber.org/cycles.html
NBERREC = read.table(textConnection(
  "Peak, Trough
  1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)
NBERREC = subset(NBERREC, Peak >= as.Date("1980-01-01") )

g = ggplot(CLI) + geom_line(aes(x=index(CLI), y=CLI)) + theme_minimal()
g = g + geom_rect(data=NBERREC, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g = g + geom_hline(yintercept=mean(CLI), linetype="dashed", color = "red")
g = g + xlab("") +  ggtitle("Comparison with NBER recessions")
g
ggsave("./Figures/CLIandRecessions.png", plot = last_plot(), width = 13, height = 10, units = c("cm"))
