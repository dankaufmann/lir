# ------------------------------------------------------------------------
# Ausstieg aus der Negativzinspolitik: Szenarien und Alternativen
# * Quartals-Modell für BIP und andere Makro-Variablen
# ------------------------------------------------------------------------
#
# Notes:
#
# TODO:
# - Szernarien aus Monatsmodell verwenden
# - OECD BIP Prognose
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
cat("\014") 
rm(list=ls())
library(tsbox)
library(forecast)
library(ggplot2)
library(RColorBrewer)
library(forecast)
library(xts)
library(xlsx)
library(lubridate)
library(ggpubr)
library(vars)
library(seasonal)
library(egcm)
library(stargazer)
source("SVARFunctions.R")

#-------------------------------------------------------------------------------------
# Einstellungen
#-------------------------------------------------------------------------------------
myStart   <- "1990-04-01"
myEnd     <- "2007-12-01"
Frequency <- "quarter"
figWdth   <- 4.5
figHeight <- 4.5
varIndex <- 1
# Wählt den jeweiligen Sektor...
#1: BIP, 2: Industrie, 3: Banken, 4: Handel
GDPLabs <- c("BIP", "Industrie", "Banken", "Handel")

#-------------------------------------------------------------------------------------
# Datenimport
#-------------------------------------------------------------------------------------
load('../Resultate/Szenarien_Endogen_Q.rdata')

GDP    = read.xlsx("../Daten/GDPDataSAQ.xlsx", sheetName = "Data", startRow = 12)
SRData = read.xlsx("../Daten/SRDataM.xlsx", sheetName = "Data", startRow = 1)
LRData = read.xlsx("../Daten/LRDataM.xlsx", sheetName = "Data", startRow = 1)
XRData = read.xlsx("../Daten/XRDataM.xlsx", sheetName = "Data", startRow = 19)
StockData = read.xlsx("../Daten/StockDataM.xlsx", sheetName = "Data", startRow = 1)
HypData = read.xlsx("../Daten/HypoCashIRM.xlsx", sheetName = "Data", startRow = 20)
GDPFor    = read.xlsx("../Daten/EAGDPForecastA.xlsx", sheetName = "Tabelle1", startRow = 2)
GDPForQ = read.xlsx("../Daten/GDPFor_Q.xlsx", sheetName = "Data", startRow = 1)
GDPForFcstQ = read.xlsx("../Daten/GDPForFcst_Q.xlsx", sheetName = "ForImport", startRow = 1)

# Domestic variables to be forecast
GDP <- ts_frequency(xts(GDP[,2:6], order.by = GDP$Date), to = Frequency, aggregate = "sum")
GDPLabs <- c("BIP", "Industrie", "Banken", "Handel")

# Domestic data scenario used from monthly model
HypData[,1] <- as.Date(HypData[,1])
HypCH <- ts_frequency(xts(HypData[,2], order.by = as.Date(paste(year(HypData[,1]), month(HypData[,1]), "01", sep = "-"))), to = Frequency, aggregate = "last")
SparCH <- ts_frequency(xts(HypData[,3], order.by = as.Date(paste(year(HypData[,1]), month(HypData[,1]), "01", sep = "-"))), to = Frequency, aggregate = "last")

SRGE <- ts_frequency(xts(subset(SRData, LOCATION == "DEU")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "DEU")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")
SRCH <- ts_frequency(xts(subset(SRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "CHE")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")
LRCH <- ts_frequency(xts(subset(LRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(LRData, LOCATION == "CHE")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")
SPCH <- ts_frequency(xts(subset(StockData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(StockData, LOCATION == "CHE")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")
NEURR <- xts(XRData[,3], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))
NEURR <- ts_frequency(ts_index(1/NEURR, "2000-12-01")*100, to = Frequency, aggregate = "mean")

# Domestic data scenario used from monthly model
G7 <- ts_frequency(xts(subset(GDPForQ, LOCATION == "G-7")[,7], order.by = as.Date(subset(GDPForQ, LOCATION == "G-7")[,6])), to = Frequency, aggregate = "last")
EA19 <- ts_frequency(xts(subset(GDPForQ, LOCATION == "EA19")[,7], order.by = as.Date(subset(GDPForQ, LOCATION == "EA19")[,6])), to = Frequency, aggregate = "last")
EA17 <- ts_frequency(xts(GDPForFcstQ[,3], order.by = as.Date(GDPForFcstQ[,2])), to = Frequency, aggregate = "last")

# Use G7 growth rate, linked with EA17 growth rate and forecast after 1991
EA17 <- (((EA17/100+1)^(1/4))-1)*100
GDPFor <- ts_bind(ts_span(G7, start = myStart, end = "1991-01-01"), EA17)

plot(ts_c(G7, EA17, EA19, GDPFor))

Data <- ts_c(SRGE, SRCH, NEURR)

myPred <- array(NA, c(4, 4))
rownames(myPred) <- paste("Scen", seq(1, length.out= 4), sep = "")
colnames(myPred) <- GDPLabs[1:4]

myMods <- list()
mySEs <- list()

# Do for each sector
for (i in 1:4){
  
  # Test linear regressions
  myData <- ts_c(ts_pc(GDP[, i]), stats::lag(ts_pc(GDP[, i]), 1),ts_pc(NEURR), ts_diff(LRCH-SRCH), ts_diff(SRCH-SRGE), ts_pc(SPCH), ts_diff(HypCH), ts_diff(LRCH), ts_diff(SparCH), GDPFor)
  colnames(myData) <- c("GDP", "GDP.l1", "NEURR", "TS", "Diff", "SPCH", "HypCH", "LRCH", "SparCH", "GDPFor")
  myData <- ts_span(myData, myStart, myEnd)
  
  GDPModel <- lm(GDP~GDP.l1+NEURR+SPCH+HypCH+SparCH, data = myData)
  summary(GDPModel)
  
  myCoeff <- GDPModel$coefficients
  myCov   <- sqrt(diag(NeweyWest(GDPModel)))
  
  myMods <- c(myMods, list(GDPModel))
  mySEs  <- c(mySEs, list(myCov))
  
  # Calculate the prediction with the scenarios
  for(j in 1:4){
      
    GDPForLevel <- cumprod(1+GDPFor/100)
    DeltaGDPFor <- (as.numeric(GDPForLevel["2024-10-01"])/as.numeric(GDPForLevel["2019-10-01"])-1)*100
    
    DeltaNEURR <- (as.numeric(Scenario["2024-10-01", paste("NEURRSc", j, sep = "")])/as.numeric(Scenario$NEURRHist["2019-10-01"])-1)*100
    DeltaSPCH <- (as.numeric(Scenario["2024-10-01", paste("SPCHSc", j, sep = "")])/as.numeric(Scenario$SPCHHist["2019-10-01"])-1)*100
    DeltaHypCH <- as.numeric(Scenario["2024-10-01", paste("HypCHSc", j, sep = "")])-as.numeric(Scenario$HypCHHist["2019-10-01"])
    DeltaSparCH <- as.numeric(Scenario["2024-10-01", paste("SparCHSc", j, sep = "")])-as.numeric(Scenario$SparCHHist["2019-10-01"])
    
    myPred[paste("Scen", j, sep = ""), i] <- (20*myCoeff["(Intercept)"]+DeltaNEURR*myCoeff["NEURR"]+DeltaSPCH*myCoeff["SPCH"]+
              DeltaHypCH*myCoeff["HypCH"]+DeltaSparCH*myCoeff["SparCH"])/(1-myCoeff["GDP.l1"])
      
  }
}


stargazer(myMods, title="Modelle für Wertschöpfungswachstum", type = "text",
          se=mySEs,
          dep.var.labels = GDPLabs,
          add.lines = list(c("Scenario 1", round(myPred[1,], 2)),
                           c("Scenario 2", round(myPred[2,], 2)),
                           c("Scenario 3", round(myPred[3,], 2)),
                           c("Scenario 4", round(myPred[4,], 2))),
          omit.stat = c("f", "ll", "sigma2", "ser", "adj.rsq"), label="tab:werts",
          model.numbers          = FALSE,
          font.size = "small",
          digits =2,
          notes        = "", style = "qje")

#covariate.labels = c("CPI", "CPI-proxy", "CPI-replication",  "CPI-proxy (t-1)", "CPI-replication (t-1)", "Constant"),

# TODO: Predictions based on monthly VAR scenarios for these variables

# TODO: Repeat for all sectors...

sdf












#-------------------------------------------------------------------------------------
# Kointegrationstests
#-------------------------------------------------------------------------------------
Data <- ts_c(SRGE, SRCH, NEURR)

# Schätzung i = e + i*(+1)-i*+eps (für Tagesmodell, gemäss Theorie)
Data$i <- Data$SRCH
Data$dis <- ts_lag(Data$SRGE, -1) -Data$SRGE
Data$is <- Data$SRGE
Data$e <- log(1/Data$NEURR*100)*100
Data <- ts_span(Data, start = myStart, end = myEnd)

# Schätzung i = i* + eps (für Monatsmodell, gemäss Theorie)
CointReg2 <- lm(i~is, data = Data)
summary(CointReg2)
Equilib2 <- CointReg2$resid
plot(Equilib2)
Test2 <- egcm(Data$is, Data$i)
Test2
summary(Test2)

cintConst <- CointReg2$coefficients[1]
cintSlope <- CointReg2$coefficients[2]

#-------------------------------------------------------------------------------------
# VARX schätzen (einfaches übertragen der Resultate des Monatsmodell)
#-------------------------------------------------------------------------------------
# Endogen: BIP und Beschäftigung
# Exogen: Variablen aus dem Monatsmodell
OrigData <- ts_c(GDP[, varIndex], NEURR, LRCH, SPCH)
colnames(OrigData) <- c("GDP", "NEURR", "LRCH", "SPCH")

EndoData <- ts_c(ts_pc(GDP[, varIndex]), ts_pc(NEURR), ts_diff(LRCH), ts_pc(SPCH))
colnames(EndoData) <- c("GDP", "NEURR", "LRCH", "SPCH")
ExoData  <- ts_c(stats::lag(SRCH-cintSlope*SRGE-cintConst, 1))
ExoData <- ts_c(ExoData)

OrigData <- ts_span(OrigData, myStart, myEnd)
EndoDataAll <- ts_span(EndoData, myStart,"2019-12-01")
EndoData <- ts_span(EndoData, myStart, myEnd)
ExoData  <- ts_span(ExoData, myStart, myEnd)
colnames(ExoData) <- c("Diff.l1")

#ExoData <- ExoData[,c(1:6)]
VARModel <- VAR(EndoData,
                exogen = ExoData,
                type = c("const"),
                lag.max = maxLag,
                ic = c("SC"))
summary(VARModel)

#-------------------------------------------------------------------------------------
# Szenarien erstellen
#-------------------------------------------------------------------------------------
load('../Resultate/Szenarien_M.rdata')

XSc1 <- ts_frequency(XSc1, to = Frequency, aggregate = "last")
XSc2 <- ts_frequency(XSc2, to = Frequency, aggregate = "last")
XSc3 <- ts_frequency(XSc3, to = Frequency, aggregate = "last")
XSc4 <- ts_frequency(XSc4, to = Frequency, aggregate = "last")

#XScenario1 <- ts_c(stats::lag(XSc1$SRCH-cintSlope*XSc1$SRGE-cintConst, 1), GDPFor)
#XScenario2 <- ts_c(stats::lag(XSc2$SRCH-cintSlope*XSc2$SRGE-cintConst, 1), GDPFor)
#XScenario3 <- ts_c(stats::lag(XSc3$SRCH-cintSlope*XSc3$SRGE-cintConst, 1), GDPFor)
#XScenario4 <- ts_c(stats::lag(XSc4$SRCH-cintSlope*XSc4$SRGE-cintConst, 1), GDPFor)
XScenario1 <- ts_c(stats::lag(XSc1$SRCH-cintSlope*XSc1$SRGE-cintConst, 1))
XScenario2 <- ts_c(stats::lag(XSc2$SRCH-cintSlope*XSc2$SRGE-cintConst, 1))
XScenario3 <- ts_c(stats::lag(XSc3$SRCH-cintSlope*XSc3$SRGE-cintConst, 1))
XScenario4 <- ts_c(stats::lag(XSc4$SRCH-cintSlope*XSc4$SRGE-cintConst, 1))

XScenario1["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])
XScenario2["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])
XScenario3["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])
XScenario4["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])

XScenario1 <- XScenario1["2020-01-01/2024-10-01",]
XScenario2 <- XScenario2["2020-01-01/2024-10-01",]
XScenario3 <- XScenario3["2020-01-01/2024-10-01",]
XScenario4 <- XScenario4["2020-01-01/2024-10-01",]

colnames(XScenario1) <- c("Diff.l1")
colnames(XScenario2) <- c("Diff.l1")
colnames(XScenario3) <- c("Diff.l1")
colnames(XScenario4) <- c("Diff.l1")

#-------------------------------------------------------------------------------------
# Prognose erstellen
#-------------------------------------------------------------------------------------
FcstScenario1 <- predict(VARModel, n.ahead = H, dumvar = XScenario1, ci = 0.95)
FcstScenario2 <- predict(VARModel, n.ahead = H, dumvar = XScenario2, ci = 0.95)
FcstScenario3 <- predict(VARModel, n.ahead = H, dumvar = XScenario3, ci = 0.95)
FcstScenario4 <- predict(VARModel, n.ahead = H, dumvar = XScenario4, ci = 0.95)

#-------------------------------------------------------------------------------------
# In Niveaus transformieren und Prognosen plotten
#-------------------------------------------------------------------------------------
fcstStart <- ts_summary(XScenario1$Diff.l1)$start
fcstEnd <- ts_summary(XScenario1$Diff.l1)$end
histEnd   <- "2019-10-01"

temp     <- ts_bind(EndoDataAll[,1], XScenario1[,1])
Scenario <- ts_c(temp, temp, temp, temp,
                 temp, temp, temp, temp,
                 temp, temp, temp, temp,
                 temp, temp, temp, temp,
                 temp, temp, temp, temp)
colnames(Scenario) <- c(paste(colnames(EndoData), "Sc1", sep = ""), 
                        paste(colnames(EndoData), "Sc2", sep = ""), 
                        paste(colnames(EndoData), "Sc3", sep = ""), 
                        paste(colnames(EndoData), "Sc4", sep = ""),
                        paste(colnames(EndoData), "Hist", sep = ""))

Scens <- list(FcstScenario1, FcstScenario2, FcstScenario3, FcstScenario4)
Trans <- c("grt", "grt", "diff", "grt")

for(j in 1:length(Scens)){
  for(i in 1:length(colnames(EndoData))){
    
    # Vergangenheit mit Prognose verbinden
    Scen     <- Scens[[j]]
    tempFcst <- matrix(unlist(Scen$fcst[i]), nrow = H)
    tempFcst <- xts(tempFcst[,1], order.by = as.Date(index(XScenario1)))
    temp1    <- ts_bind(EndoDataAll[,i], tempFcst)
    
    # Aufkumulieren (bei ersten Differenzen)
    if(Trans[i] == "diff"){
      temp2 <- ts_cumsum(temp1)
      
      # Auf tatsächlichen Anfangswert setzen
      temp2 <- (temp2-as.numeric(temp2[1]))+as.numeric(OrigData[1, i])
    }
    # Aufkumulieren (bei Wachstumsraten)
    if(Trans[i] == "grt"){
      temp2 <- ts_cumprod(1+temp1/100)
      
      # Auf tatsächlichen Anfangswert setzen
      temp2 <- temp2/as.numeric(temp2[1])*as.numeric(OrigData[1, i])
    }
    # Sonst keine transformation
    if(Trans[i] == "no"){
      temp2 <- temp1
    }
    
    # Historie und Prognose extrahieren
    Hist <- temp2
    Hist[index(temp2)>=fcstStart,] <- NA
    Fcst <- temp2
    Fcst[index(temp2)<fcstStart,] <- NA
    
    # Colname zusammenstellen
    ColFcst <- paste(colnames(EndoData)[i], "Sc", j, sep = "")
    ColHist <- paste(colnames(EndoData)[i], "Hist", sep = "")
    
    # In neuen data frame speichern
    Scenario[, ColFcst] <- Fcst
    Scenario[, ColHist] <- Hist
    
  }
}

Scenario <- ts_span(Scenario, "2015-01-01")

# Grafik mit Scenarien für BIP
p <- ts_ggplot(
  `Historie`                     = (Scenario$GDPHist),
  `S1: Status Quo`               = (Scenario$GDPSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$GDPSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$GDPSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$GDPSc4),
  title = paste(GDPLabs[varIndex], " (real, in mio. CHF zu Preisen in 2010)", sep = "")
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/WS_Sektor", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)

# Grafik mit Scenarien für Wechselkurs
p <- ts_ggplot(
  `Historie`                     = (Scenario$NEURRHist),
  `S1: Status Quo`               = (Scenario$NEURRSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$NEURRSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$NEURRSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$NEURRSc4),
  title = "Wechselkurs (Index, Dez. 2000 = 100)"
  
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/Wechselkurs_Sektor", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)

#-------------------------------------------------------------------------------------
# Tabelle erstellen
#-------------------------------------------------------------------------------------
# Erwartete Aufwertung
lastObs <- as.matrix(Scenario$NEURRHist[histEnd])
lastSc1 <- as.matrix(Scenario$NEURRSc1[fcstEnd])
lastSc2 <- as.matrix(Scenario$NEURRSc2[fcstEnd])
lastSc3 <- as.matrix(Scenario$NEURRSc3[fcstEnd])
lastSc4 <- as.matrix(Scenario$NEURRSc4[fcstEnd])

# TODO: Wkeit einer starken Aufwertung (eventuell)
# Easily done because we have confidence intervals in 
z957 = qnorm(0.975, 0, 1)

# TODO: Do Simulations with normal distribution and aggregate!
NSim = 5000    # Note that you can test your codes with a small number of simulations to increase speed and calculate the final results with a higher number
PLarge <- array(NaN, dim = c(4, 1))
rownames(PLarge) <- c("Scen1", "Scen2", "Scen3", "Scen4")
q <- 1
for(Scen in list(FcstScenario1, FcstScenario2, FcstScenario3, FcstScenario4)){
  fcsth = Scen$fcst$NEURR[,"fcst"]
  sighScen = ((Scen$fcst$NEURR[,"lower"]-Scen$fcst$NEURR[,"fcst"])/(-z957))^2
  SimFcst = matrix(NA, nrow = length(fcsth), ncol = NSim)
  for (h in 1:length(fcsth)){
    SimFcst[h, ] = rnorm(NSim, fcsth[h], sqrt(sighScen[h]))
  }
  for (i in 1:NSim){
    SimFcst[, i] = cumprod(1+ SimFcst[, i]/100)
  }
  PLarge[paste("Scen", q, sep = ""), ] = mean(SimFcst[H, ]<=0.85)
  q <- q+1
}

Table   <- (c((lastSc1/lastObs-1)*100, (lastSc2/lastObs-1)*100, (lastSc3/lastObs-1)*100, (lastSc4/lastObs-1)*100))
Table <- rbind(Table, c(PLarge))

# Erwartete Änderung BIP
lastObs <- as.matrix(Scenario$GDPHist[histEnd])
lastSc1 <- as.matrix(Scenario$GDPSc1[fcstEnd])
lastSc2 <- as.matrix(Scenario$GDPSc2[fcstEnd])
lastSc3 <- as.matrix(Scenario$GDPSc3[fcstEnd])
lastSc4 <- as.matrix(Scenario$GDPSc4[fcstEnd])

# TODO: Wkeit einer starken Aufwertung (eventuell)
# Easily done because we have confidence intervals in 
z957 = qnorm(0.975, 0, 1)

# TODO: Do Simulations with normal distribution and aggregate!
NSim = 5000    # Note that you can test your codes with a small number of simulations to increase speed and calculate the final results with a higher number
PLarge <- array(NaN, dim = c(4, 1))
rownames(PLarge) <- c("Scen1", "Scen2", "Scen3", "Scen4")
q <- 1
for(Scen in list(FcstScenario1, FcstScenario2, FcstScenario3, FcstScenario4)){
  fcsth = Scen$fcst$GDP[,"fcst"]
  sighScen = ((Scen$fcst$GDP[,"lower"]-Scen$fcst$GDP[,"fcst"])/(-z957))^2
  SimFcst = matrix(NA, nrow = length(fcsth), ncol = NSim)
  for (h in 1:length(fcsth)){
    SimFcst[h, ] = rnorm(NSim, fcsth[h], sqrt(sighScen[h]))
  }
  for (i in 1:NSim){
    SimFcst[, i] = cumprod(1+ SimFcst[, i]/100)
  }
  PLarge[paste("Scen", q, sep = ""), ] = mean(SimFcst[H, ]<=0.99)
  q <- q+1
}


Table   <- rbind(Table, c((lastSc1/lastObs-1)*100, (lastSc2/lastObs-1)*100, (lastSc3/lastObs-1)*100, (lastSc4/lastObs-1)*100))
Table <- rbind(Table, c(PLarge))

rownames(Table) <- c("Appreciation", "P Large appreciation", "GDP decline", "P large GDP decline")
print(GDPLabs[varIndex])
print(Table[3:4,])








