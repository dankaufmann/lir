# ------------------------------------------------------------------------
# Ausstieg aus der Negativzinspolitik: Szenarien und Alternativen
# * Multivariates Modell mit Quartalsdaten
# ------------------------------------------------------------------------
#
# Bemerkungen:
# - Daten Hypothekar- und Sparzinsen verknüpft. Zudem letzter Wert von November
#   genommen da noch nicht verfügbar.
# 
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
library(egcm)
library(seasonal)
source("SVARFunctions.R")

#-------------------------------------------------------------------------------------
# Einstellungen
#-------------------------------------------------------------------------------------
myStart   <- "1991-01-01"
myEnd     <- "2007-12-01"
plotStart <- "2010-01-01"
maxLag    <- 4
H         <- 60/3
figWdth   <- 4.5
figHeight <- 4.5
Frequency <- "quarter"
varIndex <- 1   # Sektor wählen: 1 Gesamtwirtschaft, 2 Industrie, 3 Banken, 4 Handel

#-------------------------------------------------------------------------------------
# Datenimport
#-------------------------------------------------------------------------------------
SRData = read.xlsx("../Daten/SRDataM.xlsx", sheetName = "Data", startRow = 1)
LRData = read.xlsx("../Daten/LRDataM.xlsx", sheetName = "Data", startRow = 1)
XRData = read.xlsx("../Daten/XRDataM.xlsx", sheetName = "Data", startRow = 19)
StockData = read.xlsx("../Daten/StockDataM.xlsx", sheetName = "Data", startRow = 1)
HypData = read.xlsx("../Daten/HypoCashIRM.xlsx", sheetName = "Data", startRow = 20)
CPIData = read.xlsx("../Daten/CPICHM.xlsx", sheetName = "Data", startRow = 4)

GDP    = read.xlsx("../Daten/GDPDataSAQ.xlsx", sheetName = "Data", startRow = 12)

# Domestic variables to be forecast
GDP <- ts_frequency(xts(GDP[,2:6], order.by = GDP$Date), to = Frequency, aggregate = "sum")
GDPLabs <- c("BIP", "Industrie", "Banken", "Handel")

HypData[,1] <- as.Date(HypData[,1])
HypCH <-  ts_frequency(xts(HypData[,2], order.by = as.Date(paste(year(HypData[,1]), month(HypData[,1]), "01", sep = "-"))), to = Frequency, aggregate = "last")
SparCH <-  ts_frequency(xts(HypData[,3], order.by = as.Date(paste(year(HypData[,1]), month(HypData[,1]), "01", sep = "-"))), to = Frequency, aggregate = "last")

SRGE <- ts_frequency(xts(subset(SRData, LOCATION == "DEU")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "DEU")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")
SRCH <- ts_frequency(xts(subset(SRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "CHE")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")

LRCH <- ts_frequency(xts(subset(LRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(LRData, LOCATION == "CHE")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")
SPCH <- ts_frequency(xts(subset(StockData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(StockData, LOCATION == "CHE")[,6], "-01", sep = ""))), to = Frequency, aggregate = "last")

NEURR <- ts_frequency(xts(XRData[,3], order.by = as.Date(paste(XRData[,1], "-01", sep = ""))), to = Frequency, aggregate = "last")
NEURR <- ts_index(1/NEURR, "2000-12-01")*100

# TODO: SET X13 PATH MANUALLY IF IT DOES NOT WORK ADJUST TO YOUR OWN COMPUTER
#Sys.setenv(X13_PATH = "//home/kaufmannd$/R/win-library/3.6/x13binary/bin")
Sys.setenv(X13_PATH = "C:/Users/daenu/Documents/R/win-library/3.6/x13binary/bin")
#checkX13()

CPI <- xts(CPIData[,2], order.by = as.Date(CPIData[,1]))
CPI = ts_span(CPI, "1970-01-01")
SEATSDecomp = seas(ts_ts(CPI))
CPI <- SEATSDecomp$series$s11
CPI <- ts_frequency(ts_index(CPI, base = "2000-12-01")*100, to = Frequency, aggregate = "mean")

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
# VARX schätzen
#-------------------------------------------------------------------------------------
# Endogen: Zinsen Schweiz (diff), Aktienpreise Schweiz (grt), Wechselkurs Schweiz (grt), Konsumentenpreise (grt)
# Exogen:  Zinsdifferenz (Kointegrationsterm)

OrigData <- ts_c(GDP[,varIndex], NEURR, SPCH, CPI, HypCH, SparCH, LRCH)
colnames(OrigData) <- c("GDP", "NEURR", "SPCH", "CPI", "HypCH", "SparCH", "LRCH")

EndoData <- ts_c(ts_pc(GDP[,varIndex]),ts_pc(NEURR), ts_pc(SPCH), ts_pc(CPI), ts_diff(HypCH), ts_diff(SparCH), ts_diff(LRCH))
colnames(EndoData) <- c("GDP", "NEURR", "SPCH", "CPI", "HypCH", "SparCH", "LRCH")
#ExoData  <- ts_c(SRCH-cintSlope*SRGE-cintConst, stats::lag(SRCH-cintSlope*SRGE-cintConst, 1))
ExoData  <- ts_c(stats::lag(SRCH-cintSlope*SRGE-cintConst, 1))

OrigData <- ts_span(OrigData, myStart, myEnd)
EndoDataAll <- ts_span(EndoData, myStart,"2019-12-01")
EndoData <- ts_span(EndoData, myStart, myEnd)
ExoData  <- ts_span(ExoData, myStart, myEnd)
#colnames(ExoData) <- c("Diff", "Diff.l1")
colnames(ExoData) <- c("Diff.l1")

#ExoData <- ExoData[,c(1:6)]
VARModel <- VAR(EndoData,
              exogen = ExoData,
              type = c("const"),
              lag.max = maxLag,
              ic = c("SC"))
summary(VARModel)

testData <- EndoData
testData$GDP.l1 <- stats::lag(EndoData$GDP, 1)
testReg <- lm(GDP~GDP.l1+NEURR+SPCH+HypCH+SparCH+LRCH, data = testData)
coeftest(testReg, vcov = NeweyWest(testReg))


sdf
#-------------------------------------------------------------------------------------
# Szenarien erstellen
#-------------------------------------------------------------------------------------
load('../Resultate/Szenarien_M.rdata')

#XScenario1 <- ts_c(XSc1$SRCH-cintSlope*XSc1$SRGE-cintConst, stats::lag(XSc1$SRCH-cintSlope*XSc1$SRGE-cintConst, 1))
#XScenario2 <- ts_c(XSc2$SRCH-cintSlope*XSc2$SRGE-cintConst, stats::lag(XSc2$SRCH-cintSlope*XSc2$SRGE-cintConst, 1))
#XScenario3 <- ts_c(XSc3$SRCH-cintSlope*XSc3$SRGE-cintConst, stats::lag(XSc3$SRCH-cintSlope*XSc3$SRGE-cintConst, 1))
#XScenario4 <- ts_c(XSc4$SRCH-cintSlope*XSc4$SRGE-cintConst, stats::lag(XSc4$SRCH-cintSlope*XSc4$SRGE-cintConst, 1))

#XScenario1["2020-01-01", 2] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-12-01"])
#XScenario2["2020-01-01", 2] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-12-01"])
#XScenario3["2020-01-01", 2] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-12-01"])
#XScenario4["2020-01-01", 2] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-12-01"])

#XScenario1 <- XScenario1[1:H,]
#XScenario2 <- XScenario2[1:H,]
#XScenario3 <- XScenario3[1:H,]
#XScenario4 <- XScenario4[1:H,]

#colnames(XScenario1) <- c("Diff", "Diff.l1")
#colnames(XScenario2) <- c("Diff", "Diff.l1")
#colnames(XScenario3) <- c("Diff", "Diff.l1")
#colnames(XScenario4) <- c("Diff", "Diff.l1")

XSc1 <- ts_frequency(XSc1, to = Frequency, aggregate = "last")
XSc2 <- ts_frequency(XSc2, to = Frequency, aggregate = "last")
XSc3 <- ts_frequency(XSc3, to = Frequency, aggregate = "last")
XSc4 <- ts_frequency(XSc4, to = Frequency, aggregate = "last")

XScenario1 <- ts_c(stats::lag(XSc1$SRCH-cintSlope*XSc1$SRGE-cintConst, 1))
XScenario2 <- ts_c(stats::lag(XSc2$SRCH-cintSlope*XSc2$SRGE-cintConst, 1))
XScenario3 <- ts_c(stats::lag(XSc3$SRCH-cintSlope*XSc3$SRGE-cintConst, 1))
XScenario4 <- ts_c(stats::lag(XSc4$SRCH-cintSlope*XSc4$SRGE-cintConst, 1))

XScenario1["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])
XScenario2["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])
XScenario3["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])
XScenario4["2020-01-01", 1] <- as.numeric((SRCH-cintSlope*SRGE-cintConst)["2019-10-01"])

XScenario1 <- XScenario1[1:H,]
XScenario2 <- XScenario2[1:H,]
XScenario3 <- XScenario3[1:H,]
XScenario4 <- XScenario4[1:H,]

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
fcstStart <- ts_summary(XScenario1$Diff)$start
fcstEnd <- ts_summary(XScenario1$Diff)$end
histEnd   <- "2019-10-01"

temp     <- ts_bind(EndoDataAll[,1], XScenario1$Diff.l1)
Scenario <- ts_c(temp, temp, temp, temp, temp, 
                 temp, temp, temp, temp, temp, 
                 temp, temp, temp, temp, temp, 
                 temp, temp, temp, temp, temp, 
                 temp, temp, temp, temp, temp, 
                 temp, temp, temp, temp, temp, 
                 temp, temp, temp, temp, temp)
colnames(Scenario) <- c(paste(colnames(EndoData), "Sc1", sep = ""), 
                        paste(colnames(EndoData), "Sc2", sep = ""), 
                        paste(colnames(EndoData), "Sc3", sep = ""), 
                        paste(colnames(EndoData), "Sc4", sep = ""),
                        paste(colnames(EndoData), "Hist", sep = ""))

Scens <- list(FcstScenario1, FcstScenario2, FcstScenario3, FcstScenario4)
Trans <- c("grt", "grt", "grt", "grt", "diff", "diff", "diff")

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

Scenario <- ts_span(Scenario, plotStart)


# Grafik mit Scenarien für Wechselkurs
p <- ts_ggplot(
  `Historie`                     = (Scenario$GDPHist),
  `S1: Status Quo`               = (Scenario$GDPSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$GDPSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$GDPSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$GDPSc4),
  title = paste( GDPLabs[varIndex], " (real, in mio CHF)", sep = "")
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/WS_", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)

# Grafik mit Scenarien für Wechselkurs
p <- ts_ggplot(
  `Historie`                     = (Scenario$NEURRHist),
  `S1: Status Quo`               = (Scenario$NEURRSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$NEURRSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$NEURRSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$NEURRSc4),
  title = "Nomineller Wechselkurs (Dez. 2000 = 100)"
  
)
p <- p + theme_minimal() + ylab("")+xlab("")+
         ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
         theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
         theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
         theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
         theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/Wechselkurs_", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)

# Grafik mit Scenarien für Aktienpreise
p <- ts_ggplot(
  `Historie`                     = (Scenario$SPCHHist),
  `S1: Status Quo`               = (Scenario$SPCHSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$SPCHSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$SPCHSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$SPCHSc4),
  title = "Aktienpreise (Index)"
  
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/Aktienpreise_", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)

# Grafik mit Scenarien für Hypozinsen
p <- ts_ggplot(
  `Historie`                     = (Scenario$HypCHHist),
  `S1: Status Quo`               = (Scenario$HypCHSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$HypCHSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$HypCHSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$HypCHSc4),
  title = "Zinsen variable Hypotheken (In %)"
  
)
p <- p + theme_minimal()+ ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/Hypozinsen_", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)


# Grafik mit Scenarien für Langfristzinsen
p <- ts_ggplot(
  `Historie`                     = (Scenario$LRCHHist),
  `S1: Status Quo`               = (Scenario$LRCHSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$LRCHSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$LRCHSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$LRCHSc4),
  title = "Zinsen 10-j. Bundesobligationen (In %)"
  
)
p <- p + theme_minimal()+ ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/Langfristzinsen_", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)


# Grafik mit Scenarien für Sparzinsen
p <- ts_ggplot(
  `Historie`                     = (Scenario$SparCHHist),
  `S1: Status Quo`               = (Scenario$SparCHSc1),
  `S2: SNB sofortiger Ausstieg`  = (Scenario$SparCHSc2),
  `S3: SNB gradueller Ausstieg`  = (Scenario$SparCHSc3),
  `S4: EZB gradueller Ausstieg`  = (Scenario$SparCHSc4),
  title = "Zinsen Spareinlagen (In %)"
  
)
p <- p + theme_minimal()+ ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = paste("../Resultate/Sparzinsen_", varIndex, "_Q.pdf", sep = ""), width = figWdth, height = figHeight)


#-------------------------------------------------------------------------------------
# Tabelle erstellen
#-------------------------------------------------------------------------------------
# Erwarteter BIP Rückgang
lastObs <- as.matrix(Scenario$GDPHist[histEnd])
lastSc1 <- as.matrix(Scenario$GDPSc1[fcstEnd])
lastSc2 <- as.matrix(Scenario$GDPSc2[fcstEnd])
lastSc3 <- as.matrix(Scenario$GDPSc3[fcstEnd])
lastSc4 <- as.matrix(Scenario$GDPSc4[fcstEnd])

# Easily done because we have confidence intervals in 
z957 = qnorm(0.975, 0, 1)

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
  PLarge[paste("Scen", q, sep = ""), ] = mean(SimFcst[H,]<=1.05)
  q <- q+1
}

Table   <- (c((lastSc1/lastObs-1)*100, (lastSc2/lastObs-1)*100, (lastSc3/lastObs-1)*100, (lastSc4/lastObs-1)*100))
Table <- rbind(Table, c(PLarge))
rownames(Table) <- c(" GDP growth", "P[Decline]")
print(Table)

# ------------------------------------------------------------------------
# END OF FILE
# ------------------------------------------------------------------------

