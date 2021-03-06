# ------------------------------------------------------------------------
# Wie weiter mit der Tiefzinspolitik? Szenarien und Alternativen
# - LoadDailyData.R
# ------------------------------------------------------------------------
# Bemerkungen: Lädt Daten für Modell mit Tagesdaten
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
cat("\014") 
rm(list=ls())
library(xts)
library(xlsx)
library(dplyr)  
library(lubridate)
source("Functions.R")

# ------------------------------------------------------------------------
# Einstellungen
# ------------------------------------------------------------------------
startExport <- as.Date("2000-01-01")   # Startdatum für Export
endExport   <- as.Date("2020-03-31")   # Enddatum für Export

# ------------------------------------------------------------------------
# SNB Entscheide
# ------------------------------------------------------------------------
SNBData <- read.xlsx("../Daten/LagebeurteilungD.xlsx", sheetName = "Data", startRow = 1)
SNBLB <- data.frame(as.Date(SNBData$Date), SNBData$Official, SNBData$Exceptional, SNBData$Time, SNBData$Surprise)
colnames(SNBLB) <- c("Date", "LB", "IrrgLB", "Time", "Surprise")
SNBLB <- SNBLB[order(SNBLB$Date),]

# ------------------------------------------------------------------------
# EZB Entscheidungen
# ------------------------------------------------------------------------
ECBData <- read.xlsx("../Daten/ECBRegularEventsD.xlsx", sheetName = "ForImportMEW", startRow = 1)
ECBDec <- data.frame(as.Date(ECBData$Date), 1, ECBData[,2:17]/100)
colnames(ECBDec) <- c("Date", "ECBDec", "ShockECB1W", "ShockECB1M", "ShockECB3M", "ShockECB6M", paste("ShockECBN", seq(1, 12, 1), sep =""))
ECBDec <- ECBDec[order(ECBDec$Date),]

# ------------------------------------------------------------------------
# Geldmarktsätze
# ------------------------------------------------------------------------
SAR <- read.xlsx("../Daten/SARD.xlsx", sheetName = "Data", startRow = 1)
SAR$Date <- as.Date(SAR$Date)
SAR <- SAR[order(SAR$Date),]

# ------------------------------------------------------------------------
# Obligationenrenditen
# ------------------------------------------------------------------------
BONDS <- read.xlsx("../Daten/BondsD.xlsx", sheetName = "Data", startRow = 17)
BONDS$Date <- as.Date(BONDS$Date)
BONDS <- BONDS[order(BONDS$Date),]

# ------------------------------------------------------------------------
# CHF Libor
# ------------------------------------------------------------------------
Libor <- read.xlsx("../Daten/LiborD.xlsx", sheetName = "Data", startRow = 22)
Libor$Date <- as.Date(Libor$Date)
Libor <- Libor[order(Libor$Date),]

# ------------------------------------------------------------------------
# EUR Libor
# ------------------------------------------------------------------------
LiborEUR <- read.xlsx("../Daten/LiborEURD.xlsx", sheetName = "Data", startRow = 22)
LiborEUR$Date <- as.Date(LiborEUR$Date)
LiborEUR <- LiborEUR[order(LiborEUR$Date),]

# ------------------------------------------------------------------------
# Aktienpreise
# ------------------------------------------------------------------------
SPISec <- read.csv("../Daten/SPISecD.csv", skip = 4)
nobs <- dim(SPISec)[1]
myDate <- dmy(SPISec[, 1])
SPI <- data.frame(myDate, SPISec[, c(2, 4, 5, 6, 7, 8, 9, 10, 11, 12)])
colnames(SPI) <- c("Date", "SPI", "SPIRaw", "SPIInd", "SPICons", "SPIHealt", "SPICSer", "SPICom", "SPIUtil", "SPIFin", "SPITech")
SPI <- SPI[order(SPI$Date),]

# ------------------------------------------------------------------------
# Aktienpreise Euroraum
# ------------------------------------------------------------------------
ESTXX <- read.xlsx("../Daten/StockDataESTXXD.xlsx", sheetName = "Data", startRow = 1)
ESTXX$Date <- as.Date(ESTXX$Date)
ESTXX <- ESTXX[order(ESTXX$Date),]

# ------------------------------------------------------------------------
# Aktienpreise USA
# ------------------------------------------------------------------------
SP500 <- read.xlsx("../Daten/StockDataSP500D.xlsx", sheetName = "Data", startRow = 1)
SP500$Date <- as.Date(SP500$Date)
SP500 <- SP500[order(SP500$Date),]

# ------------------------------------------------------------------------
# Wechselkurse (Reduktion ist Aufwertung)
# ------------------------------------------------------------------------
XR <- read.xlsx("../Daten/XRDataD.xlsx", sheetName = "Data", startRow = 19)
XR$Date <- as.Date(XR$Date)
XR$NEER <- 1/XR$NEER*100
XR$NEER <- XR$NEER/XR$NEER[XR$Date == "2000-12-01"]*100
XR$NEURR <- 1/XR$NEURR*100
XR$NEURR <- XR$NEURR/XR$NEURR[XR$Date == "2000-12-01"]*100
XR$REER <- 1/XR$REER*100
XR$REER <- XR$REER/XR$REER[XR$Date == "2000-12-01"]*100
XR$REURR <- 1/XR$REURR*100
XR$REURR <- XR$REURR/XR$REURR[XR$Date == "2000-12-01"]*100
XR <- XR[order(XR$Date),]

XRBilat <- read.xlsx("../Daten/XRDataBilatD.xlsx", sheetName = "Data", startRow = 19)
XRBilat$Date <- as.Date(XRBilat$Date)
XRBilat$CHFUSD <- 1/XRBilat$CHFUSD*100
XRBilat$CHFUSD <- XRBilat$CHFUSD/XRBilat$CHFUSD[XRBilat$Date == "2000-12-01"]*100
XRBilat$CHFDEM <- 1/XRBilat$CHFDEM*100
XRBilat$CHFDEM <- XRBilat$CHFDEM/XRBilat$CHFDEM[XRBilat$Date == "2000-12-01"]*100
XRBilat$EURUSD <- 1/XRBilat$EURUSD*100
XRBilat$EURUSD <- XRBilat$EURUSD/XRBilat$EURUSD[XRBilat$Date == "2000-12-01"]*100
XRBilat <- XRBilat[order(XRBilat$Date),]

# ------------------------------------------------------------------------------------------
# Alle Daten speichern
# ------------------------------------------------------------------------------------------
AllData <- full_join(SNBLB, ECBDec, by = "Date")
AllData <- full_join(AllData, SAR, by = "Date")
AllData <- full_join(AllData, BONDS, by = "Date")
AllData <- full_join(AllData, Libor, by = "Date")
AllData <- full_join(AllData, LiborEUR, by = "Date")
AllData <- full_join(AllData, SPI, by = "Date")
AllData <- full_join(AllData, ESTXX, by = "Date")
AllData <- full_join(AllData, SP500, by = "Date")
AllData <- full_join(AllData, XR, by = "Date")
AllData <- full_join(AllData, XRBilat, by = "Date")
AllData <- AllData[order(AllData$Date),]
AllData <- subset(AllData, (AllData$Date<=endExport&AllData$Date>=startExport))

# Wenn nötig, dummy variablen auf TRUE/FALSE setzten
for(var in c("LB", "IrrgLB", "ECBDec")){
  AllData[is.na(AllData[,var]), var] <- 0
  AllData[,var] <- AllData[,var] == 1
}

# Wochenenden entfernen
isSun <- weekdays(AllData$Date) == "Sunday"
AllData <- AllData[!isSun, ]
isSat <- weekdays(AllData$Date) == "Saturday"
AllData <- AllData[!isSat, ]

# Duplikate entfernen falls vorhanden
AllData <- AllData[duplicated(AllData$Date) == FALSE, ]

# Index anpassen
rownames(AllData) <- AllData$Date

# Datei speichern
save(AllData, file = "../Daten/Tagesdaten.RData")

#-------------------------------------------------------------------------------------
# END OF FILE
#-------------------------------------------------------------------------------------
