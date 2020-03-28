# ------------------------------------------------------------------------
# Wie weiter mit der Tiefzinspolitik? Szenarien und Alternativen
# - DailyModel.R
# ------------------------------------------------------------------------
# Bemerkungen: Schätzt Ereignisstudie mit Tagesdaten
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
cat("\014") 
rm(list=ls())
library(AER)
library(lubridate)
library(Rfast)
library(ggplot2)
library(gridExtra)
library(xtable)
library(xts)
library(egcm)
library(tsbox)
library(xlsx)
library(extrafont)
library(stargazer)
library(dplyr)
library(sandwich)
library(seasonal)
loadfonts(device = "win")
source("Functions.R")

# Grafiken
figWidth   <- 5.5
figHeight  <- 5
myStart <- as.Date("2000-01-01")
myEnd   <- as.Date("2007-10-01")

# Dieser Pfad muss auf den Ordner mit dem R Saisonbereinigungsprogramm zeigen, damit der
# code funktioniert.
PathSettings <- "C:/Users/daenu/Documents/R/win-library/3.6/x13binary/bin"

# --------------------------------------------------------------
# Daten
# --------------------------------------------------------------
load(file="../Daten/QuartalsdatenShocks.RData")
GDP     <- read.xlsx("../Daten/GDPDataSAQ.xlsx", sheetName = "DataNew", startRow = 12)
GDP     <- xts(GDP[,2:18], order.by = GDP$Date)
GDPLabs <- c("BIP", "Landwirtschaft", "Bergbau", "Industrie", "Energie", "Bau", "Handel", "Kommunikation", "Gastgewerbe", "Finanzsektor",
             "Versicherungen", "Immobilien", "Öffentliche Verwaltung", "Unterricht", "Gesundheit", "Kunst", "Private Haushalte")

GDPFor     <- read.xlsx("../Daten/GDPForQ.xlsx", sheetName = "Data", startRow = 11)
GDPEUR <- xts(GDPFor[,2], order.by = GDPFor[,1])

CPIFor     <- read.xlsx("../Daten/CPIForQ.xlsx", sheetName = "Data", startRow = 11)
CPIFor <- xts(CPIFor[,2], order.by = CPIFor[,1])

PPIFor     <- read.xlsx("../Daten/PPIForQ.xlsx", sheetName = "Data", startRow = 11)
PPIFor <- xts(PPIFor[,2], order.by = PPIFor[,1])

CPIData = read.xlsx("../Daten/CPICHM.xlsx", sheetName = "Data", startRow = 4)

Sys.setenv(X13_PATH = PathSettings)
CPI <- xts(CPIData[,2], order.by = as.Date(CPIData[,1]))
CPI = ts_span(CPI, "1970-01-01")
SEATSDecomp = seas(ts_ts(CPI))
CPI <- SEATSDecomp$series$s11
CPI <- ts_frequency(ts_index(CPI, base = "2000-12-01")*100, to = "quarter", aggregate = "mean")

SEATSDecomp = seas(ts_ts(CPIFor))
CPIFor <- SEATSDecomp$series$s11
CPIFor <- ts_frequency(ts_index(CPIFor, base = "2000-12-01")*100, to = "quarter", aggregate = "mean")

# Calculate error correction term
# Schätzung i = i* + eps (für Monatsmodell, gemäss Theorie)
CointReg <- lm(LIB1W~LIBEUR1W, data = ts_span(Schocks,  myStart, myEnd))
summary(CointReg)
Schocks$Diff <- Schocks$LIB1W - as.numeric(CointReg$coefficients[1]) -  as.numeric(CointReg$coefficients[2])*Schocks$LIBEUR1W

# Event Data
AllData <- ts_c(Schocks, GDP, GDPEUR, CPI, CPIFor, PPIFor)

# Es gab jedes Quartal eine Lagebeurteilung
Data <- AllData
Data$LB <- 1
Date <- as.Date(as.Date(index(Data$LB)))
Data <- data.frame(Data, Date)

# Controls and lags for all models
e         <- c("ShocksCH")
MainEv    <- c("LB")
lagShocks <- FALSE
Ec        <- c("")
H         <- 4
gap       <- 1
P         <- 2
NormInit  <- FALSE
depLag    <- TRUE

Zc      <- c("GDPEUR", "CPIFor")
exoLog  <- c(TRUE,     TRUE)
Yc      <- c("")
conLog  <- c()

# --------------------------------------------------------------
# BIP
# --------------------------------------------------------------
# Scale mit Tagesstudie bestimmt (3*0.25 = 0.75%)
for (i in 1:length(GDPLabs)){
  Y       <- colnames(GDP[,i])
  depLog  <- TRUE
  Reg.SNB.gdp <- EstimLP(MainEv, e, Y, Yc, Zc, Ec, H, P, depLog, conLog, exoLog, lagShocks, depLag, Data, myStart, myEnd)
  
  # Note: Scale used from event study
  Result <- createLPPlot(Reg.SNB.gdp, c(), 3, NormInit, H, gap, c(-5, 2), 
                         paste(GDPLabs[i], " (In %)", sep = ""), 
                         "Quartale nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                         paste("../Resultate/BIP_", i, "_SNB_Q", sep = ""), figWidth, figHeight)
  
}

# --------------------------------------------------------------
# Preise
# --------------------------------------------------------------
# Scale mit Tagesstudie bestimmt (3*0.25 = 0.75%)
Y       <- "CPI"
depLog  <- TRUE
Reg.SNB.cpi <- EstimLP(MainEv, e, Y, Yc, Zc, Ec, H, P, depLog, conLog, exoLog, lagShocks, depLag, Data, myStart, myEnd)

# Note: Scale used from event study
Result <- createLPPlot(Reg.SNB.cpi, c(), 3, NormInit, H, gap, c(-5, 2), 
                       "Konsumentenpreise (In %)",
                       "Quartale nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/Konsumentenpreise_SNB_Q", figWidth, figHeight)


#-------------------------------------------------------------------------------------
# END OF FILE
#-------------------------------------------------------------------------------------

