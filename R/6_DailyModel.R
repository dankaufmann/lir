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
library(missMDA)
loadfonts(device = "win")
source("Functions.R")

# Grafiken
figWidth   <- 5.5
figHeight  <- 5

# --------------------------------------------------------------
# Daten
# --------------------------------------------------------------
load(file="../Daten/TagesdatenShocks.RData")

myStart <- as.Date("2000-01-01")
myEnd   <- as.Date("2007-12-31")


# Event daten
Data <- AllData

# Missing values auffüllen
Data  <- na.locf(Data, na.rm = FALSE, maxgap = 3)   # Note fills missing values with previous value but only up to 3 days

# Calculate risk premia
Data$RPIND  <- Data$IND8Y - Data$EID8Y  - Data$FORAAA8Y
Data$RPBANK <- Data$BANK8Y - Data$EID8Y  - Data$FORAAA8Y

# Calculate error correction term
# Schätzung i = i* + eps (für Monatsmodell, gemäss Theorie)
CointReg <- lm(LIB1W~LIBEUR1W, data = subset(Data, Date>= myStart & Date <= myEnd))
summary(CointReg)
Data$Diff <- Data$LIB1W - CointReg$coefficients[1] -  CointReg$coefficients[2]*Data$LIBEUR1W

# Controls and lags for all models
Yc       <- c("ESTXXClose",  "EURUSD", "LIBEUR3M")
conLog   <- c(TRUE,         TRUE,       FALSE)
Ec       <- c("")
lagShocks <- FALSE
H        <- 15
gap      <- 2
P        <- 2
NormInit <- FALSE
depLag   <- TRUE

# --------------------------------------------------------------
# Reaktion der Aktienpreise
# --------------------------------------------------------------
MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPI"
depLog  <- TRUE
Reg.SNB.sp <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag, Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPIRaw"
depLog  <- TRUE
Reg.SNB.spraw <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPIInd"
depLog  <- TRUE
Reg.SNB.spind <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPICons"
depLog  <- TRUE
Reg.SNB.spcons <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPIHealt"
depLog  <- TRUE
Reg.SNB.spheal <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPICSer"
depLog  <- TRUE
Reg.SNB.spcser <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPICom"
depLog  <- TRUE
Reg.SNB.spcom <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPIUtil"
depLog  <- TRUE
Reg.SNB.sputil <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPIFin"
depLog  <- TRUE
Reg.SNB.spfin <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "SPITech"
depLog  <- TRUE
Reg.SNB.sptec <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)


# --------------------------------------------------------------
# Reaktion des Wechselkurses und der Zinsen
# --------------------------------------------------------------
MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "NEURR"
depLog  <- TRUE
Reg.SNB.xr <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "LIB3M"
depLog  <- FALSE
Reg.SNB.ir <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib1W" # ShocksLib or ShockECB3M
Y       <- "NEURR"
depLog  <- TRUE
Reg.SNB1W.xr <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib1W" # ShocksLib or ShockECB3M
Y       <- "LIB1W"
depLog  <- FALSE
Reg.SNB1W.ir <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib1W" # ShocksLib or ShockECB3M
Y       <- "SPI"
depLog  <- TRUE
Reg.SNB1W.sp <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib6M" # ShocksLib or ShockECB3M
Y       <- "NEURR"
depLog  <- TRUE
Reg.SNB6M.xr <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib6M" # ShocksLib or ShockECB3M
Y       <- "LIB6M"
depLog  <- FALSE
Reg.SNB6M.ir <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib6M" # ShocksLib or ShockECB3M
Y       <- "SPI"
depLog  <- TRUE
Reg.SNB6M.sp <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

# --------------------------------------------------------------
# Reaktion auf EZB Shock
# --------------------------------------------------------------
MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "NEURR"
depLog  <- TRUE
e       <- "ShocksEUR" # ShocksLib or ShockECB3M
Reg.ECB.xr <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "NEURR"
depLog  <- TRUE
e       <- "ShockECB1W" # ShocksLib or ShockECB3M
Reg.ECB1W.xr <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "LIB3M"
depLog  <- FALSE
e       <- "ShocksEUR" # ShocksLib or ShockECB3M
Reg.ECB.ir <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "LIB1W"
depLog  <- FALSE
e       <- "ShockECB1W" # ShocksLib or ShockECB3M
Reg.ECB1W.ir <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "LIBEUR3M_lead"
Yc      <- c("")
depLog  <- FALSE
e       <- "ShocksEUR" # ShocksLib or ShockECB3M
Reg.ECB.irs <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "LIBEUR1W_lead"
Yc      <- c("")
depLog  <- FALSE
e       <- "ShockECB1W" # ShocksLib or ShockECB3M
Reg.ECB1W.irs <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

# --------------------------------------------------------------
# Grafiken Aktienpreise
# --------------------------------------------------------------
Result <- createLPPlot(Reg.SNB.sp, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10),
                       "SPI (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.spraw, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10),
                       "SPI Rohstoffe (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseRohstoffeSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.spind, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10), 
                       "SPI Industrie (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseIndustrieSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.spcons, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10),
                       "SPI Verbrauchsgüter (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseKonsumgSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.spheal, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10), 
                       "SPI Gesundheit (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseGesundheitSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.spcser, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10),
                       "SPI Konsumdienstleistungen (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseKonsumDLSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.spcom, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10), 
                       "SPI Kommunikation (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseKommunikationSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.spfin, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10), 
                       "SPI Finanzdienstleistungen (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseFinanzSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.sputil, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10), 
                       "SPI Öffentliche Versorgung (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseOeffVersorgungSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.sptec, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-20,10), 
                       "SPI Technologie (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseTechSNB_D", figWidth, figHeight)

# --------------------------------------------------------------
# Grafiken Zins und Wechselkurs SNB
# --------------------------------------------------------------
Result <- createLPPlot(Reg.SNB.xr, Reg.SNB.ir, 0.75, NormInit, H, gap, c(-10, 2), 
                       "Nomineller Wechselkurs (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/WechselkursSNB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB.ir, Reg.SNB.ir, 0.75, NormInit, H, gap, c(0, 2),
                       "3M CHF Libor (In pp)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/ZinsSNB_D", figWidth, figHeight)

# --------------------------------------------------------------
# Grafiken Zins und Wechselkurs EZB
# --------------------------------------------------------------
Result <- createLPPlot(Reg.ECB.xr, Reg.ECB.irs, 0.75, NormInit, H, gap, c(10, -2), 
                       "Nomineller Wechselkurs (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Leitzinses in der Eurozone", 
                       "../Resultate/WechselkursEZB_D", figWidth, figHeight)

Result <- createLPPlot(Reg.ECB.ir, Reg.ECB.irs, 0.75, NormInit, H, gap, c(0, 2),
                       "3M CHF Libor (In pp)", 
                       "Tage nach einer 0.75 pp Anhebungs de Leitzinses in der Eurozone", 
                       "../Resultate/ZinsEZB_D", figWidth, figHeight)

# --------------------------------------------------------------
# Grafiken für 1W Shock
# --------------------------------------------------------------
Result <- createLPPlot(Reg.SNB1W.xr, Reg.SNB1W.ir, 0.75, NormInit, H, gap, c(-10, 2), 
                       "Nomineller Wechselkurs (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/WechselkursSNB1W_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB1W.ir, Reg.SNB1W.ir, 0.75, NormInit, H, gap, c(0, 2), 
                       "1W CHF Libor (In pp)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/ZinsSNB1W_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB1W.sp, Reg.SNB1W.ir, 0.75, NormInit, H, gap, c(-10, 2), 
                       "Aktienpreise (In pp)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseSNB1W_D", figWidth, figHeight)

# --------------------------------------------------------------
# Grafiken für 6M Shock
# --------------------------------------------------------------
Result <- createLPPlot(Reg.SNB6M.xr, Reg.SNB6M.ir, 0.75, NormInit, H, gap, c(-10, 2), 
                       "Nomineller Wechselkurs (In %)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/WechselkursSNB6M_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB6M.ir, Reg.SNB6M.ir, 0.75, NormInit, H, gap, c(0, 2), 
                       "6M CHF Libor (In pp)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/ZinsSNB6M_D", figWidth, figHeight)

Result <- createLPPlot(Reg.SNB6M.sp, Reg.SNB6M.ir, 0.75, NormInit, H, gap, c(-10, 2), 
                       "Aktienpreise (In pp)", 
                       "Tage nach einer 0.75 pp Anhebung des Schweizer Leitzinses", 
                       "../Resultate/AktienpreiseSNB6M_D", figWidth, figHeight)


#-------------------------------------------------------------------------------------
# END OF FILE
#-------------------------------------------------------------------------------------
