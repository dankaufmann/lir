# ------------------------------------------------------------------------
# Ausstieg aus der Negativzinspolitik: Szenarien und Alternativen
# * Tests für Kointegration Zinsdifferenz und Wechselkurs mit
#   Tagesdaten und Monatsdaten
# ------------------------------------------------------------------------
# Notes: 
#
# TODO:
# - Schätzen
# - Testen
# - Resultate dann auf andere Modelle auferlegen
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
library(tsbox)
library(xlsx)
library(extrafont)
library(stargazer)
library(doParallel)
library(dplyr)
library(sandwich)
library(missMDA)
library(egcm)
loadfonts(device = "win")
source("SVARFunctions.R")

# Grafiken
figWdth   <- 5
figHeight <- 4.5

# --------------------------------------------------------------
# Tagesdaten
# --------------------------------------------------------------
load(file="../Daten/Tagesdaten.RData")

myStart <- as.Date("2000-01-01")
myEnd   <- as.Date("2007-12-31")
Sample <- AllData

# Event daten
Data <- subset(Sample, (Sample$Date >= myStart & Sample$Date <= myEnd))

# Schätzung i = e + i*(+1)-i*+eps (für Tagesmodell, gemäss Theorie)
Data$i <- Data$LIB3M
Data$dis <- lead(Data$LIBEUR3M, 1) -Data$LIBEUR3M
Data$is <- Data$LIBEUR3M
Data$e <- log(Data$NEURR/100)*100

CointReg1 <- lm(i~e, data = Data)
summary(CointReg1)
Equilib1 <- CointReg1$resid
plot(Equilib1)
Test1 <- egcm(Data$e, Data$i)
Test1
summary(Test1)

# Schätzung i = i* + eps (für Monatsmodell, gemäss Theorie)
CointReg2 <- lm(i~is, data = Data)
summary(CointReg2)
Equilib2 <- CointReg2$resid
plot(Equilib2)
Test2 <- egcm(Data$is, Data$i)
Test2
summary(Test2)

# --------------------------------------------------------------
# Monatsdaten
# --------------------------------------------------------------
myStart   <- "1985-01-01"
myEnd     <- "2007-12-01"
SRData = read.xlsx("../Daten/SRDataM.xlsx", sheetName = "Data", startRow = 1)
XRData = read.xlsx("../Daten/XRDataM.xlsx", sheetName = "Data", startRow = 19)

SRGE <- xts(subset(SRData, LOCATION == "DEU")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "DEU")[,6], "-01", sep = "")))
SRCH <- xts(subset(SRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "CHE")[,6], "-01", sep = "")))

NEURR <- xts(XRData[,3], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))

Data <- ts_c(SRGE, SRCH, NEURR)
Data <- ts_span(Data, start = myStart, end = myEnd)

# Schätzung i = e + i*(+1)-i*+eps (für Tagesmodell, gemäss Theorie)
Data$i <- Data$SRCH
Data$dis <- ts_lag(Data$SRGE, -1) -Data$SRGE
Data$is <- Data$SRGE
Data$e <- log(1/Data$NEURR*100)*100

CointReg1 <- lm(i~e, data = Data)
summary(CointReg1)
Equilib1 <- CointReg1$resid
plot(Equilib1)
Test1 <- egcm(Data$e, Data$i)
Test1
summary(Test1)

# Schätzung i = i* + eps (für Monatsmodell, gemäss Theorie)
CointReg2 <- lm(i~is, data = Data)
summary(CointReg2)
Equilib2 <- CointReg2$resid
plot(Equilib2)
Test2 <- egcm(Data$is, Data$i)
Test2
summary(Test2)






sdf
# Add the shocks for Switzerland (just the change in the 3M Libor)
Data$ShocksLib <- Data$LIB3M - lag(Data$LIB3M, 1)
Data$LBPos <- Data$LB
Data$LBPos[Data$ShocksLib<0] <- FALSE
Data$LBNeg <- Data$LB
Data$LBNeg[Data$ShocksLib>0] <- FALSE

Data$ECBDecPos <- Data$ECBDec
Data$ECBDecPos[Data$ShockECB3M<0] <- FALSE
Data$ECBDecNeg <- Data$ECBDec
Data$ECBDecNeg[Data$ShockECB3M>0] <- FALSE

# Controls and lags for all models
Yc      <- c("NEURR", "SMIClose", "EURUSD")
conLog  <- c(TRUE,    TRUE,        TRUE,        TRUE)
#Yc      <- c("")
#conLog  <- c(TRUE)
X       <- c("EURUSD")
exoLog  <- c(TRUE,      TRUE)
exoDiff <- c(TRUE,      TRUE)
#X       <- c("")
#exoLog  <- c(TRUE)
H       <- 50
P       <- 1

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib" # ShocksLib or ShockECB3M
Y       <- "NEURR"
depLog  <- TRUE
Reg.SNB.xr <- EstimLP(MainEv, e, Y, Yc, X, H, P, exoLog, exoDiff, Data)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksLib" # ShocksLib or ShockECB3M
Y       <- "LIB3M"
depLog  <- FALSE
Reg.SNB.ir <- EstimLP(MainEv, e, Y, Yc, X, H, P, exoLog, exoDiff, Data)

MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "NEURR"
depLog  <- TRUE
e       <- "ShockECB3M" # ShocksLib or ShockECB3M
Reg.ECB.xr <- EstimLP(MainEv, e, Y, Yc, X, H, P, exoLog, exoDiff, Data)

MainEv  <- "ECBDec"        # LB or ECBDec
Y       <- "LIB3M"
depLog  <- FALSE
e       <- "ShockECB3M" # ShocksLib or ShockECB3M
Reg.ECB.ir <- EstimLP(MainEv, e, Y, Yc, X, H, P, exoLog, exoDiff, Data)

# Compute impulse responses for SNB monetary policy shock
Resp.xr <- matrix(unlist(Reg.SNB.xr[[1]][1:(H+1)]), ncol = H+1)
Var.xr  <- matrix(unlist(Reg.SNB.xr[[2]][1:(H+1)]), ncol = H+1)
Resp.ir <- matrix(unlist(Reg.SNB.ir[[1]][1:(H+1)]), ncol = H+1)
Var.ir  <- matrix(unlist(Reg.SNB.ir[[2]][1:(H+1)]), ncol = H+1)

# Remove data that is not needed
Resp.xr <- Resp.xr[2,]
Var.xr  <- Var.xr[2,]
Resp.ir <- Resp.ir[2,]
Var.ir  <- Var.ir[2,]

# Normalize to +0.75Pp
Resp.xr <- Resp.xr/Resp.ir[1]*0.75
Var.xr <- Var.xr/(Resp.ir[1]^2)*0.75^2
Resp.ir <- Resp.ir/Resp.ir[1]*0.75
Var.ir <- Var.ir/(Resp.ir[1]^2)*0.75^2

# Exchange rate: Compute impulse responses
Plot.xr <- data.frame(0:H, Resp.xr, Resp.xr+1.64*sqrt(Var.xr), Resp.xr-1.64*sqrt(Var.xr))
colnames(Plot.xr) <- c("Tage", "Wechselkurs", "Konfidenzintervall1", "Konfidenzintervall2")

# Make a plot
g <- ggplot(Plot.xr, aes(Tage))
g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2" , ymax = "Konfidenzintervall1"), fill= "gray25", alpha=3/10)
g <- g + theme_minimal() + xlab("") + ggtitle("Reaktion des Wechselkurs (In %)")+theme(plot.title = element_text(size = 10))+ylab("")+
           theme(legend.title=element_blank()) + scale_x_continuous(breaks=seq(0, H, 10))
g <- g + expand_limits(y=c(0,-20))
g <- g + geom_hline(yintercept = 0, size=0.5, linetype = "solid") + theme(legend.position = "none")
g <- g + geom_line(aes_string(y = "Wechselkurs"), size = 1)+xlab("Tage nach einer 0.75 Pp Anhebung des Schweizer Leitzinses")+theme(plot.title = element_text(size = 13))
g <- g+ theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
g
ggsave(filename = "../Resultate/WechelkursSNB_D.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechelkursSNB_D.png", width = figWdth, height = figHeight)

# Make a plot (for lecture)
g <- ggplot(Plot.xr, aes(Tage))
g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2" , ymax = "Konfidenzintervall1"), fill= "gray25", alpha=3/10)
g <- g + theme_minimal() + xlab("") + ggtitle("Response of CHF/EUR (In %)")+theme(plot.title = element_text(size = 10))+ylab("")+
  theme(legend.title=element_blank()) + scale_x_continuous(breaks=seq(0, H, 10))
g <- g + expand_limits(y=c(0,-20))
g <- g + geom_hline(yintercept = 0, size=0.5, linetype = "solid") + theme(legend.position = "none")
g <- g + geom_line(aes_string(y = "Wechselkurs"), size = 1)+xlab("Days after 0.75 pp increase in SNB policy rate")+theme(plot.title = element_text(size = 13))
g <- g+ theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
g
ggsave(filename = "../Resultate/WechelkursSNB_En_D.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechelkursSNB_En_D.png", width = figWdth, height = figHeight)

# Interest rate: Compute impulse responses
Plot.ir <- data.frame(0:H, Resp.ir, Resp.ir+1.64*sqrt(Var.ir), Resp.ir-1.64*sqrt(Var.ir))
colnames(Plot.ir) <- c("Tage", "Zins", "Konfidenzintervall1", "Konfidenzintervall2")

# Zins: Make a plot
g <- ggplot(Plot.ir, aes(Tage))
g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2" , ymax = "Konfidenzintervall1"), fill= "gray25", alpha=3/10)
g <- g + theme_minimal() + xlab("") + ggtitle("Reaktion des 3M CHF Libor (In Pp)")+theme(plot.title = element_text(size = 10))+ylab("")+
  theme(legend.title=element_blank()) + scale_x_continuous(breaks=seq(0, H, 10))
g <- g + expand_limits(y=c(-2,5))
g <- g + geom_hline(yintercept = 0, size=0.5, linetype = "solid") + theme(legend.position = "none")
g <- g + geom_line(aes_string(y = "Zins"), size = 1)+xlab("Tage nach einer 0.75 Pp Anhebung des Schweizer Leitzinses")+theme(plot.title = element_text(size = 13))
g <- g+ theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
g
ggsave(filename = "../Resultate/ZinsSNB_D.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsSNB_D.png", width = figWdth, height = figHeight)

# Compute impulse responses for SNB monetary policy shock
Resp.xr <- matrix(unlist(Reg.ECB.xr[[1]][1:(H+1)]), ncol = H+1)
Var.xr  <- matrix(unlist(Reg.ECB.xr[[2]][1:(H+1)]), ncol = H+1)
Resp.ir <- matrix(unlist(Reg.ECB.ir[[1]][1:(H+1)]), ncol = H+1)
Var.ir  <- matrix(unlist(Reg.ECB.ir[[2]][1:(H+1)]), ncol = H+1)

# Remove data that is not needed
Resp.xr <- Resp.xr[2,]
Var.xr  <- Var.xr[2,]
Resp.ir <- Resp.ir[2,]
Var.ir  <- Var.ir[2,]

# Normalize to +0.75Pp
Resp.xr <- Resp.xr*0.75
Var.xr <- Var.xr*0.75^2
Resp.ir <- Resp.ir*0.75
Var.ir <- Var.ir*0.75^2

# Compute impulse responses
Plot.xr <- data.frame(0:H, Resp.xr, Resp.xr+1.64*sqrt(Var.xr), Resp.xr-1.64*sqrt(Var.xr))
colnames(Plot.xr) <- c("Tage", "Wechselkurs", "Konfidenzintervall1", "Konfidenzintervall2")

# Make a plot
g <- ggplot(Plot.xr, aes(Tage))
g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2" , ymax = "Konfidenzintervall1"), fill= "gray25", alpha=3/10)
g <- g + theme_minimal() + xlab("") + ggtitle("Reaktion des Wechselkurs (In %)")+theme(plot.title = element_text(size = 10))+ylab("")+
  theme(legend.title=element_blank()) + scale_x_continuous(breaks=seq(0, H, 10))
g <- g + expand_limits(y=c(0,+20))
g <- g + geom_hline(yintercept = 0, size=0.5, linetype = "solid") + theme(legend.position = "none")
g <- g + geom_line(aes_string(y = "Wechselkurs"), size = 1)+xlab("Tage nach einer 0.75 Pp Anhebung des Euroraum Leitzinses")+theme(plot.title = element_text(size = 13))
g <- g+ theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
g
ggsave(filename = "../Resultate/WechelkursEZB_D.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechelkursEZB_D.png", width = figWdth, height = figHeight)

# Make a plot (for lecture)
g <- ggplot(Plot.xr, aes(Tage))
g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2" , ymax = "Konfidenzintervall1"), fill= "gray25", alpha=3/10)
g <- g + theme_minimal() + xlab("") + ggtitle("Response of CHF/EUR (In %)")+theme(plot.title = element_text(size = 10))+ylab("")+
  theme(legend.title=element_blank()) + scale_x_continuous(breaks=seq(0, H, 10))
g <- g + expand_limits(y=c(0,+20))
g <- g + geom_hline(yintercept = 0, size=0.5, linetype = "solid") + theme(legend.position = "none")
g <- g + geom_line(aes_string(y = "Wechselkurs"), size = 1)+xlab("Days after 0.75 pp increase in ECB policy rate")+theme(plot.title = element_text(size = 13))
g <- g+ theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
g
ggsave(filename = "../Resultate/WechelkursEZB_En_D.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechelkursEZB_En_D.png", width = figWdth, height = figHeight)

# Interest rate: Compute impulse responses
Plot.ir <- data.frame(0:H, Resp.ir, Resp.ir+1.64*sqrt(Var.ir), Resp.ir-1.64*sqrt(Var.ir))
colnames(Plot.ir) <- c("Tage", "Zins", "Konfidenzintervall1", "Konfidenzintervall2")

# Zins: Make a plot
g <- ggplot(Plot.ir, aes(Tage))
g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2" , ymax = "Konfidenzintervall1"), fill= "gray25", alpha=3/10)
g <- g + theme_minimal() + xlab("") + ggtitle("Reaktion des 3M CHF Libor (In Pp)")+theme(plot.title = element_text(size = 10))+ylab("")+
  theme(legend.title=element_blank()) + scale_x_continuous(breaks=seq(0, H, 10))
g <- g + expand_limits(y=c(-2,5))
g <- g + geom_hline(yintercept = 0, size=0.5, linetype = "solid") + theme(legend.position = "none")
g <- g + geom_line(aes_string(y = "Zins"), size = 1)+xlab("Tage nach einer 0.75 Pp Anhebung des Euroraum Leitzinses")+theme(plot.title = element_text(size = 13))
g <- g+ theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
g
ggsave(filename = "../Resultate/ZinsEZB_D.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsEZB_D.png", width = figWdth, height = figHeight)


# --------------------------------------------------------------
# **** END HERE  ****
# --------------------------------------------------------------
