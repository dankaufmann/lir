# ------------------------------------------------------------------------
# Wie weiter mit der Tiefzinspolitik? Szenarien und Alternativen
# - TheoreticalModel.R
# ------------------------------------------------------------------------
# Bemerkungen: Berechnet Szenarien mit theoretischem Modell
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
cat("\014") 
rm(list=ls())
library(tsbox)
library(ggplot2)
library(forecast)
library(RColorBrewer)
library(xts)
library(sandwich)
library(lmtest)
library(xlsx)
library(ggpubr)
library(dataseries)
require(lubridate)
source("Functions.R")

#-------------------------------------------------------------------------------------
# Einstellungen
#-------------------------------------------------------------------------------------
phi       <- 0.10992        # Mittelwert aus Bäurle und Menz (2008) und eigenen Berechnungen (s. unten)
PhiStudy  <- 0.16           # Mittelwert aus Bäurle und Menz (2008) und eigenen Berechnungen (s. unten)
rho       <- 0.72625        # Durchschnittliche Zinsdifferenz berenigt um Inflationsziel
iss       <- 3.35559        # Durchschnittlicher Zins Deutschland
pibar     <- 1              # Inflationsziel SNB
pibars    <- 1.9            # Inflationsziel EZB
lr        <- 10             # Maturität Langfristzins

myStart   <- "1995-01-01"   # Startpunkt für Kalibrierung
myEnd     <- "2007-12-01"   # Endpunkt für Kalibrierung
plotStart <- "2014-01-01"   # Startpunkt für Grafiken
plotEnd   <- "2024-12-01"   # Endpunkt für Grafiken

figWdth   <- 5.5           
figHeight <- 5             

#-------------------------------------------------------------------------------------
# Datenimport
#-------------------------------------------------------------------------------------
SRData = read.xlsx("../Daten/SRDataM.xlsx", sheetName = "Data", startRow = 1)
XRData = read.xlsx("../Daten/XRDataM.xlsx", sheetName = "Data", startRow = 19)
LRData = read.xlsx("../Daten/LRDataM.xlsx", sheetName = "Data", startRow = 1)

SRGE <- xts(subset(SRData, LOCATION == "DEU")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "DEU")[,6], "-01", sep = "")))
SRCH <- xts(subset(SRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "CHE")[,6], "-01", sep = "")))
LRCH <- xts(subset(LRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(LRData, LOCATION == "CHE")[,6], "-01", sep = "")))

NEER  <- xts(XRData[,2], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))
NEURR <- xts(XRData[,3], order.by = as.Date(paste(XRData[,1], "-01", sep = "")))

SRGE <- ts_frequency(SRGE, to = "month", aggregate = "mean", na.rm = TRUE)
SRCH <- ts_frequency(SRCH, to = "month", aggregate = "mean", na.rm = TRUE)
LRCH <- ts_frequency(LRCH, to = "month", aggregate = "mean", na.rm = TRUE)

NEER <- ts_index(ts_frequency(1/NEER, to = "month", aggregate = "mean", na.rm = TRUE), "2000-12-01")*100
NEURR <- ts_index(ts_frequency(1/NEURR, to = "month", aggregate = "mean", na.rm = TRUE), "2000-12-01")*100

SRCHlast <- SRCH[length(SRCH)]
SRGElast <- SRGE[length(SRGE)]

#-------------------------------------------------------------------------------------
# Schätzung Wert für phi
#-------------------------------------------------------------------------------------
EstimData <- ts_c(SRCH, log(NEURR)*100, ts_diff(SRCH), ts_pc(NEURR))
EstimData <- ts_c(EstimData, stats::lag(EstimData,1), stats::lag(EstimData,2))
colnames(EstimData) <- c("i", "e", "di", "de","i.l1", "e.l1", "di.l1", "de.l1","i.l2", "e.l2", "di.l2", "de.l2")
EstimData <- data.frame(ts_span(EstimData, myStart, myEnd))
PhiEstim <- lm(di~de+di.l1, data = EstimData)
PhiEstim2 <- lm(i~e, data = EstimData)

coeftest(PhiEstim, vcov=NeweyWest(PhiEstim))
coeftest(PhiEstim2, vcov=NeweyWest(PhiEstim2))
print(paste("Phi =",PhiEstim$coefficients["de"], sep = " " ))
print(paste("Phi2 =",PhiEstim2$coefficients["e"], sep = " " ))
print(paste("Phi average =",(PhiEstim$coefficients["de"]+PhiStudy)/2, sep = " " ))

#-------------------------------------------------------------------------------------
# Schätzung Wert für i*
#-------------------------------------------------------------------------------------
EstimData <- (SRGE)
EstimData <- data.frame(ts_span(EstimData, myStart, myEnd))
iEstim <- mean(EstimData[,1])
print(paste("i* =",iEstim))

#-------------------------------------------------------------------------------------
# Schätzung Wert für rho
#-------------------------------------------------------------------------------------
EstimData <- ts_c(SRCH, SRGE)
EstimData <- data.frame(ts_span(EstimData, myStart, myEnd))
RhoEstim <- -mean(EstimData$SRCH-pibar-(EstimData$SRGE-pibars))
print(paste("Rho =",RhoEstim))

#-------------------------------------------------------------------------------------
# Szenarien importieren
#-------------------------------------------------------------------------------------
load('../Resultate/Szenarien_M.rdata')
XSc1 <- ts_frequency(XSc1, to = "year", aggregate = "last")
XSc2 <- ts_frequency(XSc2, to = "year", aggregate = "last")
XSc3 <- ts_frequency(XSc3, to = "year", aggregate = "last")
XSc4 <- ts_frequency(XSc4, to = "year", aggregate = "last")
XSc5 <- ts_frequency(XSc5, to = "year", aggregate = "last")
XSc6 <- ts_frequency(XSc6, to = "year", aggregate = "last")

#-------------------------------------------------------------------------------------
# Berechnung Szenario 1
#-------------------------------------------------------------------------------------
# S1: Status Quo
XSc1$pibar  <- pibar
XSc1$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc1        <- calcUIPModel(XSc1, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 2
#-------------------------------------------------------------------------------------
# S2: SNB sofortiger Ausstieg
XSc2$pibar  <- pibar
XSc2$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc2        <- calcUIPModel(XSc2, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 3
#-------------------------------------------------------------------------------------
# S3: SNB gradueller Ausstieg
XSc3$pibar  <- pibar
XSc3$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc3        <- calcUIPModel(XSc3, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 4
#-------------------------------------------------------------------------------------
# S4: EZB gradueller Ausstieg
XSc4$pibar  <- pibar
XSc4$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc4        <- calcUIPModel(XSc4, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 5
#-------------------------------------------------------------------------------------
# S5: EZB weitere Lockerung
XSc5$pibar  <- pibar
XSc5$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc5        <- calcUIPModel(XSc5, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 6
#-------------------------------------------------------------------------------------
# S6: EZB weitere Lockerung mit Reaktion der SNB
XSc6$pibar  <- pibar
XSc6$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc6        <- calcUIPModel(XSc6, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 7
#-------------------------------------------------------------------------------------
# S7a: Temporäres Wechselkursziel
XSc7a        <- XSc1
XSc7a$pibar  <- pibar
XSc7a$Shocks <- 0
XSc7a["2020-01-01/2023-01-01", "Shocks"] <- -3.78
XSc7a["2024-01-01", "Shocks"] <- -5
fixIr       <- 0    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc7a       <- calcUIPModel(XSc7a, fixIr, pibars, phi, rho)

# S7b: Temporäres Wechselkursziel mit höherem Inflationsziel
XSc7b        <- XSc1
XSc7b["2020-01-01/2024-01-01", "pibar"] <- pibars
XSc7b$Shocks <- 0
XSc7b["2020-01-01/2023-01-01", "Shocks"] <- -3.78
XSc7b["2024-01-01", "Shocks"] <- -5
fixIr       <- 0    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc7b       <- calcUIPModel(XSc7b, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 8
#-------------------------------------------------------------------------------------
# S8a: Forward Guidance -0.75% für 5 Jahre
XSc8a        <- XSc1
XSc8a$pibar  <- pibar
XSc8a$SRCH["2020-01-01/2024-01-01",] <- SRCHlast
XSc8a$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc8a        <- calcUIPModel(XSc8a, fixIr, pibars, phi, rho)

# S8b: Forward Guidance 0% für 5 Jahre
XSc8b        <- XSc1
XSc8b$pibar  <- pibar
XSc8b$SRCH["2020-01-01/2024-01-01",] <- 0
XSc8b$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc8b        <- calcUIPModel(XSc8b, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Berechnung Szenario 9
#-------------------------------------------------------------------------------------
# S9a: Temporär höheres Inflationsziel und Zinspolitik von S1
XSc9a        <- XSc1
XSc9a$pibar  <- pibar
XSc9a["2020-01-01/2024-01-01", "pibar"] <- pibars
XSc9a$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc9a        <- calcUIPModel(XSc9a, fixIr, pibars, phi, rho)

# S9b: Temporär höheres Inflationsziel und Zinspolitik von S2
XSc9b        <- XSc2
XSc9b$pibar  <- pibar
XSc9b["2020-01-01/2024-01-01", "pibar"] <- pibars
XSc9b$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc9b        <- calcUIPModel(XSc9b, fixIr, pibars, phi, rho)

# S9c: Permanent höheres Inflationsziel und Zinspolitik von S1
XSc9c        <- XSc1
XSc9c$pibar  <- pibar
XSc9c[, "pibar"] <- pibars
XSc9c$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc9c        <- calcUIPModel(XSc9c, fixIr, pibars, phi, rho)

# S9d: Permanent höheres Inflationsziel und Zinspolitik von S2
XSc9d        <- XSc2
XSc9d$pibar  <- pibar
XSc9d[, "pibar"] <- pibars
XSc9d$Shocks <- 0
fixIr       <- c(1:8)    # Fixed den Zinspfad für jeweilige Perioden auf Szenario
XSc9d        <- calcUIPModel(XSc9d, fixIr, pibars, phi, rho)

#-------------------------------------------------------------------------------------
# Resultate exportieren
#-------------------------------------------------------------------------------------
i = 1
for(Res in list(XSc1, XSc2, XSc3, XSc4, XSc5,XSc6, XSc7a, XSc7b, XSc8a, XSc8b, XSc9a, XSc9b, XSc9c, XSc9d)){

  print(paste("Scen", i, sep = ""))
  
  Exp <- data.frame("",year(index(Res)), Res$i, Res$SRGE, Res$ehat)[1:5,]
  Exp[,2:5] <- round(Exp[,2:5], 1)
  write.table(Exp, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE)
  
  print("")
  i = i+1
  
}

#-------------------------------------------------------------------------------------
# Auf history normalisieren (Sc 1)
#-------------------------------------------------------------------------------------
# Normalize ebar based on baseline scenario
XSc1$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc1$ebar/100)
XSc2$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc2$ebar/100)
XSc3$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc3$ebar/100)
XSc4$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc4$ebar/100)
XSc5$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc5$ebar/100)
XSc6$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc6$ebar/100)

XSc7a$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc7a$ebar/100)
XSc7b$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc7b$ebar/100)

XSc8a$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc8a$ebar/100)
XSc8b$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc8b$ebar/100)

XSc9a$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc9a$ebar/100)
XSc9b$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc9b$ebar/100)
XSc9c$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc9c$ebar/100)
XSc9d$ebar2 <- exp(as.numeric(log(NEURR[length(NEURR)]))-as.numeric(XSc1$ehat[1]/100)+XSc9d$ebar/100)

# Then actual exchange rate is e = ebar + ehat
XSc1$e <- exp(log(XSc1$ebar2) + XSc1$ehat/100)
XSc2$e <- exp(log(XSc2$ebar2) + XSc2$ehat/100)
XSc3$e <- exp(log(XSc3$ebar2) + XSc3$ehat/100)
XSc4$e <- exp(log(XSc4$ebar2) + XSc4$ehat/100)
XSc5$e <- exp(log(XSc5$ebar2) + XSc5$ehat/100)
XSc6$e <- exp(log(XSc6$ebar2) + XSc6$ehat/100)

XSc7a$e <- exp(log(XSc7a$ebar2) + XSc7a$ehat/100)
XSc7b$e <- exp(log(XSc7b$ebar2) + XSc7b$ehat/100)

XSc8a$e <- exp(log(XSc8a$ebar2) + XSc8a$ehat/100)
XSc8b$e <- exp(log(XSc8b$ebar2) + XSc8b$ehat/100)

XSc9a$e <- exp(log(XSc9a$ebar2) + XSc9a$ehat/100)
XSc9b$e <- exp(log(XSc9b$ebar2) + XSc9b$ehat/100)
XSc9c$e <- exp(log(XSc9c$ebar2) + XSc9c$ehat/100)
XSc9d$e <- exp(log(XSc9d$ebar2) + XSc9d$ehat/100)

#-------------------------------------------------------------------------------------
# Grafiken
#-------------------------------------------------------------------------------------
NEURR <- ts_frequency(NEURR, to = "year", aggregate = "mean")
SRCH  <- ts_frequency(SRCH, to = "year", aggregate = "mean")
LRCH  <- ts_frequency(LRCH, to = "year", aggregate = "mean")
SRGE  <- ts_frequency(SRGE, to = "year", aggregate = "mean")

# S1, S2, S3 Wechselkurs
p <- ts_ggplot(
  `Historie`                      = ts_span(NEURR, plotStart),
  `S1: Basisszenario`             = ts_span(XSc1$e, "2020-01-01", plotEnd),
  `S2: Sofortiger Ausstieg SNB`   = ts_span(XSc2$e, "2020-01-01", plotEnd),
  `S3: Gradueller Ausstieg SNB`   = ts_span(XSc3$e, "2020-01-01", plotEnd),
  title = "Nomineller Wechselkurs (Index, Dez. 2000 = 100)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/WechselkursS1-3_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechselkursS1-3_T.png", width = figWdth, height = figHeight)

# S1, S2, S3 Zins
p <- ts_ggplot(
  `Historie` = ts_span(SRCH, plotStart),
  `S1: Basisszenario`                = ts_span(XSc1$i, "2020-01-01", plotEnd),
  `S2: Sofortiger Ausstieg SNB`   = ts_span(XSc2$i, "2020-01-01", plotEnd),
  `S3: Gradueller Ausstieg SNB`   = ts_span(XSc3$i, "2020-01-01", plotEnd),
  title = "Kurzfristzinsen Schweiz (In %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/ZinsS1-3_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsS1-3_T.png", width = figWdth, height = figHeight)

# S1, S4 Wechselkurs
p <- ts_ggplot(
  `Historie`                      = ts_span(NEURR, plotStart),
  `S1: Basisszenario`                = ts_span(XSc1$e, "2020-01-01", plotEnd),
  `S4: Gradueller Ausstieg EZB`   = ts_span(XSc4$e, "2020-01-01", plotEnd),
  title = "Nomineller Wechselkurs (Index, Dez. 2000 = 100)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/WechselkursS1-4_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechselkursS1-4_T.png", width = figWdth, height = figHeight)

# S1, S4 Zins
p <- ts_ggplot(
  `Historie` = ts_span(SRCH, plotStart),
  `S1: Basisszenario`                = ts_span(XSc1$i, "2020-01-01", plotEnd),
  `S4: Gradueller Ausstieg EZB`   = ts_span(XSc4$i, "2020-01-01", plotEnd),
  title = "Kurzfristzinsen Schweiz (In %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/ZinsS1-4_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsS1-4_T.png", width = figWdth, height = figHeight)

# S1, S5, S6 Wechselkurs
p <- ts_ggplot(
  `Historie`                      = ts_span(NEURR, plotStart),
  `S1: Basisszenario`                = ts_span(XSc1$e, "2020-01-01", plotEnd),
  `S5: Zinssenkung EZB ohne Reaktion`   = ts_span(XSc5$e, "2020-01-01", plotEnd),
  `S6: Zinssenkung EZB mit Reaktion`   = ts_span(XSc6$e, "2020-01-01", plotEnd),
  title = "Nomineller Wechselkurs (Index, Dez. 2000 = 100)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/WechselkursS1-6_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechselkursS1-6_T.png", width = figWdth, height = figHeight)

# S1, S5, S6 Zins
p <- ts_ggplot(
  `Historie` = ts_span(SRCH, plotStart),
  `S5: EZB Zinssenkung ohne Reaktion`   = ts_span(XSc5$i, "2020-01-01", plotEnd),
  `S6: EZB Zinssenkung mit Reaktion`   = ts_span(XSc6$i, "2020-01-01", plotEnd),
  title = "Kurzfristzinsen Schweiz (In %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/ZinsS1-6_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsS1-6_T.png", width = figWdth, height = figHeight)

# S7 Wechselkursziel Wechselkurs
p <- ts_ggplot(
  `Historie`                          = ts_span(NEURR, plotStart),
  `S1: Basisszenario`                    = ts_span(XSc1$e, "2020-01-01", plotEnd),
  `S7a: Wechselkursziel`              = ts_span(XSc7a$e, "2020-01-01", plotEnd),
  `S7b: Wechselkurs- und höheres Inflationsziel`   = ts_span(XSc7b$e, "2020-01-01", plotEnd),
  title = "Nomineller Wechselkurs (Index, Dez. 2000 = 100)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/WechselkursPeg_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechselkursPeg_T.png", width = figWdth, height = figHeight)

# S7 Wechselkursziel Zins
p <- ts_ggplot(
  `Historie`                      = ts_span(SRCH, plotStart),
  `S1: Basisszenario`                    = ts_span(XSc1$i, "2020-01-01", plotEnd),
  `S7a: Wechselkursziel`              = ts_span(XSc7a$i, "2020-01-01", plotEnd),
  `S7b: Wechselkurs- und höheres Inflationsziel`   = ts_span(XSc7b$i, "2020-01-01", plotEnd),
  title = "Kurzfristzinsen Schweiz (In %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/ZinsPeg_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsPeg_T.png", width = figWdth, height = figHeight)

# S8 Forward Guidance Wechselkurs
p <- ts_ggplot(
  `Historie`                  = ts_span(NEURR, plotStart),
  `S1: Basisszenario`            = ts_span(XSc1$e, "2020-01-01", plotEnd),
  `S8a: -0.75% für 5 Jahre`   = ts_span(XSc8a$e, "2020-01-01", plotEnd),
  `S8b: 0% für 5 Jahre`       = ts_span(XSc8b$e, "2020-01-01", plotEnd),
  title = "Nomineller Wechselkurs (Index, Dez. 2000 = 100)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/WechselkursForwardG_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechselkursForwardG_T.png", width = figWdth, height = figHeight)

# S8 Forward Guidance Zins
p <- ts_ggplot(
  `Historie` = ts_span(SRCH, plotStart),
  `S1: Basisszenario`            = ts_span(XSc1$i, "2020-01-01", plotEnd),
  `S8a: -0.75% für 5 Jahre`   = ts_span(XSc8a$i, "2020-01-01", plotEnd),
  `S8b: 0% für 5 Jahre`       = ts_span(XSc8b$i, "2020-01-01", plotEnd),
  title = "Kurzfristzinsen Schweiz (In %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/ZinsForwardG_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsForwardG_T.png", width = figWdth, height = figHeight)

# S9 Inflationsziel Wechselkurs
p <- ts_ggplot(
  `Historie`                      = ts_span(NEURR, plotStart),
  `S1: Basisszenario`                = ts_span(XSc1$e, "2020-01-01", plotEnd),
  `S9a: Temporär höheres Ziel`     = ts_span(XSc9a$e, "2020-01-01", plotEnd),
  `S9b: Temporär höheres Ziel und Ausstieg`   = ts_span(XSc9b$e, "2020-01-01", plotEnd),
  `S9c: Permanent höheres Ziel`   = ts_span(XSc9c$e, "2020-01-01", plotEnd),
  `S9d: Permanent höheres Ziel und Ausstieg`   = ts_span(XSc9d$e, "2020-01-01", plotEnd),
  title = "Nomineller Wechselkurs (Index, Dez. 2000 = 100)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/WechselkursPi_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/WechselkursPi_T.png", width = figWdth, height = figHeight)

# S9 Inflationsziel Zins
p <- ts_ggplot(
  `Historie`                      = ts_span(SRCH, plotStart),
  `S1: Basisszenario`                = ts_span(XSc1$i, "2020-01-01", plotEnd),
  `S9a: Temporär höheres Ziel`     = ts_span(XSc9a$i, "2020-01-01", plotEnd),
  `S9b: Temporär höheres Ziel und Ausstieg`   = ts_span(XSc9b$i, "2020-01-01", plotEnd),
  `S9c: Permanent höheres Ziel`   = ts_span(XSc9c$i, "2020-01-01", plotEnd),
  `S9d: Permanent höheres Ziel und Ausstieg`   = ts_span(XSc9d$i, "2020-01-01", plotEnd),
  title = "Kurzfristzinsen Schweiz (In %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/ZinsPi_T.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/ZinsPi_T.png", width = figWdth, height = figHeight)


# Show how much Swiss franc appreciates relative to base scenario
print(100*(XSc2$e["2020-01-01"]/XSc1$e["2020-01-01"]-1))
print(100*(XSc3$e["2020-01-01"]/XSc1$e["2020-01-01"]-1))
print(100*(XSc4$e["2020-01-01"]/XSc1$e["2020-01-01"]-1))
print(100*(XSc5$e["2020-01-01"]/XSc1$e["2020-01-01"]-1))
print(100*(XSc6$e["2020-01-01"]/XSc1$e["2020-01-01"]-1))

#-------------------------------------------------------------------------------------
# END OF FILE
#-------------------------------------------------------------------------------------