# ------------------------------------------------------------------------
# Wie weiter mit der Tiefzinspolitik? Szenarien und Alternativen
# - CreateScenarios.R
# ------------------------------------------------------------------------
# Bemerkungen: Konstruiert Zinsszenarien für theoretisches und monatliches
#              Modell
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
cat("\014") 
rm(list=ls())
library(tsbox)
library(xlsx)
library(xts)
library(forecast)
library(ggplot2)

#-------------------------------------------------------------------------------------
# Einstellungen
#-------------------------------------------------------------------------------------
myStart   <- "1985-01-01"  # Startdatum
myStartSS <- "1995-01-01"  # Startdatum für die Berechnung des Gleichgewichtszinses
myEndSS   <- "2007-12-01"  # Enddatum für die Berechnung des Gleichgewichstzinses
myEnd     <- "2020-03-01"  # Enddatum der Daten (fehlende Werte wurden mit einfachen Prognosen ergänzt)
plotEnd   <- "2030-12-01"  # Enddatum für die Grafiken
plotStart <- "2005-01-01"  # Startdatum für die Grafiken

H         <- 12*3          # Simulationshorizont für Szenario (Monate)
Hlong     <- 12*5          # Simulationshorizont für Normalisierung (Monate)
HlongFac  <- 10            # Simulationshorizont nach der Normalisierung (HlongFac*Hlong)
figWidth  <- 5.5
figHeight <- 5

#-------------------------------------------------------------------------------------
# Datenimport
#-------------------------------------------------------------------------------------
SRData = read.xlsx("../Daten/SRDataM.xlsx", sheetName = "Data", startRow = 1)
SRGE <- xts(subset(SRData, LOCATION == "DEU")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "DEU")[,6], "-01", sep = "")))
SRCH <- xts(subset(SRData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(SRData, LOCATION == "CHE")[,6], "-01", sep = "")))

ARIMAModel <- Arima(ts_ts(SRGE), order = c(1, 0, 1), include.constant= TRUE)
ARIMAFcst  <- forecast(ARIMAModel, h = H+Hlong*HlongFac)
ARIMAFcst  <- ts_xts(ARIMAFcst$mean)

ExoData  <- ts_c(SRCH, SRGE)
ExoData  <- ts_span(ExoData, myStart, myEnd)

#-------------------------------------------------------------------------------------
# Szenarien erstellen
#-------------------------------------------------------------------------------------
# Determine last observation and steady state value
lastObs <- as.numeric(c(ExoData$SRCH[length(ExoData$SRCH)], ExoData$SRGE[length(ExoData$SRGE)]))
Xss     <- c(mean(ts_span(SRCH, myStartSS, myEndSS)), mean(ts_span(SRGE, myStartSS, myEndSS)))

# S1: Status Quo.
#     Zinsen CH bleiben auf heutigem Niveau für 3 Jahre. Danach gradueller 
#     Anstieg über 5 Jahre bis zum Steady State
#     EZB belässt Zinsen bei heutigem Niveau. Danach gradueller Anstieg über 5 Jahre.
XSc1           <- ts_c(ARIMAFcst, ARIMAFcst)
XSc1[,]        <- NA
colnames(XSc1) <- c("SRCH", "SRGE")
XSc1[1:H,]     <- matrix(lastObs, ncol = 2, nrow = H, byrow = TRUE)
XSc1[(H+1):(Hlong+H), 1] <- seq(as.numeric(XSc1[H, 1]), Xss[1], length.out = Hlong)
XSc1[(H+1):(Hlong+H), 2] <- seq(as.numeric(XSc1[H, 2]), Xss[2], length.out = Hlong)
XSc1[(Hlong+H+1):(HlongFac*Hlong+H), 1] <- Xss[1]
XSc1[(Hlong+H+1):(HlongFac*Hlong+H), 2] <- Xss[2]
plot(XSc1)

# S2: SNB sofortiger Ausstieg.
#     Zinsen CH steigen sofort auf 0% und bleiben für 3 Jahre. Danach gradueller
#     Anstieg über 5 Jahre bis zum Steady State.
#     EZB belässt Zinsen bei heutigem Niveau für 3 Jahre. Danach gradueller Anstieg über 5 Jahre
XSc2 <- XSc1
XSc2[1:H,1]     <- 0
XSc2[(H+1):(Hlong+H), 1] <- seq(as.numeric(XSc2[H, 1]), Xss[1], length.out = Hlong)
XSc2[(H+1):(Hlong+H), 2] <- seq(as.numeric(XSc2[H, 2]), Xss[2], length.out = Hlong)
XSc2[(Hlong+H+1):(HlongFac*Hlong+H), 1] <- Xss[1]
XSc2[(Hlong+H+1):(HlongFac*Hlong+H), 2] <- Xss[2]
plot(XSc2)

# S3: SNB gradueller Ausstieg.
#     Zinsen CH steigen graduell bis auf 0% über 3 Jahre. Danach gradueller
#     Anstieg bis zum Steady state.
#     EZB belässt Zinsen bei heutigem Niveau für 3 Jahre. Danach gradueller Anstieg über 5 Jahre
XSc3 <- XSc1
XSc3[1:H,1]     <- seq(as.numeric(XSc3[1, 1]), 0, length.out = H)
XSc3[(H):(Hlong+H), 1] <- seq(0, Xss[1], length.out = Hlong+1)
XSc3[(H+1):(Hlong+H), 2] <- seq(as.numeric(XSc3[H, 2]), Xss[2], length.out = Hlong)
XSc3[(Hlong+H+1):(HlongFac*Hlong+H), 1] <- Xss[1]
XSc3[(Hlong+H+1):(HlongFac*Hlong+H), 2] <- Xss[2]
plot(XSc3)

# S4: EZB gradueller Ausstieg.
#     Zinsen EUR steigen graduell bis auf 0% über 3 Jahre. Danach gradueller Anstieg bis 
#     zum Steady State.
XSc4 <- XSc1
XSc4[1:H,2]     <- seq(as.numeric(XSc4[1, 2]), 0, length.out = H)
XSc4[(H):(Hlong+H), 2] <- seq(0, Xss[2], length.out = Hlong+1)
XSc4[(H+1):(Hlong+H), 1] <- seq(as.numeric(XSc4[H, 1]), Xss[1], length.out = Hlong)
XSc4[(Hlong+H+1):(HlongFac*Hlong+H), 1] <- Xss[1]
XSc4[(Hlong+H+1):(HlongFac*Hlong+H), 2] <- Xss[2]
plot(XSc4)

# S5: EZB Lockerung ohne SNB Reaktion
#     Zinsen EUR fallen um 0.5% für 3 JahRE. Danach gradueller Anstieg bis 
#     zum Steady State.
XSc5 <- XSc1
XSc5[1:H,2]     <-as.numeric(XSc5[1, 2])-0.5
XSc5[(H):(Hlong+H), 2] <- seq(as.numeric(XSc5[H, 2]), Xss[2], length.out = Hlong+1)
XSc5[(Hlong+H+1):(HlongFac*Hlong+H), 2] <- Xss[2]
XSc5[(H+1):(Hlong+H), 1] <- seq(as.numeric(XSc5[H, 1]), Xss[1], length.out = Hlong)
XSc5[(Hlong+H+1):(HlongFac*Hlong+H), 1] <- Xss[1]
plot(XSc5)

# S6: EZB Lockerung mit SNB Reaktion
#     Zinsen EUR fallen um 0.5% für 3 JahrE. Danach gradueller Anstieg bis 
#     zum Steady State.
XSc6 <- XSc1
XSc6[1:H,2]     <-as.numeric(XSc6[1, 2])-0.5
XSc6[(H):(Hlong+H), 2] <- seq(as.numeric(XSc6[H, 2]), Xss[2], length.out = Hlong+1)
XSc6[(Hlong+H+1):(HlongFac*Hlong+H), 2] <- Xss[2]
XSc6[1:H,1]     <-as.numeric(XSc6[1, 1])-0.25
XSc6[(H):(Hlong+H), 1] <- seq(as.numeric(XSc6[H, 1]), Xss[1], length.out = Hlong+1)
XSc6[(Hlong+H+1):(HlongFac*Hlong+H), 1] <- Xss[1]
plot(XSc6)


#-------------------------------------------------------------------------------------
# Exportieren der Szenarien für Monatliches, Tägliches, und Theoretisches Modell
#-------------------------------------------------------------------------------------
save(list = c("XSc1", "XSc2", "XSc3", "XSc4", "XSc5", "XSc6"), file="../Resultate/Szenarien_M.rdata")

#-------------------------------------------------------------------------------------
# Grafiken erstellen
#-------------------------------------------------------------------------------------
ExoData <- ts_span(ExoData, plotStart, plotEnd)
XSc1 <- ts_span(XSc1, plotStart, plotEnd)
XSc2 <- ts_span(XSc2, plotStart, plotEnd)
XSc3 <- ts_span(XSc3, plotStart, plotEnd)
XSc4 <- ts_span(XSc4, plotStart, plotEnd)
XSc5 <- ts_span(XSc5, plotStart, plotEnd)
XSc6 <- ts_span(XSc6, plotStart, plotEnd)

# Szenario 1
p <- ts_ggplot(
  `Kurzfristzinsen Schweiz`    = ExoData$SRCH,
  `Kurzfristzinsen Deutschland` = ExoData$SRGE,
  `Szenario Schweiz` = XSc1$SRCH,
  `Szenario Deutschland` = XSc1$SRGE,
  title = "S1: Basisszenario"
  )
# brewer.pal(n = 8, name = "Dark2")
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#7fad9f", "#e3a678", "#1B9E77", "#D95F02"))+ 
  ggplot2::scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Szenario1_M.pdf", width = figWidth, height = figHeight)
ggsave(filename = "../Resultate/Szenario1_M.png", width = figWidth, height = figHeight)

# Szenario 2
p <- ts_ggplot(
  `Kurzfristzinsen Schweiz`    = ExoData$SRCH,
  `Kurzfristzinsen Deutschland` = ExoData$SRGE,
  `Szenario Schweiz` = XSc2$SRCH,
  `Szenario Deutschland` = XSc2$SRGE,
  title = "S2: Sofortiger Ausstieg SNB"
)
# brewer.pal(n = 8, name = "Dark2")
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#7fad9f", "#e3a678", "#1B9E77", "#D95F02"))+ 
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Szenario2_M.pdf", width = figWidth, height = figHeight)
ggsave(filename = "../Resultate/Szenario2_M.png", width = figWidth, height = figHeight)

# Szenario 3
p <- ts_ggplot(
  `Kurzfristzinsen Schweiz`    = ExoData$SRCH,
  `Kurzfristzinsen Deutschland` = ExoData$SRGE,
  `Szenario Schweiz` = XSc3$SRCH,
  `Szenario Deutschland` = XSc3$SRGE,
  title = "S3: Gradueller Ausstieg SNB"
)
# brewer.pal(n = 8, name = "Dark2")
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#7fad9f", "#e3a678", "#1B9E77", "#D95F02"))+ 
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Szenario3_M.pdf", width = figWidth, height = figHeight)
ggsave(filename = "../Resultate/Szenario3_M.png", width = figWidth, height = figHeight)

# Szenario 4
p <- ts_ggplot(
  `Kurzfristzinsen Schweiz`    = ExoData$SRCH,
  `Kurzfristzinsen Deutschland` = ExoData$SRGE,
  `Szenario Schweiz` = XSc4$SRCH,
  `Szenario Deutschland` = XSc4$SRGE,
  title = "S4: Gradueller Ausstieg SNB"
)
# brewer.pal(n = 8, name = "Dark2")
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#7fad9f", "#e3a678", "#1B9E77", "#D95F02"))+ 
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Szenario4_M.pdf", width = figWidth, height = figHeight)
ggsave(filename = "../Resultate/Szenario4_M.png", width = figWidth, height = figHeight)

# Szenario 5
p <- ts_ggplot(
  `Kurzfristzinsen Schweiz`    = ExoData$SRCH,
  `Kurzfristzinsen Deutschland` = ExoData$SRGE,
  `Szenario Schweiz` = XSc5$SRCH,
  `Szenario Deutschland` = XSc5$SRGE,
  title = "S5: Zinssenkung EZB ohne Reaktion SNB"
)
# brewer.pal(n = 8, name = "Dark2")
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#7fad9f", "#e3a678", "#1B9E77", "#D95F02"))+ 
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Szenario5_M.pdf", width = figWidth, height = figHeight)
ggsave(filename = "../Resultate/Szenario5_M.png", width = figWidth, height = figHeight)

# Szenario 6
p <- ts_ggplot(
  `Kurzfristzinsen Schweiz`    = ExoData$SRCH,
  `Kurzfristzinsen Deutschland` = ExoData$SRGE,
  `Szenario Schweiz` = XSc6$SRCH,
  `Szenario Deutschland` = XSc6$SRGE,
  title = "S6: Zinssenkung EZB mit Reaktion SNB"
)
# brewer.pal(n = 8, name = "Dark2")
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#7fad9f", "#e3a678", "#1B9E77", "#D95F02"))+ 
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=3,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Szenario6_M.pdf", width = figWidth, height = figHeight)
ggsave(filename = "../Resultate/Szenario6_M.png", width = figWidth, height = figHeight)

#-------------------------------------------------------------------------------------
# END OF FILE
#-------------------------------------------------------------------------------------

