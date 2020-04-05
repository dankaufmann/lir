# ------------------------------------------------------------------------
# Wie weiter mit der Tiefzinspolitik? Szenarien und Alternativen
# - Motivation.R
# ------------------------------------------------------------------------
# Bemerkungen: Grafiken für Einleitung
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
cat("\014") 
rm(list=ls())
library(ggplot2)
library(xts)
library(tsbox)
library(extrafont)
loadfonts(device = "win")
source("Functions.R")

# Grafiken
figWdth   <- 5.5
figHeight <- 5



#-------------------------------------------------------------------------------------
# Grafik Inflation
#-------------------------------------------------------------------------------------
CPIData = read.csv("../Daten/CPIOECD.csv", sep = ",")
colnames(CPIData) <- c("LOCATION", "INDICATOR", "SUBJECT", "MEASURE",  "FREQUENCY", "TIME", "Value")
CPIGE <- xts(subset(CPIData, LOCATION == "DEU")[,7], order.by = as.Date(paste(subset(CPIData, LOCATION == "DEU")[,6], "-01", sep = "")))
CPICH <- xts(subset(CPIData, LOCATION == "CHE")[,7], order.by = as.Date(paste(subset(CPIData, LOCATION == "CHE")[,6], "-01", sep = "")))

CPIGE <- ts_span(CPIGE, "2000-01-01")
CPICH <- ts_span(CPICH, "2000-01-01")

# Grafik Inflation
p <- ts_ggplot(
  `Schweiz - Deutschland` = CPICH-CPIGE,
  title = "Inflationsdifferenz Schweiz - Deutschland (In pp)"
  
)
# 1) Steps up SNB Bills program (first auction > 20bn), 2) Removes the minimum exchange rate
myLines <- c(as.numeric(as.Date("2010-04-13")), as.numeric(as.Date("2015-01-15")), as.numeric(as.Date("2020-02-01")))
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=1,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) +
  geom_vline(xintercept=myLines , colour="blue", size = 1, alpha = 0.5) 
p <- p+  geom_text(x =myLines[1], y = -2.0, label="Erhöhung Emissionen SNB Bills", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[2], y = -2.5, label="Aufgabe Mindestkurs", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[3], y = -2.5, label="Corona-Virus Krise", colour="blue", angle=90, vjust = -1)
p
ggsave(filename = "../Resultate/Inflation_diff_Motivation.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/Inflation_diff_Motivation.png", width = figWdth, height = figHeight)


# Grafik Inflation
p <- ts_ggplot(
  `Schweiz` = CPICH,
  title = "Inflation Schweiz (In %)"
  
)
# 1) Steps up SNB Bills program (first auction > 20bn), 2) Removes the minimum exchange rate
myLines <- c(as.numeric(as.Date("2010-04-13")), as.numeric(as.Date("2015-01-15")), as.numeric(as.Date("2020-02-01")))
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=1,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) +
  geom_vline(xintercept=myLines , colour="blue", size = 1, alpha = 0.5) 
p <- p+  geom_text(x =myLines[1], y = 1.9, label="Erhöhung Emissionen SNB Bills", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[2], y = 2, label="Aufgabe Mindestkurs", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[3], y = 2, label="Corona-Virus Krise", colour="blue", angle=90, vjust = -1)
p
ggsave(filename = "../Resultate/Inflation_Motivation.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/Inflation_Motivation.png", width = figWdth, height = figHeight)


# --------------------------------------------------------------
# Grafik mit Kurzfristzinsen
# --------------------------------------------------------------
load(file="../Daten/Tagesdaten.RData")

NEEREUR <- xts(AllData$NEURR, order.by = AllData$Date)
NEER    <- xts(AllData$NEER, order.by = AllData$Date)

# Grafik Wechselkurs
p <- ts_ggplot(
  `Euroraum`       = NEEREUR,
  `Handelspartner` = NEER,
    title = "Wechselkurs (Index, Dez. 2000 = 100)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=1,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Wechselkurs_Motivation.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/Wechselkurs_Motivation.png", width = figWdth, height = figHeight)

LIBCH <- xts(AllData$LIB3M, order.by = AllData$Date)
LIBEUR <- xts(AllData$LIBEUR3M, order.by = AllData$Date)

# Grafik Kurzfristzinsen
p <- ts_ggplot(
  `Euroraum` = LIBEUR,
  `Schweiz`  = LIBCH,
  title = "Kurzfristzinsen (In %)"
  
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=1,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "../Resultate/Zinsen_Motivation.pdf", width = figWdth, height = figHeight)
ggsave(filename = "../Resultate/Zinsen_Motivation.png", width = figWdth, height = figHeight)

#-------------------------------------------------------------------------------------
# END OF FILE
#-------------------------------------------------------------------------------------
