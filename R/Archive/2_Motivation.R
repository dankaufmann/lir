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
source("SVARFunctions.R")

# Grafiken
figWdth   <- 5.5
figHeight <- 5

# --------------------------------------------------------------
# Daten
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
