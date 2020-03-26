# Wie weiter mit der Tiefzinspolitik? Szenarien und Alternativen
# - ConstructShocks.R
# ------------------------------------------------------------------------
# Bemerkungen: Konstruiert geldpolitische shocks und exportiert diese 
#              auf Tages- und Quartalsbasis
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

# Anzahl Faktoren für die Interpolation von fehlenden Werten
nFactors   <- 4

# --------------------------------------------------------------
# Daten
# --------------------------------------------------------------
load(file="../Daten/Tagesdaten.RData")

# CH Schocks mit Faktoranalyse erstellen
# SAR Zinsen (Close vom Vortag und 12.00 fixing)
AllData$CHS1 <- AllData$SAR1W12 - dplyr::lag(AllData$SAR1WClose, 1)
AllData$CHS2 <- AllData$SAR2W12 - dplyr::lag(AllData$SAR2WClose, 1)
AllData$CHS3 <- AllData$SAR3W12 - dplyr::lag(AllData$SAR3WClose, 1)
AllData$CHS4 <- AllData$SAR1M12 - dplyr::lag(AllData$SAR1MClose, 1)
AllData$CHS5 <- AllData$SAR3M12 - dplyr::lag(AllData$SAR3MClose, 1)
AllData$CHS6 <- AllData$SAR6M12 - dplyr::lag(AllData$SAR6MClose, 1)
AllData$CHS7 <- AllData$SAR12M12 - dplyr::lag(AllData$SAR12MClose, 1)

# Do the same for LIBOR rates
AllData$CHS8 <- AllData$LIB3M - dplyr::lag(AllData$LIB3M, 1)
AllData$CHS9 <- AllData$LIB1M - dplyr::lag(AllData$LIB1M, 1)
AllData$CHS10 <- AllData$LIB5M - dplyr::lag(AllData$LIB5M, 1)
AllData$CHS11 <- AllData$LIB11M - dplyr::lag(AllData$LIB11M, 1)
AllData$CHS12 <- AllData$LIB7M - dplyr::lag(AllData$LIB7M, 1)
AllData$CHS13 <- AllData$LIB8M - dplyr::lag(AllData$LIB8M, 1)
AllData$CHS14 <- AllData$LIB4M - dplyr::lag(AllData$LIB4M, 1)
AllData$CHS15 <- AllData$LIB12M - dplyr::lag(AllData$LIB12M, 1)
AllData$CHS16 <- AllData$LIB6M - dplyr::lag(AllData$LIB6M, 1)
AllData$CHS17 <- AllData$LIB1W - dplyr::lag(AllData$LIB1W, 1)
AllData$CHS18 <- AllData$LIB2M - dplyr::lag(AllData$LIB2M, 1)

# Normalize
X <- AllData[, paste("CHS", seq(1, 18, 1),sep = "")]
X <- normalize(as.matrix(X))

# Interpolate
X = imputePCA(as.matrix(X), ncp=nFactors)

# Use the first principal component as shock series
PCX = prcomp(X$completeObs)
ShocksCH = PCX$x[,"PC1"]

# EZB Schocks mit Faktoranalyse erstellen
AllData$ECS1 <- AllData$ShockECB1M
AllData$ECS2 <- AllData$ShockECB1W
AllData$ECS3 <- AllData$ShockECB3M
AllData$ECS4 <- AllData$ShockECB6M
AllData$ECS5 <- AllData$ShockECBN1
AllData$ECS6 <- AllData$ShockECBN2
AllData$ECS7 <- AllData$ShockECBN3
AllData$ECS8 <- AllData$ShockECBN4
AllData$ECS9 <- AllData$ShockECBN5
AllData$ECS10 <- AllData$ShockECBN6
AllData$ECS11 <- AllData$ShockECBN7
AllData$ECS12 <- AllData$ShockECBN8
AllData$ECS13 <- AllData$ShockECBN9
AllData$ECS14 <- AllData$ShockECBN10
AllData$ECS15 <- AllData$ShockECBN11
AllData$ECS16 <- AllData$ShockECBN12

# Normalize
X <- AllData[, paste("ECS", seq(1, 16, 1),sep = "")]
X <- normalize(as.matrix(X))

# Interpolate
X = imputePCA(as.matrix(X), ncp=nFactors)

# Use the first principal component as shock series
PCX = prcomp(X$completeObs)
ShocksEUR = PCX$x[,"PC1"]

# Add the shocks for Switzerland (just the change in the 3M Libor)
#AllData$Equilib <- Equilib1
AllData$ShocksCH <- as.numeric(ShocksCH)
AllData$ShocksCH2 <- as.numeric(ShocksCH)
AllData$ShocksCHPlacebo <- as.numeric(ShocksCH)

AllData$ShocksLib <- AllData$LIB3M - dplyr::lag(AllData$LIB3M, 1)
AllData$ShocksLib1W <- AllData$LIB1W - dplyr::lag(AllData$LIB1W, 1)
AllData$ShocksLib6M <- AllData$LIB6M - dplyr::lag(AllData$LIB6M, 1)
AllData$ShocksEUR <- as.numeric(ShocksEUR)

AllData$LIBEUR3M_lead <- dplyr::lead(AllData$LIBEUR3M, 1)
AllData$LIBEUR1W_lead <- dplyr::lead(AllData$LIBEUR1W, 1)

# Construct random LB for placebo tests
AllData$Rand <- runif(length(AllData$Date))
AllData$DateQ <- floor_date(AllData$Date, "quarter")

AllData$LBPlacebo <- FALSE
for(tempD in unique(AllData$DateQ)){
  tempRand <- AllData$Rand[AllData$DateQ == tempD]
  indMax <- which.max(tempRand)
  valMax <- tempRand[indMax]
  
  AllData$LBPlacebo[AllData$DateQ == tempD&valMax==AllData$Rand] <- TRUE
}

AllData$ShocksCH[AllData$LB == FALSE] <- NA
AllData$ShocksCH2[(AllData$LB == FALSE & AllData$IrrgLB == FALSE)] <- NA
AllData$ShocksCHPlacebo[AllData$LBPlacebo == FALSE] <- NA
AllData$ShocksLib[AllData$LB == FALSE] <- NA
AllData$ShocksLib1W[AllData$LB == FALSE] <- NA
AllData$ShocksLib6M[AllData$LB == FALSE] <- NA
AllData$ShocksEUR[AllData$ECBDec == FALSE] <- NA

# Determine sign of shocks because not identified in factor model
sign1 <- sign(cor(AllData$ShocksCH, AllData$LIB3M, use = "pairwise.complete.obs"))
AllData$ShocksCH <- as.numeric(sign1)*AllData$ShocksCH
AllData$ShocksCH2 <- as.numeric(sign1)*AllData$ShocksCH2

sign2 <- sign(cor(AllData$ShocksCHPlacebo, AllData$LIB3M, use = "pairwise.complete.obs"))
AllData$ShocksCHPlacebo <- as.numeric(sign2)*AllData$ShocksCHPlacebo

sign3<- sign(cor(AllData$ShocksEUR, AllData$LIBEUR3M, use = "pairwise.complete.obs"))
AllData$ShocksEUR <- as.numeric(sign3)*AllData$ShocksEUR


# Datei für Tagesmodell speichern
save(AllData, file = "../Daten/TagesdatenShocks.RData")

# Datei für Quartalsmodell speichern
Schocks <- xts(AllData[, c("EID10Y", "NEURR", "EURUSD", "ESTXXClose", "SPI", "LIB1W", "LIBEUR1W", "LIB3M", "ShocksLib", "LIBEUR3M", "ShocksCH", "ShocksEUR")], order.by = AllData$Date)
Schocks <- ts_frequency(Schocks, to = "quarter", aggregate = "mean", na.rm = TRUE)

# Datei speichern
save(Schocks, file = "../Daten/QuartalsdatenShocks.RData")
