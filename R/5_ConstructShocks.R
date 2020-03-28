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
nFactors   <- 5

# --------------------------------------------------------------
# Daten
# --------------------------------------------------------------
load(file="../Daten/Tagesdaten.RData")
AllData$CHS1 <- NA
AllData$CHS2 <- NA
AllData$CHS3 <- NA
AllData$CHS4 <- NA
AllData$CHS5 <- NA
AllData$CHS6 <- NA
AllData$CHS7 <- NA
AllData$CHS8 <- NA
AllData$CHS9 <- NA
AllData$CHS10 <- NA
AllData$CHS11 <- NA
AllData$CHS12 <- NA
AllData$CHS13 <- NA
AllData$CHS14 <- NA
AllData$CHS15 <- NA
AllData$CHS16 <- NA
AllData$CHS17 <- NA
AllData$CHS18 <- NA

AllData$CHS19 <- NA
AllData$CHS20 <- NA
AllData$CHS21 <- NA
AllData$CHS22 <- NA
AllData$CHS23 <- NA
AllData$CHS24 <- NA
AllData$CHS25 <- NA

AllData$STX <- NA


AllData$Time[is.na(AllData$Time)] <- "1899-12-30 09:30:00 GMT"

for(t in 1:length(AllData$Date)){

  # If afternoon
  # Close same day - 12:00
  if(hour(AllData$Time[t])>11){
    AllData$CHS1[t] <- (AllData$SAR1WClose - AllData$SAR1W12)[t]
    AllData$CHS2[t] <- (AllData$SAR2WClose - AllData$SAR2W12)[t]
    AllData$CHS3[t] <- (AllData$SAR3WClose - AllData$SAR3W12)[t]
    AllData$CHS4[t] <- (AllData$SAR1MClose - AllData$SAR1M12)[t]
    AllData$CHS5[t] <- (AllData$SAR3MClose - AllData$SAR3M12)[t]
    AllData$CHS6[t] <- (AllData$SAR6MClose - AllData$SAR6M12)[t]
    AllData$CHS7[t] <- (AllData$SAR12MClose - AllData$SAR12M12)[t]

    # Do the same for LIBOR rates
    AllData$CHS8[t] <- (dplyr::lead(AllData$LIB3M, 1) - AllData$LIB3M)[t]
    AllData$CHS9[t] <- (dplyr::lead(AllData$LIB1M, 1) - AllData$LIB1M)[t]
    AllData$CHS10[t] <- (dplyr::lead(AllData$LIB5M, 1) - AllData$LIB5M)[t]
    AllData$CHS11[t] <- (dplyr::lead(AllData$LIB11M, 1) - AllData$LIB11M)[t]
    AllData$CHS12[t] <- (dplyr::lead(AllData$LIB7M, 1) - AllData$LIB7M)[t]
    AllData$CHS13[t] <- (dplyr::lead(AllData$LIB8M, 1) - AllData$LIB8M)[t]
    AllData$CHS14[t] <- (dplyr::lead(AllData$LIB4M, 1) - AllData$LIB4M)[t]
    AllData$CHS15[t] <- (dplyr::lead(AllData$LIB12M, 1) - AllData$LIB12M)[t]
    AllData$CHS16[t] <- (dplyr::lead(AllData$LIB6M, 1)- AllData$LIB6M)[t]
    AllData$CHS17[t] <- (dplyr::lead(AllData$LIB2M, 1) - AllData$LIB2M)[t]
    AllData$CHS18[t] <- (dplyr::lead(AllData$LIB1W, 1) - AllData$LIB1W)[t]
   }

  # If morning
  # 12:00 - close day before
  if(hour(AllData$Time[t])<=11){
    AllData$CHS1[t] <- (AllData$SAR1W12 - dplyr::lag(AllData$SAR1WClose, 1))[t]
    AllData$CHS2[t] <- (AllData$SAR2W12 - dplyr::lag(AllData$SAR2WClose, 1))[t]
    AllData$CHS3[t] <- (AllData$SAR3W12 - dplyr::lag(AllData$SAR3WClose, 1))[t]
    AllData$CHS4[t] <- (AllData$SAR1M12 - dplyr::lag(AllData$SAR1MClose, 1))[t]
    AllData$CHS5[t] <- (AllData$SAR3M12 - dplyr::lag(AllData$SAR3MClose, 1))[t]
    AllData$CHS6[t] <- (AllData$SAR6M12 - dplyr::lag(AllData$SAR6MClose, 1))[t]
    AllData$CHS7[t] <- (AllData$SAR12M12 - dplyr::lag(AllData$SAR12MClose, 1))[t]

    # Do the same for LIBOR rates
    AllData$CHS8[t] <- (AllData$LIB3M - dplyr::lag(AllData$LIB3M, 1))[t]
    AllData$CHS9[t] <- (AllData$LIB1M - dplyr::lag(AllData$LIB1M, 1))[t]
    AllData$CHS10[t] <- (AllData$LIB5M - dplyr::lag(AllData$LIB5M, 1))[t]
    AllData$CHS11[t] <- (AllData$LIB11M - dplyr::lag(AllData$LIB11M, 1))[t]
    AllData$CHS12[t] <- (AllData$LIB7M - dplyr::lag(AllData$LIB7M, 1))[t]
    AllData$CHS13[t] <- (AllData$LIB8M - dplyr::lag(AllData$LIB8M, 1))[t]
    AllData$CHS14[t] <- (AllData$LIB4M - dplyr::lag(AllData$LIB4M, 1))[t]
    AllData$CHS15[t] <- (AllData$LIB12M - dplyr::lag(AllData$LIB12M, 1))[t]
    AllData$CHS16[t] <- (AllData$LIB6M - dplyr::lag(AllData$LIB6M, 1))[t]
    AllData$CHS17[t] <- (AllData$LIB2M - dplyr::lag(AllData$LIB2M, 1))[t]
    AllData$CHS18[t] <- (AllData$LIB1W - dplyr::lag(AllData$LIB1W, 1))[t]
  }
  
  # Add Bond data, day before and day after because time not known
  AllData$CHS19[t] <- (dplyr::lead(AllData$EID1Y, 1) - dplyr::lag(AllData$EID1Y, 1))[t]
  AllData$CHS20[t] <- (dplyr::lead(AllData$EID5Y, 1) - dplyr::lag(AllData$EID5Y, 1))[t]
  AllData$CHS21[t] <- (dplyr::lead(AllData$EID10Y, 1) - dplyr::lag(AllData$EID10Y, 1))[t]
  AllData$CHS22[t] <- (dplyr::lead(AllData$EID8Y, 1) - dplyr::lag(AllData$EID8Y, 1))[t]
  AllData$CHS23[t] <- (dplyr::lead(AllData$CANT8Y, 1) - dplyr::lag(AllData$CANT8Y, 1))[t]
  AllData$CHS24[t] <- (dplyr::lead(AllData$BANK8Y, 1) - dplyr::lag(AllData$BANK8Y, 1))[t]
  AllData$CHS25[t] <- (dplyr::lead(AllData$IND8Y, 1) - dplyr::lag(AllData$IND8Y, 1))[t]
  
  # Add stock data for checks
  AllData$STX[t] <- (dplyr::lead(AllData$SPI, 1) - dplyr::lag(AllData$SPI, 1))[t]
  
    # # Make sur decision is in window
  # AllData$CHS1[t] <- (AllData$SAR2WClose-dplyr::lag(AllData$SAR2WClose, 1))[t]
  # AllData$CHS2[t] <- (AllData$SAR3WClose-dplyr::lag(AllData$SAR3WClose, 1))[t]
  # AllData$CHS3[t] <- (AllData$SAR1MClose-dplyr::lag(AllData$SAR1MClose, 1))[t]
  # AllData$CHS4[t] <- (AllData$SAR3MClose-dplyr::lag(AllData$SAR3MClose, 1))[t]
  # AllData$CHS5[t] <- (AllData$SAR6MClose-dplyr::lag(AllData$SAR6MClose, 1))[t]
  # AllData$CHS6[t] <- (AllData$SAR12MClose-dplyr::lag(AllData$SAR12MClose, 1))[t]
  # 
  # # Do the same for LIBOR rates
  # AllData$CHS7[t] <- (dplyr::lead(AllData$LIB3M, 1) - dplyr::lag(AllData$LIB3M, 1))[t]
  # AllData$CHS8[t] <- (dplyr::lead(AllData$LIB1M, 1) - dplyr::lag(AllData$LIB1M, 1))[t]
  # AllData$CHS9[t] <- (dplyr::lead(AllData$LIB5M, 1) - dplyr::lag(AllData$LIB5M, 1))[t]
  # AllData$CHS10[t] <- (dplyr::lead(AllData$LIB11M, 1) - dplyr::lag(AllData$LIB11M, 1))[t]
  # AllData$CHS11[t] <- (dplyr::lead(AllData$LIB7M, 1) - dplyr::lag(AllData$LIB7M, 1))[t]
  # AllData$CHS12[t] <- (dplyr::lead(AllData$LIB8M, 1) - dplyr::lag(AllData$LIB8M, 1))[t]
  # AllData$CHS13[t] <- (dplyr::lead(AllData$LIB4M, 1) - dplyr::lag(AllData$LIB4M, 1))[t]
  # AllData$CHS14[t] <- (dplyr::lead(AllData$LIB12M, 1) - dplyr::lag(AllData$LIB12M, 1))[t]
  # AllData$CHS15[t] <- (dplyr::lead(AllData$LIB6M, 1)- dplyr::lag(AllData$LIB6M, 1))[t]
  # AllData$CHS16[t] <- (dplyr::lead(AllData$LIB2M, 1) - dplyr::lag(AllData$LIB2M, 1))[t]
  # 
  # # Only one week, do nto take those
  # AllData$CHS17[t] <- (dplyr::lead(AllData$LIB1W, 1) - dplyr::lag(AllData$LIB1W, 1))[t]
  # AllData$CHS18[t] <- (AllData$SAR1WClose-dplyr::lag(AllData$SAR1WClose, 1))[t]
  
  
}

AllData$ShocksLib   <- AllData$CHS7
AllData$ShocksLib6M <- AllData$CHS15
AllData$ShocksLib1W <- AllData$CHS17

# Normalize
X <- AllData[, paste("CHS", seq(1, 25, 1),sep = "")]
X <- normalize(as.matrix(X))

# Interpolate
X = imputePCA(as.matrix(X), ncp=nFactors)

# Use the first principal component as shock series
PCX = prcomp(X$completeObs)
ShocksCH1 = PCX$x[,"PC1"]
ShocksCH2 = PCX$x[,"PC2"]

# Check correlation of Ranaldo/Rossi Surprises with my surprises and set the sign accordingly
cor(data.frame(AllData$Surprise[!is.na(AllData$Surprise)], ShocksCH1[!is.na(AllData$Surprise)]), use = "pairwise.complete.obs")
cor(data.frame(AllData$Surprise[!is.na(AllData$Surprise)], ShocksCH2[!is.na(AllData$Surprise)]), use = "pairwise.complete.obs")
cor(data.frame(ShocksCH1[AllData$LB == TRUE], ShocksCH2[AllData$LB == TRUE]), use = "pairwise.complete.obs")
cor(data.frame(AllData$STX[AllData$LB == TRUE], ShocksCH1[AllData$LB == TRUE]), use = "pairwise.complete.obs")
cor(data.frame(AllData$STX[AllData$LB == TRUE], ShocksCH2[AllData$LB == TRUE]), use = "pairwise.complete.obs")

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
ShocksEUR1 = PCX$x[,"PC1"]
ShocksEUR2 = PCX$x[,"PC2"]

# Add the shocks for Switzerland (just the change in the 3M Libor)
#AllData$Equilib <- Equilib1
AllData$ShocksCH1 <- as.numeric(ShocksCH1)
AllData$ShocksCH2 <- as.numeric(ShocksCH2)
AllData$ShocksEUR1 <- as.numeric(ShocksEUR1)
AllData$ShocksEUR2 <- as.numeric(ShocksEUR2)

AllData$LIBEUR3M_lead <- dplyr::lead(AllData$LIBEUR3M, 1)
AllData$LIBEUR1W_lead <- dplyr::lead(AllData$LIBEUR1W, 1)

AllData$ShocksCH1[AllData$LB == FALSE] <- NA
AllData$ShocksCH2[AllData$LB == FALSE] <- NA
AllData$ShocksLib[AllData$LB == FALSE] <- NA
AllData$ShocksLib1W[AllData$LB == FALSE] <- NA
AllData$ShocksLib6M[AllData$LB == FALSE] <- NA
AllData$ShocksEUR1[AllData$ECBDec == FALSE] <- NA
AllData$ShocksEUR2[AllData$ECBDec == FALSE] <- NA

# Determine sign and scale of of shocks because not identified in factor model
sign1 <- sign(cor(AllData$ShocksCH1, AllData$Surprise, use = "pairwise.complete.obs"))
scale1 <- sqrt(var(AllData$Surprise, na.rm = TRUE))
AllData$ShocksCH1 <- as.numeric(scale1)*as.numeric(sign1)*AllData$ShocksCH1
sign2 <- sign(cor(AllData$ShocksCH2, AllData$Surprise, use = "pairwise.complete.obs"))
AllData$ShocksCH2 <- as.numeric(scale1)*as.numeric(sign2)*AllData$ShocksCH2
AllData$ShocksCH <- 0.5*(AllData$ShocksCH1+AllData$ShocksCH2) 

sign1<- sign(cor(AllData$ShocksEUR1, AllData$ShockECB3M, use = "pairwise.complete.obs"))
scale1 <- sqrt(var(AllData$ShockECB3M, na.rm = TRUE))
AllData$ShocksEUR1 <- as.numeric(scale1)*as.numeric(sign1)*AllData$ShocksEUR1
sign2<- sign(cor(AllData$ShocksEUR2, AllData$ShockECB3M, use = "pairwise.complete.obs"))
AllData$ShocksEUR2 <- as.numeric(scale1)*as.numeric(sign2)*AllData$ShocksEUR2
AllData$ShocksEUR <- 0.5*(AllData$ShocksEUR1+AllData$ShocksEUR2) 

# Datei für Tagesmodell speichern
save(AllData, file = "../Daten/TagesdatenShocks.RData")

# Datei für Quartalsmodell speichern

ExpData <- xts(AllData[, c("EID10Y", "NEURR", "EURUSD", "ESTXXClose", "SPI", "LIB1W", "LIBEUR1W", "LIB3M", "LIB1W", "LIBEUR3M")], order.by = AllData$Date)
ExpData <- ts_frequency(ExpData, to = "quarter", aggregate = "last", na.rm = TRUE)

ExpData2 <- xts(AllData[, c("ShocksCH", "ShocksCH1", "ShocksCH2", "ShocksEUR1", "ShocksEUR2", "ShocksLib", "Surprise")], order.by = AllData$Date)
ExpData2 <- ts_frequency(ExpData2, to = "quarter", aggregate = "sum", na.rm = TRUE)

Schocks <- ts_c(ExpData, ExpData2)

# Datei speichern
save(Schocks, file = "../Daten/QuartalsdatenShocks.RData")

