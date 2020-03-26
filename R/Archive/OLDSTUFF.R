
# ------------------------------------------------------------------------
# Irreguläre SNB Operationen
# ------------------------------------------------------------------------
IrrData <- read.xlsx("../Daten/SNBMonOpsD.xlsx", sheetName = "Data", startRow = 21)
RRepo <- subset(IrrData, IrrData$Art == "Reverse Repo")
RRepo <- data.frame(RRepo$Date, 1)
colnames(RRepo) <- c("Date", "RRepo")
RRepo <- RRepo[order(RRepo$Date),]

SNBBill <- subset(IrrData, IrrData$Art == "SNB Bills")
SNBBill <- data.frame(SNBBill$Date, 1)
colnames(SNBBill) <- c("Date", "SNBBill")
SNBBill <- SNBBill[order(SNBBill$Date),]

Swaps <- subset(IrrData, IrrData$Art == "Swaps")
Swaps <- data.frame(Swaps$Date, 1)
colnames(Swaps) <- c("Date", "Swaps")
Swaps <- Swaps[order(Swaps$Date),]

# ------------------------------------------------------------------------
# GMBF Auktionen (immer Dienstag)
# ------------------------------------------------------------------------
myDate <- seq(as.Date("1999-01-01"), as.Date("2019-12-31"), "days")
GMBF <- data.frame(myDate, rep(0, length(myDate)))
colnames(GMBF) <- c("Date", "GMBF")

# Create Dummy for GMBF (always Tuesday)
isTue <- weekdays(GMBF$Date) == "Tuesday"
GMBF[, "GMBF"] <- 0
GMBF[isTue, "GMBF"] <- 1
GMBF <- GMBF[order(GMBF$Date),]


DAX <- read.xlsx("../Daten/StockDataDAXD.xlsx", sheetName = "Data", startRow = 1)
DAX$Date <- as.Date(DAX$Date)
DAX <- DAX[order(DAX$Date),]


# ------------------------------------------------------------------------
# Obligationen
# ------------------------------------------------------------------------
# Don't need those because I don't know when recorderd
#Bonds <- read.xlsx("../Daten/BondsD.xlsx", sheetName = "Data", startRow = 17)
#Bonds$Date <- as.Date(Bonds$Date)
#Bonds <- Bonds[order(Bonds$Date),]



# --------------------------------------------------------------
# Reaktion der Obligationen
# --------------------------------------------------------------
MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "EID1Y"
depLog  <- FALSE
Reg.SNB.eid1 <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "EID5Y"
depLog  <- FALSE
Reg.SNB.eid5 <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)



MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "EID10Y"
depLog  <- FALSE
Reg.SNB.eid10 <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "RPBANK"
depLog  <- FALSE
Reg.SNB.rpb <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "RPIND"
depLog  <- FALSE
Reg.SNB.rpi <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LB"        # LB or ECBDec
e       <- "ShocksCH" # ShocksLib or ShockECB3M
Y       <- "FORAAA8Y"
depLog  <- FALSE
Reg.SNB.pie <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)



# --------------------------------------------------------------
# Reaktion auf Placebo Shock
# --------------------------------------------------------------
MainEv  <- "LBPlacebo"        # LB or ECBDec
e       <- "ShocksCHPlacebo" # ShocksLib or ShockECB3M
Y       <- "NEURR"
depLog  <- TRUE
Reg.SNB.xrp <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)

MainEv  <- "LBPlacebo"        # LB or ECBDec
e       <- "ShocksCHPlacebo" # ShocksLib or ShockECB3M
Y       <- "LIB3M"
depLog  <- FALSE
Reg.SNB.irp <- EstimLP(MainEv, e, Y, Yc, Ec, H, P, depLog, conLog, lagShocks, depLag,Data, myStart, myEnd)
