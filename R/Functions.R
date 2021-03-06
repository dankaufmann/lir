# ------------------------------------------------------------------------
# Wie weiter mit der Tiefzinspolitik? Szenarien und Alternativen
# - Functions.R
# ------------------------------------------------------------------------
# Bemerkungen: Verschiedene Funktionen die in den Hauptdateien verwendet 
#              werden.
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------

getForecastVariance <- function(fcst){
  # Function to extract forecast error variance from a forecast object
  # CI lower = y(t+h|t)-1.96*sig(h)
  # Therefore sig(h)^2 = [CI lower - y(t+h|t))/(-1.96)]^2
  # Get exact percentile (1.96 yield basically the same)
  
  z957 = qnorm(0.975, 0, 1)
  sigh2 = ((fcst$lower[,"95%"]-fcst$mean)/(-z957))^2
  return(sigh2)
}

normalize <- function(x){
  # Function to normalize a matrix to mean zero and standard deviation 1
  x_norm = (x-mean(x, na.rm =TRUE))/sd(x, na.rm =TRUE)
  return(x_norm)
}

ts_cumsum <- function(x){
  # Function to calculate cumulative sums of a time series
  return(ts_(cumsum)(x))
}

ts_cumprod <- function(x){
  # Function to calculate cumulative product of a time series
  return(ts_(cumprod)(x))
}

compLags <- function(x, lags){
  # compute lags of a variable and return a data frame
  df <- array(data = NA, dim = c(length(x), lags))
  for (p in 1:lags){
    df[, p] <- lag(x, p)
  }
  return(as.data.frame(df))
}

createLPPlot <- function(Resp, NormResp, NormVal, NormInit, H, gap, myYlim, title, xtitle, filename, figWidth, figHeight) {
  
  # Compute impulse responses from local projections
  Resp.plot <- matrix(unlist(Resp[[1]][1:(H+1)]), ncol = H+1)
  Var.plot  <- matrix(unlist(Resp[[2]][1:(H+1)]), ncol = H+1)
  Resp.plot <- Resp.plot[2,]
  Var.plot  <- Var.plot[2,]
  
  if(is.null(NormResp)){
    Resp.norm <- t(matrix(rep(1, H+1), ncol = H+1))
    Var.norm  <-  t(matrix(rep(1, H+1), ncol = H+1))
  }else{
    Resp.norm <- matrix(unlist(NormResp[[1]][1:(H+1)]), ncol = H+1)
    Var.norm  <- matrix(unlist(NormResp[[2]][1:(H+1)]), ncol = H+1)
    
    Resp.norm <- Resp.norm[2,]
    Var.norm  <- Var.norm[2,]
    
    if(NormInit == TRUE){
      Resp.norm <- t(matrix(rep(Resp.norm[1], H+1), ncol = H+1))
      Var.norm  <-  t(matrix(rep(Var.norm[1], H+1), ncol = H+1))
    }
  }

  # Normalize to +0.75Pp
  Resp.plot <- Resp.plot/Resp.norm*NormVal
  Var.plot <- Var.plot/(Resp.norm^2)*NormVal^2

  # Exchange rate: Compute impulse responses
  Plot.plot <- data.frame(0:H, Resp.plot, Resp.plot+1.96*sqrt(Var.plot), Resp.plot-1.96*sqrt(Var.plot), Resp.plot+1.64*sqrt(Var.plot), Resp.plot-1.64*sqrt(Var.plot))
  colnames(Plot.plot) <- c("Tage", "Mittelwert", "Konfidenzintervall1", "Konfidenzintervall2", "Konfidenzintervall1b", "Konfidenzintervall2b")
  
  # Make a plot
  g <- ggplot(Plot.plot, aes(Tage))
  g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2" , ymax = "Konfidenzintervall1"), fill= "gray25", alpha=3/10)
  g <- g + geom_ribbon(aes_string(ymin = "Konfidenzintervall2b" , ymax = "Konfidenzintervall1b"), fill= "gray25", alpha=5/10)
  g <- g + theme_minimal() + xlab("") + ggtitle(title)+theme(plot.title = element_text(size = 10))+ylab("")+
    theme(legend.title=element_blank()) + scale_x_continuous(breaks=seq(0, H, gap))
  g <- g + expand_limits(y=myYlim)
  g <- g + geom_hline(yintercept = 0, size=0.5, linetype = "solid") + theme(legend.position = "none")
  g <- g + geom_line(aes_string(y = "Mittelwert"), size = 1)+xlab(xtitle)+theme(plot.title = element_text(size = 13))
  g <- g+ theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
    theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
    theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
  g
  ggsave(filename = paste(filename, ".pdf", sep = ""), width = figWidth, height = figHeight)
  ggsave(filename = paste(filename, ".png", sep = ""), width = figWidth, height = figHeight)
  
}

EstimLP <- function(MainEv, e, Y, Yc, Zc, Ec, H, P, depLog, conLog, exoLog, lagShocks, depLag, Data, myStart, myEnd){

  # Estimates local projection with HAC robust standard errors
  ResultsReg <- list()
  ResultsVar <- list()
  ResultsCov <- list()
  ResultsObs <- list()
  for(h in 0:H){
    
    # Set up the dependent variable
    RegFormula <- "depVar~"
    if(depLog == TRUE){
      depVar <- (dplyr::lead(log(Data[, Y]), h)-dplyr::lag(log(Data[, Y]), 1))*100
    }else{
      depVar <- dplyr::lead(Data[, Y], h)-dplyr::lag(Data[, Y], 1)
    }
    
    # Set up the Shocks
    if(lagShocks == TRUE){
      Shocks <- data.frame(Data[, e], dplyr::lag(Data[, e], 1))
      colnames(Shocks) <- c("Shocks", "Shocks.l1")
      RegFormula <- paste(RegFormula, "Shocks+Shocks.l1", sep = "")
      
    }else{
      Shocks <- Data[, e]
      RegFormula <- paste(RegFormula, "Shocks", sep = "")
      
    }
    
    # Set up the equilibrium relationships
    Equilib <- data.frame(Data$Date)
    if(Ec[1] != ""){
      for(i in 1:length(Ec)){
        #for(h2 in 0:h){
        #  if (h2 == 0){
        temp <- dplyr::lag(Data[, Ec[i]], 1)
        
        #  }else{
        #  temp <- temp+lead(lag(Data[, Ec[i]], 1), h2)
        #}
        Equilib[, Ec[i]] <- temp
        RegFormula <- paste(RegFormula, Ec[i], sep = "+")
        #}
      }
    }
    
    # Set up the controls
    Controls <- data.frame(Data$Date)
    if(P>0 & Yc[1] != ""){
      for(i in 1:length(Yc)){
        for(p in 1:P){
          if(conLog[i] == TRUE){
            Controls[, paste(Yc[i], ".l", p, sep = "")] <- (dplyr::lag(log(Data[, Yc[i]]), p)-dplyr::lag(log(Data[, Yc[i]]), p+1))*100
          }else{
            Controls[, paste(Yc[i], ".l", p, sep = "")] <- dplyr::lag(Data[, Yc[i]], p)-dplyr::lag(Data[, Yc[i]], p+1)  
          }
          
          RegFormula <- paste(RegFormula, paste(Yc[i], ".l", p, sep = ""), sep = "+")
        }
      }
    }
    
    # Set up the controls
    Exogenous <- data.frame(Data$Date)
    if(Zc[1] != ""){
      for(i in 1:length(Zc)){
        for(p in 0:P){
          if(exoLog[i] == TRUE){
            Exogenous[, paste(Zc[i], ".l", p, sep = "")] <- (dplyr::lag(log(Data[, Zc[i]]), p)-dplyr::lag(log(Data[, Zc[i]]), p+1))*100
          }else{
            Exogenous[, paste(Zc[i], ".l", p, sep = "")] <- dplyr::lag(Data[, Zc[i]], p)-dplyr::lag(Data[, Zc[i]], p+1)  
          }
          
          RegFormula <- paste(RegFormula, paste(Zc[i], ".l", p, sep = ""), sep = "+")
        }
      }
    }
    
    if(depLag == TRUE){
      # Repeat for dependent variable
      LagDepvar <- data.frame(Data$Date)
     
        for(p in 1:1){
          if(depLog == TRUE){
            LagDepvar[, paste("Y.l", p, sep = "")] <- (dplyr::lag(log(Data[, Y]), p)-dplyr::lag(log(Data[, Y]), p+1))*100
          }else{
            LagDepvar[, paste("Y.l", p, sep = "")] <- dplyr::lag(Data[, Y], p)-dplyr::lag(Data[, Y], p+1)  
          }
          
          RegFormula <- paste(RegFormula, paste("Y.l", p, sep = ""), sep = "+")
        }

    }
  
    # Collect all in RegData
    if(depLag == TRUE){
      RegData <- data.frame(depVar, Shocks, Equilib, Controls, Exogenous, LagDepvar)
    }
    else{
      RegData <- data.frame(depVar, Shocks, Equilib, Controls, Exogenous)
      
    }
    
    # Remove all observations where shocks are zero or missing
    inSample <- Data$Date>= myStart & Data$Date<=myEnd & Data[, MainEv] != 0
    RegData <- RegData[inSample,]
    
    # Do local projection regression
    LPReg <- lm(RegFormula, data = RegData)
    
    # HAC robust standard errors
    ResultsReg[[h+1]] <- LPReg$coefficients
    ResultsVar[[h+1]] <- diag(NeweyWest(LPReg))
    ResultsCov[[h+1]] <- coeftest(LPReg, vcov. = NeweyWest(LPReg))
    ResultsObs[[h+1]] <- nobs(LPReg)
    
    
  }
  
  # Return results
  return(list(ResultsReg, ResultsVar, ResultsCov, ResultsObs))
}

calcUIPModel <- function(Scen, fixIr, pibars, phi, rho){

  # Calculate simulation of theoretical model
  Scen$e     <- NA
  Scen$ehat  <- NA
  Scen$ebar  <- NA
  Scen$i     <- NA
  Scen$il    <- NA
  if(sum(fixIr) == 0){
    Scen$d     <- Scen$Shocks
  }else{
    Scen$d     <- 0
  }
  
  # Start with last observation
  myTime <- index(Scen)
  for(i in length(myTime):1){
    
    t <- myTime[i]
    
    # Compute equilibrium exchange rate
    eBar <- xts(cumsum(Scen$pibar-pibars), order.by = index(Scen))
    
    # Compute weights
    inSum <- xts((index(Scen)>=t), order.by = index(Scen))
    wPhi  <- inSum*xts(1/(1+phi)^(cumsum(inSum)), order.by = index(Scen))
    inLR  <- xts((year(index(Scen))>=year(t) & year(index(Scen))<=year(t)+lr), order.by = index(Scen))
    
    # Compute shocks so that interest rate is fixed
    if(any(i == fixIr)){
      
      # What should be the interest rate?
      targetI <- Scen$SRCH[t]
      
      # What is the interest rate without a current shock?
      tempI   <- iss+Scen$pibar[as.Date(t)]-pibars-rho+phi*sum(wPhi*(Scen[,"SRGE"]-iss-Scen[,"d"]))+phi*(wPhi*Scen[,"d"])[as.Date(t)]+0
      
      # Compute how large the shock has to be  
      Scen$d[i] <- (1+phi)*(targetI - tempI)
      
    }
    # Otherwise, set to Shock
    else{
      Scen$d[i] <- Scen$Shocks[i]
    }
    
    Scen[as.Date(t),"ebar"] <- eBar[as.Date(t),]
    Scen[as.Date(t),"e"] <- sum(wPhi*(Scen[,"SRGE"]-iss-Scen[,"d"]))+eBar[as.Date(t),]
    Scen[as.Date(t),"i"] <- iss+Scen$pibar[as.Date(t)]-pibars-rho+phi*sum(wPhi*(Scen[,"SRGE"]-iss-Scen[,"d"]))+Scen[as.Date(t),"d"] 
    Scen[as.Date(t),"ehat"] <- sum(wPhi*(Scen[,"SRGE"]-iss-Scen[,"d"]))
    Scen[as.Date(t),"il"] <- mean(inLR*(Scen$i), na.rm =TRUE) 
    
  }
  return(Scen)
}
