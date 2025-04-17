lineplot.ci4 <- function(data, 
                         dv, 
                         iv, 
                         id = 1, 
                         group = NULL, 
                         x = 1, 
                         off = 0,        # offset for sub-groups within the same call
                         shiftX = 0,     # <--- NEW ARG: entire-call horizontal shift
                         Bakeman = TRUE, 
                         upper = TRUE, 
                         lower = TRUE, 
                         ylim = c(0,1), 
                         xlim = NULL,
                         pt = 15:25, 
                         lty = rep(1,11), 
                         col = rep("black", 11), 
                         ptcol = rep("white", 11),
                         na.rm = TRUE, 
                         cex = 1,
                         xticks = NULL,  
                         add = FALSE,      
                         alpha = 1,        
                         main = NULL, 
                         xlab = NULL, 
                         ylab = NULL, 
                         ...
) 
{
  # We need Hmisc for errbar()
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package 'Hmisc' needed for this function to work. Please install it.")
  }
  
  # Convert data to data.frame if it’s a tibble
  data <- as.data.frame(data)
  
  # If user passes numeric column indices, convert them to names
  if (is.character(iv) & is.numeric(id)) {
    id <- names(data)[id] 
  }
  
  #### (1) Bakeman & McArthur correction if requested ####
  idvar <- data[, id]
  subjMeans <- aggregate(x = data[, dv], by = list(data[, id]), FUN = mean, na.rm = na.rm)
  names(subjMeans) <- c(id, dv)
  ids <- unique(idvar)
  
  if (Bakeman) {
    if (!is.null(group)) {
      groupMeans <- aggregate(x = data[, dv], by = list(data[, group]), FUN = mean, na.rm = na.rm)
      grandMean <- groupMeans[,"x"]
      groupVar <- aggregate(x = data[, group], by = list(data[, id]), FUN = mean)
      names(groupVar)[1:2] <- c("ids", "grouplevel")
      groupVar$groupidx <- 0
      glevels <- sort(unique(groupVar$grouplevel))
      for (g in seq_along(glevels)) {
        groupVar[groupVar$grouplevel == glevels[g], "groupidx"] <- g  
      }
    } else {
      # single grand mean across all participants
      grandMean <- mean(subjMeans[, dv], na.rm = na.rm)
      groupVar <- data.frame(ids = ids, groupidx = 1)
    }
    
    # subtract each participant’s mean, add back appropriate group-level grand mean
    corrdata <- data
    for (ii in seq_along(ids)) {
      pMean <- subjMeans[subjMeans[, id] == ids[ii], dv]
      gidx <- groupVar[groupVar$ids == ids[ii], "groupidx"]
      corrdata[data[, id] == ids[ii], dv] <- 
        corrdata[data[, id] == ids[ii], dv] - pMean + grandMean[gidx]
    }
  } else {
    corrdata <- data
  }
  
  #### (2) Aggregate by subject & IV(s) => condition means ####
  dimensions <- length(iv)
  aggdata <- aggregate(x = corrdata[, dv], by = corrdata[, c(id, iv)], FUN = mean, na.rm = na.rm)
  
  #### (3) Then average across subjects => group-level means, SD, and CI ####
  if (dimensions == 1) {
    byVar <- list(aggdata[, 2])
  } else {
    byVar <- aggdata[, 2:(1+dimensions)]
  }
  
  Mdata <- aggregate(x = aggdata[,"x"], by = byVar, FUN = mean, na.rm = na.rm)
  SD    <- aggregate(x = aggdata[,"x"], by = byVar, FUN = sd,   na.rm = na.rm)
  Nsubj <- aggregate(x = aggdata[, id], by = byVar, FUN = function(x){length(unique(x))})
  Mdata$ci <- 1.96 * SD[,"x"] / sqrt(Nsubj[,"x"])
  
  # rename columns if needed
  if (dimensions == 1) {
    names(Mdata)[1] <- iv
    names(SD)[1]    <- iv
  }
  
  #### (4) Prepare the x-axis factor or numeric ####
  if (!is.numeric(Mdata[, iv[x]])) {
    Mdata[, iv[x]] <- as.factor(Mdata[, iv[x]])
  }
  xaxis <- sort(unique(as.numeric(Mdata[, iv[x]])))
  
  if (is.null(xlim)) {
    if (length(xaxis) > 1) {
      step <- xaxis[2] - xaxis[1]
      xlim <- c(xaxis[1] - 0.5*step, xaxis[length(xaxis)] + 0.5*step)
    } else {
      xlim <- c(xaxis[1] - 1, xaxis[1] + 1)
    }
  }
  
  xaxt <- "s"
  if (is.factor(Mdata[, iv[x]])) {
    xticks <- levels(Mdata[, iv[x]])
    xaxt <- "n"
  }
  
  #### (5) If we have 2 IVs, define how many lines we’ll loop over ####
  if (dimensions == 1) {
    loop <- 1
  } else {
    otherFactor <- iv[ setdiff(1:2, x) ]
    if (!is.numeric(Mdata[, otherFactor])) {
      Mdata[, otherFactor] <- as.factor(Mdata[, otherFactor])
    }
    iv2vals <- sort(unique(as.numeric(Mdata[, otherFactor])))
    loop <- length(iv2vals)
  }
  
  # expand col/ptcol/lty if needed
  if (length(pt)    < loop) pt    <- rep(pt,    length.out = loop)
  if (length(ptcol) < loop) ptcol <- rep(ptcol, length.out = loop)
  if (length(col)   < loop) col   <- rep(col,   length.out = loop)
  if (length(lty)   < loop) lty   <- rep(lty,   length.out = loop)
  
  #### (6) Optionally adjust transparency via alpha ####
  if (alpha < 1 || alpha > 1) {
    # clamp alpha to (0,1]
    alpha <- max(0.01, min(alpha, 1))
    col   <- sapply(col,   function(cc) grDevices::adjustcolor(cc, alpha.f = alpha))
    ptcol <- sapply(ptcol, function(cc) grDevices::adjustcolor(cc, alpha.f = alpha))
  }
  
  #### (7) Helper function to do the actual drawing ####
  drawOne <- function(xx, y, dci, index) {
    yHigh <- y + dci
    yLow  <- y - dci
    
    if (!add) {
      # Full code path as in the original
      if (index == 1) {
        # first group triggers plot() call
        plot(xx, y, 
             xlim = xlim, ylim = ylim, 
             type = "b", 
             lty = lty[index], 
             pch = pt[index], 
             col = col[index], 
             bg  = ptcol[index], 
             xaxt = xaxt, 
             cex = cex,
             main = if (!is.null(main)) main else "",
             xlab = if (!is.null(xlab)) xlab else "",
             ylab = if (!is.null(ylab)) ylab else "",
             ...)
        
        Hmisc::errbar(xx, y, yplus = yHigh, yminus = yLow, 
                      add = TRUE, type = "n", 
                      errbar.col = col[index], 
                      ...)
        
        if (xaxt == "n") axis(side=1, at = xaxis, labels = xticks)
        
      } else {
        # subsequent lines => par(new=TRUE)
        par(new = TRUE)
        plot(xx, y, 
             xlim = xlim, ylim = ylim,
             type = "b", 
             lty = lty[index], 
             pch = pt[index], 
             col = col[index], 
             bg  = ptcol[index], 
             xaxt = xaxt, 
             cex = cex,
             axes = FALSE, 
             xlab = "", ylab = "", main = "",
             ...)
        
        Hmisc::errbar(xx, y, yplus = yHigh, yminus = yLow,
                      add = TRUE, type = "n",
                      errbar.col = col[index],
                      ...)
      }
      
    } else {
      # add=TRUE => just overlay lines and error bars
      lines(xx, y, 
            lty = lty[index], 
            pch = pt[index], 
            col = col[index], 
            type= "b",
            bg  = ptcol[index],
            cex = cex,
            ...)
      
      Hmisc::errbar(xx, y, yplus = yHigh, yminus = yLow,
                    add = TRUE, type = "n",
                    errbar.col = col[index], 
                    ...)
    }
  }
  
  #### (8) Main loop to draw lines for each sub-group ####
  for (j in seq_len(loop)) {
    if (dimensions == 2) {
      otherFactor <- iv[ setdiff(1:2, x) ]
      sub <- Mdata[ as.numeric(Mdata[, otherFactor]) == iv2vals[j], ]
      
      # If empty, skip
      if (nrow(sub) == 0) {
        next
      }
      
      xx  <- as.numeric(sub[, iv[x]])
      y   <- sub[,"x"]
      dci <- sub[,"ci"]
    }
    
    # original within-call offset
    xx <- xx + (j-1)*off
    # now apply the global shift
    xx <- xx + shiftX
    
    drawOne(xx = xx, y = y, dci = dci, index = j)
  }
}
