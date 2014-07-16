# Funktion completeForestPlot von A. Trakowski
# (eine Erweiterung von forest() aus dem metafor-Paket
#  in Anlehnung an Kambeitz, Romanos & Ettinger (2014))

completeForestPlot <- function(dataDF, rmaResult, 
                               xTicks = seq(-4, 4, 2), 
                               xlab = "Gesamt-Effektstärke", 
                               mlab = "Modell-Zusammenfassung",
                               ayWidth = 4, ilabWidth = 1, 
                               colMargin = 0.5,
                               defaultFontSize = 1) {
  
  # extra für Plot formatierte Daten
  ay = paste(dataDF$Autor, dataDF$Jahr, sep = ", ")
  ilabs = round(dataDF[,c("yi", "ci.lb", "ci.ub", "zi", "pi")], 4)
  
  # Positionen für zusätzliche Daten
  ayXPos = min(xTicks) - ncol(ilabs)*ilabWidth - (ncol(ilabs)+1)*colMargin - 1/2*(ayWidth)
  ilabXPos = (ayXPos + 1/2*(ayWidth)) + colMargin + 1/2*(ilabWidth) + 
    (1:ncol(ilabs)-1)*(ilabWidth+colMargin)
  
  # xlim insgesamt
  xLimits = c(ayXPos - 1/2*(ayWidth), max(xTicks)) 
  
  # weitere Schriftgröße
  smallFontSize = 2/3*defaultFontSize
  
  # Abstand zwischen Zeilen
  rowMargin = defaultFontSize
  
  # Plot selbst
  forest(rmaResult, slab = ay, xlim = xLimits, at = xTicks, 
         xlab = xlab, mlab = mlab, annotate = FALSE, addfit = TRUE, 
         ilab = ilabs, ilab.xpos = ilabXPos, cex = defaultFontSize, 
         showweight = FALSE)
  
  # Spaltenüberschriften hinzufügen
  text(x = min(xLimits), y = nrow(dataDF)+rowMargin+defaultFontSize, labels="Autor und Jahr",
       font = 2, pos = 4, cex = defaultFontSize)
  text(x = ilabXPos, 
       y = nrow(dataDF)+rowMargin+rep(c(defaultFontSize, smallFontSize, defaultFontSize), 
                                      c(1, 2, 2)),
       labels=c("D", "UG", "OG", "Z-Wert", "P-Wert"),
       cex=rep(c(defaultFontSize, smallFontSize, defaultFontSize), c(1, 2, 2)),
       font=2)
  text(x = mean(ilabXPos[2:3]), 
       y = nrow(dataDF)+2*rowMargin+smallFontSize, 
       labels="95%-KI", cex = defaultFontSize, font = 2)
  text (x = 0, y = nrow(d)+rowMargin+defaultFontSize, 
        labels = "9er vs. 10/10", 
        cex = defaultFontSize, font = 2)
  
  # Modell-Zusammenfassung hinzufügen
  text(x = ilabXPos, y = -rowMargin,
       labels = round(unlist(rmaResult[c("b", "ci.lb", "ci.ub", "zval", "pval")]), 4),
       cex = defaultFontSize, font = 1)
  
}