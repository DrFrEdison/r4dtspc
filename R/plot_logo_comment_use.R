dtplot <- function(filename
                   , Kommentar
                   , Bearbeiter = "Markus Kurtz"
                   , matrixvector = c(1,2)
                   , nrow = 2
                   , heights = c(.1, .4)
                   , x1 = 1
                   , x2 = 1
                   , x3 = 1){
  graphics.off()
  png(pngname <- paste0(date.dt(),"_", filename, ".png")
      ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"Eurostile Next LT Pro",T,"cairo")
  
  layout( matrix( matrixvector, nrow = nrow, byrow = T), heights = heights <- heights)
  par( col.main = dauschblue, col.axis = dauschblue, col.lab = dauschblue, cex.axis = .65
       , mar = c(3,4, 3, 1))
  
  par(mar = c(0,0,0,0))
  plot(.5,.5,type = "n", xlab = "", ylab = "", axes = F, ylim = c(0, 1), xlim = c(0, 1))
  segments(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[3], xpd = T, col = dauschblue)
  dauschlogo.draw( y = 1 - (heights[1] / sum( heights ) / 2) )
  
  text(x1 <- .03 * x1, y1 <- .9
       , paste0( "Dateiname = ", pngname, "\n"
                 , "Datum = ", Sys.Date(), "\n"
                 , "Bearbeiter = ", Bearbeiter)
       , col = dauschblue, adj = c(0, 1)
       , cex = .8)
  
  text(.325 * x2, y1
       , "Kommentar:"
       , col = dauschblue, adj = c(0, 1)
       , cex = .8)
  text(.38 * x3, y1
       , comment_break(Kommentar)
       , col = dauschblue, adj = c(0, 1)
       , cex = ifelse(length( unlist(gregexpr("\n", comment_break(Kommentar))) ) > 4, .65, .8))
  
  
}

