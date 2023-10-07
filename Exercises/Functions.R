myfunction <- function(x){return(x+3)}
secfunction <- function(x=10){return(x+3)}

myfunction(10)
secfunction()


source("./") Runs r script from anywhere

#function for hyperion data

clean_the_bird_data("../Data/Bird_Measurements.csv")


if(inmem) {
  v <- as.data.table(as.data.frame(x, row.names=row.names, optional=optional, xy=xy, ...))
  coln <- names(x)
  if(xy) coln <- c("x", "y", coln)
  setnames(v, coln)
} else {
  tr <- blockSize(x)
  l <- lapply(1:tr$n, function(i) {
    DT <- as.data.table(as.data.frame(getValues(x, row = tr$row[i], nrows = tr$nrows[i]), ...))  
    if(xy == TRUE) {
      cells <- cellFromRowCol(x, c(tr$row[i], tr$row[i] + tr$nrows[i] - 1), c(1, ncol(x)))
      coords <- xyFromCell(x, cell = cells[1]:cells[2])
      DT[, c("x", "y") := data.frame(xyFromCell(x, cell = cells[1]:cells[2]))]
    } 
    DT
  })
  v <- rbindlist(l)
  coln <- names(x)
  if(xy) {
    coln <- c("x", "y", coln)
    setcolorder(v, coln)
  }
}


as.data.table.raster <- function(x, row.names = NULL, optional = FALSE, xy=FALSE, inmem = canProcessInMemory(x, 2), ...) {
  stopifnot(require("data.table"))
  if(inmem) {
    v <- as.data.table(as.data.frame(x, row.names=row.names, optional=optional, xy=xy, ...))
  } else {
    tr <- blockSize(x, n=2)
    l <- lapply(1:tr$n, function(i) 
      as.data.table(as.data.frame(getValues(x, 
                                            row=tr$row[i], 
                                            nrows=tr$nrows[i]), 
                                  row.names=row.names, optional=optional, xy=xy, ...)))
    v <- rbindlist(l)
  }
  coln <- names(x)
  if(xy) coln <- c("x", "y", coln)
  setnames(v, coln)
  v
}

