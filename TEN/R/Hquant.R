Hquant = function(x, lev.q=15){
  xmin = min(x)
  xmax = max(x)
  qstep = (xmax - xmin)/ lev.q
  quantINT = seq(from = xmin, to = xmax, by = qstep)
  xquant = rep(0, length(x))
  for (i in 1:length(x)){
    stop = 0
    qlevel = 2
    while (stop == 0){
      if(x[i] <= quantINT[qlevel]) {
        xquant[i] = qlevel -1
        stop = 1} 
        else {
          qlevel = qlevel +1
        }
    }
  }
  
# change into return(xquant)
# used fo consistency with quantentr
  return(xquant-1)
}
