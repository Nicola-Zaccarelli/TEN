THrank = function(Ts1, Ts2, l1, l2, tlag1, tlag2, lev.q=15){
#
# Ts1 = c(1, 10, 15, 8, 25, 26, 17, 9, 13, 19, 1, 10, 15, 8, 25, 26, 17, 9, 13, 19, 1, 10, 15, 8, 25, 26, 17, 9, 13, 19)
# Ts2 = c(2, 5, 8, 4, 1, 4, 2, 5, 5, 2, 2, 5, 4, 8, 4, 1, 2, 5, 5, 2, 2, 2, 3, 4, 5, 6, 2, 1, 5, 4)
# l1 = 4
# l2 = 4
# tlag1 = 2
# tlag2 = 2
# lev.q = 7
#
# ordinal sampling
totLength = length(Ts1)
tmp = sort(Ts1, index.return=T)
Ts1[tmp$ix]=c(1:totLength)
tmp = sort(Ts2, index.return=T)
Ts2[tmp$ix]=c(1:totLength)

# qunatize ts1 and ts2
QTs1  = Hquant(Ts1, lev.q)
QTs12 = c(QTs1[2:length(QTs1)], NA)
QTs2  = Hquant(Ts2, lev.q)

# populate Xpat, Ypat, Yt
codeTs1 = (lev.q^(seq(from=(l1 -1), to= 0, by=-1))) #check for orientation
codeTs2 = (lev.q^(seq(from=(l2 -1), to= 0, by=-1)))

Xpat = c()
Ypat = c()
Yt   = c()

for (i in max(c(l1, tlag1)):min(c(length(QTs1),length(QTs2)))){
  Xpat=c(Xpat, codeTs1[(i-l1-tlag1+1):(i-tlag1)]*QTs1)
  Ypat=c(Ypat, codeTss[(i-l2-tlag2+1):(i-tlag2)]*QTs2)
  Yt = c(Yt, codeTs2[i])
}

# calculate the value of trH
C3C1C2 = as.data.frame(table(QTs12, QTs1, QTs2))
C1     = as.matrix(table(QTs1))
C1C2   = as.matrix(table(QTs1, QTs2))
C3C1   = as.matrix(table(QTs12, QTs1))

# Transform into frequencies
C3C1C2$Freq = C3C1C2$Freq / sum(C3C1C2$Freq)
sumINDEX = length(C3C1C2$Freq)
C1 = C1/sum(C1)
C1C2 = C1C2/sum(C1C2)
C3C1 = C3C1/sum(C3C1)

trH = c()

for (i in 1:sumINDEX){
  trH = c(trH, (C3C1C2[i, 4]*C1[C3C1C2[i, 2], 1])/(C1C2[C3C1C2[i, 2], C3C1C2[i, 3]]*C3C1[C3C1C2[i, 1], C3C1C2[i, 2]]))
}

trH[trH==0]=1
trH[is.nan(trH)]=1
trH = sum(t(C3C1C2[, 4])*log10(trH))

return(trH)
}
