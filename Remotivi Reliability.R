

####################################################################
## CALCULATE RELIABILITY
####################################################################


### Identify double-coded articles
double.coded = data.frame(matrix(NA,nrow=1, ncol=4))
colnames(double.coded) = c("coder1", "coder2", "id", "reliability")

for (i in 1:(num.coders-1)){
  for (j in (i+1):num.coders) {
    
    overlap = which(dat[[i]][,"id"] %in% dat[[j]][,"id"])
    
    if (length(overlap)>0) {
      double.coded = rbind(double.coded, data.frame(coder1=i, coder2=j, id=dat[[i]][overlap,"id"], reliability=NA))
    }
  }
}

double.coded = double.coded[-1,]


### Define col.reliability (adding score from multiple entries, delete multiple entries)
col.reliability = c("formal.konteks", "standar.sumber", "standar.berimbang", "standar.privasi", "standar.perkosaan", "standar.clickbait",
                    "jumlah.narsum", "score.inklusifitas.narsum", "score.inklusifitas.tone", "inklusifitas.marking", "agenda.isu")

### create matrix to store responses to all double-coded articles
mat.double.coded = data.frame(matrix(NA, nrow=1, ncol=2+length(col.reliability)))
colnames(mat.double.coded) = c("id", "coder", col.reliability)


### Calculate Kripp Alpha and Average across Combinations
options(warn = 2)
for (i in 1:nrow(double.coded)) {

  mat = data.frame(matrix(NA, nrow=2, ncol=length(col.reliability)))
  colnames(mat) = col.reliability
  
  coder1 = double.coded$coder1[i]                                                 ## get coder 1 from the double-coded list
  coder2 = double.coded$coder2[i]                                                 ## get coder 2 from the double-coded list  
  
  
  index1 = which(dat[[coder1]][,"id"] == double.coded$id[i])
  index2 = which(dat[[coder2]][,"id"] == double.coded$id[i])
  
  
  mat[1,] = dat[[coder1]][index1, col.reliability]                                   ## add coder 1 responses to the matrix
  mat.double.coded = rbind(mat.double.coded,
                           cbind(id = dat[[coder1]][index1, "id"],
                                 coder = coder1,
                                 dat[[coder1]][index1, col.reliability])
                          )
  
  # special case for "i-88-ti" because there are two i-88-ti for coder 5, one from January and one from November
  # if (double.coded$id[i] == "i-88-ti") index2 = 101
  
  mat[2,] = dat[[coder2]][index2, col.reliability]                                   ## add coder 2 responses to the matrix
  mat.double.coded = rbind(mat.double.coded,
                           cbind(id = dat[[coder2]][index2, "id"],
                                 coder = coder2,
                                 dat[[coder2]][index2, col.reliability])
                          )
  
  double.coded$reliability[i] = kripp.alpha(as.matrix(mat), method="interval")$value
}
options(warn = 0)


### Output Double Coded Articles and Their Reliabilities
summary(double.coded$reliability)
mat.double.coded = mat.double.coded[-1, ]

write.xlsx2(double.coded, file="Out - List of Double Coded Articles.xlsx", row.names = FALSE)  
write.xlsx2(mat.double.coded, file="Out - Responses to Double Coded Articles.xlsx", row.names = FALSE)  


### Find negative reliability
negative.alpha = double.coded[which(double.coded$reliability < 0), c("id", "coder1", "coder2")]
mat.double.coded.neg = mat.double.coded[which(mat.double.coded$id %in% negative.alpha$id),]


png("Out - Reliability of Double Coded Articles.png", width=800, height=800, res=100)
x <- hist(double.coded$reliability)
plot(x, ylim=c(0,max(x$counts)+5), xaxt='n',
          xlab="Kripp Alpha Reliability", 
          main=paste("Reliability of Double Coded Articles;", "Kripp Alpha =", 
                     round(mean(double.coded$reliability, na.rm=TRUE),2)))
text(x$mids,x$counts,labels=x$counts, adj=c(0.5, -0.5))
axis(side=1, at=seq(-.2, 1, .1), labels=seq(-.2, 1, .1))
dev.off()

