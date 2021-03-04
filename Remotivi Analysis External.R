
####################################################################
## READ AND PREPARE DATA EXTERNAL
####################################################################

rm(list = ls())

dat.agama = dat.disabilitas = dat.minoritasseksual = dat.perempuan = list(resp1 = NA, 
                                                                          resp2 = NA,
                                                                          resp3 = NA,
                                                                          resp4 = NA,
                                                                          resp4 = NA)


### Read Data Assignment
assigned.agama = read.xlsx2("Out - External Cluster Agama.xlsx", "ALL", header=TRUE)
assigned.disabilitas = read.xlsx2("Out - External Cluster Disabilitas.xlsx", "ALL", header=TRUE)
assigned.minoritasseksual = read.xlsx2("Out - External Cluster Gender.xlsx", "ALL", header=TRUE)
assigned.perempuan = read.xlsx2("Out - External Cluster Perempuan.xlsx", "ALL", header=TRUE)
dat.full = rbind(assigned.agama, assigned.disabilitas, assigned.minoritasseksual, assigned.perempuan)
colnames(dat.full) = c("kode", "cluster", "media", "hari", "bulan", "tanggal.terbit", "judul", "link",
                       "nilai.judul", "angle.frame", "komposisi.narasumber", "tone", "representasi", "bahasa",
                       "catatan", "coder")

### Read Data from Respondents
wait = 2
for (i in 1:5) {
  Sys.sleep(wait)
  dat.agama[[i]] = read.xlsx2("Lembar Responden - Agama.xlsx", sheetIndex=i, startRow=24, header=TRUE)[1:50,1:16]
  colnames(dat.agama[[i]])[10] = "Nilai.Judul"
  colnames(dat.agama[[i]]) = tolower(colnames(dat.agama[[i]]))
  
  Sys.sleep(wait)
  dat.disabilitas[[i]] = read.xlsx2("Lembar Responden - Disabilitas.xlsx", sheetIndex=i, startRow=24, header=TRUE)[1:50,1:16]
  colnames(dat.disabilitas[[i]])[10] = "Nilai.Judul"
  colnames(dat.disabilitas[[i]]) = tolower(colnames(dat.disabilitas[[i]]))
  
  Sys.sleep(wait)
  dat.minoritasseksual[[i]] = read.xlsx2("Lembar Responden - Minoritas Seksual.xlsx", sheetIndex=i, startRow=24, header=TRUE)[1:50,1:16]
  colnames(dat.minoritasseksual[[i]])[10] = "Nilai.Judul"
  colnames(dat.minoritasseksual[[i]]) = tolower(colnames(dat.minoritasseksual[[i]]))
  
  Sys.sleep(wait)
  dat.perempuan[[i]] = read.xlsx2("Lembar Responden - Perempuan.xlsx", sheetIndex=i, startRow=24, header=TRUE)[1:50,1:16]
  colnames(dat.perempuan[[i]])[10] = "Nilai.Judul"
  colnames(dat.perempuan[[i]]) = tolower(colnames(dat.perempuan[[i]]))
}



### Turn into Numeric
col.scoring = c("nilai.judul", "angle.frame", "komposisi.narasumber", "tone", "representasi")
for (i in 1:5) {
  for (j in col.scoring) {
    dat.agama[[i]][,j] = as.numeric(dat.agama[[i]][,j])
    dat.disabilitas[[i]][,j] = as.numeric(dat.disabilitas[[i]][,j])
    dat.minoritasseksual[[i]][,j] = as.numeric(dat.minoritasseksual[[i]][,j])
    dat.perempuan[[i]][,j] = as.numeric(dat.perempuan[[i]][,j])
    dat.full[, j] = as.numeric(dat.full[, j])
  } 
}

dat.full[, "coder"] = as.numeric(dat.full[, "coder"])



####################################################################
## Create One Big Data Frame
####################################################################

### Create One Big Data Frame
one.big.dataframe <- function(i, temp.dat) {
  coder = dat.full[i, "coder"]
  if (coder == 10) {
    scores = NA
    for (j in col.scoring) {
      vec.coders = c(temp.dat[[1]][which(temp.dat[[1]][,"kode"] == dat.full[i, "kode"]), j],
                     temp.dat[[2]][which(temp.dat[[2]][,"kode"] == dat.full[i, "kode"]), j],
                     temp.dat[[3]][which(temp.dat[[3]][,"kode"] == dat.full[i, "kode"]), j],
                     temp.dat[[4]][which(temp.dat[[4]][,"kode"] == dat.full[i, "kode"]), j],
                     temp.dat[[5]][which(temp.dat[[5]][,"kode"] == dat.full[i, "kode"]), j]
      )
      scores = c(scores, mean(vec.coders))
    }
    scores = scores[-1]
  } else {
    scores = temp.dat[[coder]][which(temp.dat[[coder]][,"kode"] == dat.full[i, "kode"]), col.scoring]
  } 
  
  return (scores)
}

for (i in 1:nrow(dat.full)) {
  # Agama
  if (dat.full$cluster[i] == "agama marjinal") dat.full[i, col.scoring] = one.big.dataframe(i, dat.agama)
  
  # Disabilitas
  if (dat.full$cluster[i] == "disabilitas") dat.full[i, col.scoring] = one.big.dataframe(i, dat.disabilitas)
  
  # Minoritas Seksual
  if (dat.full$cluster[i] == "identitas gender dan orientasi seksual") dat.full[i, col.scoring] = one.big.dataframe(i, dat.minoritasseksual)
  
  # Perempuan
  if (dat.full$cluster[i] == "perempuan dalam kekerasan") dat.full[i, col.scoring] = one.big.dataframe(i, dat.perempuan)
}

### Save to File
write.xlsx2(dat.full, file="Out - External Data Full.xlsx")

### Calculate Score
dat.full$score = rowMeans(dat.full[, col.scoring])


####################################################################
### Skor/ranking tiap media secara umum
####################################################################

decimal = 3
rtffile <- RTF("Out - Results of External Respondents.doc")

out = data.frame(media = unique(dat.full$media),
                 min=NA, q1=NA, median=NA, mean=NA, q3=NA, max=NA)

for (i in 1:nrow(out)) {
  index = which(dat.full$media == out[i, "media"])
  out[i,2:7] = round(summary(dat.full[index, "score"]), decimal)
}

addHeader(rtffile, "Skor/Ranking Tiap Media Secara Umum", font.size = 16)
addTable(rtffile, out, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


####################################################################
### Skor/ranking tiap media untuk tiap klaster (disabilitas, perempuan, agama marginal, gender)
####################################################################

out.agama = out.disabilitas = out.gender = out.perempuan = 
  data.frame( media = unique(dat.full$media),
              min=NA, q1=NA, median=NA, mean=NA, q3=NA, max=NA)

### Agama
for (i in 1:nrow(out.agama)) {
  temp.dat = subset(dat.full, subset=cluster == "agama marjinal")
  index = which(temp.dat$media == out.agama[i, "media"])
  out.agama[i,2:7] = round(summary(temp.dat[index, "score"]), decimal)
}

### Disabilitas
for (i in 1:nrow(out.disabilitas)) {
  temp.dat = subset(dat.full, subset=cluster == "disabilitas")
  index = which(temp.dat$media==out.disabilitas[i, "media"])
  out.disabilitas[i,2:7] = round(summary(temp.dat[index, "score"]), decimal)
}

### Identitas Gender
for (i in 1:nrow(out.gender)) {
  temp.dat = subset(dat.full, subset=cluster == "identitas gender dan orientasi seksual")
  index = which(temp.dat$media==out.gender[i, "media"])
  out.gender[i,2:7] = round(summary(temp.dat[index, "score"]), decimal)
}

### Perempuan Dalam Kekerasan
for (i in 1:nrow(out.perempuan)) {
  temp.dat = subset(dat.full, subset=cluster == "perempuan dalam kekerasan")
  index = which(temp.dat$media==out.perempuan[i, "media"])
  out.perempuan[i,2:7] = round(summary(temp.dat[index, "score"]), decimal)
}


addPageBreak(rtffile)
addHeader(rtffile, "Skor Media per Cluster", font.size = 16)
addParagraph(rtffile, "Cluster Agama")
addTable(rtffile, out.agama, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Disabilitas")
addTable(rtffile, out.disabilitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Identitas Gender")
addTable(rtffile, out.gender, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Perempuan")
addTable(rtffile, out.perempuan, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


####################################################################
### Skor/ranking per variabel: keseluruhan
####################################################################

out = data.frame(variable = c("nilai.judul", "angle.frame", "komposisi.narasumber", 
                              "tone", "representasi"),
                 min=NA, q1=NA, median=NA, mean=NA, q3=NA, max=NA)

for (i in 1:nrow(out)) {
  out[i,2:7] = round(summary(dat.full[, out[i,1]]), decimal)
}

addHeader(rtffile, "Skor/Ranking Per Variable Secara Umum", font.size = 16)
addTable(rtffile, out, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


####################################################################
### Skor/ranking per variabel: per cluster
####################################################################

vars = c("nilai.judul", "angle.frame", "komposisi.narasumber", 
         "tone", "representasi")

out = data.frame(matrix(NA, nrow=length(unique(dat.full$cluster)), ncol=length(vars)+2))
colnames(out) = c("cluster", vars, "all vars")
out[,1] = unique(dat.full$cluster)


for (i in 1:nrow(out)) {
  index = which(dat.full$cluster == out$cluster[i])
  for (j in vars) {
    out[i, j] = round(mean(dat.full[index, j]), decimal)
  }
  out[i, "all vars"] = round(mean(dat.full[index, "score"]), decimal)
}

out$cluster = recode(out$cluster, 
                       "'identitas gender dan orientasi seksual' = 'identitas gender';
                       'perempuan dalam kekerasan' = 'perempuan'")

colnames(out) = recode(colnames(out), 
                     "'nilai.judul' = 'judul';
                      'angle.frame' = 'angle';
                      'komposisi.narasumber' = 'narasumber'")


addHeader(rtffile, "Rata-rata (Mean) Skor Per Variable per Cluster", font.size = 16)
addTable(rtffile, out, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


####################################################################
### Skor/ranking per variabel: per media
####################################################################

vars = c("nilai.judul", "angle.frame", "komposisi.narasumber", 
         "tone", "representasi")

out = data.frame(matrix(NA, nrow=length(unique(dat.full$media)), ncol=length(vars)+2))
colnames(out) = c("media", vars, "all vars")
out[,1] = unique(dat.full$media)


for (i in 1:nrow(out)) {
  index = which(dat.full$media == out$media[i])
  for (j in vars) {
    out[i, j] = round(mean(dat.full[index, j]), decimal)
  }
  out[i, "all vars"] = round(mean(dat.full[index, "score"]), decimal)
}

colnames(out) = recode(colnames(out), 
                       "'nilai.judul' = 'judul';
                      'angle.frame' = 'angle';
                      'komposisi.narasumber' = 'narasumber'")


addHeader(rtffile, "Rata-rata (Mean) Skor Per Variable per Media", font.size = 16)
addTable(rtffile, out, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


####################################################################
### Daftar Istilah
####################################################################

combined.entries = rbind(dat.agama[[1]],
                         dat.agama[[2]],
                         dat.agama[[3]],
                         dat.agama[[4]],
                         dat.agama[[5]],
                         dat.disabilitas[[1]],
                         dat.disabilitas[[2]],
                         dat.disabilitas[[3]],
                         dat.disabilitas[[4]],
                         dat.disabilitas[[5]],
                         dat.minoritasseksual[[1]],
                         dat.minoritasseksual[[2]],
                         dat.minoritasseksual[[3]],
                         dat.minoritasseksual[[4]],
                         dat.minoritasseksual[[5]],
                         dat.perempuan[[1]],
                         dat.perempuan[[2]],
                         dat.perempuan[[3]],
                         dat.perempuan[[4]],
                         dat.perempuan[[5]]
                         )

list.istilah = ("PLACEHOLDER TO DELETE")

for (i in 1:nrow(combined.entries)){
  ### if n/a
  if (combined.entries[i , "bahasa"] == "n/a") next;
  
  ### if -
  if (combined.entries[i , "bahasa"] == "-") next;
  
  ### if " "
  if (combined.entries[i , "bahasa"] == " ") next;
  
  split = strsplit(combined.entries[i , "bahasa"], "\\*\\*\\*")[[1]]
  list.istilah = c(list.istilah, split)
}


for (i in 1:length(list.istilah)) {
  ### some istilah use quotation "", remove quotations
  list.istilah[i] =  gsub("\"", "", list.istilah[i])
  
  ### make lower case
  list.istilah[i] =  tolower(list.istilah[i])
}

### delete placeholder
list.istilah = list.istilah[-which(list.istilah == "placeholder to delete")]
list.istilah = list.istilah[-which(list.istilah == "")]


out.1 = data.frame(table(list.istilah))
out.1 = out.1[order(out.1$Freq, decreasing=TRUE),]

addPageBreak(rtffile)
addHeader(rtffile, "Istilah Apa Saja Yang Bermasalah", font.size = 16)
addTable(rtffile, out.1, col.justify = c("L","C"))
done(rtffile)


####################################################################
## CALCULATE RELIABILITY
####################################################################

### add coder information to combined.entries
combined.entries = cbind(combined.entries, 
                         coder=c(rep(c(rep(1,50), rep(2,50), rep(3,50), rep(4,50), rep(5,50)),4)))

### Save to File
write.xlsx2(combined.entries, file="Out - External Data with Multiple Entries.xlsx")


### Identify shared articles
double.coded = data.frame(kode = dat.full[which(dat.full$coder==10), "kode"], 
                          reliability = NA,
                          cluster=NA)

### Define col.reliability (adding score from multiple entries, delete multiple entries)
col.reliability = col.scoring


mat.double.coded = data.frame(matrix(NA, nrow=5, ncol=length(col.reliability)+2))
colnames(mat.double.coded) = c("kode", "coder", col.reliability)

### Calculate Kripp Alpha and Average across Combinations
options(warn = 2)
for (i in 1:nrow(double.coded)) {
  
  mat = data.frame(matrix(NA, nrow=5, ncol=length(col.reliability)))
  colnames(mat) = col.reliability
  
  index1 = which(combined.entries$kode == double.coded$kode[i] & combined.entries$coder == 1)
  index2 = which(combined.entries$kode == double.coded$kode[i] & combined.entries$coder == 2)
  index3 = which(combined.entries$kode == double.coded$kode[i] & combined.entries$coder == 3)
  index4 = which(combined.entries$kode == double.coded$kode[i] & combined.entries$coder == 4)
  index5 = which(combined.entries$kode == double.coded$kode[i] & combined.entries$coder == 5)
  
  mat[1,] = combined.entries[index1, col.reliability]                                   ## add coder 1 responses to the matrix
  mat[2,] = combined.entries[index2, col.reliability]                                   ## add coder 2 responses to the matrix
  mat[3,] = combined.entries[index3, col.reliability]                                   ## add coder 3 responses to the matrix
  mat[4,] = combined.entries[index4, col.reliability]                                   ## add coder 4 responses to the matrix
  mat[5,] = combined.entries[index5, col.reliability]                                   ## add coder 5 responses to the matrix
  
  double.coded$reliability[i] = kripp.alpha(as.matrix(mat), method="interval")$value
  double.coded$cluster[i] = combined.entries[index1, "cluster"]
  
  mat.double.coded = rbind(mat.double.coded, 
                           cbind(kode=double.coded$kode[i], coder=1:5, mat))
}
options(warn = 0)

mat.double.coded = mat.double.coded[-c(1:5),]

### Save to File
write.xlsx2(mat.double.coded, file="Out - External Responses to Double Coded Entries.xlsx")

### Output Double Coded Articles and Their Reliabilities
summary(double.coded$reliability)
tapply(double.coded$reliability, double.coded$cluster, mean)

png("Out - Reliability of External Articles.png", width=800, height=800, res=100)
x <- hist(double.coded$reliability)
plot(x, ylim=c(0,max(x$counts)+5), xaxt='n',
     xlab="Kripp Alpha Reliability", 
     main=paste("Reliability of External Articles;", "Kripp Alpha =", 
                round(mean(double.coded$reliability, na.rm=TRUE),2)))
text(x$mids,x$counts,labels=x$counts, adj=c(0.5, -0.5))
axis(side=1, at=seq(-.3, 1, .1), labels=seq(-.3, 1, .1))
dev.off()

