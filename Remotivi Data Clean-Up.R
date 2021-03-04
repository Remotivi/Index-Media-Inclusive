
####################################################################
## LISTING INCOMPLETE ENTRIES
####################################################################

### Identify incomplete entries
incomplete.entries = data.frame(matrix(NA, nrow=1, ncol=3))
colnames(incomplete.entries) = c("coder", "id", "variable")

for (i in 1:num.coders) {
  for (j in col.must.complete) {
    
    na = is.na(dat[[i]][,j])
    
    # !!!! delete this when running actual; do this because too many incomplete
    #dat[[i]][na,j] = 0
    # !!!! end delete
    
    if (sum(na)>0) incomplete.entries = rbind(incomplete.entries, data.frame(coder=i, id=dat[[i]][na, "id"], variable=j))
  }
}


incomplete.entries = incomplete.entries[-1,]
if (nrow(incomplete.entries) > 0) {
  write.xlsx2(incomplete.entries, file="To Fix - Incomplete Entries.xlsx", row.names = FALSE)
}
    
  
####################################################################
## DATA VALIDITY CHECKING FOR COLUMNS WITH MULTIPLE ENTRIES (***)
####################################################################


### create new dataset because manipulation will be done on entries by adding *** at the end of entries
### perfectly alright because this is just to list mismatches
temp.dat = dat


### Change row names because otherwise would use old index and get confusing
for (i in 1:num.coders) {
  row.names(temp.dat[[i]]) = 1:nrow(temp.dat[[i]])
}


### Ensure that list of narsum, list of narsum representativeness, and list of narsum tone are consistent
inconsistent.narsum = as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(inconsistent.narsum) = c("coder", "id", "formal.narsum", "inklusifitas.narsum", "inklusifitas.tone")

for (i in 1:num.coders){
  for (j in 1:nrow(temp.dat[[i]])) {
    
    ### add *** at the end of entry, if entry is not empty
    ### this makes it easier to distinguish between empty string and one narsum
    if (str_length(temp.dat[[i]][j, "formal.narsum"]) > 0) {
      temp.dat[[i]][j, "formal.narsum"] = paste0(temp.dat[[i]][j, "formal.narsum"], "***")
    }
    
    if (str_length(temp.dat[[i]][j, "inklusifitas.narsum"]) > 0) {
      temp.dat[[i]][j, "inklusifitas.narsum"] = paste0(temp.dat[[i]][j, "inklusifitas.narsum"], "***")
    }
    
    if (str_length(temp.dat[[i]][j, "inklusifitas.tone"]) > 0) {
      temp.dat[[i]][j, "inklusifitas.tone"] = paste0(temp.dat[[i]][j, "inklusifitas.tone"], "***")
    }
    
    
    ### positions of stars in formal.narsum
    if (str_length(temp.dat[[i]][j, "formal.narsum"]) == 0) {        # no entry
      len.stars.formal.narsum = 0
    } else {
      stars.formal.narsum = as.vector(gregexpr("\\*\\*\\*", temp.dat[[i]][j, "formal.narsum"])[[1]])
      len.stars.formal.narsum = length(stars.formal.narsum)
    }
    
    
    ### positions of stars in inklusifitas.narsum
    if (str_length(temp.dat[[i]][j, "inklusifitas.narsum"]) == 0) {        # no entry
      len.stars.inklusifitas.narsum = 0
    } else {
      stars.inklusifitas.narsum = as.vector(gregexpr("\\*\\*\\*", temp.dat[[i]][j, "inklusifitas.narsum"])[[1]])
      len.stars.inklusifitas.narsum = length(stars.inklusifitas.narsum)
    }
    
    
    ### positions of stars in inklusifitas.tone
    if (str_length(temp.dat[[i]][j, "inklusifitas.tone"]) == 0) {        # no entry
      len.stars.inklusifitas.tone = 0
    } else {
      stars.inklusifitas.tone = as.vector(gregexpr("\\*\\*\\*", temp.dat[[i]][j, "inklusifitas.tone"])[[1]])
      len.stars.inklusifitas.tone = length(stars.inklusifitas.tone)
    }
    
    ### inconsistent
    if ((len.stars.formal.narsum != len.stars.inklusifitas.narsum) |
        (len.stars.formal.narsum != len.stars.inklusifitas.tone) |
        (len.stars.inklusifitas.narsum != len.stars.inklusifitas.tone)) {
      
      inconsistent.narsum = rbind(inconsistent.narsum, 
                                  c(coder=i, dat[[i]][j, c("id", "formal.narsum", "inklusifitas.narsum", "inklusifitas.tone")]))
    } 
  }
}


### Ensure that the number of listed istilah in inklusifitas.istilah is the same as inklusifitas.jumlah
inconsistent.istilah = as.data.frame(matrix(NA,nrow=1,ncol=4))
colnames(inconsistent.istilah) = c("coder", "id", "inklusifitas.istilah", "inklusifitas.jumlah")

### add *** at the end of entry, if entry is not empty
### this makes it easier to distinguish between empty string and one narsum
for (i in 1:num.coders){
  for (j in 1:nrow(temp.dat[[i]])) {

    ### change n/a to empty string
    if (temp.dat[[i]][j, "inklusifitas.istilah"] == "n/a") temp.dat[[i]][j, "inklusifitas.istilah"] = ""
    
    ### change - to empty string
    if (temp.dat[[i]][j, "inklusifitas.istilah"] == "-") temp.dat[[i]][j, "inklusifitas.istilah"] = ""
    
    ### add *** at the end of entry, if entry is not empty
    ### this makes it easier to distinguish between empty string and one narsum
    if (str_length(temp.dat[[i]][j, "inklusifitas.istilah"]) > 0) {
      temp.dat[[i]][j, "inklusifitas.istilah"] = paste0(temp.dat[[i]][j, "inklusifitas.istilah"], "***")
    }
    
    ### positions of stars in inklusifitas.istilah
    if (str_length(temp.dat[[i]][j, "inklusifitas.istilah"]) == 0) {        # no entry
      len.stars.inklusifitas.istilah = 0
    } else {
      stars.inklusifitas.istilah = as.vector(gregexpr("\\*\\*\\*", temp.dat[[i]][j, "inklusifitas.istilah"])[[1]])
      len.stars.inklusifitas.istilah = length(stars.inklusifitas.istilah)
    }
    
    ### inconsistent
    if (is.na(temp.dat[[i]][j, "inklusifitas.jumlah"])) {         # inklusifitas.jumlah == NA
      inconsistent.istilah = rbind(inconsistent.istilah, 
                                   c(i, dat[[i]][j, c("id", "inklusifitas.istilah", "inklusifitas.jumlah")]))
    } else {
      
      if (len.stars.inklusifitas.istilah != temp.dat[[i]][j, "inklusifitas.jumlah"]) {        # jumlah tidak match
        inconsistent.istilah = rbind(inconsistent.istilah, 
                                     c(coder=i, dat[[i]][j, c("id", "inklusifitas.istilah", "inklusifitas.jumlah")]))
      } #end if
    } #end else
  } # end j for
} # end i for


inconsistent.narsum = inconsistent.narsum[-1,]
inconsistent.istilah = inconsistent.istilah[-1,]

if (nrow(inconsistent.narsum) > 0) {
  write.xlsx2(inconsistent.narsum, file="To Fix - Inconsistent Narsum.xlsx", row.names = FALSE)  
}

if (nrow(inconsistent.istilah) > 0) {
  write.xlsx2(inconsistent.istilah, file="To Fix - Inconsistent Istilah.xlsx", row.names = FALSE)  
}
