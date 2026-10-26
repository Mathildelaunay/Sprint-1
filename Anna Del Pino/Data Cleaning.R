knitr::opts_chunk$set(echo = TRUE)

setwd("/Users/oy23975/OneDrive - University of Bristol/Documents/GitHub/Sprint-1/Anna Del Pino")

data24_25 <-read.csv("./Premier League Train Data/24_25.csv")
data23_24 <-read.csv("./Premier League Train Data/23_24.csv")
data22_23 <-read.csv("./Premier League Train Data/22_23.csv")
data21_22 <-read.csv("./Premier League Train Data/21_22.csv")
data20_21 <-read.csv("./Premier League Train Data/20_21.csv")
data19_20 <-read.csv("./Premier League Train Data/19_20.csv")
data18_19 <-read.csv("./Premier League Train Data/18_19.csv")
data17_18 <-read.csv("./Premier League Train Data/17_18.csv")
data16_17 <-read.csv("./Premier League Train Data/16_17.csv")
data15_16 <-read.csv("./Premier League Train Data/15_16.csv")
data14_15 <-read.csv("./Premier League Train Data/14_15.csv")

vectordata<-list(data24_25 = data24_25, data23_24 = data23_24, data22_23 = data22_23, data21_22 = data21_22, data20_21 = data20_21, data19_20 = data19_20, data18_19 = data18_19, data17_18 = data17_18, data16_17 = data16_17, data15_16 = data15_16, data14_15 = data14_15)

columncheck <- function(a) {
  coln <- sapply(a, ncol)
  names(coln)[which.max(coln)]
}

print(columncheck(vectordata))


ncol(data24_25)

matches<-c()
for (i in seq_along(vectordata)) {
  matches[i] <-sum(colnames(data24_25) %in% colnames(vectordata[[i]]))
}
names(matches) <- names(vectordata)
print(matches) 

colnames(data18_19)[which(!colnames(data18_19) %in% colnames(data24_25))]

#create a vector of columns to remove
odds_columns <- c("1XBH", "1XBD", "1XBA","B365H", "B365D", "B365A","BFH", "BFD", "BFA","BFDH", "BFDD", "BFDA","BMGMH", "BMGMD", "BMGMA","BVH", "BVD", "BVA","BSH", "BSD", "BSA","BWH", "BWD", "BWA","CLH", "CLD", "CLA","GBH", "GBD", "GBA","IWH", "IWD", "IWA","LBH", "LBD", "LBA","PPH", "PPD", "PPA","PSH", "PH", "PSD", "PD", "PSA", "PA","SKH", "SKD", "SKA","SOH", "SOD", "SOA","SBH", "SBD", "SBA","SJH", "SJD", "SJA","SYH", "SYD", "SYA","VCH", "VCD", "VCA","WHH", "WHD", "WHA","Bb1X2","BbMxH", "BbAvH","BbMxD", "BbAvD","BbMxA", "BbAvA","MaxH", "MaxD", "MaxA","AvgH", "AvgD", "AvgA","BFEH", "BFED", "BFEA","BbOU","BbMx>2.5", "BbAv>2.5","BbMx<2.5", "BbAv<2.5","GB>2.5", "GB<2.5","B365>2.5", "B365<2.5","P>2.5", "P<2.5","Max>2.5", "Max<2.5","Avg>2.5", "Avg<2.5","BbAH", "BbAHh", "AHh","BbMxAHH", "BbAvAHH","BbMxAHA", "BbAvAHA","GBAHH", "GBAHA", "GBAH","LBAHH", "LBAHA", "LBAH","B365AHH", "B365AHA", "B365AH","PAHH", "PAHA","MaxAHH", "MaxAHA","AvgAHH", "AvgAHA")

for (i in seq_along(vectordata)) {
  cols_to_remove <- intersect(colnames(vectordata[[i]]),odds_columns) #
  vectordata[[i]] <- vectordata[[i]][
    , !colnames(vectordata[[i]]) %in% cols_to_remove,
    drop = FALSE
  ]
}

for (i in seq_along(vectordata)) {
  cat(
    names(vectordata)[i],
    "-",
    ncol(vectordata[[i]]),
    "columns remaining\n"
  )
}

