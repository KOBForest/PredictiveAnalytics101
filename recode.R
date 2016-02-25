#here is the first of my many code styles
RecodeMany <- function (data, vars, Recodings){
  varlist=list(vars)
dataret <- data
  for (i in 1:length(vars)) {
    dataret[,i] <- car::recode(data[,i], recodes=Recodings)
  }
return(dataret)
}
##lolz at the hardcoded idiocy
##i wrote a longer function, as i did not have time to make it shorter
createSumScores <- function(data) {
data$physfun <- rowMeans(data[,grep("RANDQ[3456789]$|RANDQ[1][012]$",x=names(data))], na.rm=TRUE)
data$rolelim <- rowMeans(data[,grep("RANDQ[1][3456]$",x=names(data))], na.rm=TRUE)
data$rolelimem <- rowMeans(data[,grep("RANDQ[1][789]$",x=names(data))], na.rm=TRUE)
data$energyfat <- rowMeans(data[,grep("RANDQ[2][379]$|RANDQ31$",x=names(data))], na.rm=TRUE)
data$emwellbeing <- rowMeans(data[,grep("RANDQ[2][4568]$|RANDQ30$",x=names(data))], na.rm=TRUE)
data$socialfunctioning <- rowMeans(data[,grep("RANDQ20|RANDQ32",x=names(data))], na.rm=TRUE)
data$pain <- rowMeans(data[,grep("RANDQ[2][12]$",x=names(data))], na.rm=TRUE)
data$generalhealth <- rowMeans(data[,grep("RANDQ1$|RANDQ[3][3456]$",x=names(data))], na.rm=TRUE)
data$mindfulness <- rowMeans(data[, grep("MAAS", x=names(data))], na.rm=TRUE)
data$optimism <- rowMeans(data[,grep("LOTR", x=names(data))], na.rm=TRUE)
return(data)
}
grep.rand <- grep("^RANDQ", x=names(hom))
randitems <- hom[,grep.rand]
rand <- "RANDQ"
randset1 <-paste(rand, c(1, 2, 20, 22, 34, 36), sep="")
randset1 <- hom[,randset1]
randrecode1 <- RecodeMany(randset1, vars=c("RANDQ1", "RANDQ2","RANDQ20", "RANDQ34","RANDQ36"), Recodings=("1=100;2=75;3=50;4=25;5=0"))
randset2 <- paste(rand, c(3:12), sep="")
randset2 <- hom[,randset2]
randrecode2 <- RecodeMany(randset2, vars=c("RANDQ3", "RANDQ4","RANDQ5","RANDQ6","RANDQ7","RANDQ8","RANDQ9", "RANDQ10","RANDQ11","RANDQ12"),Recodings="1=0;2=50;3=100")
randset3 <- paste(rand, c(13:19), sep="")
randset3 <- hom[,randset3]
randrecode3 <- RecodeMany(randset3, vars=c("RANDQ13", "RANDQ14", "RANDQ15", "RANDQ16","RANDQ17","RANDQ18", "RANDQ19"), Recodings="1=0;2=100")
randset4 <- paste(rand, c(21,23,26,27,30), sep="")
randset4 <- hom[,randset4]
randrecode4 <- RecodeMany(randset4, vars=c("RANDQ21", "RANDQ23", "RANDQ26","RANDQ27","RANDQ30"), Recodings="1=100;2=80;3=60;4=40;5=20;6=0")
randset5 <- paste(rand, c(24,25,28,29,31), sep="")
randset5 <- hom[,randset5]
randrecode5 <- RecodeMany(randset5, vars=c("RANDQ24", "RANDQ25", "RANDQ28", "RANDQ29", "RANDQ31"), Recodings="1=0;2=20;3=40;4=60;5=80;6=100")
randset6 <- paste(rand, c(32,33,35), sep="")
randset6 <- hom[,randset6]
randrecode6 <- RecodeMany(randset6,vars=c("RANDQ32","RANDQ33","RANDQ35"), Recodings="1=0;2=25;3=50;4=75;5=100")
randrecoding <- ls(pattern="randrecode")
randrecoding.df <- as.data.frame(lapply(randrecoding, function (x) get(x)))
randsortpaste <- paste(rand, c(1:36), sep="")
randitems.unscored <- hom[,grep.rand]
randitems.scored <- randrecoding.df[,randsortpaste]
hom[,grep.rand] <- randitems.scored
hom <- createSumScores(hom)
hom.scores <- createSumScores(na.omit(hom))
hom1 <- hom[hom$CollectMeth=="Paper",]
hom2 <- hom[hom$CollectMeth=="Online",]
