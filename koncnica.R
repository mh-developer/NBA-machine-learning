################################
# 4.) KONČNICA (PLAYOFFS)      #
################################

# Branje podatkov iz tekstovne datoteke:

playoffs <- read.table(file="playoff.txt", sep=",", header=TRUE)
playoffs
head(playoffs)
summary(playoffs)
str(playoffs)
names(playoffs)

# Spremenljivki, ki hranita ime zaporedne zmagovalne / porazene ekipe

winns <- ifelse(playoffs$HPTS > playoffs$APTS, as.character(playoffs$HOME), as.character(playoffs$AWAY))
losses <- ifelse(playoffs$HPTS > playoffs$APTS, as.character(playoffs$AWAY), as.character(playoffs$HOME))

winns
losses

# Dva podatkovna okvirja za zmage in poraze

dfW <- data.frame(team=winns) 
dfW$winns <- table(dfW)[as.character(dfW$team)]
dfW <- unique(dfW)

dfL <- data.frame(team=losses) 
dfL$losses <- table(dfL)[as.character(dfL$team)]
dfL <- unique(dfL)

dfW
dfL

# Oba podatkovna okvirja združimo

total <- merge(dfW, dfL)
total

pie(total$winns, labels = total$team)
lbls <- total$team
pct <- round(total$winns/sum(total$winns)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(main="Krozni diagram porazdelitve zmag", total$winns, labels = lbls, cex=0.75)
barplot(main="Histogram zmag po ekipah", total$winns, names.arg=total$team, cex.names=0.75, col="blue", ylim=c(0, 50))

pie(total$losses, labels = total$team)
lbls <- total$team
pct <- round(total$losses/sum(total$losses)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(main="Krozni diagram porazdelitve porazov", total$losses, labels = lbls, cex=0.75)
barplot(main="Histogram porazov po ekipah", total$losses, names.arg=total$team, cex.names=0.75, col="red")


# Uspešnost metanja trojk na ekipo v vseh treh končnicah:

a <- aggregate(playoffs$H3PM / playoffs$H3PA, by=list(playoffs$HOME), FUN=mean)
names(a) <- c("team", "threesP")
a

b <- aggregate(playoffs$A3PM / playoffs$A3PA, by=list(playoffs$AWAY), FUN=mean)
names(b) <- c("team", "threesP")
b

c <- data.frame(a$team, a$threesP + b$threesP)
names(c) <- c("team", "threesP")
c

head(a)
head(b)
head(c)

playoffs$H3PM / playoffs$H3PA

barplot(main="Histogram uspešnosti trojk po ekipah v %", c$threesP, names.arg=c$team, cex.names=0.75, col="orange", ylim=c(0, 1))
plot(x=c$team, y=c$threesP, ylim=c(0,1), type="o")

# a) VEČINSKI KLASIFIKATOR

# Za ucno mnozico vzamemo prvi dve sezoni (2014-2015 in 2015-2016), 
# za testno pa zadnjo sezono (2015-2016)

playoffs[as.integer(playoffs$SEASON) == 1,]
playoffs[as.integer(playoffs$SEASON) == 2,]
playoffs[as.integer(playoffs$SEASON) == 3,]

ucnaMnozica <- playoffs[as.integer(playoffs$SEASON) < 3, ]
ucnaMnozica

testnaMnozica <- playoffs[as.integer(playoffs$SEASON) == 3, ]
testnaMnozica

# SEASON in DATE atribut nista vec pomembna za klasifikacijo
playoffs$SEASON <- NULL
playoffs$DATE <- NULL

ucnaMnozica$SEASON <- NULL
ucnaMnozica$DATE <- NULL

testnaMnozica$SEASON <- NULL
testnaMnozica$DATE <- NULL

nrow(ucnaMnozica)
table(ucnaMnozica$HPTS)

# Domacim zmagam iz prvih dveh sezon pripisemo 0, porazom 1
winns <- ifelse(ucnaMnozica$HPTS > ucnaMnozica$APTS, 0, 1)
tabelaZmag <- table(winns)

domaceZmage <- tabelaZmag[1]
domaceZmage
gostujoceZmage <- tabelaZmag[2]

# Vecinski razred je prvi
majorityClass <- names(which.max(tabelaZmag))
majorityClass

# Tocnost vecinskega klasifikatorja
# (delez pravilnih napovedi, ce bi vse testne primere klasificirali v vecinski razred)

tocnost <- sum(winns == majorityClass) / length(winns)
tocnost

klasificirajZVecinskim <- function(domacin, gost) {
  if (domacin %in% playoffs$HOME && gost %in% playoffs$AWAY) {
    paste("Domace mostvo zmaga z verjetnostjo", sum(winns == majorityClass) / length(winns)) 
  } else {
    "Ime ene od ekip ni v mnozici podatkov."
  }
}

klasificirajZVecinskim("HOU", "GSW")
klasificirajZVecinskim("Mitja", "Anze")

# b) ODLOCITVENO DREVO

#Nalozimo knjiznico "rpart".
library(rpart)

# Ucni in testni mnozici dodamo atribut HWIN, ki pove, ali je domaca ekipa zmagala ali ne
winns <- ifelse(ucnaMnozica$HPTS > ucnaMnozica$APTS, 1, 0)
ucnaMnozica$HWIN <- as.factor(winns)
table(ucnaMnozica$HWIN)

winns <- ifelse(testnaMnozica$HPTS > testnaMnozica$APTS, 1, 0)
testnaMnozica$HWIN <- as.factor(winns)
table(testnaMnozica$HWIN)

summary(ucnaMnozica)

# Zgradimo odlocitveno drevo
dt <- rpart(HWIN ~ ., data = ucnaMnozica)
dt <- rpart(HWIN ~ H2PM+H2PA+H3PM+H3PA+HFTM+HFTA+HORB+HDRB+HAST+HSTL+HTOV+
              HBLK+HPF+A2PM+A2PA+A3PM+A3PA+AFTM+AFTA+AORB+
              ADRB+AAST+ASTL+ATOV+ABLK+APF, data = ucnaMnozica)
dt

# Izris odlocitvenega drevesa
plot(dt)
text(dt, pretty = 0, cex = 0.8)

# Prave vrednosti testnih primerov

observed <- testnaMnozica$HWIN
observed

# Napovedane vrednosti modela
# Uporabimo funkcijo "predict", ki potrebuje model, testne primere in obliko, 
# v kateri naj poda svoje napovedi. Nastavitev "class" pomeni, da nas zanimajo
# samo razredi, v katere je model klasificiral testne primere.
predicted <- predict(dt, testnaMnozica, type = "class")
predicted

# Zgradimo tabelo napacnih klasifikacij
t <- table(observed, predicted)
t

# Klasifikacijska tocnost modela
sum(diag(t)) / sum(t)

# Funkcija za izracun klasifikacijske tocnosti modela (CA)
CA <- function(prave, napovedane)
{
  t <- table(prave, napovedane)
  
  sum(diag(t)) / sum(t)
}



# Funkcija za izracun Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

# metoda precnega preverjanja je implementirana v knjiznici "ipred"
library(ipred)

# pomozne funkcije, ki jih potrebujemo za izvedbo precnega preverjanja
mypredict.generic <- function(object, newdata){predict(object, newdata, type = "class")}
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}



# Klic funkcije za klasifikacijsko tocnost
CA(observed, predicted)

# -------------

# Napoved verjetnosti za končno zmago pred playoffi

regular <- read.table(file="regular.txt", sep=",", header=TRUE)

# Spremenljivki, ki hranita ime zaporedne zmagovalne / porazene ekipe

winns <- ifelse(regular$HPTS > regular$APTS, as.character(regular$HOME), as.character(regular$AWAY))
losses <- ifelse(regular$HPTS > regular$APTS, as.character(regular$AWAY), as.character(regular$HOME))

regular$WINNER <- winns
summary(regular)

# Spremeljivka za domače zmage
homeWinns <- ifelse(as.character(regular$HOME) == as.character(regular$WINNER), as.character(regular$HOME), "LOSS")
homeWinns

winns
losses

# Podatkovni okvirji

dfW <- data.frame(team=winns) 
dfW$winns <- table(dfW)[as.character(dfW$team)]
dfW <- unique(dfW)

dfHW <- data.frame(team=homeWinns) 
dfHW$homeWinns <- table(dfHW)[as.character(dfHW$team)]
dfHW <- unique(dfHW)

dfL <- data.frame(team=losses) 
dfL$losses <- table(dfL)[as.character(dfL$team)]
dfL <- unique(dfL)

dfW
dfHW <- dfHW[-c(1),]
dfL

# Vse tri podatkovne okvirje združimo:

totalR <- merge(dfW, dfL)
totalR <- merge(totalR, dfHW)

totalR$awayWinns <- totalR$winns - totalR$homeWinns
totalR$HWINP <- totalR$homeWinns / totalR$winns
totalR$AWINP <- 1 - totalR$HWINP
totalR

# Ekipa GSW ima največ zmag po rednem delu lige:
totalR[max(totalR$winns) == totalR$winns,]

# Dodamo še verjetnost konče zmage, če gledamo dosežene zmage po rednem delu:
totalR$FWINNP <- totalR$winns / sum(totalR$winns) 
totalR

# Spremenljivki, ki hranita ime zaporedne zmagovalne / porazene ekipe

winns <- ifelse(playoffs$HPTS > playoffs$APTS, as.character(playoffs$HOME), as.character(playoffs$AWAY))
losses <- ifelse(playoffs$HPTS > playoffs$APTS, as.character(playoffs$AWAY), as.character(playoffs$HOME))

nrow(data.frame(winns))
nrow(playoffs)

playoffs$WINNER <- winns
summary(playoffs)

# Spremenljivka za domače zmage
homeWinns <- ifelse(as.character(playoffs$HOME) == as.character(playoffs$WINNER), as.character(playoffs$HOME), "LOSS")
homeWinns

# Dva podatkovna okvirja za zmage in poraze in še en za domače zmage

dfW <- data.frame(team=winns) 
dfW$winns <- table(dfW)[as.character(dfW$team)]
dfW <- unique(dfW)

dfHW <- data.frame(team=homeWinns) 
dfHW$homeWinns <- table(dfHW)[as.character(dfHW$team)]
dfHW <- unique(dfHW)

dfL <- data.frame(team=losses) 
dfL$losses <- table(dfL)[as.character(dfL$team)]
dfL <- unique(dfL)

dfW
dfHW <- dfHW[-c(4),]
dfL

# Vse tri podatkovne okvirje združimo:

totalF <- merge(dfW, dfL)
totalF <- merge(total, dfHW)

# Dodamo še gostujoče zmage:
totalF$awayWinns <- totalF$winns - totalF$homeWinns
totalF$HWINP <- totalF$homeWinns / totalF$winns
totalF$AWINP <- 1 - totalF$HWINP
totalF

# Ekipa GSW ima največ zmag tudi v playoffih lige:
totalF[max(totalF$winns) == totalF$winns,]

# Dodamo še verjetnost konče zmage, če gledamo dosežene zmage v končnici:
totalF$FWINNP <- totalF$winns / sum(totalF$winns) 
totalF

totalR[order(totalR$FWINNP), ]

# Funkcija za izvedbo playoffov:
executePlayyofs <- function() {
  east <- c("TOR", "MIL", "IND", "BOS", "CHA", "WAS", "MIA", "BKN", "ATL", "CHI", "CLE")
  west <- c("LAC", "OKC", "GSW", "MEM", "POR", "DAL", "HOU", "MIN", "SAS")
  
  for (i in east)
    totalRE[nrow(totalRE) + 1,] <- totalR[totalR$team == i,]
  
  for (i in west)
    totalRW[nrow(totalRW) + 1,] <- totalR[totalR$team == i,]
  
  orderedRE <- totalRE[order(totalRE$FWINNP),]
  orderedRE <- orderedRE[seq(dim(orderedRE)[1],1),]
  orderedRE <- orderedRE[c(1:8),]
  
  orderedRW <- totalRW[order(totalRW$FWINNP),]
  orderedRW <- orderedRW[seq(dim(orderedRW)[1],1),]
  orderedRW <- orderedRW[c(1:8),]
  
  pair1 <- orderedRE[c(1, 8), ]
  pair1W <- pair1[max(pair1$FWINNP) == pair1$FWINNP,]
  pair2 <- orderedRE[c(2, 7), ]
  pair2W <- pair2[max(pair2$FWINNP) == pair2$FWINNP,]
  pair3 <- orderedRE[c(3, 6), ]
  pair3W <- pair3[max(pair3$FWINNP) == pair3$FWINNP,]
  pair4 <- orderedRE[c(4, 5), ]
  pair4W <- pair4[max(pair4$FWINNP) == pair4$FWINNP,]
  
  pair5 <- orderedRW[c(1, 8), ]
  pair5W <- pair5[max(pair5$FWINNP) == pair5$FWINNP,]
  pair6 <- orderedRW[c(2, 7), ]
  pair6W <- pair6[max(pair6$FWINNP) == pair6$FWINNP,]
  pair7 <- orderedRW[c(3, 6), ]
  pair7W <- pair7[max(pair7$FWINNP) == pair7$FWINNP,]
  pair8 <- orderedRW[c(4, 5), ]
  pair8W <- pair8[max(pair8$FWINNP) == pair8$FWINNP,]
  
  sf1 <- rbind(pair1W, pair2W)
  sf1W <- sf1[max(sf1$FWINNP) == sf1$FWINNP,]
  sf2 <- rbind(pair3W, pair4W)
  sf1W <- sf2[max(sf2$FWINNP) == sf2$FWINNP,]
  
  sf3 <- rbind(pair5W, pair6W)
  sf3W <- sf3[max(sf3$FWINNP) == sf3$FWINNP,]
  sf4 <- rbind(pair7W, pair8W)
  sf4W <- sf4[max(sf4$FWINNP) == sf4$FWINNP,]
  
  f1 <- rbind(sf1W, sf2W)
  f1W <- f1[max(f1$FWINNP) == f1$FWINNP,]
  f2 <- rbind(sf3W, sf4W)
  f2W <- f2[max(f2$FWINNP) == f2$FWINNP,]
  
  final <- rbind(f1W, f2W)
  finalWinner <- final[max(final$FWINNP) == final$FWINNP,]
  finalWinner
} 






#
#
# NAIVNI BAYESOV KLASIFIKATOR
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(e1071)

nb <- naiveBayes(HWIN ~ ., data = ucnaMnozica)
predicted <- predict(nb, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(nb, testnaMnozica, type = "raw")
obsMat <- model.matrix(~HWIN-1, testnaMnozica)
brier.score(obsMat, predMat)

errorest(HWIN~., data=ucnaMnozica, model = naiveBayes, predict = mypredict.generic)



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.nb <- CoreModel(HWIN ~ ., data = ucnaMnozica, model="bayes")
predicted <- predict(cm.nb, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(cm.nb, testnaMnozica, type = "probability")
brier.score(obsMat, predMat)

errorest(HWIN~., data=ucnaMnozica, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="bayes")




#
#
# K-NAJBLIZJIH SOSEDOV
#
#

# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.knn <- CoreModel(HWIN ~ ., data = ucnaMnozica, model="knn", kInNN = 5)
predicted <- predict(cm.knn, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(cm.knn, testnaMnozica, type = "probability")
brier.score(obsMat, predMat)

errorest(HWIN~., data=ucnaMnozica, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="knn")




#
#
# NAKLJUCNI GOZD
#
#

# gradnja modela s pomocjo knjiznice "randomForest"

library(randomForest)

rf <- randomForest(HWIN ~ ., data = ucnaMnozica)
predicted <- predict(rf, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(rf, testnaMnozica, type = "prob")
brier.score(obsMat, predMat)

mypredict.rf <- function(object, newdata){predict(object, newdata, type = "class")}
errorest(HWIN~., data=ucnaMnozica, model = randomForest, predict = mypredict.generic)



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.rf <- CoreModel(HWIN ~ ., data = ucnaMnozica, model="rf")
predicted <- predict(cm.rf, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(cm.rf, testnaMnozica, type = "probability")
brier.score(obsMat, predMat)

errorest(HWIN~., data=ucnaMnozica, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="rf")




#
#
# SVM
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(e1071)

sm <- svm(HWIN ~ ., data = ucnaMnozica)
predicted <- predict(sm, testnaMnozica, type="class")
CA(observed, predicted)

sm <- svm(HWIN ~ ., ucnaMnozica, probability = T)
pred <- predict(sm, testnaMnozica, probability = T)
predMat <- attr(pred, "probabilities")

# v tem konkretnem primeru, vrstni red razredov (stolpcev) v matriki predMat je 
# obraten kot v matriki obsMat. 
colnames(obsMat)
colnames(predMat)

# Iz tega razloga zamenjemo vrstni red stolpcev v matriki predMat
brier.score(obsMat, predMat[,c(2,1)])

errorest(HWIN~., data=ucnaMnozica, model = svm, predict = mypredict.generic)


# gradnja modela s pomocjo knjiznice "kernlab"

library(kernlab)

model.svm <- ksvm(HWIN ~ ., data = ucnaMnozica, kernel = "rbfdot")
predicted <- predict(model.svm, testnaMnozica, type = "response")
CA(observed, predicted)

model.svm <- ksvm(HWIN ~ ., data = ucnaMnozica, kernel = "rbfdot", prob.model = T)
predMat <- predict(model.svm, testnaMnozica, type = "prob")
brier.score(obsMat, predMat)

mypredict.ksvm <- function(object, newdata){predict(object, newdata, type = "response")}
errorest(HWIN~., data=ucnaMnozica, model = ksvm, predict = mypredict.ksvm)

#
#
# UMETNE NEVRONSKE MREZE
#
#

# gradnja modela s pomocjo knjiznice "nnet"

library(nnet)

# implementacija funkcije za ucenje nevronske mreze daje boljse rezultate v primeru,
# ko so ucni primeri normalizirani. 

scale.data <- function(data)
{
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}

# ce normaliziramo ucne primere, je potrebno na enak nacin normalizirati 
# tudi testne primere!

norm.data <- scale.data(rbind(ucnaMnozica,testnaMnozica))
norm.learn <- norm.data[1:nrow(ucnaMnozica),]
norm.test <- norm.data[-(1:nrow(ucnaMnozica)),]

nn <- nnet(HWIN ~ ., data = norm.learn, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, norm.test, type = "class")
CA(observed, predicted)

# v primeru binarne klasifikacije bo funkcija predict vrnila verjetnosti samo enega razreda.
# celotno matriko moramo rekonstruirati sami

pm <- predict(nn, norm.test, type = "raw")
predMat <- cbind(1-pm, pm)
brier.score(obsMat, predMat)

mypredict.nnet <- function(object, newdata){as.factor(predict(object, newdata, type = "class"))}
errorest(HWIN~., data=norm.learn, model = nnet, predict = mypredict.nnet, size = 5, decay = 0.0001, maxit = 10000)





