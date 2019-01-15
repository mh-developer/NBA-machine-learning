################################
# 2.) KLASIFIKACIJA PODATKOV  #
################################

# a) VECINSKI KLASIFIKATOR

# Branje podatkov iz tekstovne datoteke:

regular <- read.table(file="regular.txt", sep=",", header=TRUE)
regular
head(regular)
summary(regular)
str(regular)
names(regular)

------------------------------------------------------------------
  
# Za ucno mnozico vzamemo prvi dve sezoni (2014-2015 in 2015-2016), 
# za testno pa zadnjo sezono (2015-2016)

regular[as.integer(regular$SEASON) == 1,]
regular[as.integer(regular$SEASON) == 2,]
regular[as.integer(regular$SEASON) == 3,]
  
ucnaMnozica <- regular[as.integer(regular$SEASON) < 3, ]
summary(ucnaMnozica)

testnaMnozica <- regular[as.integer(regular$SEASON) == 3, ]
summary(testnaMnozica)

# SEASON in DATE atribut nista vec pomembna za klasifikacijo
regular$SEASON <- NULL
regular$DATE <- NULL

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
gostujoceZmage <- tabelaZmag[2]

# Vecinski razred je prvi
majorityClass <- names(which.max(tabelaZmag))
majorityClass

# Tocnost vecinskega klasifikatorja
# (delez pravilnih napovedi, ce bi vse testne primere klasificirali v vecinski razred)

tocnost <- sum(winns == majorityClass) / length(winns)

klasificirajZVecinskim <- function(domacin, gost) {
  if (domacin %in% regular$HOME && gost %in% regular$AWAY) {
    paste("Domace mostvo zmaga z verjetnostjo", sum(winns == majorityClass) / length(winns)) 
  } else {
    "Ime ene od ekip ni v mnozici podatkov."
  }
}

klasificirajZVecinskim("LAL", "GSW")
klasificirajZVecinskim("Mitja", "Anze")

# b) ODLOCITVENO DREVO

#Nalozimo knjiznico "rpart".
library(rpart)

# Učni in testni množici dodamo atribut HWIN, ki pove, ali je domača ekipa zmagala ali ne
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
names(ucnaMnozica)
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

# Klic funkcije za klasifikacijsko tocnost
CA(observed, predicted)


# Izračun senzitivnosti in specifičnosti
# senzitivnost ("0" je pozitivni razred)

senz <- 506 / (506 + 37)
senz

# specificnost ("0" je pozitivni razred)

spec <- 681 / (681 + 6)
spec

# Druga oblika napovedi modela (nastavitev "prob") vraca verjetnosti, 
# da posamezni testni primer pripada dolocenemu razredu.

# Napovedane verjetnosti pripadnosti razredom (dobimo matriko)
predMat <- predict(dt, testnaMnozica, type = "prob")
predMat

# Prave verjetnosti pripadnosti razredom (dejanski razred ima verjetnost 1.0 ostali pa 0.0)
obsMat <- model.matrix( ~ HWIN-1, testnaMnozica)
obsMat

# Funkcija za izracun Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

# Izracun Brierjeve mere za napovedi drevesa
brier.score(obsMat, predMat)

# c) PRECNO PREVERJANJE

install.packages(c("ipred", "prodlim", "CORElearn"))
library(ipred)

# Podatkom dodamo atribut HWIN, ki pove, ali je domača ekipa zmagala ali ne
winns <- ifelse(regular$HPTS > regular$APTS, 1, 0)
regular$HWIN <- as.factor(winns)
table(regular$HWIN)

# funkcija "errorest" potrebuje poleg formule modela in ucnih primerov se dva vhodna parametra: 
# funkcijo za gradnjo modela ter funkcijo za generiranje napovedi tega modela.
# Funkcijo za generiranje napovedi napisemo tako, da klicemo obicajno funkcijo "predict" 
# s parametrom type="class", ter na ta nacin zahtevamo napovedi v obliki vektorja oznak razredov.

mypredict <- function(object, newdata){predict(object, newdata, type = "class")}

res <- errorest(HWIN~., data=regular, model = rpart, predict = mypredict)
res

# klasifikacijska tocnost
1-res$error

# metoda "izloci enega"
res <- errorest(HWIN~., data=regular, model = rpart, predict = mypredict, est.para=control.errorest(k = nrow(regular)))
res

# klasifikacijska tocnost
1-res$error

#
# odlocitveno drevo (CORElearn)
#

library(CORElearn)

# Model bomo zgradili s pomocjo funkcije "CoreModel", ki potrebuje informacijo o tem, kateri tip modela naj zgradi.
# Funkcijo za gradnjo modela napisemo tako, da klicu funkcije "CoreModel" dodamo parameter za izbiro tipa modela.
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}

# Funkcijo za generiranje napovedi napisemo tako, da iz dobljenih napovedi modela obdrzimo samo oznake razredov.
# Ko model vrne zahtevane napovedi, ga ne potrebujemo vec - zato ga odstranimo iz pomnilnika.
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

# 10-kratno precno preverjanje 
res <- errorest(HWIN~., data=regular, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
1-res$error

# metoda "izloci enega"
res <- errorest(HWIN~., data=regular, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree", est.para=control.errorest(k = nrow(regular)))
1-res$error








# Funkcija za izracun Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

# Funkcija za izracun klasifikacijske tocnosti
CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}

# metoda precnega preverjanja je implementirana v knjiznici "ipred"
library(ipred)

# pomozne funkcije, ki jih potrebujemo za izvedbo precnega preverjanja
mypredict.generic <- function(object, newdata){predict(object, newdata, type = "class")}
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}




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









