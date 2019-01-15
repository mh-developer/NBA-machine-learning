################################
# 3.) REGRESIJA                #
################################

install.packages(c("randomForest", "e1071", "kknn", "nnet", "ipred"))

# Branje podatkov iz tekstovne datoteke:

regular <- read.table(file="regular.txt", sep=",", header=TRUE)
regular
head(regular)
summary(regular)
str(regular)
names(regular)

# Prave razlike med tockami domacinov in gostov:

regular$PDIFF <- (regular$HPTS - regular$APTS)

------------------------------------------------------------------
  
# Za ucno mnozico vzamemo prvi dve sezoni (2014-2015 in 2015-2016), 
# za testno pa zadnjo sezono (2015-2016)
  
regular[as.integer(regular$SEASON) == 1,]
regular[as.integer(regular$SEASON) == 2,]
regular[as.integer(regular$SEASON) == 3,]

ucnaMnozica <- regular[as.integer(regular$SEASON) < 3, ]
ucnaMnozica

testnaMnozica <- regular[as.integer(regular$SEASON) == 3, ]
testnaMnozica

# SEASON in DATE atribut nista vec pomembna za regresijo
regular$SEASON <- NULL
regular$DATE <- NULL

ucnaMnozica$SEASON <- NULL
ucnaMnozica$DATE <- NULL

testnaMnozica$SEASON <- NULL
testnaMnozica$DATE <- NULL

# izrisimo graf razlik v tockah med domao in gostujoco ekipo
hist(regular$PDIFF, main="Graf razlik med ekipama", xlab="Razlika v tockah", ylab="Zaporedna tekma", col = "red", ylim = c(0, 1300))

# linearni model
regular$ID <- seq.int(nrow(regular))
razlike <- data.frame(regular$ID, regular$PDIFF)
names(razlike) <- c("ID", "PDIFF")
plot(razlike, main="Graf razlik med ekipama", xlab="Zaporedna tekma", ylab="Razlika v tockah", cex = 0.5)

lm1 <- lm(razlike$PDIFF ~ razlike$ID, razlike)
lm1
abline(lm1, col = "red", lwd = 3)
regular$ID <- NULL

####################################################################
#
# Mere za ocenjevanje ucenja v regresiji
#
####################################################################

mae <- function(observed, predicted)
{
  mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
  sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
  mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
  sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}

#
# linearni model
#

observed <- testnaMnozica$PDIFF
lm.model <- lm(PDIFF ~ HOME+AWAY, data = ucnaMnozica)
lm.model

predicted <- predict(lm.model, testnaMnozica)
mae(observed, predicted)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

#
# regresijsko drevo
#

library(rpart)

rt.model <- rpart(PDIFF ~ HOME+AWAY+HPTS+APTS, ucnaMnozica)
predicted <- predict(rt.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

plot(rt.model);text(rt.model, pretty = 0)


# nastavitve za gradnjo drevesa
rpart.control()

# zgradimo drevo z drugimi parametri
rt <- rpart(PDIFF ~ ., ucnaMnozica, minsplit = 100)
plot(rt);text(rt, pretty = 0)

# parameter cp kontrolira rezanje drevesa
rt.model <- rpart(PDIFF ~ HOME+AWAY+HPTS+APTS, ucnaMnozica, cp = 0)
plot(rt.model);text(rt.model, pretty = 0)

# drugacno drevo
rt <- rpart(PDIFF ~ H2PM+H2PA+H3PM+H3PA+HFTM+HFTA+HORB+HDRB+HAST+HSTL+HTOV+
              HBLK+HPF+A2PM+A2PA+A3PM+A3PA+AFTM+AFTA+AORB+
              ADRB+AAST+ASTL+ATOV+ABLK+APF, ucnaMnozica, minsplit = 100)
plot(rt);text(rt, pretty = 0)


# izpisemo ocenjene napake drevesa za razlicne vrednosti parametra cp
printcp(rt.model)

# drevo porezemo z ustrezno vrednostjo cp, pri kateri je bila minimalna napaka
rt.model2 <- prune(rt.model, cp = 0.02)
plot(rt.model2);text(rt.model2, pretty = 0)
predicted <- predict(rt.model2, testna)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))


# regresijska drevesa lahko gradimo tudi s pomocjo knjiznice CORElearn
library(CORElearn)

rt.core <- CoreModel(PDIFF ~ HOME+AWAY+HPTS+APTS, data=ucnaMnozica, model="regTree", modelTypeReg = 1)
plot(rt.core, ucnaMnozica)
predicted <- predict(rt.core, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))



modelEval(rt.core, testnaMnozica$PDIFF, predicted)

# preveliko drevo se prevec prilagodi podatkom in slabse napoveduje odvisno spremeljivko
rt.core2 <- CoreModel(PDIFF ~ HOME+AWAY+HPTS+APTS, data=ucnaMnozica, model="regTree",  modelTypeReg = 1, minNodeWeightTree = 1, selectedPrunerReg = 0)
plot(rt.core2, ucnaMnozica)
predicted <- predict(rt.core2, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

# drevo z linearnim modelom v listih se lahko prevec prilagodi ucnim primerom
rt.core3 <- CoreModel(PDIFF ~ HOME+AWAY+HPTS+APTS, data=ucnaMnozica, model="regTree",  modelTypeReg = 3, selectedPrunerReg = 2)
plot(rt.core3, ucnaMnozica)
predicted <- predict(rt.core3, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

# model se prilega ucnim podatkom, ker ima prevec parametrov.
# rezultat lahko izboljsamo tako, da poenostavimo model (npr. uporabimo manj atributov)

rt.core4 <- CoreModel(PDIFF ~ HOME+AWAY+HPTS+APTS, data=ucnaMnozica, model="regTree", modelTypeReg = 3)
plot(rt.core4, testnaMnozica)
predicted <- predict(rt.core4, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

#
# nakljucni gozd
#

library(randomForest)

rf.model <- randomForest(PDIFF ~ HOME+AWAY+HPTS+APTS, ucnaMnozica)
predicted <- predict(rf.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

# Vsi atributi

rf.model <- randomForest(PDIFF ~ ., ucnaMnozica)
predicted <- predict(rf.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

#
# svm
#

library(e1071)

svm.model <- svm(PDIFF ~ HOME+AWAY, ucnaMnozica)
predicted <- predict(svm.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

# Vsi atributi

svm.model <- svm(PDIFF ~ ., ucnaMnozica)
predicted <- predict(svm.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)


#
# k-najblizjih sosedov
#

library(kknn)

knn.model <- kknn(PDIFF ~ HOME+AWAY, ucnaMnozica, testnaMnozica, k = 5)
predicted <- fitted(knn.model)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

# Vsi atributi

knn.model <- kknn(PDIFF ~ ., ucnaMnozica, testnaMnozica, k = 5)
predicted <- fitted(knn.model)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)


#
# nevronska mreza
#

library(nnet)

#
# pomembno!!! 
# za regresijo je potrebno nastaviti linout = T

# zaradi nakljucne izbire zacetnih utezi bo vsakic nekoliko drugacen rezultat
# zato je dobro, da veckrat naucimo mrezo in zadrzimo model, ki se je najboljse obnesel

#set.seed(6789)
nn.model <- nnet(PDIFF ~ HOME+AWAY, ucnaMnozica, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)


# Vsi atributi

nn.model <- nnet(PDIFF ~ ., ucnaMnozica, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

#######################################################################################
#
# Izbira podmnozice atributov
#
#######################################################################################

#
# ocena kvalitete atributov pri regresijskih problemih
#

sort(attrEval(PDIFF ~ HOME+AWAY, ucnaMnozica, "MSEofMean"), decreasing = TRUE)
sort(attrEval(PDIFF ~ HOME+AWAY, ucnaMnozica, "RReliefFexpRank"), decreasing = TRUE)


# model lahko dodatno izboljsamo z izbiro ustrezne podmnozice atributov

rt.core <- CoreModel(PDIFF ~ ., data=ucnaMnozica, model="regTree", selectionEstimatorReg="MSEofMean")
plot(rt.core, ucnaMnozica)
predicted <- predict(rt.core, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))


# pri izbiri podmnozice atributov si lahko pomagamo z wrapper metodo
source("wrapperReg.R")
wrapperReg(ucnaMnozica, "PDIFF", folds=10, selectionEstimatorReg="MSEofMean")
