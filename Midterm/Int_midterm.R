library(psych)
library(irr)
library(lavaan)
library(semPlot)

#PART ONE:

CTT <- read.csv("Dichotomous Data.csv", header = TRUE)
names <- colnames(CTT)

diff1 <- mean(CTT$Item_1)*100 # I realized that 'colmeans()' could be used to do this faster; didn't feel like changing it though
diff2 <- mean(CTT$Item_2)*100
diff3 <- mean(CTT$Item_3)*100
diff4 <- mean(CTT$Item_4)*100
diff5 <- mean(CTT$Item_5)*100
diff6 <- mean(CTT$Item_6)*100
diff7 <- mean(CTT$Item_7)*100
diff8 <- mean(CTT$Item_8)*100
diff9 <- mean(CTT$Item_9)*100
diff10 <- mean(CTT$Item_10)*100
diff11 <- mean(CTT$Item_11)*100
diff12 <- mean(CTT$Item_12)*100
diff13 <- mean(CTT$Item_13)*100
diff14 <- mean(CTT$Item_14)*100
diff15 <- mean(CTT$Item_15)*100

Difficulty <- as.matrix(c(diff1, diff2, diff3, diff4, diff5, diff6, diff7, diff8, diff9, diff10, diff11, diff12, diff13, diff14, diff15))

CTT$Total <- rowSums(CTT, na.rm = TRUE)
dis1 <- biserial(CTT$Total - CTT$Item_1, CTT$Item_1)
dis2 <- biserial(CTT$Total - CTT$Item_2, CTT$Item_2)
dis3 <- biserial(CTT$Total - CTT$Item_3, CTT$Item_3)
dis4 <- biserial(CTT$Total - CTT$Item_4, CTT$Item_4)
dis5 <- biserial(CTT$Total - CTT$Item_5, CTT$Item_5)
dis6 <- biserial(CTT$Total - CTT$Item_6, CTT$Item_6)
dis7 <- biserial(CTT$Total - CTT$Item_7, CTT$Item_7)
dis8 <- biserial(CTT$Total - CTT$Item_8, CTT$Item_8)
dis9 <- biserial(CTT$Total - CTT$Item_9, CTT$Item_9)
dis10 <- biserial(CTT$Total - CTT$Item_10, CTT$Item_10)
dis11 <- biserial(CTT$Total - CTT$Item_11, CTT$Item_11)
dis12 <- biserial(CTT$Total - CTT$Item_12, CTT$Item_12)
dis13 <- biserial(CTT$Total - CTT$Item_13, CTT$Item_13)
dis14 <- biserial(CTT$Total - CTT$Item_14, CTT$Item_14)
dis15 <- biserial(CTT$Total - CTT$Item_15, CTT$Item_15)
Discrimination <- as.matrix(c(dis1, dis2, dis3, dis4, dis5, dis6, dis7, dis8, dis9, dis10, dis11, dis12, dis13, dis14, dis15))

CTTAnalysis <- cbind(Difficulty, Discrimination)
colnames(CTTAnalysis) <- c("Difficulty", "Discrimination")
rownames(CTTAnalysis) <- names

# Create Discrimination Index

quantiles <- quantile(CTT$Total, probs = c(.25, .33, .66, .75)) # Quantiles
lowExtreme <- CTT[CTT$Total <= 6,]
highExtreme <- CTT[CTT$Total >= 10,]

DiscIndex <- data.frame(cbind(colMeans(lowExtreme), colMeans(highExtreme), (colMeans(highExtreme) - colMeans(lowExtreme))))
colnames(DiscIndex) <- c("Low Group", "High Group", "Difference")

#PART TWO:

EFA <- read.csv("EFA Data.csv", header = TRUE)

fa.parallel(EFA, fm="ml", fa="fa", main="Midterm: Factor Analysis (ML)", n.iter=1000, SMC=TRUE, show.legend=TRUE)
print(fa.parallel(EFA, fm="ml", fa="fa", main="Midterm: Factor Analysis (ML)", n.iter=1000, SMC=TRUE, show.legend=TRUE))
fa(EFA, nfactors=3, rotate="oblimin", SMC=TRUE, max.iter=1000)

fact1 <- data.frame(cbind(EFA$Item6, EFA$Item9))
colnames(fact1) <- c("Item6", "Item9")
fact2 <- data.frame(cbind(EFA$Item1, EFA$Item2))
colnames(fact2) <- c("Item1", "Item2")
fact3 <- data.frame(cbind(EFA$Item4, EFA$Item5))
colnames(fact3) <- c("Item4", "Item5")

alpha1 <- alpha(fact1, title="Factor 1", check.keys = TRUE)
alpha2 <- alpha(fact2, title="Factor 2", check.keys = TRUE)
alpha3 <- alpha(fact3, title="Factor 3", check.keys = TRUE)

CFA <- read.csv("CFA Data.csv", header = TRUE)

Model3cut <-
'fact1 =~ Item6 + Item9
fact2 =~ Item1 + Item2
fact3 =~ Item4 + Item5
fact1~~fact2
fact2~~fact3
fact1~~fact3'

Model3all <-
'fact1 =~ Item6 + Item7 + Item8 + Item9 + Item10
fact2 =~ Item1 + Item2
fact3 =~ Item3 + Item4 + Item5
fact1~~fact2
fact2~~fact3
fact1~~fact3'

FitCut <- cfa(Model3cut, data=CFA, std.lv=TRUE, orthogonal=FALSE)
FitAll <- cfa(Model3all, data=CFA, std.lv=TRUE, orthogonal=FALSE)

summary(FitCut, standardized=TRUE, fit.measure=TRUE)
summary(FitAll, standardized=TRUE, fit.measure=TRUE)