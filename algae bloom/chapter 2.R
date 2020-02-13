library("DMwR")
View(algae)
algae <- read.table('Analysis.txt', 
                    header = F, 
                    dec = '.',
                    col.names = c('season','size','speed','mxPH','mnO2','Cl',
                                  'NO3','NH4','oPO4','PO4','Chla','a1','a2',
                                  'a3','a4', 'a5','a6','a7'),
                    na.strings=c('XXXXXXX')
)
View(algae)
summary(algae)


library(car)
par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab='', main='Histogram of maximum pH value',ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))
qqPlot(algae$mxPH,main='Normal QQ plot of maximum pH')
par(mfrow=c(1,1))

boxplot(algae$oPO4, ylab = "Orthophosphate (oPO4)")
rug(jitter(algae$oPO4), side = 2)
abline(h = mean(algae$oPO4, na.rm = T), lty = 2)

# indicate outliers

plot(algae$NH4,xlab = "")
abline(h = mean(algae$NH4, na.rm = T), lty = 1)
abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2 )
abline(h = median(algae$NH4, na.rm = T), lty = 3)
identify(algae$NH4) #show row number in the algae data frame

plot(algae$NH4, xlab = "")
clicked.lines <- identify(algae$NH4)
algae[clicked.lines, ]

algae[!is.na(algae$NH4)&algae$NH4 > 19000, ]

#influence
library(lattice)
bwplot(size ~ a1, data=algae, ylab='River Size', xlab='Algral A1')


library("Hmisc")
bwplot(size~a1,data = algae, panel = panel.bpplot, probs=seq(.01,.49, by=.01), datadensity = TRUE,
       ylab = 'River size', xlab = 'Algal A1')

minO2 <- equal.count(na.omit(algae$mnO2), number=4,overlap=1/5)
stripplot(season~a3|minO2,data=algae[!is.na(algae$mnO2), ])

#Unknown value
#Removing the Observations with Unknown Values
library(DMwR)
data(algae)

algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae), ])
algae <- na.omit(algae)
data("algae")
algae <- algae[-c(62,199), ]

data("algae")
apply(algae, 1 , function(x) sum(is.na(x)))

data("algae")
manyNAs(algae, 0.2)
algae <- algae[-manyNAs(algae), ]

#Filling in the Unknowns with the Most Frequent Values
#hello github
data("algae")
algae[48, "mxPH"] <- mean(algae$mxPH, na.rm = T)
algae[48, ]

algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm = T)

data(algae)
algae <- algae[-manyNAs(algae), ]
algae <- centralImputation(algae)

#Filling in the Unknown Values by Exploring Correlations
symnum(cor(algae[, 4:18], use = "complete.obs"))

# find the form of the linear correlation
data("algae")
algae <- algae[-manyNAs(algae), ]
lm(PO4 ~ oPO4, data = algae)

algae[28, "PO4"] <- 42.897 + 1.293 * algae[28,"oPO4"]
algae[28, ]

# use the linear relationship to fill all the unknowns
data("algae")
algae <- algae[-manyNAs(algae), ]
fillPO4 <- function(oP) {
  if (is.na(oP))
    return(NA)
  else return(42.897 + 1.293*oP)
}
algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4), "oPO4"], fillPO4)

fillPO4(6.4)
#lattice R
histogram(~mxPH | season, data = algae)
# the natural temporal ordering of the seasons
algae$season <- factor(algae$season, level = c("spring", "summer", "autumn", "winter"))
histogram(~mxPH | season, data = algae)

histogram(~mxPH | size, data = algae)
stripplot(size~mxPH|speed, data = algae, jitter = T)

#Filling in the Unknown Values by Exploring Similarities between Cases
data("algae")
algae <- algae[-manyNAs(algae), ]

algae <- knnImputation(algae, k = 10)
algae <- knnImputation(algae, k = 10, meth = "median")

#2.6 Obtaining Prediction Models
# Multiple Linear Regression

data("algae")
algae <- algae[-manyNAs(algae), ]
clean.algae <- knnImputation(algae, k = 10)
lm.a1 <- lm(a1~., data = clean.algae[,1:12])
summary(lm.a1)
