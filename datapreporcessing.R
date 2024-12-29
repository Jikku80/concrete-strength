train <- concrete_strength_train
test <- concrete_strength_test
combined <- rbind(train, test)
View(combined)
head(combined)
str(combined)
min(combined$Strength)
max(combined$Strength)
range <- function(x){
  rnge <- max(x) - min(x)
  return(rnge)
}
range(combined$Strength)
concStrength <- combined$Strength
concStrength
mean(concStrength)
median(concStrength)
quantile(concStrength, 0.25)
quantile(concStrength, 0.75)
sd(concStrength)
var(concStrength)
summary(combined)
by(combined, concStrength, summary)
IQR(concStrength)
strength_r <- round(concStrength)
table(strength_r)
table(combined$Strength)
names(combined)
table(combined$Strength, combined$Age)
aggregate(Strength ~ Age, data = combined, mean)
library("psych")
describe(combined)
coefficientVariation <- sd(concStrength) / mean(concStrength)
coefficientVariation
tab <- table(concStrength)
sort(tab, decreasing = TRUE)
descr(combined)
hist(combined$Strength)
library("ggplot2")
ggplot(combined, aes(x = Strength)) + geom_bar()
ggplot(combined, aes(x = Strength)) + geom_bar(aes(y = (..count..)/sum(..count..))) + xlab("Concrete Strength") + scale_y_continuous(labels = scales::percent, name = "Proportion") + facet_grid(Cement ~ Age) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data = combined, aes(x=Strength)) + geom_histogram(binwidth = 0.2, color="gray", aes(fill=Strength)) + xlab("Strength (mpa)") + ylab("Frequency") + theme(legend.position = "none") + ggtitle("Histogram of Concrete Strength")
ggplot(combined, aes(x=Strength, colour=Strength, fill=Strength)) + geom_density(alpha=.3)
boxplot(concStrength)
dnsity <- density(combined$Strength)
plot(dnsity)
boxplot(combined)
colSums(sapply(combined, is.na))

plot(combined$Cement, combined$Strength, main = "Cement Strength Scatter plot", xlab="Cement (kg/m3)", ylab="Strength (Mpa)")

histogm <- lapply(concreteImputed[, c("Cement", "Blast.Furnace.Slag", "Fly.Ash", "Water", "Superplasticizer", "Coarse.Aggregate", "Fine.Aggregate", "Age", "Strength")], function(x) ggplot(data = combined, aes(x=x)) + geom_histogram(fill = "green", color = "black", bins = 30) + labs(title = paste("Histogram", names(x))) + theme_minimal())
scatter <- ggplot(data = concreteImputed, aes(x = Cement, y = Strength)) + geom_point() + labs(title = "Cement vs. Strength")
boxplot <- ggplot(data = concreteImputed, aes(x = factor(Age), y = Strength)) + geom_boxplot(fill = "green", color = "black") + labs (title = "strength by Age", x = "Age", y = "Strength" ) 
install.packages("gridExtra")
library(gridExtra)
grid.arrange(histogm[[1]], histogm[[2]], histogm[[3]], histogm[[4]], histogm[[5]], histogm[[6]], histogm[[7]], histogm[[8]], histogm[[9]], scatter, boxplot, ncol =2)


correlation_matrix <- cor(combined[, -ncol(combined)])

#pca of combined dataset
library(mice)
imput_rf <- mice(data=combined,m=1,method="rf",maxit=10)
completed_imput <- complete(imput_rf)
concmatrix=as.matrix(completed_imput[1:9])

conc_pca <- prcomp(concmatrix)
summary(conc_pca)

pc_scores<-as.data.frame(conc_pca$x)
dim(pc_scores)

library('ggplot2')
ggplot(pc_scores, aes(x = PC1, y = PC2)) +
  geom_point()

loadings<-conc_pca$rotation
loadings

#Extract the loadings for the first PC
loadings<-conc_pca$rotation[,1]
loadings<-abs(loadings)
#sort PCA in the decreasing order
loadings<-sort(loadings,decreasing = T)
loadings
#select 4 top names of the original variables based on the highest weights
top4<-names(loadings[1:4])
conc_pca$rotation[top4, 1]

#Extract the loadings for the second PC
loadings2<-conc_pca$rotation[,2]
loadings2<-abs(loadings2)
loadings2<-sort(loadings2,decreasing = T)
loadings2
top4_2<-names(loadings2[1:4])
conc_pca$rotation[top4_2, 2]


top_cs<-unique(c(top4, top4_2))
pc_loadings <-as.data.frame(conc_pca$rotation[top_cs,1:2])


#Plot the relation in ggplo2

pc_loadings$Cement<-rownames(pc_loadings)
ggplot(pc_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               col = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = Cement),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))
cor(concreteImputed[,1:9])

#********* Data Pre Processing **********

#missing values
install.packages('mice')
library("mice")
is.na(combined)
missing_data <- apply(combined, 2, function(x) (sum(is.na(x)))/nrow(combined))
avgMissing <- mean(missing_data) * 100
md.pattern(combined)
par(mar=c(1,1,1,1))
lapply(names(combined), function(col){
  hist(combined[[col]], main=col, xlab=col, col = "gray", border = "black")
})
imputed_data <- mice(data = combined, m = 1, method="rf", maxit=10)
concreteImputed <- complete(imputed_data)
View(concreteImputed)
missingSum <- sum(is.na(concreteImputed))
missingSum
md.pattern(concreteImputed)

for(i in 1:9){
  boxplot(concreteImputed[i], col="blue", main="Box Plot", xlab=colnames(concreteImputed)[[i]])
}

#outlier using IQR
outliersDF <- data.frame()
cols = ncol(concreteImputed)
for(i in 1:cols){
  Q1 <- quantile(concreteImputed[[i]], 0.25)
  Q3 <- quantile(concreteImputed[[i]], 0.75)
  IQR <- IQR(concreteImputed[[i]])
  
  lowWhisker <- Q1 - 1.5 * IQR
  upWhisker <- Q3 + 1.5 * IQR
  
  outliersDF <- rbind(outliers, concreteImputed[concreteImputed[[i]] < lowWhisker | concreteImputed[[i]] > upWhisker, ])
}
outliers <- unique(outliersDF)
outliers
ouliersNull <- concreteImputed[!(rownames(concreteImputed) %in% rownames(outliers)), ]
View(ouliersNull)
summary(ouliersNull)

#Multicollinearity
varStrength <- ouliersNull[9]
nullStrength <- ouliersNull[c(1:7)]
corStrength <- cor(varStrength, nullStrength)
legend <- colnames(ouliersNull)[-8][-8]
par(mfrow=c(1,1))
barplot(corStrength, beside = TRUE, col=c("purple", "orange", "lightblue", "pink", "green", "brown", "yellow"), xlab="Concrete's Data", names=legend, ylim=c(-0.6,0.6))

#investigate variables
variance <- apply(ouliersNull[, sapply(ouliersNull, is.numeric)], 2, var)
variance
varianceTotal <- sum(variance)
threshold <- 0.05 * varianceTotal
lowVariance <- names(variance[variance < threshold])
lowVariance
constVariance  <- sapply(ouliersNull, function(x) length(unique(x))) == 1
constVariance <- names(ouliersNull[constVariance])
noises <- c(lowVariance, constVariance)
noises
noiseNull <- ouliersNull[, !(names(ouliersNull) %in% noises)]
noiseNull

#Scaling
numCols <- sapply(ouliersNull, is.numeric)
scaling <- scale(ouliersNull[, numCols], center = TRUE, scale = TRUE)
scaling
par(mar = c(1,1,1,1))
for(i in 1:ncol(scaling)){
  hist(scaling[,i], main=colnames(scaling)[i], xlab="Scaled Variable Values")
}
