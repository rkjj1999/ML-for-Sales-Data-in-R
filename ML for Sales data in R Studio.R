data = read.csv("C:/Users/ronit/Downloads/data-marketing-budget-12mo.csv", header = T,
                colClasses= c("numeric", "numeric", "numeric"))
head(data)
simple.fit= lm(Sales~Spend, data= data)
summary(simple.fit)


multi.fit= lm(Sales~Spend+Month, data = data)
summary(multi.fit)



layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot

plot(simple.fit$resid~data$Spend[order(data$Spend)],
     main="Spend x Residuals\nfor Simple Regression",
     xlab="Marketing Spend", ylab="Residuals")
abline(h=0,lty=2)

#Histogram of Residuals
hist(simple.fit$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(simple.fit$resid)
qqline(simple.fit$resid)

plot(simple.fit$resid~data$Spend[order(data$Spend)],
     main="Spend x Residuals\nfor Simple Regression",
     xlab="Marketing Spend", ylab="Residuals")
abline(h=0,lty=2)
boxplot()



install.packages("pheatmap")
library(pheatmap)
pheatmap(data[,1:3], cluster_rows = TRUE, cluster_cols = TRUE, clustering_method = "complete")


