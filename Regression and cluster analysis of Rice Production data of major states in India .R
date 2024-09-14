Year<-c(2017-2017,2018-2017,2019-2017,2020-2017,2021-2017,2022-2017)
production_WB<-c(15.3,14.97,16.05,15.57,16.65,16.76)
production_Punjab<-c(11.59,13.38,12.82,11.78,12.18,12.89)
production_UP<-c(13.75,13.27,13.34,15.52,15.66,15.27)
production_AP<-c(7.45,8.18,8.25,8.64,7.89,7.79)
production_Bihar<-c(8.24,7.91,6.04,6.05,6.88,7.06)
production_TN<-c(2.37,7.28,6.45,7.18,7.28,8.07)
production_Odisha<-c(8.33,6.53,7.31,8.04,8.77,9.14)
production_Tel<-c(5.17,6.25,6.7,7.34,7.7,12.3)
production_as<-c(4.73,5.16,5.14,5.1,5.26,5.27)
production_chat<-c(8.05,4.73,6.53,6.5,7.16,7.9)

##############

plot(Year,production_WB,ylim = c(0,18),xlab = "Year",ylab = "Production",main = "Scatter Plot and line diagram of production of rice in different states over the years")
abline(lm(production_WB~Year),col='green',lwd=4)
par(new=TRUE)
plot(Year,production_Punjab,ylim=c(0,18),xlab = "",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_Punjab~Year),col="blue",lwd=4)
par(new=TRUE)
plot(Year,production_UP,ylim = c(0,18),xlab="",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_UP~Year),col="orange",lwd=4)
par(new=TRUE)
plot(Year,production_AP,ylim=c(0,18),xlab = "",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_AP~Year),col="violet",lwd=4)
par(new=TRUE)
plot(Year,production_Bihar,ylim = c(0,18),xlab = "",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_Bihar~Year),col="yellow",lwd=4)
par(new=TRUE)
plot(Year,production_TN,ylim = c(0,18),xlab = "",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_TN~Year),col="sea green",lwd=4)
par(new=TRUE)
plot(Year,production_Odisha,ylim = c(0,18),xlab = "",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_Odisha~Year),col="red",lwd=4)
par(new=TRUE)
plot(Year,production_Tel,ylim = c(0,18),xlab = "",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_Tel~Year),col="skyblue",lwd=4)
par(new=TRUE)
plot(Year,production_as,ylim = c(0,18),xlab = "",ylab = "",main = "",xaxt="n",yaxt="n")
abline(lm(production_as~Year),col="pink",lwd=4)
par(new=TRUE)
plot(Year,production_chat,ylim = c(0,18),xlab = "",ylab = "",xaxt="n",yaxt="n")
abline(lm(production_chat~Year),col="purple",lwd=4)
legend("bottom",c("West Bengal","Punjab","Uttar Pradesh","Andhra Pradesh","Bihar","Tamil Nadu","Odisha","Telengana","Assam","Chattisgarh"),fill=c("green","blue","orange","violet","yellow","sea green","red","skyblue","pink","purple"))

#############

Rice_Production_Data <- data.frame(State = c("West Bengal", "Punjab", "Uttar Pradesh", "Andhra Pradesh", "Bihar", "Tamil Nadu", "Odisha", "Telengana", "Assam", "Chattisgarh"),
  `0` = c(15.30, 11.59, 13.75, 7.45, 8.24, 2.37, 8.33, 5.17, 4.73, 8.05),
  `1` = c(14.97, 13.38, 13.27, 8.18, 7.91, 7.28, 6.53, 6.25, 5.16, 4.73),
  `2` = c(16.05, 12.82, 13.34, 8.25, 6.04, 6.45, 7.31, 6.7, 5.14, 6.53),
  `3` = c(15.57, 11.78, 15.52, 8.64, 6.05, 7.18, 8.04, 7.34, 5.1, 6.5),
  `4` = c(16.65, 12.18, 15.66, 7.89, 6.88, 7.28, 8.77, 7.7, 5.26, 7.16),
  `5` = c(16.76, 12.89, 15.27, 7.79, 7.06, 8.07, 9.14, 12.3, 5.27, 7.9))
Rice_Production_Data

range_years<-seq(min(Year),max(Year),1)

###############
model_wb <- lm(production_WB ~ Year)
model_punjab <- lm(production_Punjab ~ Year)
pred_wb <- predict(model_wb, newdata = data.frame(years = range_years))
pred_punjab <- predict(model_punjab, newdata = data.frame(years = range_years))
distance <- (sum((pred_wb - pred_punjab)^2))

####################

models <- lapply(1:nrow(Rice_Production_Data), function(i) {
  lm(as.numeric(Rice_Production_Data[i, -1]) ~ Year)
})

predictions <- sapply(models, function(model) {
  predict(model, newdata = data.frame(years = range_years))
})


attach(Rice_Production_Data)
n <- length(State)
distance_matrix <- matrix(0, n, n)

for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    distances <- (predictions[, i] - predictions[, j])^2
    sum_distance <- sum(distances)
    distance_matrix[i, j] <- sum_distance
    distance_matrix[j, i] <- sum_distance
  }
}
rownames(distance_matrix) <- State
colnames(distance_matrix) <- State
distance_matrix
View(distance_matrix)

###################

class(distance_matrix)
dim(distance_matrix)
hclust(as.dist(distance_matrix))
plot(hclust(as.dist(distance_matrix)))
cutree(hclust(as.dist(distance_matrix)),k=2)

install.packages("knitr")
library(knitr)
R.home("bin")


tinytex::reinstall_tinytex()
