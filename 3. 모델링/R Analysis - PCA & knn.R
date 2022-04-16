# 데이터 세팅
setwd("C:/Users/박소담/Desktop/통계청대회/Newtown")
load("NewTown2")
town2

# 변수간의 상관관계 확인
town2_1 <- town2[, -c(1:2)]
town2_1
x <- cor(town2_1[, -1])

if(!require(corrplot)) {
  install.packages("corrplot")
}
library(corrplot)
corrplot(x) # 유난히 큰 상관관계를 여러곳에서 발견할 수 있었다. 변수끼리 상관성이 높기에 주성분분석을 해야함

# 변수선택 방법을 좀 더 강구할 것!!
# 종속변수를 단순히 factor형으로 말고, pop_pr 도 생각해볼 것

# 주성분 분석
town.pca1 <- prcomp(town2_1[, -1], center = T, scale = T)
summary(town.pca1) # 3번째 주성분 부터 누적설명력이 80%를 넘기 때문에 PCA3까지 선택

# 주성분들의 고유벡터
town.pca1$rotation

# Eigenvalues(고유값) 확인
plot(town.pca1)

# 1, 2번째 주성분의 좌표에 그린 그림
biplot(town.pca1)

# 주성분계수와 데이터 내적
data <- as.matrix(town2_1[, -1]) %*% town.pca1$rotation
data1 <- as.data.frame(cbind(town2_1$success, as.data.frame(data)))
colnames(data1)[1] <- "success"
data1$success <- as.factor(data1$success)

# PC3까지 선택한 데이터 만들기
if(!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)
pcadata1 <- data1 %>% select(success, PC1, PC2, PC3)


# 교차타당성 기법을 이용하여 knn분류기 만들기 - 적정 k의 갯수 찾기
if(!require(class)) {
  install.packages("class")
}
library(class)

acc <- matrix(data = NA, nrow = 11, ncol = 10)
for(i in 1:nrow(town2_1)) {
  for(j in 1:10) {
  train <- pcadata1[-i, ]
  test <- pcadata1[i, ]
  
  pred <- knn(train = train[, -1], test = test[-1], cl = train[, 1], k = j, prob = T)
  acc[i, j] <- mean(pred == test[, 1])
  }
}
acc <- as.data.frame(acc)

# accuracy 확인
accuracy <- NULL
for(i in 1:10) {
  accuracy[i] <- mean(acc[, i])
}
accuracy
plot(1:10, accuracy, type = "l")


## 변수 선택 후 knn
town2_2 <- town2_1[,-c(8, 11, 13, 14)]
x <- cor(town2_2[, -1])

library(corrplot)
corrplot(x) # 유난히 큰 상관관계를 여러곳에서 발견할 수 있었다. 변수끼리 상관성이 높기에 주성분분석을 해야함

# 변수선택 방법을 좀 더 강구할 것!!
# 종속변수를 단순히 factor형으로 말고, pop_pr 도 생각해볼 거

# 주성분 분석
town.pca2 <- prcomp(town2_2[, -1], center = T, scale = T)
summary(town.pca2) # 2번째 주성분 부터 누적설명력이 80%를 넘기 때문에 PC2까지 선택


# 주성분들의 고유벡터
town.pca2$rotation

# Eigenvalues(고유값) 확인
plot(town.pca2)

# 1, 2번째 주성분의 좌표에 그린 그림
biplot(town.pca2)


# 주성분계수와 데이터 내적
data <- as.matrix(town2_2[, -1]) %*% town.pca2$rotation
data1 <- as.data.frame(cbind(town2_2$success, as.data.frame(data)))
colnames(data1)[1] <- "success"
data1$success <- as.factor(data1$success)

# PC2까지 선택한 데이터 만들기
pcadata2 <- data1 %>% select(success, PC1, PC2)


# 교차타당성 기법을 이용하여 knn분류기 만들기 - 적정 k의 갯수 찾기
acc <- matrix(data = NA, nrow = 11, ncol = 10)
for(i in 1:nrow(town2_2)) {
  for(j in 1:10) {
    train <- pcadata2[-i, ]
    test <- pcadata2[i, ]
    pred <- knn(train = train[, -1], test = test[-1], cl = train[, 1], k = j, prob = T)
    acc[i, j] <- mean(pred == test[, 1])
  }
}
acc <- as.data.frame(acc)

# accuracy
accuracy <- NULL
for(i in 1:10) {
  accuracy[i] <- mean(acc[, i])
}
accuracy
plot(1:10, accuracy, type = "l", xlab = "k의 갯수")
