
##필요한 library 실행
library(readr)#csv파일 읽기
library(PerformanceAnalytics)#상관분석 그래프 생성
library(reshape2)#데이터 변환
library(ggplot2)#박스플롯 행렬 생성
library(bestglm)#로지스틱 회귀를 위한 교차 검증 
library(InformationValue)#로지스틱 회귀 모델 혼동 행렬 검증
library(e1071)#tune 함수 사용
library(caret)#svm 함수 사용 및 혼동 행렬 검증


##CSV데이터 가져오기
wbcd <- read.csv(file.choose(), stringsAsFactors = FALSE)


##데이터 확인
#데이터 탐색 및 결측치 마지막 컬럼 삭제
str(wbcd)
summary(wbcd)
table(wbcd$diagnosis)

summary(wbcd$X)
wbcd <- wbcd[,-33]


##피처 선택
#데이터 정규화
normalize <- function(x) {
  return((x-min(x)) / (max(x) - min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[,3:32], normalize))
summary(wbcd_n)


##컬럼 id, diagnosis와 정규화 한 데이터셋과 결합
wbcd_n_2 <- cbind(wbcd[,c(1, 2)], wbcd_n)


##diagnosis 컬럼 펙터 코딩
wbcd_n_2$diagnosis <- ifelse(wbcd_n_2$diagnosis == 'M', 1, 0)
wbcd_n_2$diagnosis <- factor(wbcd_n_2$diagnosis, levels = c("1", "0"),
                         labels = c("1", "0"))
table(wbcd_n_2$diagnosis)


##1)박스 플롯 확인
wbcd_n_id <- wbcd_n_2[,-1]
#평균
wbcd_n_m <- melt(wbcd_n_id[,c(1:11)], id.vars = "diagnosis")
ggplot(data = wbcd_n_m, aes(x = diagnosis, y = value)) + geom_boxplot() + facet_wrap(~ variable)
#상위 3개 평균 
wbcd_n_h <- melt(wbcd_n_id[,c(1,22:31)], id.vars = "diagnosis")
ggplot(data = wbcd_n_h, aes(x = diagnosis, y = value)) + geom_boxplot() + facet_wrap(~ variable)


##급성과 양성으로 데이터셋 나누기
wbcd_M <- subset(wbcd_n_2, wbcd_n_2$diagnosis == "1")
wbcd_B <- subset(wbcd_n_2, wbcd_n_2$diagnosis == "0")
#2)상관관계 계수 확인
#평균
chart.Correlation(wbcd_M[,c(3:12)], histogram = TRUE, pch = 19)
chart.Correlation(wbcd_B[,c(3:12)], histogram = TRUE, pch = 19)
#상위 3개 평균
chart.Correlation(wbcd_M[,c(23:32)], histogram = TRUE, pch = 19)
chart.Correlation(wbcd_B[,c(23:32)], histogram = TRUE, pch = 19)
#'core.test'로 상관 관계의 유효성(p-value) 확인 (정규화 했기 때문에 Pearson 상관관계 계산)
#상위 급성
cor.test(wbcd_M$compactness_worst, wbcd_M$concavity_worst, method = 'pearson')
cor.test(wbcd_M$concavity_worst, wbcd_M$concave.points_worst, method = 'pearson')
cor.test(wbcd_M$compactness_worst, wbcd_M$concave.points_worst, method = 'pearson')
cor.test(wbcd_M$radius_worst, wbcd_M$concave.points_worst, method = 'pearson')
cor.test(wbcd_M$perimeter_worst, wbcd_M$concave.points_worst, method = 'pearson')
cor.test(wbcd_M$area_worst, wbcd_M$concave.points_worst, method = 'pearson')
#전체 급성
cor.test(wbcd_M$compactness_mean, wbcd_M$concavity_mean, method = 'pearson')
cor.test(wbcd_M$concavity_mean, wbcd_M$concave.points_mean, method = 'pearson')
cor.test(wbcd_M$compactness_mean, wbcd_M$concave.points_mean, method = 'pearson')
cor.test(wbcd_M$radius_mean, wbcd_M$concave.points_mean, method = 'pearson')
cor.test(wbcd_M$perimeter_mean, wbcd_M$concave.points_mean, method = 'pearson')
cor.test(wbcd_M$area_mean, wbcd_M$concave.points_mean, method = 'pearson')
#상위 양성
cor.test(wbcd_B$compactness_worst, wbcd_B$concavity_worst, method = 'pearson')
cor.test(wbcd_B$concavity_worst, wbcd_B$concave.points_worst, method = 'pearson')
cor.test(wbcd_B$compactness_worst, wbcd_B$concave.points_worst, method = 'pearson')
cor.test(wbcd_B$radius_worst, wbcd_B$concave.points_worst, method = 'pearson')
cor.test(wbcd_B$perimeter_worst, wbcd_B$concave.points_worst, method = 'pearson')
cor.test(wbcd_B$area_worst, wbcd_B$concave.points_worst, method = 'pearson')
#전체 양성
cor.test(wbcd_B$compactness_mean, wbcd_B$concavity_mean, method = 'pearson')
cor.test(wbcd_B$concavity_mean, wbcd_B$concave.points_mean, method = 'pearson')
cor.test(wbcd_B$compactness_mean, wbcd_B$concave.points_mean, method = 'pearson')
cor.test(wbcd_B$radius_mean, wbcd_B$concave.points_mean, method = 'pearson')
cor.test(wbcd_B$perimeter_mean, wbcd_B$concave.points_mean, method = 'pearson')
cor.test(wbcd_B$area_mean, wbcd_B$concave.points_mean, method = 'pearson')

#3)로지스틱 회귀를 위한 교차 검증 축소된 모형 확인 
##컬럼 id, diagnosis와 정규화 한 데이터셋과 결합
wbcd_n_2 <- cbind(wbcd[,c(1, 2)], wbcd_n)
#diagnosis 컬럼 펙터 코딩
wbcd_n_2$diagnosis <- ifelse(wbcd_n_2$diagnosis == 'M', 1, 0)
wbcd_n_2$diagnosis <- factor(wbcd_n_2$diagnosis, levels = c("1", "0"),
                         labels = c("1", "0"))
#훈련(80%) 데이터와 테스트(20%) 데이터 나누기
#평균 훈련 데이터
wbcd_train_m <- wbcd_n_2[1:455, c(2, 3:12)]
#평균 데스트 데이터
wbcd_test_m <- wbcd_n_2[456:569, c(2, 3:12)]
#상위 훈련 데이터
wbcd_train_h <- wbcd_n_2[1:455, c(2, 23:32)]
#상위 테스트 데이터
wbcd_test_h <- wbcd_n_2[456:569, c(2, 23:32)]
#모든 변수 기준
#cv설정
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
#평균
fit_m_full  <- train(diagnosis ~ .,  data=wbcd_train_m, method="glm", family="binomial",
                     trControl = ctrl)
summary(fit_m_full)
pred_m <- predict(fit_m_full, newdata=wbcd_test_m)
pred_fac_df <- as.data.frame(as.factor(pred_m))
test_fac_df <- as.data.frame(wbcd_test_m$diagnosis)
m_table <- table(c(pred_fac_df, test_fac_df)); m_table
sum(diag(m_table))/sum(m_table) #overall accuracy
#상위
fit_h_full  <- train(diagnosis ~ .,  data=wbcd_train_h, method="glm", family="binomial",
                     trControl = ctrl)
summary(fit_h_full)
pred_h <- predict(fit_h_full, newdata=wbcd_test_h)
pred_fac_df_h <- as.data.frame(as.factor(pred_h))
test_fac_df_h <- as.data.frame(wbcd_test_h$diagnosis)
h_table <- table(c(pred_fac_df_h, test_fac_df_h)); h_table
sum(diag(h_table))/sum(h_table) #overall accuracy

#최량부분집합
#평균
#데이터 셋 변환
X_m <- wbcd_n_2[, c(3:12)]
wbcd_n_dia <- wbcd_n_2$diagnosis
wbcd_n_dia <- ifelse(wbcd_n_dia  == 'M', 1, 0)
Xy_m <- data.frame(cbind(X_m, wbcd_n_dia ))
#교차 검증
bestglm(Xy = Xy_m, IC = "CV",
        CVArgs = list(Method="HTF", K = 10,
        REP = 1), family = binomial)
#상위 3개 평균
#데이터 셋 변환
X_h <- wbcd_n_2[, c(23:32)]
Xy_h <- data.frame(cbind(X_h, wbcd_n_dia))
#교차 검증
bestglm(Xy = Xy_h, IC = "CV",
        CVArgs = list(Method="HTF", K = 10,
                      REP = 1), family = binomial)
#평균 로지스틱 회귀 실행 및 예측 평가
fit_m_CV  <- train(diagnosis ~ texture_mean + area_mean + concave.points_mean,
                   data=wbcd_train_m, method="glm", family="binomial")
summary(fit_m_CV)
pred_m_cv <- predict(fit_m_CV, newdata=wbcd_test_m)
pred_fac_df_cv <- as.data.frame(as.factor(pred_m_cv))
test_fac_df_cv <- as.data.frame(wbcd_test_m$diagnosis)
m_table_cv <- table(c(pred_fac_df_cv, test_fac_df_cv)); m_table_cv
sum(diag(m_table_cv))/sum(m_table_cv) #overall accuracy
#상위 로지스틱 회귀 실행 및 예측 평가
fit_h_CV <- glm(diagnosis ~ texture_worst + area_worst + smoothness_worst + concave.points_worst,
              data = wbcd_train_h)
test_h_cv_probs <- predict(fit_h_CV, newdata = wbcd_test_h, type = "response")
misClassError(wbcd_test_h$diagnosis, test_h_cv_probs)
confusionMatrix(wbcd_test_h$diagnosis, test_h_cv_probs)

fit_h_CV  <- train(diagnosis ~ texture_worst + area_worst + smoothness_worst + concave.points_worst,
                   data=wbcd_train_h, method="glm", family="binomial")
summary(fit_h_CV)
pred_h_CV <- predict(fit_h_full, newdata=wbcd_test_h)
pred_fac_df_h_CV <- as.data.frame(as.factor(pred_h_CV))
test_fac_df_h_CV <- as.data.frame(wbcd_test_h$diagnosis)
h_table_CV <- table(c(pred_fac_df_h_CV, test_fac_df_h_CV)); h_table_CV
sum(diag(h_table_CV))/sum(h_table_CV) #overall accuracy


##클래스 선택
#전체 피처로 적용한 SVM
#'id'열 삭제
wbcd_n_fac <- cbind(wbcd[,c(1, 2)], wbcd_n)
wbcd_n_fac <- wbcd_n_fac[,-1]
wbcd_n_num <- cbind(wbcd[,c(1, 2)], wbcd_n)
wbcd_n_num <- wbcd_n_num[,-1]
#diagnosis 컬럼 펙터 코딩
wbcd_n_fac$diagnosis <- factor(wbcd_n_fac$diagnosis, levels = c("M", "B"),
                               labels = c("1", "0"))
table(wbcd_n_fac$diagnosis)
#diagnosis 컬럼 numeric 코딩
wbcd_n_num$diagnosis <- as.numeric(ifelse(wbcd_n_num$diagnosis == 'M', 1, 0)) 
#데이터셋 내에서 전체 평균에 대한 변수 전체 추출
wbcd_n_fac_2_full <- wbcd_n_fac[,c("diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean",
                              "smoothness_mean", "compactness_mean", "concavity_mean", "concave.points_mean",
                              "symmetry_mean", "fractal_dimension_mean")] 
wbcd_n_num_2_full <- wbcd_n_num[,c("diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean",
                                   "smoothness_mean", "compactness_mean", "concavity_mean", "concave.points_mean",
                                   "symmetry_mean", "fractal_dimension_mean")] 
#숫자형으로 변환
wbcd_n_fac_2_full$radius_mean <- as.numeric(wbcd_n_fac_2_full$radius_mean)   
wbcd_n_fac_2_full$texture_mean <- as.numeric(wbcd_n_fac_2_full$texture_mean) 
wbcd_n_fac_2_full$perimeter_mean <- as.numeric(wbcd_n_fac_2_full$perimeter_mean)
wbcd_n_fac_2_full$area_mean <- as.numeric(wbcd_n_fac_2_full$area_mean)
wbcd_n_fac_2_full$smoothness_mean <- as.numeric(wbcd_n_fac_2_full$smoothness_mean)
wbcd_n_fac_2_full$compactness_mean <- as.numeric(wbcd_n_fac_2_full$compactness_mean)
wbcd_n_fac_2_full$concavity_mean <- as.numeric(wbcd_n_fac_2_full$concavity_mean)   
wbcd_n_fac_2_full$concave.points_mean <- as.numeric(wbcd_n_fac_2_full$concave.points_mean)
wbcd_n_fac_2_full$symmetry_mean <- as.numeric(wbcd_n_fac_2_full$symmetry_mean)
wbcd_n_fac_2_full$fractal_dimension_mean <- as.numeric(wbcd_n_fac_2_full$fractal_dimension_mean)

wbcd_n_num_2_full$radius_mean <- as.numeric(wbcd_n_num_2_full$radius_mean)   
wbcd_n_num_2_full$texture_mean <- as.numeric(wbcd_n_num_2_full$texture_mean) 
wbcd_n_num_2_full$perimeter_mean <- as.numeric(wbcd_n_num_2_full$perimeter_mean)
wbcd_n_num_2_full$area_mean <- as.numeric(wbcd_n_num_2_full$area_mean)
wbcd_n_num_2_full$smoothness_mean <- as.numeric(wbcd_n_num_2_full$smoothness_mean)
wbcd_n_num_2_full$compactness_mean <- as.numeric(wbcd_n_num_2_full$compactness_mean)
wbcd_n_num_2_full$concavity_mean <- as.numeric(wbcd_n_num_2_full$concavity_mean)   
wbcd_n_num_2_full$concave.points_mean <- as.numeric(wbcd_n_num_2_full$concave.points_mean)
wbcd_n_num_2_full$symmetry_mean <- as.numeric(wbcd_n_num_2_full$symmetry_mean)
wbcd_n_num_2_full$fractal_dimension_mean <- as.numeric(wbcd_n_num_2_full$fractal_dimension_mean)
#훈련(80%) 데이터와 테스트(20%) 데이터 나누기
wbcd_n_fac_2_full_train <- wbcd_n_fac_2_full[1:455, ]
wbcd_n_fac_2_full_test <- wbcd_n_fac_2_full[456:569, ]

wbcd_n_num_2_train <- wbcd_n_num_2_full[1:455, ]
wbcd_n_num_2_test <- wbcd_n_num_2_full[456:569, ]
#linear 커널 타입
tune.out1_full = tune(svm, diagnosis~., data = wbcd_n_num_2_train, kernel = 'linear', 
                 ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out1_full)
wbcd_svm_linear_full <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train, kernel = 'linear', gamma = 0.5, cost = 0.5)
summary(wbcd_svm_linear_full)
wbcd_svm_linear_predict_full <- predict(wbcd_svm_linear_full, wbcd_n_fac_2_full_test[,-1])
confusionMatrix(wbcd_n_fac_2_full_test[,1], wbcd_svm_linear_predict_full)
#polynomial 커널 타입
tune.out2_full = tune(svm, diagnosis~., data = wbcd_n_num_2_train, kernel = 'polynomial', 
                      ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out2_full)
wbcd_svm_polynomial_full <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train, kernel = 'polynomial', gamma = 0.5, cost = 0.001)
summary(wbcd_svm_polynomial_full)
wbcd_svm_polynomial_predict_full <- predict(wbcd_svm_polynomial_full, wbcd_n_fac_2_full_test[,-1])
confusionMatrix(wbcd_n_fac_2_full_test[,1], wbcd_svm_polynomial_predict_full)
#radial 커널 타입
tune.out3_full = tune(svm, diagnosis~., data = wbcd_n_num_2_train, kernel = 'radial', 
                      ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out3_full)
wbcd_svm_radial_full <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train, kernel = 'radial', gamma = 0.5, cost = 0.5)
summary(wbcd_svm_radial_full)
wbcd_svm_radial_predict_full <- predict(wbcd_svm_radial_full, wbcd_n_fac_2_full_test[,-1])
confusionMatrix(wbcd_n_fac_2_full_test[,1], wbcd_svm_radial_predict_full)
#sigmoid 커널 타입
tune.out4_full = tune(svm, diagnosis~., data = wbcd_n_num_2_train, kernel = 'sigmoid', 
                      ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out4_full)
wbcd_svm_sigmoid_full <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train, kernel = 'sigmoid', gamma = 0.5, cost = 0.01)
summary(wbcd_svm_sigmoid_full)
wbcd_svm_sigmoid_predict_full <- predict(wbcd_svm_sigmoid_full, wbcd_n_fac_2_full_test[,-1])
confusionMatrix(wbcd_n_fac_2_full_test[,1], wbcd_svm_sigmoid_predict_full)

#데이터셋 내에서 상위 평균에 대한 변수 전체 추출
wbcd_n_fac_2_full_h <- wbcd_n_fac[,c("diagnosis", "radius_worst", "texture_worst", "perimeter_worst", "area_worst",
                                   "smoothness_worst", "compactness_worst", "concavity_worst", "concave.points_worst",
                                   "symmetry_worst", "fractal_dimension_worst")] 
wbcd_n_num_2_full_h <- wbcd_n_num[,c("diagnosis", "radius_worst", "texture_worst", "perimeter_worst", "area_worst",
                                   "smoothness_worst", "compactness_worst", "concavity_worst", "concave.points_worst",
                                   "symmetry_worst", "fractal_dimension_worst")] 
#숫자형으로 변환
wbcd_n_fac_2_full_h$radius_worst <- as.numeric(wbcd_n_fac_2_full_h$radius_worst)   
wbcd_n_fac_2_full_h$texture_worst <- as.numeric(wbcd_n_fac_2_full_h$texture_worst) 
wbcd_n_fac_2_full_h$perimeter_worst <- as.numeric(wbcd_n_fac_2_full_h$perimeter_worst)
wbcd_n_fac_2_full_h$area_worst <- as.numeric(wbcd_n_fac_2_full_h$area_worst)
wbcd_n_fac_2_full_h$smoothness_worst <- as.numeric(wbcd_n_fac_2_full_h$smoothness_worst)
wbcd_n_fac_2_full_h$compactness_worst <- as.numeric(wbcd_n_fac_2_full_h$compactness_worst)
wbcd_n_fac_2_full_h$concavity_worst <- as.numeric(wbcd_n_fac_2_full_h$concavity_worst)   
wbcd_n_fac_2_full_h$concave.points_worst <- as.numeric(wbcd_n_fac_2_full_h$concave.points_worst)
wbcd_n_fac_2_full_h$symmetry_worst <- as.numeric(wbcd_n_fac_2_full_h$symmetry_worst)
wbcd_n_fac_2_full_h$fractal_dimension_worst <- as.numeric(wbcd_n_fac_2_full_h$fractal_dimension_worst)

wbcd_n_num_2_full_h$radius_worst <- as.numeric(wbcd_n_num_2_full_h$radius_worst)   
wbcd_n_num_2_full_h$texture_worst <- as.numeric(wbcd_n_num_2_full_h$texture_worst) 
wbcd_n_num_2_full_h$perimeter_worst <- as.numeric(wbcd_n_num_2_full_h$perimeter_worst)
wbcd_n_num_2_full_h$area_worst <- as.numeric(wbcd_n_num_2_full_h$area_worst)
wbcd_n_num_2_full_h$smoothness_worst <- as.numeric(wbcd_n_num_2_full_h$smoothness_worst)
wbcd_n_num_2_full_h$compactness_worst <- as.numeric(wbcd_n_num_2_full_h$compactness_worst)
wbcd_n_num_2_full_h$concavity_worst <- as.numeric(wbcd_n_num_2_full_h$concavity_worst)   
wbcd_n_num_2_full_h$concave.points_worst <- as.numeric(wbcd_n_num_2_full_h$concave.points_worst)
wbcd_n_num_2_full_h$symmetry_worst <- as.numeric(wbcd_n_num_2_full_h$symmetry_worst)
wbcd_n_num_2_full_h$fractal_dimension_worst <- as.numeric(wbcd_n_num_2_full_h$fractal_dimension_worst)
#훈련(80%) 데이터와 테스트(20%) 데이터 나누기
wbcd_n_fac_2_full_train_h <- wbcd_n_fac_2_full_h[1:455, ]
wbcd_n_fac_2_full_test_h <- wbcd_n_fac_2_full_h[456:569, ]

wbcd_n_num_2_train_h <- wbcd_n_num_2_full_h[1:455, ]
wbcd_n_num_2_test_h <- wbcd_n_num_2_full_h[456:569, ]
#linear 커널 타입
tune.out1_full_h = tune(svm, diagnosis~., data = wbcd_n_num_2_train_h, kernel = 'linear', 
                      ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out1_full_h)
wbcd_svm_linear_full_h <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train_h, kernel = 'linear', gamma = 0.5, cost = 0.5)
summary(wbcd_svm_linear_full_h)
wbcd_svm_linear_predict_full_h <- predict(wbcd_svm_linear_full_h, wbcd_n_fac_2_full_test_h[,-1])
confusionMatrix(wbcd_n_fac_2_full_test_h[,1], wbcd_svm_linear_predict_full_h)
#polynomial 커널 타입
tune.out2_full_h = tune(svm, diagnosis~., data = wbcd_n_num_2_train_h, kernel = 'polynomial', 
                      ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out2_full_h)
wbcd_svm_polynomial_full_h <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train_h, kernel = 'polynomial', gamma = 0.5, cost = 0.001)
summary(wbcd_svm_polynomial_full_h)
wbcd_svm_polynomial_predict_full_h <- predict(wbcd_svm_polynomial_full_h, wbcd_n_fac_2_full_test_h[,-1])
confusionMatrix(wbcd_n_fac_2_full_test_h[,1], wbcd_svm_polynomial_predict_full_h)
#radial 커널 타입
tune.out3_full_h = tune(svm, diagnosis~., data = wbcd_n_num_2_train_h, kernel = 'radial', 
                      ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out3_full_h)
wbcd_svm_radial_full_h <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train_h, kernel = 'radial', gamma = 0.5, cost = 0.5)
summary(wbcd_svm_radial_full_h)
wbcd_svm_radial_predict_full_h <- predict(wbcd_svm_radial_full_h, wbcd_n_fac_2_full_test_h[,-1])
confusionMatrix(wbcd_n_fac_2_full_test_h[,1], wbcd_svm_radial_predict_full_h)
#sigmoid 커널 타입
tune.out4_full_h = tune(svm, diagnosis~., data = wbcd_n_num_2_train_h, kernel = 'sigmoid', 
                      ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out4_full_h)
wbcd_svm_sigmoid_full_h <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_train_h, kernel = 'sigmoid', gamma = 0.5, cost = 0.01)
summary(wbcd_svm_sigmoid_full_h)
wbcd_svm_sigmoid_predict_full_h <- predict(wbcd_svm_sigmoid_full_h, wbcd_n_fac_2_full_test_h[,-1])
confusionMatrix(wbcd_n_fac_2_full_test_h[,1], wbcd_svm_sigmoid_predict_full_h)

#유의미한 피처 적용한 SVM
#데이터셋 내에서 전체 평균에 대한 변수만 추출
wbcd_n_fac_2 <- wbcd_n_fac[,c("diagnosis", "radius_mean", "perimeter_mean", "area_mean",
                        "compactness_mean", "concavity_mean", "concave.points_mean")] 
wbcd_n_num_2 <- wbcd_n_num[,c("diagnosis", "radius_mean", "perimeter_mean", "area_mean",
                              "compactness_mean", "concavity_mean", "concave.points_mean")] 
#숫자형으로 변환
wbcd_n_fac_2$radius_mean <- as.numeric(wbcd_n_fac_2$radius_mean)   
wbcd_n_fac_2$perimeter_mean <- as.numeric(wbcd_n_fac_2$perimeter_mean)
wbcd_n_fac_2$area_mean <- as.numeric(wbcd_n_fac_2$area_mean)
wbcd_n_fac_2$compactness_mean <- as.numeric(wbcd_n_fac_2$compactness_mean)
wbcd_n_fac_2$concavity_mean <- as.numeric(wbcd_n_fac_2$concavity_mean)   
wbcd_n_fac_2$concave.points_mean <- as.numeric(wbcd_n_fac_2$concave.points_mean)

wbcd_n_num_2$radius_mean <- as.numeric(wbcd_n_num_2$radius_mean)   
wbcd_n_num_2$perimeter_mean <- as.numeric(wbcd_n_num_2$perimeter_mean)
wbcd_n_num_2$area_mean <- as.numeric(wbcd_n_num_2$area_mean)
wbcd_n_num_2$compactness_mean <- as.numeric(wbcd_n_num_2$compactness_mean)
wbcd_n_num_2$concavity_mean <- as.numeric(wbcd_n_num_2$concavity_mean)   
wbcd_n_num_2$concave.points_mean <- as.numeric(wbcd_n_num_2$concave.points_mean)
#훈련(80%) 데이터와 테스트(20%) 데이터 나누기
wbcd_n_fac_train <- wbcd_n_fac_2[1:455, ]
wbcd_n_fac_test <- wbcd_n_fac_2[456:569, ]

wbcd_n_num_train <- wbcd_n_num_2[1:455, ]
wbcd_n_num_test <- wbcd_n_num_2[456:569, ]
#linear 커널 타입
tune.out1 = tune(svm, diagnosis~., data = wbcd_n_num_train, kernel = 'linear', 
                 ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out1)
wbcd_svm_linear <- svm(diagnosis ~ ., data = wbcd_n_fac_train, kernel = 'linear', gamma = 0.5, cost = 0.01)
summary(wbcd_svm_linear)
wbcd_svm_linear_predict <- predict(wbcd_svm_linear, wbcd_n_fac_test[,-1])
confusionMatrix(wbcd_n_fac_test[,1], wbcd_svm_linear_predict)
#polynomial 커널 타입
tune.out2 = tune(svm, diagnosis~., data = wbcd_n_num_train, kernel = 'polynomial', 
                 ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out2)
wbcd_svm_polynomial <- svm(diagnosis ~ ., data = wbcd_n_fac_train, kernel = 'polynomial', gamma = 1, cost = 0.01)
wbcd_svm_polynomial_predict <- predict(wbcd_svm_polynomial, wbcd_n_fac_test[,-1])
confusionMatrix(wbcd_n_fac_test[,1], wbcd_svm_polynomial_predict)
#radial 커널 타입
tune.out3 = tune(svm, diagnosis~., data = wbcd_n_num_train, kernel = 'radial', 
                 ranges = list(gamma = 2^(-1:1), cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out3)
wbcd_svm_radial <- svm(diagnosis ~ ., data = wbcd_n_fac_train, kernel = 'radial', gamma = 0.5, cost = 0.5)
wbcd_svm_radial_predict <- predict(wbcd_svm_radial, wbcd_n_fac_test[,-1])
confusionMatrix(wbcd_n_fac_test[,1], wbcd_svm_radial_predict)
#sigmoid 커널 타입
tune.out4 = tune(svm, diagnosis~., data = wbcd_n_num_train, kernel = 'sigmoid', 
                 ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out4)
wbcd_svm_sigmoid <- svm(diagnosis ~ ., data = wbcd_n_fac_train, kernel = 'sigmoid', gamma = 1, cost = 0.01)
wbcd_svm_sigmoid_predict <- predict(wbcd_svm_sigmoid, wbcd_n_fac_test[,-1])
confusionMatrix(wbcd_n_fac_test[,1], wbcd_svm_sigmoid_predict)

#데이터셋 내에서 상위에 대한 변수만 추출
wbcd_n_fac_3 <- wbcd_n_fac[,c("diagnosis", "radius_worst", "perimeter_worst", "area_worst",
                              "compactness_worst", "concavity_worst", "concave.points_worst")] 
wbcd_n_num_3 <- wbcd_n_num[,c("diagnosis", "radius_worst", "perimeter_worst", "area_worst",
                              "compactness_worst", "concavity_worst", "concave.points_worst")] 
#숫자형으로 변환
wbcd_n_fac_3$radius_worst <- as.numeric(wbcd_n_fac_3$radius_worst)   
wbcd_n_fac_3$perimeter_worst <- as.numeric(wbcd_n_fac_3$perimeter_worst)
wbcd_n_fac_3$area_worst <- as.numeric(wbcd_n_fac_3$area_worst)
wbcd_n_fac_3$compactness_worst <- as.numeric(wbcd_n_fac_3$compactness_worst)
wbcd_n_fac_3$concavity_worst <- as.numeric(wbcd_n_fac_3$concavity_worst)   
wbcd_n_fac_3$concave.points_worst <- as.numeric(wbcd_n_fac_3$concave.points_worst)

wbcd_n_num_3$radius_worst <- as.numeric(wbcd_n_num_3$radius_worst)   
wbcd_n_num_3$perimeter_worst <- as.numeric(wbcd_n_num_3$perimeter_worst)
wbcd_n_num_3$area_worst <- as.numeric(wbcd_n_num_3$area_worst)
wbcd_n_num_3$compactness_worst <- as.numeric(wbcd_n_num_3$compactness_worst)
wbcd_n_num_3$concavity_worst <- as.numeric(wbcd_n_num_3$concavity_worst)   
wbcd_n_num_3$concave.points_worst <- as.numeric(wbcd_n_num_3$concave.points_worst)
#훈련(80%) 데이터와 테스트(20%) 데이터 나누기
wbcd_n_fac_train_h <- wbcd_n_fac_3[1:455, ]
wbcd_n_fac_test_h <- wbcd_n_fac_3[456:569, ]

wbcd_n_num_train_h <- wbcd_n_num_3[1:455, ]
wbcd_n_num_test_h <- wbcd_n_num_3[456:569, ]
#linear 커널 타입
tune.out1_h = tune(svm, diagnosis~., data = wbcd_n_num_train_h, kernel = 'linear', 
                 ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out1_h)
wbcd_svm_linear_h <- svm(diagnosis ~ ., data = wbcd_n_fac_train_h, kernel = 'linear', gamma = 0.5, cost = 0.5)
summary(wbcd_svm_linear_h)
wbcd_svm_linear_predict_h <- predict(wbcd_svm_linear_h, wbcd_n_fac_test_h[,-1])
confusionMatrix(wbcd_n_fac_test_h[,1], wbcd_svm_linear_predict_h)
#polynomial 커널 타입
tune.out2_h = tune(svm, diagnosis~., data = wbcd_n_num_train_h, kernel = 'polynomial', 
                   ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out2_h)
wbcd_svm_polynomial_h <- svm(diagnosis ~ ., data = wbcd_n_fac_train_h, kernel = 'polynomial', gamma = 0.5, cost = 0.01)
summary(wbcd_svm_polynomial_h)
wbcd_svm_polynomial_predict_h <- predict(wbcd_svm_polynomial_h, wbcd_n_fac_test_h[,-1])
confusionMatrix(wbcd_n_fac_test_h[,1], wbcd_svm_polynomial_predict_h)
#radial 커널 타입
tune.out3_h = tune(svm, diagnosis~., data = wbcd_n_num_train_h, kernel = 'radial', 
                 ranges = list(gamma = 2^(-1:1), cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out3_h)
wbcd_svm_radial_h <- svm(diagnosis ~ ., data = wbcd_n_fac_train_h, kernel = 'radial', gamma = 0.5, cost = 0.5)
wbcd_svm_radial_predict_h <- predict(wbcd_svm_radial_h, wbcd_n_fac_test_h[,-1])
confusionMatrix(wbcd_n_fac_test_h[,1], wbcd_svm_radial_predict_h)
#sigmoid 커널 타입
tune.out4_h = tune(svm, diagnosis~., data = wbcd_n_num_train_h, kernel = 'sigmoid', 
                 ranges = list(gamma = 2^(-1:1) ,cost = c(0.001,0.01,0.1,0.5)))
summary(tune.out4_h)
wbcd_svm_sigmoid_h <- svm(diagnosis ~ ., data = wbcd_n_fac_train_h, kernel = 'sigmoid', gamma = 2, cost = 0.01)
wbcd_svm_sigmoid_predict_h <- predict(wbcd_svm_sigmoid_h, wbcd_n_fac_test_h[,-1])
confusionMatrix(wbcd_n_fac_test_h[,1], wbcd_svm_sigmoid_predict_h)


##세포 크기 상위 평균_ Linear(선형) 함수, 세포 크기 전체 평균_ Polynomial(다항) 함수 서포트 벡터 확인
#평균 전체 테스트 데이터 생성
wbcd_n_fac_2_full_test_final <- wbcd_n_fac_2_full
#polynomial 커널 타입
wbcd_svm_polynomial_full_final <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_test_final, kernel = 'polynomial', gamma = 0.5, cost = 0.001)
summary(wbcd_svm_polynomial_full_final)
wbcd_svm_polynomial_full_final$index
length(wbcd_svm_polynomial_full_final$index)

#상위 전체 테스트 데이터 생성
wbcd_n_fac_2_full_test_h_final <- wbcd_n_fac_2_full_h
#linear 커널 타입
wbcd_svm_linear_full_h_final <- svm(diagnosis ~ ., data = wbcd_n_fac_2_full_test_h_final, kernel = 'linear', gamma = 0.5, cost = 0.5)
summary(wbcd_svm_linear_full_h_final)
wbcd_svm_linear_full_h_final$index
length(wbcd_svm_linear_full_h_final$index)

#전체 서포트 벡터 확인 
unique(c(wbcd_svm_polynomial_full_final$index, wbcd_svm_linear_full_h_final$index))
length(unique(c(wbcd_svm_polynomial_full_final$index, wbcd_svm_linear_full_h_final$index)))
#중복 클랙스 확인
length(which(duplicated(c(wbcd_svm_polynomial_full_final$index, wbcd_svm_linear_full_h_final$index))))


##전체 평균에서 서포트 벡터 클래스와 상관관계를 보이는 변수 적용 급성 양성 구분
wbcd_svm_sv <- as.data.frame(wbcd_n_2[unique(c(wbcd_svm_polynomial_full_final$index, wbcd_svm_linear_full_h_final$index)), ])
wbcd_svm_sv_2 <- wbcd_svm_sv[,c("diagnosis", "radius_mean", "perimeter_mean", "area_mean",
                                "compactness_mean", "concavity_mean", "concave.points_mean")] 
wbcd_svm_sv_M <- subset(wbcd_svm_sv_2, wbcd_svm_sv_2$diagnosis == "M")
wbcd_svm_sv_B <- subset(wbcd_svm_sv_2, wbcd_svm_sv_2$diagnosis == "B")


##상위 평균에서 서포트 벡터 클래스와 상관관계를 보이는 변수 적용 급성 양성 구분
wbcd_svm_sv_3 <- wbcd_svm_sv[,c("diagnosis", "radius_worst", "perimeter_worst", "area_worst",
                                "compactness_worst", "concavity_worst", "concave.points_worst")] 
wbcd_svm_sv_H_M <- subset(wbcd_svm_sv_3, wbcd_svm_sv_3$diagnosis == "M")
wbcd_svm_sv_H_B <- subset(wbcd_svm_sv_3, wbcd_svm_sv_3$diagnosis == "B")


##각 표본의 평균  
wbcd_svm_sv_M_mean <- aggregate(wbcd_svm_sv_M[,c(2:7)], 
                                by = list(wbcd_svm_sv_M$diagnosis), 
                                FUN = mean)
wbcd_svm_sv_B_mean <- aggregate(wbcd_svm_sv_B[,c(2:7)], 
                                by = list(wbcd_svm_sv_B$diagnosis), 
                                FUN = mean) 

wbcd_svm_sv_H_M_mean <- aggregate(wbcd_svm_sv_H_M[,c(2:7)], 
                                  by = list(wbcd_svm_sv_H_M$diagnosis), 
                                  FUN = mean)
wbcd_svm_sv_H_B_mean <- aggregate(wbcd_svm_sv_H_B[,c(2:7)], 
                                  by = list(wbcd_svm_sv_H_B$diagnosis), 
                                  FUN = mean) 


##M_급성-양성/H_급성-양성 차이 확인 및 유효성 검증
find_1 <-  wbcd_svm_sv_M_mean[,2:7] - wbcd_svm_sv_B_mean[,2:7]
colnames(find_1) <- c("radius", "perimeter", "area", "compactness", "concavity", "concave.points")
find_1
#유효성 검증
t.test(wbcd_svm_sv_M$radius_mean, wbcd_svm_sv_B$radius_mean)
t.test(wbcd_svm_sv_M$perimeter_mean, wbcd_svm_sv_B$perimeter_mean)
t.test(wbcd_svm_sv_M$area_mean, wbcd_svm_sv_B$area_mean)
t.test(wbcd_svm_sv_M$compactness_mean, wbcd_svm_sv_B$compactness_mean)
t.test(wbcd_svm_sv_M$concavity_mean, wbcd_svm_sv_B$concavity_mean)
t.test(wbcd_svm_sv_M$concave.points_mean, wbcd_svm_sv_B$concave.points_mean)

find_2 <- wbcd_svm_sv_H_M_mean[,2:7] - wbcd_svm_sv_H_B_mean[,2:7]
colnames(find_2) <- c("radius", "perimeter", "area", "compactness", "concavity", "concave.points")
find_2
#유효성 검증
t.test(wbcd_svm_sv_H_M$radius_worst, wbcd_svm_sv_H_B$radius_worst)
t.test(wbcd_svm_sv_H_M$perimeter_worst, wbcd_svm_sv_H_B$perimeter_worst)
t.test(wbcd_svm_sv_H_M$area_worst, wbcd_svm_sv_H_B$area_worst)
t.test(wbcd_svm_sv_H_M$compactness_worst, wbcd_svm_sv_H_B$compactness_worst)
t.test(wbcd_svm_sv_H_M$concavity_worst, wbcd_svm_sv_H_B$concavity_worst)
t.test(wbcd_svm_sv_H_M$concave.points_worst, wbcd_svm_sv_H_B$concave.points_worst)


##M_급성-양성/H_급성-양성 차이 시각화
bet_1 <- rbind("M" = find_1, "B" = find_2)

barplot(as.matrix(bet_1),
        beside = T,
        col= c("hotpink4","royalblue"),
        legend = TRUE, 
        args.legend = list(x = "top"),
        legend.text = c("전체평균", "상위평균"))
#차이 확인
find_2 - find_1


##H_급성-M_급성/h_양성-M_양성 차이 확인 및 유효성 검증
colnames(wbcd_svm_sv_M_mean) <- c("diagnosis", "radius", "perimeter", "area", "compactness", "concavity", "concave.points")
colnames(wbcd_svm_sv_H_M_mean) <- c("diagnosis", "radius", "perimeter", "area", "compactness", "concavity", "concave.points")
find_3 <-  wbcd_svm_sv_H_M_mean[,2:7] - wbcd_svm_sv_M_mean[,2:7] 
find_3
##유효성 검증
t.test(wbcd_svm_sv_H_M$radius_worst, wbcd_svm_sv_M$radius_mean)
t.test(wbcd_svm_sv_H_M$perimeter_worst, wbcd_svm_sv_M$perimeter_mean)
t.test(wbcd_svm_sv_H_M$area_worst, wbcd_svm_sv_M$area_mean)
t.test(wbcd_svm_sv_H_M$compactness_worst, wbcd_svm_sv_M$compactness_mean)
t.test(wbcd_svm_sv_H_M$concavity_worst, wbcd_svm_sv_M$concavity_mean)
t.test(wbcd_svm_sv_H_M$concave.points_worst, wbcd_svm_sv_M$concave.points_mean)

colnames(wbcd_svm_sv_B_mean) <- c("diagnosis", "radius", "perimeter", "area", "compactness", "concavity", "concave.points")
colnames(wbcd_svm_sv_H_B_mean) <- c("diagnosis", "radius", "perimeter", "area", "compactness", "concavity", "concave.points")
find_4 <- wbcd_svm_sv_H_B_mean[,2:7] - wbcd_svm_sv_B_mean[,2:7] 
find_4
##유효성 검증
t.test(wbcd_svm_sv_H_B$radius_worst, wbcd_svm_sv_B$radius_mean)
t.test(wbcd_svm_sv_H_B$perimeter_worst, wbcd_svm_sv_B$perimeter_mean)
t.test(wbcd_svm_sv_H_B$area_worst, wbcd_svm_sv_B$area_mean)
t.test(wbcd_svm_sv_H_B$compactness_worst, wbcd_svm_sv_B$compactness_mean)
t.test(wbcd_svm_sv_H_B$concavity_worst, wbcd_svm_sv_B$concavity_worst)
t.test(wbcd_svm_sv_H_B$concave.points_worst, wbcd_svm_sv_B$concave.points_mean)


##시각화
bet_2 <- rbind("M" = find_3, "B" = find_4)

barplot(as.matrix(bet_2),
        beside = T,
        col= c("hotpink4","royalblue"),
        legend = TRUE, 
        args.legend = list(x = "top"))
#차이 확인
find_3 - find_4


##전체 시각화
bet_final <- rbind("전체평균(M-B)" = find_1, "상위평균(M-B)" = find_2, 
                   "M(상위-전체)" = find_3, "B(상위-전체)" = find_4)

barplot(as.matrix(bet_final),
        beside = T,
        col= c("Steel Blue ", "Tomato", "Gold", "Plum"),
        legend = TRUE, 
        args.legend = list(x = "top"))