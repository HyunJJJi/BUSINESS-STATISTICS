library(readxl)
library(readr)
library(dplyr)
library(forcats)
library(psych)
library(car)
library(lm.beta)
library(car)


# 데이터 불러오기
final_data <- read_excel("usedata_final3.xlsx")

# 데이터 전처리 
final_data$member_num <- NULL
str(final_data)
final_data$region <- as.factor(final_data$region)
final_data$income_quintile <- as.factor(final_data$income_quintile)
final_data$gender <- as.factor(final_data$gender)
final_data$education <- as.factor(final_data$education)
final_data$disability_grade <- as.factor(final_data$disability_grade)
final_data$marriage <- as.factor(final_data$marriage)
final_data$religion <- as.factor(final_data$religion)
final_data$occ_category <- as.factor(final_data$occ_category)
final_data$occ_size <- as.factor(final_data$occ_size)
final_data$house_category <- as.factor(final_data$house_category)
str(final_data)

# 데이터프레임 쪼개기
normal <- final_data %>% filter(income_quintile == 1)
normal$income_quintile <- NULL
low <- final_data %>% filter(income_quintile == 2)
low$income_quintile <- NULL

#### normal #### 
# 변수 이동 
str(normal)
normal$id <- NULL
normal <- normal %>% relocate(where(is.factor))
corr.test(normal[10:28], method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")
pairs(normal[10:28])

# 정규성 검증
shapiro.test(normal$income)

# 회귀식 수립 
lm <- lm(income~., data = normal)
plot(lm)
# 1620, 893, 791, 669, 2245, 1960 그래프에 나타난 이상치를 제거하고 다시 확인 
normal_1 <- normal[-c(1620, 893, 791, 669, 2245, 1960),]

# 선형성, 등분산성, 정규성 개선 1
shapiro.test(normal_1$income)
lm_1 <- lm(income~., data = normal_1)
plot(lm_1)
# 811, 1469, 1036, 326, 370 그래프에 나타난 이상치를 제거하고 다시 확인 
normal_2 <- normal_1[-c(811, 1469, 1036, 326, 370),]

# 선형성, 등분산성, 정규성 개선 2
shapiro.test(normal_2$income)
lm_2 <- lm(income~., data = normal_2)
plot(lm_2)

# 변수의 상대적 중요도 
summary(normal_2)
lm.beta(lm_2)
lm(lm_2)

# 다중공선성
vif(lm_2)



# 일반가구 1
normal_original <- read.csv("normal_original.csv")
normal_original$X <- NULL
lm_no <- lm(income~., data = normal_original)
plot(lm_no)
normal_original_1 <- normal_original[-c(1620,791,893,669,1620,2245),]
lm_no_1 <- lm(income~., data = normal_original_1)
plot(lm_no_1)
normal_original_2 <- normal_original_1[-c(1960,370,120,813,1472,1039),]
lm_no_2<- lm(income~., data = normal_original_2)
plot(lm_no_2)
normal_original_3 <- normal_original_2[-c(1960,367,375,813,1472,1039),]
lm_no_3<- lm(income~., data = normal_original_3)
plot(lm_no_3)



# 일반가구 2
normal_final <- read.csv("normal_final.csv")
normal_final$X <- NULL
lm_nf <- lm(income~., data = normal_final)
plot(lm_nf)


# 저소득가구 1
low_original <- read.csv("low_original.csv")
low_original$X <- NULL
lm_lo <- lm(income~., data = low_original)
plot(lm_lo)



# 저소득가구 2
low_final <- read.csv("low_final.csv")
low_final$X <- NULL
lm_lf <- lm(income~., data = low_final)
plot(lm_lf)














