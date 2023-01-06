library(readr)
library(dplyr)

#### 문제1 ####
bike <- read_csv("bike.csv")
glimpse(bike)

#### 문제2 ####
bike <- bike %>% relocate(where(is.character))

#### 문제3 ####
table(bike$model_name)
n_distinct(bike$model_name)
table(bike$owner)
table(bike$location)
n_distinct(bike$location)
table(bike$model_year)

#### 문제4&5 ####
bike$owner <- as.factor(bike$owner)
library(forcats)
bike$owner <- fct_collapse(bike$owner, "third owner or more" = c("third owner", "fourth owner or more"))

#### 문제6&7 ####
library(psych)
descr <- describe(bike[5:9])
descr <- descr %>% mutate(UL = mean + 3*sd)
descr <- descr %>% mutate(LL = mean - 3*sd)

bike <- bike %>% filter(kms_driven <= 112639.98027)
bike <- bike %>% filter(mileage <= 89.86444)
bike <- bike %>% filter(power <= 72.30192)
bike <- bike %>% filter(price <= 545572.20872)

table(is.na(bike))

#### 문제8 ####
corr.test(bike[5:9], method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")
pairs.panels(bike[5:9])

#### 문제9 ####
summary(bike$price)
hist(bike$price, breaks = seq(0, 550000, 10000))
shapiro.test(bike$price)

bike <- bike %>% mutate(lnprice = log(price))
summary(bike$lnprice)
bike <- bike %>% filter(lnprice != "-Inf")
hist(bike$lnprice, breaks = seq(7, 14, 0.1))
shapiro.test(bike$lnprice)

#### 문제10 ####
# H1: kms_driven이 증가할수록 price는 낮아진다(음의 인과관계) #
# H2: mileage가 증가할수록 price는 낮아진다(음의 인과관계) #
# H3: power가 증가할수록 price는 높아진다(양의 인과관계) #

#### 문제11 ####
lm_1 <- lm(price~kms_driven+mileage+power, data = bike)
plot(lm_1)

# 선형성: Residuals vs Fitted 그래프에서 빨간 선이 수평이어야 하고 점들이 패턴을 나타내면 안됨 #
# 정규성: Q-Q 그래프에서 점들이 대각선 위에 존재해야 함 #
# 등분산성과 선형성: Scale-Location 그래프에서 빨간 선이 수평이어야 하고 점들이 패턴을 나타내면 안됨 #
# Residuals vs Leverage 그래프: Y축 기준 이상치(좌측 상/하단) 판정, Cook's Distance 0.5 혹은 1을 벗어나는 영향점(회귀계수 추정치에 지나치게 큰 영향 미치는 사례인데 현재 Cook' Distance가 표시되지 않아서 없음) 판정, X축(Leverage) 기준 고레버리지점(예측값이 이상한 사례로 우측 상/하단) 판정 #
## 이상의 그래프에서 언급된 case 5개를 제거하면 선형성, 등분산성, 정규성을 개선 가능함 ##

#### 문제12 ####
library(car)
durbinWatsonTest(lm_1)

# DW = 1.09이고(2보다 작고), rho != 0 이므로 positive autocorrelation 존재함 #
# 만약 DW > 2이고, rho != 0 이면 negative autocorrelation 존재하고, rho = 0이면 autocorrelation 없음 #

#### 문제13 ####
# 단측검정: p-value와 알파를 비교 #
# forward, backward, stepwise(both) 세 가지 방식: f는 IV를 하나씩 추가하는 방식. b는 IV를 다 추가한 상태에서 하나씩 제거하는 방식. s는 두 가지 방식의 혼합. AIC가 작을수록 GoF가 좋은데, f/b/s는 AIC가 가장 작게 되도록 IV를 선택 #
step(lm_1, direction = "forward")
step(lm_1, direction = "backward")
step(lm_1, direction = "both")
summary(lm_1)
# 회귀계수 추정치의 부호가 맞고, 모두 유의하므로(주의! 도출된 p-value는 2p-value이므로 2alpha(=0.1)와 비교해야 함) 세 개 가설 모두 채택 #
# 추정된 회귀식 Yi(hat) = -0.845X1i - 120.825X2i + 6175.722X3i #

#### 문제14 ####
summary(bike)
install.packages("lm.beta")
library(lm.beta)
lm.beta(lm_1)
# 표준화회귀계수 추정치의 절대값이 큰 순서대로, power > kms_driven > mileage #

#### 문제15 ####
library(car)
vif(lm_1)
# 세 개의 VIF값이 모두 5.3보다 작으므로 다중공선성 문제는 우려할 필요가 없다. #

#### 문제16 ####
# H4: repair가 증가할수록 price는 낮아진다(음의 인과관계) #

#### 문제17 ####
lm_2 <- lm(price~kms_driven + mileage + power + repair, data = bike)
summary(lm_1)
summary(lm_2)
anova(lm_1, lm_2)
# 우선 수정 R-squares가 증가했고, R-squares 증가량(약 0.013)에 따른 F통계량(224.52)의 p-value가 0으로 유의하므로 lm_2가 lm_1 보다 설명력이 더 좋다. 즉 repair를 추가함으로써 종속변수 분산(특성)을 더 추가적으로 설명할 수 있다. #
summary(lm_2)
# H1, H3, H4는 채택되고, H2는 채택되지 못함. #
# 추정된 회귀식 Yi(hat) = 15884.931 -0.755X1i + 5909.657X3i - 2726.490X4i #

#### 문제18 ####
table(bike$owner)
bike$owner <- factor(bike$owner, levels = c("third owner or more", "first owner", "second owner")) # 만약에 third owner or more를 reference로 하려면 출력순서를 이와 같이 조정해야 함 #
bike$owner <- factor(bike$owner, levels = c("first owner", "second owner", "third owner or more"))
lm_3 <- lm(price~kms_driven + mileage + power + repair + owner, data = bike)
summary(lm_2)
summary(lm_3)
anova(lm_2, lm_3)
# 두 개 더미변수(dv1: second owner인지 여부, dv2: third owner or more인지 여부)를 추가한 lm_3가 lm_2 보다 모형적합도가 더 좋음. 두 개 더미변수를 추가해야 함 #

#### 문제19 ####
summary(lm_3)
# 추정된 회귀식 Yi(hat) = 15810 -0.701X1i + 5951X3i - 2650X4i - 13540dv1i - 17590dv2i#
# first owner에 비해서 second owner는 b5만큼 중고차 가격이 하락하고, first owner에 비해서 third owner or more는 b6만큼 중고차 가격이 하락함 #

#### 문제20 ####
bike <- bike %>% mutate(dv3 = ifelse(owner == "first owner", 1, 0)) # 조절변수 dv3i를 생성 #
lm_4 <- lm(price~kms_driven + mileage + power + repair + owner + dv3 + repair*dv3, data = bike)
# dv3(조절변수)와 상호작용변수를 동시에 추가 #
summary(lm_3)
summary(lm_4)
anova(lm_3, lm_4)
# lm_3에 비해서 lm_4의 설명력이 더 좋아짐 #
# 손바뀜이 적을수록(first owner일수록) repair 회수가 가격하락에 미치는 음의 인과관계/영향이 강화된다(더 가격이 떨어진다). #

# H5: first owner일수록 repair가 가격하락에 미치는 음의 인과관계가 더 강화된다. (기존 인과관계 강화하는 조절효과) #

#### 문제21 ####
step(lm_4, direction = "both")
# stepwise 방식으로 추정하면 유의한 상수추정치와 회귀계수추정치에 해당되는 변수만 살아남는다 #
lm_s <- step(lm_4, direction = "both")
# 추정된 회귀식 Yi(hat) = -10960.12 -0.7043X1i + 5989.142X3i - 1181.238X4i + 25791.9dv3i - 1747.037X4i*dv3i  #
# kms_driven: 62,000 / power: 11 / repair: 4/ dv3: 1 / repair*dv3: 4 #
-10960.12 -0.7043*62000 + 5989.142*11 - 1181.238*4 + 25791.9*1 - 1747.037*4