library(readxl)
library(ggplot2)
library(dplyr)
library(magrittr)
library(car)
library(cowplot)
library(lme4)
library(performance)



speed <- read_excel("Data/cell_mov_speed.xlsx", sheet = "tidy_data")
area <- read_excel("Data/cell_mov_speed.xlsx", sheet = "cristal_area")

area_summ <- 
area  %>% 
  group_by(Well) %>% 
  summarise(Median_area = median(Area), 
            Mean_area = mean(Area),   
            SD = sd(Area),  
            CV = SD/Mean_area)

all_data <- 
merge(area_summ, speed)

######### Предварительный анализ ###############

# Думаем о дизайне сбора материала 

# Смотрим на то, как оформлен датасет. Приводим данные в соответсвии с требованями tidyverse

# Проверяем нет ли пропущенных значений (NA). Если есть, то принмаем решение, как с нми быть.


# Смотрим на связи между переменными, вошедшими в датасет


all_data %>% 
  select(-Well, -Field, -Field2) %$% 
  pairs(.)


# Ищем отскоки (выбросы, outliers)

all_data %>% 
  ggplot(aes(y = 1:nrow(.), x = Velocity)) + 
  geom_point() +   
  labs(y = 'Порядковый номер \nв датасете', x = 'Значения переменной')


all_data %>% 
  ggplot(aes(y = 1:nrow(.), x = Median_area)) + 
  geom_point() +   
  labs(y = 'Порядковый номер \nв датасете', x = 'Значения переменной')


# Строим модель

Model <- lm(Velocity ~ Median_area + Mean_area + CV + Cond_T , data = all_data)

# Так хочется!

summary(Model) #СУПЕР!!! Бежим писать статью в Nature?

# ВАЖНО: Не надо сразу смотреть в summary()!!!


# Условия применимости линейной регрессии
# 1. Линейная связь между зависимой переменной (Y) и предикторами (X)
# 2. Независимость значений Y друг от друга
# 3. Нормальное распределение Y для каждого уровня значений X
# 4. Гомогенность дисперсий Y для каждого уровня значений X
# 5. Отсутствие коллинеарности предикторов (для можественной регрессии)


# Проверка на коллениарность предикторов

# Variance inflation factor, VIF

car::vif(Model)

# Мультиколлинеарность опасна!
# - Оценки коэффициентов модели нестабильны (даже могут менять знак при небольших изменениях модели или исходных данных).
# - Предикторы "маскируют" друг друга
# - Стандартные ошибки оценок параметров увеличатся в sqrt(VIF) раз.
# - В результате меньше шансов заметить влияние предиктора, т.к. уровень значимости (p-value) в тестах будет выше.



# Меняем модель

Model <- lm(Velocity ~ Median_area + CV + Cond_T , data = all_data)

vif(Model)


# Меняем модель

Model <- lm(Velocity ~ Median_area + CV , data = all_data)

vif(Model)

###################
# Анализ остатков #
###################

Model_diagnostic <- fortify(Model)


#Есть ли влиятельные наблюдения?

# График расстояния Кука

Model_diagnostic %>% 
ggplot(aes(x = 1:nrow(.), y = .cooksd)) + 
  geom_bar(stat = "identity") 


# Нормальное распределение ОСТАТКОВ (!)

Model_diagnostic %$% 
car::qqPlot(.stdresid)

Model_diagnostic %>% 
ggplot(aes(sample = .stdresid)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Квантили стадартизованного нормального распределения", y = "Квантили в распределении стандартизованных остатков")
  
Model_diagnostic %>%
  ggplot(aes(x = .stdresid)) +
  geom_histogram()

# Самый главный график: график рассеяния остатков

Model_diagnostic %>%
  ggplot(aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  geom_smooth(method = "lm", color = "red")


#Сага о ПАТТЕРНАХ В ОСТАТКАХ (!!!)

dat <- read.table('Data/orly_owl_Lin_4p_5_flat.txt')
fit <- lm(V1 ~ V2 + V3 + V4 + V5, data = dat)
summary(fit) #Супер!!!

# Смотрим на график рассеяня остатков

fit %>% 
  fortify() %>%   
# Вставьте недостающий кусок кода


car::residualPlot(fit, pch = ".")

# В графике остатков не должно быть паттернов! В хорошей модели остатки - это просто ШУМ вокруг нуля

#################################################################
# Зоопарк нарушений условий применимости регрессионного анализа # 
#################################################################

# Загон №1 (Curva nonlinearis)

#### Генерируем данные #########
set.seed(90829)
x1 <- rnorm(100, 5, 1)
y1 <- 10 + x1  - x1^2  + rnorm(100, 0, 2)
dat1 = data.frame(x1, y1)
################################

ggplot(dat1, aes(x = x1, y =  y1)) + geom_point()+ 
  geom_smooth(method="lm", alpha = 0.7)


mod1 <- lm(y1 ~ x1, data = dat1)

summary(mod1) #Можно ли доверять этим резултатам?



# Данные для графиков остатков
mod1_diag <- fortify(mod1)

# 1) График расстояния Кука
ggplot(mod1_diag, aes(x = 1:nrow(mod1_diag), y = .cooksd)) + 
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <-
ggplot(data = mod1_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели (желательно, включить и график зависимости остатков от тех предикторов, которые не включены в модель)
gg_resid + aes(x = x1)

# 4) Квантильный график остатков
qqPlot(mod1, id = FALSE)

# В чем проблема?
gg_resid + geom_smooth()


# --------------------------------------------------------

# Загон № 2 (Heteroscedastica horribilis)

#######################
set.seed(7657674)
x2 <- runif(1000, 1, 100)
b_0 <- 100;  b_1 <- 20
h <- function(x) x^0.5
eps <- rnorm(1000, 0, h(x2))
y2 <- b_0 + b_1 * x2 + eps
dat2 <- data.frame(x2, y2)
######################

ggplot(dat2, aes(x = x2, y = y2)) + 
  geom_point() + 
  geom_smooth(method = "lm")

mod2 <- lm(y2 ~ x2, data = dat2)

summary(mod2) #Можно ли доверять этим резултатам?

# Данные для графиков остатков
mod2_diag <- fortify(mod2)
# 1) График расстояния Кука
ggplot(mod2_diag, aes(x = 1:nrow(mod2_diag), y = .cooksd)) + 
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod2_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x2)

# 4) Квантильный график остатков
qqPlot(mod2, id = FALSE)

# В чем проблема?

# --------------------------------------------------------

# Загон №3 (Mensurae atypica)

#################
set.seed(9283)
x3 <- rnorm(25, 50, 10)
b_0 <- 20; b_1 <- 20; eps <- rnorm(50, 0, 100)
y3 <- b_0 + b_1*x3 + eps
y3[100] <- 1000; x3[100] <- 95; y3[99] <- 1300; x3[99] <- 90; y3[98] <- 1500; x3[98] <- 80
dat3 <- data.frame(x3, y3)
#################

ggplot(dat3, aes(x=x3, y=y3)) + geom_point() + geom_smooth(method="lm")


mod3 <- lm(y3 ~ x3, data = dat3)

summary(mod3) #Можно ли доверять этим резултатам?


# Данные для графиков остатков
mod3_diag <- fortify(mod3)
# 1) График расстояния Кука
ggplot(mod3_diag, aes(x = 1:nrow(mod3_diag), y = .cooksd)) + 
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod3_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x3)

# 4) Квантильный график остатков
qqPlot(mod3, id = FALSE)


# В чем проблема?

# --------------------------------------------------------

# Загон № 4 (Anima amissa)

#######################
set.seed(7657674)
x1 <- runif(30, 1, 100)
x2 <- runif(30, 1, 100)
x3 <- runif(30, 1, 100)
b_0 <- 100;  b_1 <- -20; b_2 <- 20; b_3 <- 0
eps <- rnorm(30, 0, 1)
y <- b_0 + b_1 * x1 + b_2*x2 + b_3*x3 + eps
dat4 <- data.frame(x1, x2, x3, y)
######################

ggplot(dat4, aes(x=x1, y=y)) + geom_point() + geom_smooth(method="lm")


mod4 <- lm(y ~ x1, data = dat4) 

summary(mod4) #Можно ли доверять этим результатам?


# Данные для графиков остатков
mod4_diag <- fortify(mod4)
# 1) График расстояния Кука
ggplot(mod4_diag, aes(x = 1:nrow(mod4_diag), y = .cooksd)) + 
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod4_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline(yintercept = 0)
gg_resid +
  geom_smooth(method = "lm")

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x1)

gg_resid + aes(x = x2)

gg_resid + aes(x = x3)

# 4) Квантильный график остатков
qqPlot(mod4, id = FALSE)


# В чем проблема?

# --------------------------------------------------------

# Загон № 5 (Perdidit commercium)

#######################
set.seed(7657674)
x1 <- rnorm(100, 10, 1)
x2 <- rnorm(100, 100, 1)
b_0 <- 100;  b_1 <- -1; b_2 <- 2; b_12 <- 10
dat5 <- data.frame(x1, x2)
X <- model.matrix(~ x1 * x2, data = dat5)
dat5$y <- X %*% c(b_0, b_1, b_2, b_12) + rnorm(100, 0, 1)
###################### 

ggplot(dat5, aes(x=x1, y=y)) + geom_point() + geom_smooth(method="lm")

ggplot(dat5, aes(x=x2, y=y)) + geom_point() + geom_smooth(method="lm")


mod5 <- lm(y ~ x1 + x2, data = dat5)

summary(mod5) #Можно ли доверять этим резултатам?

# Данные для графиков остатков
mod5_diag <- fortify(mod5)
# 1) График расстояния Кука
ggplot(mod5_diag, aes(x = 1:nrow(mod5_diag), y = .cooksd)) + 
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod5_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline(yintercept = 0)

gg_resid 

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x1)

gg_resid + aes(x = x2)


# 4) Квантильный график остатков
qqPlot(mod5, id = FALSE)

# В чем проблема?

# --------------------------------------------------------

# Загон № 6 (Data longitudinalis)

#######################
set.seed(657674)
ts_AR1 <- as.numeric(arima.sim(n = 30, list(ar = 0.999)))
ts_AR2 <- as.numeric(arima.sim(n = 30, list(ar = 0.999)))
dat6 <- data.frame(Year = 1:30, V1 = ts_AR1, V2 = ts_AR2)
###################### 

ggplot(dat6, aes(x=V2, y=V1)) + geom_point() + geom_smooth(method="lm")



mod6 <- lm(V1 ~ V2, data = dat6)

summary(mod6) #Можно ли доверять этим результатам?

# Данные для графиков остатков
mod6_diag <- fortify(mod6)
# 1) График расстояния Кука
ggplot(mod6_diag, aes(x = 1:nrow(mod6_diag), y = .cooksd)) + 
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod6_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline(yintercept = 0)

gg_resid 

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = V2)

gg_resid + aes(x = dat6$Year) 


# 4) Квантильный график остатков
qqPlot(mod6, id = FALSE)

# В чем проблема?

# Autocorrelation function
acf(residuals(mod6))

# Durbin-Watson Test
lmtest::dwtest(residuals(mod6) ~ dat6$Year)


#########################################################


# Возвращаемся к нашим данным


# 1) График расстояния Кука
ggplot(Model_diagnostic, aes(x = 1:nrow(Model_diagnostic), y = .cooksd)) + 
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = Model_diagnostic, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline(yintercept = 0)

gg_resid 



# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = all_data$Median_area ) + geom_smooth(method = "lm")

gg_resid + aes(x = all_data$Mean_area ) + geom_smooth(method = "lm")

gg_resid + aes(x = all_data$SD ) + geom_smooth(method = "lm")

gg_resid + aes(x = all_data$CV ) + geom_smooth(method = "lm")

gg_resid + aes(x = all_data$Cond_T ) + geom_smooth(method = "lm")

ggplot(data = Model_diagnostic, aes(x = all_data$Well, y = .stdresid)) + 
  geom_boxplot() +
  geom_hline(yintercept = 0)


# 4) Квантильный график остатков
qqPlot(Model, id = FALSE)


# Теперь смотрим на summary()
summary(Model)

########################
# Визуализация модели  #
########################


# Годится ли такая визуализация?
  
library(cowplot)

Pl1 <-
  ggplot(all_data, aes(x = Median_area, y = Velocity)) +
  geom_point()+
  geom_smooth(method = "lm")

Pl2 <-
  ggplot(all_data, aes(x = CV, y = Velocity)) +
  geom_point()+
  geom_smooth(method = "lm")

plot_grid(Pl1, Pl2)

# В чем проблема данной визуализации?



#### Правильное решение #################

# Создаем искусственный датасет, отражающий варьирование предикторов

My_Data <- data.frame(
  Median_area = seq(min(all_data$Median_area), max(all_data$Median_area), length.out = 100),
  CV = mean(all_data$CV))

predicted_values <- predict(Model, newdata = My_Data, se.fit = TRUE)

My_Data$Predicted <- predicted_values$fit 

My_Data$SE <- predicted_values$se.fit

t_crit <- qt(p = 0.975, df = Model$df.residual) 

ggplot(My_Data, aes(x = Median_area)) +
  geom_line(aes(y = Predicted), color = "blue") +
  geom_ribbon(aes(ymin = Predicted - t_crit*SE, ymax = Predicted + t_crit*SE), alpha = 0.2) +
  geom_point(data = all_data, aes(y = Velocity))


# Неплохо бы нанести данные по контролю

cont <- read_excel("Data/cell_mov_speed.xlsx", sheet = "Control")

Model_control <- lm(Velocity ~ 1, data = cont)
summary(Model_control)

predict(Model_control, se.fit = TRUE)$fit [1]
predict(Model_control, se.fit = TRUE)$se.fit [1]


mean(cont$Velocity)
sd(cont$Velocity)/sqrt(nrow(cont))

t_crit_cont <- qt(p = 0.975, df = Model_control$df.residual)

My_Data$Control <- mean(cont$Velocity)
My_Data$SE_Control <- sd(cont$Velocity)/sqrt(nrow(cont))


ggplot(My_Data, aes(x = Median_area)) +
  geom_line(aes(y = Predicted), color = "blue") +
  geom_ribbon(aes(ymin = Predicted - t_crit*SE, ymax = Predicted + t_crit*SE), alpha = 0.2) +
  geom_point(data = all_data, aes(y = Velocity)) +
  geom_ribbon(aes(ymin = Control - t_crit_cont*SE_Control, ymax = Control + t_crit_cont*SE_Control ), alpha = 0.2) +
  geom_line(aes(y = Control), linetype = 2)




# НО!!! Здесь все равно осталась одна (может быть даже большая) проблема!

######################
# Mixed Effect Model #
######################


library(lme4)

Model_ri <- lmer(Velocity ~ Median_area + CV + (1|Field2), data = all_data)   


Model_ri <- lmer(Velocity ~ scale(Median_area) + scale(CV) + (1|Field2), data = all_data)   

plot(Model_ri)

Anova(Model_ri)

summary(Model_ri)

library(performance)
icc(Model_ri)

# Роль случайного фактора не столь высока!

##################
# But not today! #
##################

