#Долгополов Дмитрий Леонидович - создайте модель множественной линейной
#регрессии ночных потоков углекислого газа за осенний период 2013 года
#по данным измерений методом турбулентной пульсации
setwd("C:/MathMod")
library(rnoaa)
library(tidyverse)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)
eddypro=read.csv("eddypro.csv",skip=1,na=c("","NA","-9999","-9999.0"), comment=c("["))
#удаляем первую строку
eddypro=eddypro[-1,]
glimpse(eddypro)
#удаляем переменную roll
eddypro=select(eddypro, -(roll))
#преобразуем char в factor
eddypro = eddypro %>% mutate_if(is.character, factor)
glimpse(eddypro)
#преобразуем символы в текст
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
#удаляем строки, где есть значения  NA
eddypro = drop_na(eddypro)
#фильтруем данные за осенний период, а именно, с  1 сентября (244 день) по 11 ноября (315.854 день)
eddypro = filter(eddypro, DOY >= 244 & DOY <= 315.854)
#Отфильтруем данные для нoчного периода
eddypro = filter(eddypro, daytime==FALSE)
#отберем переменные типа numeric
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
#Таблица, содержащая все остальные колонки 
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]
#Переходим к кореляционному анализу
cor_eddy = cor(eddypro_numeric );
cor_eddy
#Преобразуем в data.frame
cor_eddy = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)
#Коэф. детерминации >0.1
vars = row.names(cor_eddy)[cor_eddy$co2_flux^2 > 0.1] %>% na.exclude;vars
#преобразуем имена переменных из вектора в одну формулу
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")); formula
#Создадим обучающую и тестовую непересекающиеся выборки
row_numbers = 1:length(eddypro$co2_flux);row_numbers
teach = sample(row_numbers, floor(length(eddypro$co2_flux)*0.7));teach
test = row_numbers[-teach];test
teaching_eddypro = eddypro_numeric[teach,]
testing_eddypro = eddypro_numeric[test,]

#Построение модели
#Модель 1, по обучающей выборке с учётом всех переменных в formula
model1=lm(formula,data=teaching_eddypro)
# помострим коэффициенты
coef(model1)
#остатки
resid(model1)
# доверительный интервал
confint(model1)
# посмотрим р-значения по модели
summary(model1)
#Коэффициент детерминации = 0.9378
#Дисперсионный анализ
anova(model1)
#Построим график нормального распределения:
plot(model1,2)
# согласно ДА мы видим какие переменные у нас не значимые: 
# air_temperature, e , es, RH, flowrate

# cоздадим модель 2 добавив в неё значимые переменные из результатов
#функции anova()(со значимостью до 0.01, соответственно ***,** и * пометки)
formula2= co2_flux~DOY+ h2o_molar_density +h2o_mole_fraction+
  h2o_mixing_ratio +sonic_temperature+air_pressure+
  air_density+air_heat_capacity+air_molar_volume+water_vapor_density+
  specific_humidity+ Tdew+ u_unrot+un_co2_flux+w.co2_cov+h2o+h2o.1
model2= lm(formula2, data = teaching_eddypro)
#Посмотрим информацию о модели
summary(model2)
#Посмотрим коэффициенты
coef(model2)
#Выведем остатки
resid(model2)
#доверительный интервал
confint(model2)
#дисперсионный анализ
anova(model2)
#Построим график нормального распределения:
plot(model2,2)
# Сравним модели 2 и 1
anova(model2, model1)

# Модель 3 - добавим в неё переменные, полученные при помощи
#функции anova() с коэффициентом значимости меньше 0.001
formula3= co2_flux~DOY+ h2o_molar_density +h2o_mole_fraction+
  h2o_mixing_ratio +sonic_temperature+air_pressure+
  air_heat_capacity+water_vapor_density+
  Tdew+ u_unrot+un_co2_flux+h2o.1
model3= lm(formula3, data = teaching_eddypro)
#Посмотрим информацию о модели
summary(model3)
#Посмотрим коэффициенты
coef(model3)
#Выведем остатки
resid(model3)
#доверительный интервал
confint(model3)
#дисперсионный анализ
anova(model3) 
# Сравним модели 2 и 3
anova(model3, model2)
#Посмотрим графики модели
plot(model3,2)

# Модель 4 - с переменными, полученные при помощии функции anova() с пометками "***" 
formula4= co2_flux~DOY+ h2o_molar_density +h2o_mole_fraction+
  h2o_mixing_ratio +sonic_temperature+air_pressure+
  air_heat_capacity+water_vapor_density+
  u_unrot+un_co2_flux+h2o.1
model4= lm(formula4, data = teaching_eddypro)
#Посмотрим информацию о модели
summary(model4)
#Посмотрим коэффициенты
coef(model4)
#Выведем остатки
resid(model4)
#доверительный интервал
confint(model4)
#дисперсионный анализ
anova(model4)
# Сравним модели 4 и 3
anova(model4, model3)
#Посмотрим графики
plot(model4,2)

#Корреляционный анализ
#Используем только переменные, участвующие в корреляционном анализе
cor_teaching_eddypro = select(teaching_eddypro, DOY, h2o_molar_density,
                                h2o_mole_fraction,h2o_mixing_ratio,
                                sonic_temperature,air_pressure,
                                air_heat_capacity,water_vapor_density,
                                u_unrot,un_co2_flux,h2o.1)
#Получаем таблицу коэффициентов корреляции
cor_eddypro = cor(cor_teaching_eddypro) %>% as.data.frame
#Построение графиков по полученной модели

#Построим график co2_flux от co2_flux, использовав значения,
#полученные на модели 4, и на основе обучающей выборки
# в идеале линия должна  пройти через все точки, а так как график co2_flux
#от самого себя, то он должен идти под углом 45 градусов
qplot(co2_flux,co2_flux, data = teaching_eddypro)+ geom_line(aes(y = predict(model4, teaching_eddypro)))
#График расположен под углом 45 градусов и проходит почти через все точки

#Построим график co2_flux от co2_flux, использовав значения,
#полученные на модели 4, и на основе тестирующей выборки
qplot(co2_flux,co2_flux, data = testing_eddypro)+ geom_line(aes(y = predict(model4, testing_eddypro)))

#Для примера выведем несколько графиков зависимостей переменной co2_flux от:
#sonic_temperature, air_pressure, h2o_molar_density, 
#un_co2_flux на основе тестирующей модели
qplot(sonic_temperature,co2_flux, data = testing_eddypro) + geom_line(aes(y = predict(model4, testing_eddypro)))
qplot(air_pressure,co2_flux, data = testing_eddypro) + geom_line(aes(y = predict(model4, testing_eddypro)))
qplot(h2o_molar_density,co2_flux, data = testing_eddypro) + geom_line(aes(y = predict(model4, testing_eddypro)))
qplot(un_co2_flux,co2_flux, data = testing_eddypro) + geom_line(aes(y = predict(model4, testing_eddypro)))
