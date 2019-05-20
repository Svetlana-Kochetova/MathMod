# Кочетова - создайте модель множественной линейной регрессии ночных потоков углекислого газа за весенний период 2013 года по данным измерений методом турбулентной пульсации
setwd ("D:/modeli") 
getwd() 
#подключаем tidyverse 
library(tidyverse) 
library(lubridate) 
data=write.csv("eddypro.csv")
#читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
data = read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
#ещё одну строку убираем
data = data[-1,] 
#убираем ненужные колонки 
data = data[, c(-1, -3, -9, -12, -15, -18, -21, -30, -35, -63 , -70, -88:-99) ] 
#преобразуем строковые значения в факторные 
data = data %>% mutate_if(is.character, factor) 
#заменяем конфликтующие знаки колонок 
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>% 
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
#Посмотрим, что получилось 
glimpse(data) 


#изменим тип данных колонки daytime 
data $ daytime  = as.logical (data $ daytime)
#оставим данные только по ночным измерениям 2013 года:
data  =  data [data $ daytime  == FALSE,]

#оставим данные только по весеннему периоду 2013 года: 
data = data[data$DOY>=60 & data$DOY<=151 & year(data$date) == 2013, c(1:ncol(data))] 
#выберем все переменные типа numeric 
data_numeric = data[,sapply(data,is.numeric) ] 
#все остальные переменные: 
data_non_numeric = data[,!sapply(data,is.numeric) ] 


# создадим матрицу для корелляционного анализа и преобразуем ее в таблицу, выбрав нужный столбец (потоки паров углекислого газа) 
#коэффициенты корреляции
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% 
select(co2_flux) 
#выберем имена переменных (строк) с коэффициентом детерминации больше 0.2 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .2] %>% na.exclude; vars 
#соберем переменные из вектора в одну формулу: 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")); formula 
#не обязательно
#создадим обучающую и тестирующую выборки: 
row_numbers = 1:length(data$date) 
teach = sample(row_numbers, floor(length(data$date)*.7)) 
test = row_numbers[-teach] 
#непересекающиеся подвыборки: 
teaching_tbl_unq = data[teach,] 
testing_tbl_unq = data[test,] 

# МОДЕЛЬ 1 
#создаем модель линейной регрессии 
model1 = lm(formula, data = data);model1
#коэффициенты 
coef(model1) 
#остатки 
resid(model1) 
#доверительный интервал 
confint(model1) 
#P-значения по модели 
summary(model1) 
#дисперсионный анализ 
anova(model1) 
#графическое представление модели: 


# МОДЕЛЬ 2 
formula = as.formula(paste("co2_flux~", "(", paste(vars,collapse = "+"), ")^2", sep="", collapse = NULL));formula 
#создаем модель линейной регрессии 
model2 = lm(formula, data = data);model2 
#коэффициенты 
coef(model2) 
#остатки 
resid(model2) 
#доверительный интервал 
confint(model2) 
#P-значения по модели 
summary(model2) 
#дисперсионный анализ 
anova(model2) 
#графическое представление модели: 


# МОДЕЛЬ 3 
formula2 = co2_flux~(T.+rand_err_co2_flux+co2_molar_density+un_co2_flux) 
#создаем модель линейной регрессии 
model3 = lm(formula2, data = data);model3 
#коэффициенты 
coef(model3) 
#остатки 
resid(model3) 
#доверительный интервал 
confint(model3) 
#P-значения по модели 
summary(model3) 
#дисперсионный анализ 
anova(model3) 
#графическое представление модели: 

# МОДЕЛЬ 4 
formula3 = co2_flux~(T.+rand_err_co2_flux+co2_molar_density+un_co2_flux)^2 
#создаем модель линейной регрессии 
model4 = lm(formula3, data = data);model4 
#коэффициенты 
coef(model4) 
#остатки 
resid(model4) 
#доверительный интервал 
confint(model4) 
#P-значения по модели 
summary(model4) 
#дисперсионный анализ 
anova(model4) 

# МОДЕЛЬ 5 
formula4 = co2_flux~(T.+rand_err_co2_flux+co2_molar_density+un_co2_flux+T.:rand_err_co2_flux+T.:co2_molar_density+T.:un_co2_flux)^2 
#создаем модель линейной регрессии 
model5 = lm(formula4, data = data);model5 
#коэффициенты 
coef(model5) 
#остатки 
resid(model5) 
#доверительный интервал 
confint(model5) 
#P-значения по модели 
summary(model5) 
#дисперсионный анализ 
anova(model5) 

# МОДЕЛЬ 6 
formula5 = co2_flux~(T.+rand_err_co2_flux+co2_molar_density+un_co2_flux+T.:rand_err_co2_flux+T.:co2_molar_density+T.:un_co2_flux)^2 
#создаем модель линейной регрессии 
model6 = lm(formula4, data = data);model6 
#коэффициенты 
coef(model6) 
#остатки 
resid(model6) 
#доверительный интервал 
confint(model6) 
#P-значения по модели 
summary(model6) 
#дисперсионный анализ 
anova(model6) 
