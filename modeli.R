#Кочетова – для региона 55 рассчитайте урожайность пшеницы в 2002 году, взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с 24 ближайших метеостанций
setwd ("D:/modeli") 
getwd() 
#Подключим библиотеки
library(tidyverse) 
library(rnoaa) 
library(lubridate) 
#данные для расчета
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00) 
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00) 
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00) 
Kf = 300 # Коэффициент использования ФАР посевом 
Qj = 1600 # калорийность урожая культуры 
Lj = 2.2 # сумма частей основной и побочной продукции 
Ej = 25 # стандартная влажность культуры 
#station_data = ghcnd_stations() 
station_data = read.csv("station_data.csv") 
#После получения списка всех станций, получим список станций ближайших к столице данного региона,создав таблицу с именем региона и координатами его столицы
omsk = data.frame(id = "OMSK", latitude = 54.988 , longitude = 73.360 ) 
#найдем станции, соответствующие критериям
omsk_around = meteo_nearby_stations(lat_lon_df = omsk, station_data = station_data, 
                                    limit = 24, var = c("PRCP", "TAVG"), 
                                    year_min = 1993, year_max = 2002) 
# объединим данные по всем метеостанциям 
all_omsk_data = tibble() 
for (v in 1:24) 
{ 
  omsk_id = omsk_around[["OMSK"]][["id"]][v] 
  # 
  data = meteo_tidy_ghcnd(stationid = omsk_id, 
                          var="TAVG", 
                          date_min="2002-01-01", 
                          date_max="2002-12-31") 
  all_omsk_data = bind_rows(all_omsk_data, data) 
} 
# произведем обработку полученных данных 
clean_data = all_omsk_data %>% 
  # создадим колонки year и month для группировки... 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month, id) %>% 
  # суммирования 
  summarize(tavg = sum(tavg[tavg>5], na.rm=TRUE)/10) %>% 
  group_by(month) %>% 
  # нахождения средних месячных активных температур: 
  summarize(s = mean(tavg, na.rm = TRUE)) %>% 
  # создадим колонки для расчета: 
  mutate (a = afi, b = bfi, d = di) %>% 
  # и рассчитаем урожайность для каждого месяца: 
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) ) 
# сумма урожайностей равна: 
Yield = sum(clean_data$fert); Yield
