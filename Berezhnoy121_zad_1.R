#Бережной Егор ПАЭ 121 2020 год - для региона 48 Липецкая область
#Задание: рассчитайте урожайность пшеницы в период с 2005 по 2015 год взяв для рассчета средние суммы активных температур за эти годы, с 8 ближайших метеостанций
#Установка и проверка рабочей директории
setwd("C:/Rstudio")
getwd()
library (tidyverse)
library (rnoaa)
#Скачиваем список метеостанций
station_data=ghcnd_stations()
write.csv(station_data,"station_data20.csv")
station_data = read.csv("station_data20.csv")
#Формирование списка метеостанций
lipetsk = data.frame(id = "Lipetsk", latitude = 52.6031,  longitude = 39.5708)
#Задание временного периода и необходимых переменных 
lipetsk_around = meteo_nearby_stations(lat_lon_df = lipetsk, station_data = station_data, limit = 8, var = c("TAVG"), year_min = 2005, year_max = 2015)
#Получение индентификатора метеостанций Липецка
lipetsk_id =lipetsk_around[["Lipetsk"]][["id"]][1]
summary(lipetsk_id)
str(lipetsk_id)

#Получение таблицы ближайших метеостанций
lipetsk_table = data.frame (lipetsk_around)
summary (lipetsk_table)

#Создание цикла, в котором скачиваюсь необходимые данные с метеостанций
#Промежуточный объект куда скачиваются данные с конкретной метеостанции
all_i = data.frame()
#Объкт куда скачиваются все данные всех метеостанций
all_lipetsk_meteodata = data.frame()
#Создание цикла для всех метеостанций
for(i in 1:8)
{
all_i=meteo_tidy_ghcnd(stationid =lipetsk_id, var = "TAVG", date_min = "2005-01-01", date_max = "2015-12-31") 
all_lipetsk_meteodata=rbind(all_lipetsk_meteodata, all_i)
}
#Запись полученных данных в файл
write.csv (all_lipetsk_meteodata,"all_lipetsk_meteodata.csv")
all_lipetsk_meteodata

#Ввод констант для расчета урожайности
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) #отношение числа дней i-го месяца входящих в период вегетации культуры, к общему числу дней в месяце
Kf = 300 #коэф использования ФАР посевом
Qj = 1600 #калорийность урожая культуры
Lj = 2.2 #сумма частей основной и побочной продукции
Ej = 25 # Стандартная влажность культуры

# Устанавливаем пакет для работы с датами
install.packages(lubridate)
library(lubridate)

#Расчет суммы активных температур
dat = all_lipetsk_meteodata %>%
#Разбиваем дату на года и месяца
mutate(year = year(date), month = month(date)) %>%
#Приводим температуру к нормальному виду
mutate(tavg = tavg/10) %>%
#Фильтруем температуру на больше 5 градусов
filter(tavg>5) %>%
group_by(year,month,id) %>%
#Суммируем температуру без пустых значений
summarise (summ = sum(tavg,na.rm=TRUE))%>%
group_by(month) %>%
#Считаем среднюю температуру
summarise (s = mean (summ, na.rm = TRUE)) %>%
#Создаем колонки для расчета
mutate (a = afi[min(month):max(month)], b = bfi[min(month):max(month)], d = di[min(month):max(month)]) %>%  
#Рассчитываем урожайность по формуле
mutate (fert = (((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)))/10 )

#Определяем урожайность пшеницы
Yield = sum(dat$fert); Yield
  
