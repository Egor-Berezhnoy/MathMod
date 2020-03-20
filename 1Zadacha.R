#Бережной Егор - для региона 48 Липецкая область
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
lipetsk = data.frame(id = "LIPETSK", latitude = 52.6031,  longitude = 39.5708)
#Задание временного периода и необходимых переменных 
lipetsk_around = meteo_nearby_stations(lat_lon_df = lipetsk, station_data = station_data, limit = 8, var = c("TAVG"), year_min = 2005, year_max = 2015)
#Получение индентификатора метеостанций Липецка
lipetsk_id =lipetsk_around[["LIPETSK"]][["id"]][i]
summary(lipetsk_id)
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
all_i=meteo_tidy_ghcnd(stationid =lipetsk_id, var="TAVG", date_min = "2005-01-01", date_max = "2015-12-31") 
all_lipetsk_meteodata=rbind(all_lipetsk_meteodata, all_i)
}
#Запись полученных данных в файл
write.csv (all_lipetsk_meteodata,"all_lipetsk_meteodata.csv")
all_lipetsk_meteodata
  
