#Бережной Егор ПАЭ 121 2020 год - для региона 48 Липецкая область
#Задание: рассчитайте урожайность пшеницы в период с 2005 по 2015 год взяв для рассчета средние суммы активных температур за эти годы, с 8 ближайших метеостанций
#Установка и проверка рабочей директории
setwd("C:/Rstudio")
getwd()
#Установление пакеты необходимые для дальнейшей работы
library (tidyverse)
library (rnoaa)
#Скачиваем список метеостанций и записываем их в файл
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
#Создание цикла для всех метеостанций с 2005 по 2015 год с учетом температур
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

#Создание датафрейма с данными 
all_lipetsk_meteodata=read.csv("all_lipetsk_meteodata.csv")
all_lipetsk_meteodata [,"year"] = year (all_lipetsk_meteodata$date)
all_lipetsk_meteodata [,"month"] = month (all_lipetsk_meteodata$date)
all_lipetsk_meteodata [,"day_of_the_year"] = yday(all_lipetsk_meteodata$date)
years_lipetsk_meteodata = filter (all_lipetsk_meteodata, year>2005 & year<2015)

#Подсчет средних активных температур по месяцам, приводим к нормальному виду, превращаем в 0 все NA и значения меньше 5 градусов
years_lipetsk_meteodata [,"tavg"] = years_lipetsk_meteodata$tavg/10
years_lipetsk_meteodata [is.na(years_lipetsk_meteodata$tavg),"tavg"] = 0
years_lipetsk_meteodata [years_lipetsk_meteodata$tavg<5, "tavg"] = 0

#Проверяем, какие данные получились
summary(years_lipetsk_meteodata)

#Считаем суммарную температуру за месяц для всех станций
#Группируем по id станциям, годами и месяцам
alldays=group_by (years_lipetsk_meteodata,id,year,month)
#Суммируем температуру по этим группам
sum_t=summarize (alldays, tsum = sum(tavg))
#Группируем данные по месяцам
group_months=group_by(sum_t,month)
#Находим для метеостанций среднее по месяцам
sum_t_months=summarize(group_months, St = mean(tsum))
#Рассчитываем Fi по месяцам
sum_t_months = mutate(sum_t_months, Fi = afi+bfi*1*St)
#Рассчитываем Yi
sum_t_months = mutate (sum_t_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
#Рассчитываем урожай как сумму по месяцам
Yield = sum(sum_t_months$Yi)
Yield
#Результат = 14,79 ц/га урожайность пшеницы
