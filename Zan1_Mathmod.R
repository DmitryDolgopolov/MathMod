#Долгополов Дмитрий Леонидович – для региона 13 (Республика Мордовия) рассчитайте урожайность
#пшеницы в 2003 году, взяв для расчета средние суммы активных температур
#за текущий год, с 14 ближайших метеостанций.
library(tidyverse)
library(rnoaa)
library(lubridate)
#station_data = ghcnd_stations()
#write.csv(station_data, "station_data.csv")
station_data = read.csv("station_data.csv")
saransk=data.frame(id="SARANSK",latitude=54.1101,longitude=45.1029)
#выберем 14 ближайших метеостанций в заданный временной период
saransk_around=meteo_nearby_stations(lat_lon_df =saransk, station_data = station_data,
                                     limit = 14, var ="TAVG",
                                     year_min = 2003, year_max = 2003)
saransk_around
#вычислим идентификаторы ближайших метеостанций
all_saransk_meteodata=data.frame()
for (v in 1:14)
{saransk_id=saransk_around[["SARANSK"]][["id"]][v]
#получим все данные с ближайших метеостанций
data=meteo_tidy_ghcnd(stationid = saransk_id,
                      var ="TAVG",
                      date_min = "2003-01-01",
                      date_max = "2003-12-31")
all_saransk_meteodata =(bind_rows(all_saransk_meteodata, data))}
#запишем полученые данные в файл csv
write.csv(all_saransk_meteodata, "all_saransk_meteodata.csv")

#cчитаем их
all_saransk_meteodata=read.csv("all_saransk_meteodata.csv")
#делим значения температур на 10
all_saransk_meteodata[,"tavg"]=all_saransk_meteodata$tavg/10
#добавляем колонки "год","месяц","день" в таблицу
all_saransk_meteodata=mutate(all_saransk_meteodata,year = year(date),
                             month = month(date), day = day(date))
#выбираем активные температуры больше 5 градусов 
all_saransk_meteodata_temp =filter(all_saransk_meteodata,tavg>5 ) 

#группируем по метеостанциям и месяцам
group_saransk=group_by(all_saransk_meteodata_temp,id,month)
#находим сумму температур по месяцам на каждой станции
sum_temp=summarize(group_saransk,tsum=sum(tavg))
#группируем по месяцам
sum_temp=group_by(sum_temp,month)
##получаем среднюю активную температуру по месяцам со всех ближайших станций
mean_sum_month=summarize(sum_temp,tmean=mean(tsum))

##Расчёт урожайности
#константы
afi=c(32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83)
bfi=c(11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87)
#отношение числа дней i-го месяца, входящих в период вегетации культуры,
#к общему числу дней в месяце
di=c(0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00)
#коэффициент для экспозиции склона - считаем, что поля идеально ровные
y=1.0
#коэффициент использования ФАР посевом
Kf=300
#калорийность урожая культуры
Qj=1600
#коэффициент "Сумма частей основной и побочной продукции"
Lj=2.2
#коэффициент "Стандартная влажность культуры"
Ej=25
#номер месяца
i={4:10}
St=mean_sum_month$tmean

#Расчёт Fi по месяцам
Fi=afi + bfi * y * St
Fi
#Расчёт урожайности Yi по месяцам
Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej))
Yi
#Расчёт урожайности за 2003 год
Yeild=sum(Yi)
Yeild
##Урожайность пшеницы в Мордовии за 2003 год составляет 16.54087 ц/га
