#import packiet�w
library(sqldf)
library(MASS)
library(ggplot2)
library(moments)
library(olsrr)
library(car)
library(gridExtra)
library(rpart)
library(SDMTools)
library(caret)
library(BBmisc)
library(class)

#definicje funkcji:
univariate_statistics <- function(data_variable) {
    print(sprintf("�rednia: %.4f", mean(data_variable)))
    print(sprintf("Wariancja: %.4f", var(data_variable)))
    print(sprintf("Mediana: %.4f", median(data_variable)))
    medi_temp = median(data_variable)
    print(sprintf("�rednie odchylenie bezwgl�dne: %.4f", mad(data_variable, medi_temp))) 
    library(moments)
    print(sprintf("Sko�no�� rozk�adu: %.4f", skewness(data_variable)))
    print(sprintf("Kurtoza rozk�adu: %.4f", kurtosis(data_variable)))
    par(mfrow=c(1,3))
    hist(data_variable)
    boxplot(data_variable)
    plot(data_variable)
}

#---------------------zadanie 1-----------------------------
#---------------------a-----------------------------

#wczytanie danych
t_cars = MASS::Cars93

#tworzenie przelicznik�w:
gal_l = 3.8         #gallons to litres
m_km = 1.5          #miles to kilometers
pound_kg = 0.4536   #pounds to kilogrames
dollar_PLN = 3.35   #dollar to PLNs

#dodanie nowych zmiennych
km_per_l_1 =  (t_cars$MPG.city * m_km / gal_l)^(-1) * 100
km_per_l_2 =  (t_cars$MPG.highway * m_km / gal_l)^(-1) * 100
Weight_kg =   t_cars$Weight * pound_kg
price_PLN =   t_cars$Min.Price * dollar_PLN * 1000

t_cars <- cbind(t_cars, km_per_l_1)
t_cars <- cbind(t_cars, km_per_l_2)
t_cars <- cbind(t_cars, Weight_kg)
t_cars <-  cbind(t_cars, price_PLN)
#---------------------b---------------------
#statystyki pr�bkowe
average_price   = mean(t_cars$price_PLN)
variance        = var(t_cars$price_PLN)
median_price    = median(t_cars$price_PLN)
mad_factor      = mad(t_cars$price_PLN, median_price) #�rednie odchylenie bezwgl�dne
library(moments)
skew_price      = skewness(t_cars$price_PLN)
kurt_price      = kurtosis(t_cars$price_PLN)
quant           = quantile(t_cars$price_PLN, .95)     # 95-ty percentyl

#wypisanie cen i modeli dro�szych ni� .95 percentyl
library(sqldf)
fn$sqldf(
"select Model, price_PLN
from t_cars
where price_PLN > $quant"
)

#---------------------c---------------------
#rysowanie wykres�w
types <- sqldf("select count(Type) as Hits, Type from t_cars group by Type")
barplot(types$Hits,
        names.arg = types$Type)
pie(types$Hits, types$Type)

#ile sportowych?
sqldf("select Hits from types where Type = 'Sporty'")$Hits

#---------------------d---------------------
am_cars <- sqldf("select * from t_cars where Origin = 'USA'")
non_am_cars <- sqldf("select * from t_cars where Origin <> 'USA'")
par(mfrow=c(1,2))
boxplot(am_cars$km_per_l_1, main='American', ylab='Fuel combustion [l/100km]')
boxplot(non_am_cars$km_per_l_1, main='Nor-American', ylab='Fuel combustion [l/100km]')
#interpretacja:
    #ameryka�sie og�em pal� wi�cej
    # a� 1/4 ameryka�skich pali powy�ej 14l
    # a� 1/4 nieameryka�skich pali poni�ej 10l

#---------------------e---------------------
#stosuj� wsp�czynniki korelacji liniowej w obu przypadkach
corr_1 = cor(t_cars$km_per_l_1, t_cars$price_PLN)
corr_2 = cor(t_cars$km_per_l_2, t_cars$km_per_l_1)
par(mfrow=c(1,2))
plot(t_cars$km_per_l_1, t_cars$price_PLN, main = "Cena samochodu a zu�ycie paliwa",
     xlab = "Zu�ycie paliwa w l/100km", ylab = "Cena w PLN", 
     sub = sprintf("R = %.4f", corr_1))
plot(t_cars$km_per_l_2, t_cars$km_per_l_1, main = "Zu�ycie na autostradzie a spalanie w mie�cie",
     xlab = "Spalanie na autostradzie", ylab = "Spalanie w mie�cie", 
     sub = sprintf("R = %.4f", corr_2))

#---------------------f---------------------
#histogram masy samochodu
par(mfrow=c(1,1))
hist(t_cars$Weight_kg, main="Rozk�ad masy samochod�w", xlab="Masa samochodu w kg")

#---------------------zadanie 2---------------------
#---------------------a---------------------
path = "E:/9sem/DM/dane/dane/airpollution.txt"
airpollution <- read.delim(path)
#statystyka opisowa
print("Statystyki dot. �miertelno�ci")
univariate_statistics(airpollution$Mortality)
print("Statystyki dot. lat kszta�cenia")
univariate_statistics(airpollution$Education)
print("Statystyki dot. ludno�ci nie-bia�ej")
univariate_statistics(airpollution$X.NonWhite)
print("Statystyki dot. zarobk�w")
univariate_statistics(airpollution$income)
print("Statystyki dot. temperatury w styczniu")
univariate_statistics(airpollution$JanTemp)
print("Statystyki dot. temperatury w lipcu")
univariate_statistics(airpollution$JulyTemp)
print("Statystyki dot. st�enia tlenk�w azotu")
univariate_statistics(airpollution$NOx)

#regresja liniowa
linear_model = lm(airpollution$Mortality ~ airpollution$NOx)

#---------------------b---------------------
print(summary.lm(linear_model)) #slope = -0.1043
#PR(>|t|) jest zbli�one do |t|. Aby jednoznacznie oceni� jako�� regresji wspomog� si� grafik�
par(mfrow=c(1,1))
plot(airpollution$NOx, airpollution$Mortality)
abline(linear_model)
#wniosek: Dwie obserwacje dla miast o olbrzymim st�eniu NOX
        #sprawiaj�, �e niemo�liwe jest dopasowanie prostej do tych dancyh
        #miasta o olbrzymim st�eniu nale�a�oby analizowa� odr�bnie od reszty
        #analiza tak jak jest teraz prowadzi do mylnego wniosku �e 
        #wysokie st�enie NOx obni�a �miertelno��


#nowy model z przetworzonymi danych
#---------------------c---------------------
linear_log_model <- lm(airpollution$Mortality ~ log(airpollution$NOx))
print(summary.lm(linear_log_model)) #slope = 15.1
R2 = summary.lm(linear_log_model)$r.squared
#test Pr(>|t|) wskazuje �e model dobrze opisuje dane, a grafika to potwierdza
par(mfrow=c(1,1))
plot(log(airpollution$NOx), airpollution$Mortality, 
     xlab = "St�enie NOX�w", ylab = "Smiertelno�� na 1e6 mieszka�c�w",
     main = "�miertelno�� w miastach a st�enie tlenk�w azotu", 
     sub = sprintf("R2 = %.4f", R2))
abline(linear_log_model)
#po poprawkach R2 jest du�o wi�ksze ni� przed

#---------------------d---------------------
#znalezienie i usuni�cie danych o du�ym residuum
abs_res <- abs(studres(linear_log_model))
indices <- abs_res >= 0.8
airpollution_proc <- airpollution
airpollution_proc$is_res_big <- indices
#nie wiem dlaczego delete from nie dzia�a 
#https://stackoverflow.com/questions/58670664/r-sqldf-delete-rows-using-filter-on-logic-column
airpollution_proc <- sqldf(
    "select * from airpollution_proc where is_res_big = 0"
)
linear_log_model2 <- lm(Mortality ~ log(NOx), data=airpollution_proc)
R2 = summary.lm(linear_log_model2)$r.squared
par(mfrow=c(1,1))
plot(log(airpollution_proc$NOx), airpollution_proc$Mortality, 
     xlab = "St�enie NOX�w", ylab = "Smiertelno�� na 1e6 mieszka�c�w",
     main = "�miertelno�� w miastach a st�enie tlenk�w azotu", 
     sub = sprintf("R2 = %.4f", R2))
abline(linear_log_model2)
#uzyskali�my jeszcze lepszy wynik regresji
#R2 = 0.57
#---------------------zadanie 3---------------------
#---------------------a---------------------
path = "E:/9sem/DM/dane/dane/savings.txt"
savings_table <- read.csv(path, sep="")
#statystyka opisowa
print("Statystyki dot. oszcz�dno�ci w odniesieniu do dochodu netto")
univariate_statistics(savings_table$Savings)
print("Statystyki dot. odsetka populacji do 15 roku �ycia")
univariate_statistics(savings_table$Pop15)
print("Statystyki dot. odsetka populacji powy�ej 75 roku �ycia")
univariate_statistics(savings_table$Pop75)
print("Statystyki dot. dochodu netto na osobnika")
univariate_statistics(savings_table$dpi)
print("Statystyki dot. tempa wzrostu dochod�w")
univariate_statistics(savings_table$ddpi)

#model liniowy
savings_lm <- lm(savings_table$Savings ~ savings_table$Pop15 + 
                     savings_table$Pop75 + savings_table$ddpi + savings_table$dpi)
#---------------------b---------------------
#warto�ci reszt, chyba zwyk�ych + area plot
indices <- 1:50
res <- savings_lm$residuals
res_frame <- data.frame(indices, res)
ggplot(res_frame, aes(indices, res)) + geom_area()

savings_ind <- savings_table
savings_ind$indices <- 1:50

low_res <- abs(savings_lm$residuals) < 0.5
high_res <- abs(savings_lm$residuals) > 6

saving_res <- savings_table
saving_res$low_res <- low_res
saving_res$high_res <-high_res

saving_res_low = sqldf("select * from saving_res where low_res = 1")
saving_res_low
#najmniejsze reszty: Kanada, Niemcy, RPA, Holandia
saving_res_high = sqldf("select * from saving_res where high_res = 1")
saving_res_high
#najwi�ksze reszty: Chile, Islandia, Zambia, Korea, Paragwaj, Filipiny, Peru
#---------------------c---------------------
#leveragesy wed�ug gotowej funkcji - wykresy
leveragePlots(savings_lm)
#nie znalaz�em wbudowanego pola/metody do zbierania indeks�w d�wigniarzy, wi�c spisz� je r�cznie
#obserwacje wysokiej d�wigni:
#Kanada, Chile, Irlandia, Japonia, Rodezja, USA, Zambia, Libia

#wykres reszt studentyzowanych
ols_plot_resid_stud(savings_lm)

#identyfikacja kraj�w o najmniejszej i najwi�kszej reszcie studentyzowanej
savings_proc <- savings_table
stud_res <- abs(studres(savings_lm))
indices_low <- stud_res < 0.1
indices_high <- stud_res > 2
savings_proc$low_res <- indices_low
savings_proc$high_res <- indices_high

savings_low_res_stud <- sqldf("select * from savings_proc where low_res = 1")
savings_low_res_stud
#najmniejsze reszty studentyzowane: Kanada, Niemcy
savings_high_res_stud <- sqldf("select * from savings_proc where high_res = 1")
savings_high_res_stud
#najwi�ksze reszty studentyzowane: Chile, Zambia

#---------------------d---------------------
#wyznaczenie miary DFFITs
ols_plot_dffits(savings_lm) #interpretacja: obecno�� pr�bek 49 - Libya, 46 - Zambia, 23 Japonia
#najbardziej zmieni�y prost� najlepszego dopasowania
#Zambia i Libya pojawia�y si� przy analizie reszt jako wyr�niaj�ce si� du�ymi resztami
#Ca�a ta tr�jk� zosta�y sklasyfikowane jako obserwacje o du�ej d�wigni

#wyznaczenie miary DFBETAs
ols_plot_dfbetas(savings_lm)
#dane z kt�rych kraj�w najbardziej wp�ywaj� na wsp�czynniki przy poszczeg�lnych wej�ciach:
#wyraz wolny: Libia, Japonia, Irlandia
#Pop75: Libia, Zambia, Japonia, Irlandia
#Pop15: Kostaryka, Irlandia, Japonia, Libya
#ddpi: Japonia, Peru, Jamajka, Libya

#wyznaczenie odleg�o�ci Cooka
ols_plot_cooksd_bar(savings_lm)
#znowu Libia, Zambia i Japonia usuni�cie tych pr�bek przed dopasowaniem najbardziej zmienia reszt� dla nich

#okre�lenie obserwacji wp�ywowych: Japonia, Libia, Zambia, Irlandia, Kostaryka, Peru, Jamajka

#---------------------e---------------------
#regresja dla danych bez libii
saving_Libia_out <- sqldf("select * from savings_table where Country not like 'Libya'")
saving_lm_libia <- lm(saving_Libia_out$Savings ~ saving_Libia_out$Pop15 +
                          saving_Libia_out$Pop75 + saving_Libia_out$dpi + saving_Libia_out$dpi)

#por�wnanie modeli - por�wnanie reszt
indices <- 1:50
res <- savings_lm$residuals
res_frame <- data.frame(indices, res)

indices2 <- 1:49
res2 <- saving_lm_libia$residuals
res_frame2 <- data.frame(indices2, res2)

p1 <- ggplot(res_frame, aes(indices, res)) + geom_area() + ggtitle("Reszty pocz�tkowego modelu")
p2 <- ggplot(res_frame2, aes(indices2, res2)) + geom_area() + ggtitle("Reszty zmodyfikowanego modelu")

grid.arrange(p1, p2, nrow=1)
print(summary(saving_lm_libia))
print(summary(savings_lm))
#usuni�cie obserwacji o najwi�kszej warto�ci odleg�o�ci Cooka zwi�kszy�o nieznacznie reszty dopasowania
# z 3.8 standardowy b��d resztowy na 3.96

#ocena istotno�ci zmiennej dpi i w oryginalnym modelu
print(summary(savings_lm))
#waga wsp�czynnika dla tej wielko�ci wynosi 3e-4 i jest o 3 rz�dy mniejsza
#ni� drugi najmniejszy wsp�czynnik, wi�c zmienna ta jest w og�lnie istotna

#---------------------f---------------------
#takie wykresy to nic innego jak wska�niki DFBETAs (Difference of Betas)
ols_plot_dfbetas(savings_lm)
#Najwi�kszy wp�yw ma Libia - indeks 49

#---------------------zadanie 4---------------------
#wczytanie datasetu
realest_base <- read.csv("E:/9sem/DM/dane/dane/realest.txt", sep="")

#---------------------a---------------------
est_lm_base <- lm(Price ~ Bedroom + Space +Room + Lot + Tax + 
                  Bathroom + Garage + Condition, data = realest_base)

print(summary(est_lm_base))
#wsp�czynnik dla wej�cie Bedroom wynosi -7.75
#wi�c cena zmaleje o 7.75 (nie wiem jakiego przedrostka)
#potencjalne wyt�umaczenie: przy okre�lonym metra�u i liczbie pokoi
#te� nie chcia�bym zamieni� jednego z pokoi na dodatkow� sypialni�

est_lm_bedroom <- lm(realest_base$Price ~ realest_base$Bedroom)
print(summary(est_lm_bedroom))
#gdy uzwgl�dniamy tylko licz� wypialni to liczba wypialni og�lnie podnosi cen� o prawie 4czegosie
#ale warto zwr�ci� uwag� �e wsp�czynnik determinacji R tego modelu
#osi�ga porywaj�c� warto�� 0.17, wi�c nie jest to chyba najlepszy model

#---------------------b---------------------
#predykcja dla podanego zestawy danych:
test_data <- data.frame(Bedroom = 3, Space = 1500, Room = 8,
                            Lot = 40, Tax = 1000, Bathroom = 5,
                            Garage = 1, Condition = 1)

prediction <- predict.lm(est_lm_base, test_data)
#spodziewamy si� sprzeda� za 91.2

#---------------------zadanie 5---------------------
#---------------------a---------------------
gala_data <- read.csv("E:/9sem/DM/dane/dane/gala_data.txt", sep="")
gala_lm <- lm(Species ~ Area + Elevation + Nearest + 
                  Scruz + Adjacent, data = gala_data)

#diagnostyka modelu
print(summary(gala_lm))
#Wska�nik nearest ma bardzo ma�� warto�� wsp�czynnika beta
#co wi�cej test Pr(>|t|) daje wynik o dwa rz�dy wielko�ci wi�kszy od t
#b�ad standardowy dla tego wsp�czynnika jest duzy
#wi�c ta zmienna tylko przeszkadza w modelu

mean_res = mean(gala_lm$residuals)
#�rednia b��d�w jest bliska zeru podstawowe za�o�enie regresji spe�nione

par(mfrow=c(1,1))
hist(gala_lm$residuals)
ols_plot_resid_qq(gala_lm)
#histogram wykazuje sko�no�c prawostronn� i jest du�e odst�pstwo
#zwi�zane z wysp� Santa Cruz, za�o�enie o normalno�ci moz� by� niespe�nione
#cho� moim zdaniem to po rpostu tak wygl�da przez obserwacj� odstaj�c�

ols_plot_resid_stand(gala_lm)
#obserwacje odstaj�ce o indeksach 16, 19, 25 ale nie wiadomo czy s� one wp�ywowe

gala_data$ind <- 1:30
leveragePlots(gala_lm) #mo�e nie dzia�a� w ma�ym oknie plots w RStudio
#mamy du�o obserwacji wysokiej d�wignii 8, 12, 15, 19, 23, 24, 25, 30
#widzimy �e 19 i 25 jest dziwniarzem i odstajacy wi�c prawie na pewno b�dzie wp�ywowy

ols_plot_dfbetas(gala_lm)
#du�y wp�yw na wsp�czynniki kierunkowe maj� - wielko�ci wp�ywowe wg DFBETAs
#Scruz 16, 23
#Adjacent 12, 16, 19, 25, 27
#Elevation 16, 19, 25, 27
#Area 16
#NEarest 15, 16, 25, 26 - parametr kt�ry moim zdaniem nie powinien by� w modelu

#W podanych danych s� obserwacje, kt�re wp�ywaj� na wiele wsp�czynnik�w kierunowych
#s� to obserwacje wp�ywowe - 16(odstaj�cy), 19 i 25 (przed chwil� wspomniane), 27

#weryfikacja czy wariancja b��d�w zale�y od podanych warto�ci wej�ciowych
ols_plot_resid_fit(gala_lm)
#charkater heteroskedastyczny - dla ma�ych warto�ci mniejszy b��d
#mamy niesta�� wariancj� b��d�w za�o�enie o sta�ej wariancji jest niespe�nione

#---------------------b---------------------
#spierwiastkowanie zmiennej obja�nianej i stworzenie nowego modelu
gala_data2 <- gala_data
gala_data2$Species <- sqrt(gala_data$Species)
gala_lm2 <- lm(Species ~ Area + Elevation + Nearest + 
                  Scruz + Adjacent, data = gala_data2)

#diagnostyka nowego modelu
print(summary(gala_lm2))
#znowu wej�cie Nearest ma P(t) > |t| i najwi�ksz� warto�� P(t)
#moze zwiekszy� jako�c modelu

mean_res2 = mean(gala_lm2$residuals)
#�rednia reszt bardzo bliska zeru

ols_plot_resid_fit(gala_lm2)
#tym razem spe�nione za�o�enie o sta�ej wariancji reszt
#�rednia warto�� b��du raczej te� nie zal�y od y_i 
#za�o�enie od niezale�no�ci spe�nione

ols_plot_resid_qq(gala_lm2)
hist(gala_lm2$residuals)
#niewielka sko�no��, kt�ra moim zdaniem wynika z ma�ej liczby danch
#w na wykresie norm-qq dane uk�adaj� si� wzd�� prostej
#za�o�enie o normalno�ci reszt spe�nione

ols_plot_dfbetas(gala_lm2)
#mamy kilka obserwacji, kt�re maj� du�y wp�yw na wsp�czynniki kierunkowe
#Scruz: 11, 13,15, 16, 23
#Adjacent: 12, 16, 19, 25, 27
#Elevation: 15, 16, 19, 25, 27
#Area: 16,
#Nearest: 11, 13, 15, 23
#mamy obserwacje wp�ywowe

#usuni�cie zmiennej Nearest
gala_lm3 <- lm(Species ~ Area + Elevation +  Scruz + 
                   Adjacent, data = gala_data2)

print(summary(gala_lm)) #R2 = 0.76
print(summary(gala_lm2)) #R2 = 0.78
print(summary(gala_lm3)) #R2 = 0.78 bety te� ma�o si� zmieni�y
#powierdzili�y �e zmienan Nearest by�a bezu�yteczna

#---------------------zadanie 6---------------------
#---------------------a---------------------
iris <- read.csv("E:/9sem/DM/dane/dane/iris.txt", row.names=NULL)

#podzia� danych na treningowe i testowe - losowy 75% - 25%
iris_count = nrow(iris)
sample_size <- floor(0.70 * nrow(iris))
set.seed(1)
train_indices <- sample(seq_len(iris_count), size = sample_size)
train_set <- iris[train_indices, ]
test_set <- iris[-train_indices, ]

tree <- rpart(class ~ petal.length + petal.width + sepal.length + sepal.width,
        data=train_set, method="class")

prediction <- predict(tree, test_set)
prediction <- data.frame(prediction)
prediction$class <- iris[-train_indices, "class"]
#opis tekstowy drzewa klasyfikacyjnego
print(tree)
#opis graficzny drzewa - nie wiem dlaczego ale trzeba mocno
#powi�kszy� okno wykresu �eby si� poprawnie narysowa�
par(mfrow=c(1,1))
plot(tree)
text(tree)
#wyja�nienie regu�
#je�li mamy d�ugo�c p�atka mniejsz� ni� 2.6 to jest iris-setosa
#je�li jest r�wne b�d� wi�ksze ni� 2.6 to idziemy dalej
#je�li d�ugo�c p�atka jest mniejsza od 4.95 to iris-versicolor
#w przeciwynym razie to iris-virginica

#macierz b��du dla iris-setosa
iris_setosa <- sqldf(
    "select case 
                when class like 'iris_setosa' then 1
                when class like 'iris_versicolor' then 0
                when class like'iris_virginica' then 0
            end res
            from test_set"
)
pred_setosa <- prediction$Iris.setosa
confusion.matrix(iris_setosa$res, pred_setosa, .5)
#bezb��dnie

#macierz b��du dla versicolor
iris_versicolor <- sqldf(
    "select case 
                when class like 'iris_setosa' then 0
                when class like 'iris_versicolor' then 1
                when class like'iris_virginica' then 0
            end res
            from test_set"
)
pred_versicolor <- prediction$Iris.versicolor
confusion.matrix(iris_versicolor$res, pred_versicolor, .5)
#cztery b��dy - czterokrotnie false positive

#macierz b��du dla virginica
iris_virginica <- sqldf(
    "select case 
                when class like 'iris_setosa' then 0
                when class like 'iris_versicolor' then 0
                when class like'iris_virginica' then 1
            end res
            from test_set"
)
pred_virginica <- prediction$Iris.virginica
confusion.matrix(iris_virginica$res, pred_virginica, .5)
#cztery b��dy - czterokrotnie false negative

#drzewo cztery razy zakwalifikowa�o virginic� jako versicolora
print(sprintf("Mamy %.2f procent poprawnych wskaza�", 41/45*100))

#---------------------zadanie 7---------------------
#---------------------a---------------------
iris <- read.csv("E:/9sem/DM/dane/dane/iris.txt", row.names=NULL)
#processing <- preProcess(iris, methos = 'range')
#iris.norm <- predict(processing, iris)
#nie wiem czemu ale zwraca bzdury

#sensowniej mog�by by by� najpierw podzieli� dane a potem normalizowa�
#zgodnie z parametrami obrnaych dla zbioru treningowego, ale robi� zgodnie z instrukcj�
#normalizacja na przedzia� [0,1]
iris.norm <- normalize(iris, method = "range", range = c(0, 1), margin = 2)
classes <- iris.norm$class
iris.norm <- subset(iris.norm, select = -5)

#losowy podzia� danych w proporcjach 70 : 30
#podzia� jest ten sam jak w poprzednim zadaniu
iris_count = nrow(iris)
sample_size <- floor(0.70 * nrow(iris))
set.seed(1)
train_indices <- sample(seq_len(iris_count), size = sample_size)
train_set <- iris.norm[train_indices, ]
test_set <- iris.norm[-train_indices, ]

classes <- classes[train_indices]

clust <- knn(train_set, test_set, classes, 3)
clust_df <- data.frame(clust)
#wyniki klasyfikacji dla 3 s�siad�w. Klasyfikacja zale�y od k, ale poka�e pro forma
print(clust)

clust2 <- knn(train_set, test_set, classes, 2)
clust2_df <- data.frame(clust2)

#---------------------b---------------------
#tu powielam, bo jest ma�e zamieszanie z etykietami
#klasyfikator p�acze �e ich nie chce a s� mi potrzebne
#do wygenerowania macierzy b��du

test_set_2 <- iris.norm[-train_indices, ]
test_set_2$class <- iris$class[-train_indices]
#macierz b��du dla iris-setosa

iris_setosa <- sqldf(
    "select case 
                when class like 'iris_setosa' then 1
                when class like 'iris_versicolor' then 0
                when class like'iris_virginica' then 0
            end res
            from test_set_2"
)
setosa <- sqldf(
"select case
    when clust like 'iris_setosa' then 1
    when clust like 'iris_versicolor' then 0
    when clust like 'iris_virginica' then 0
end res
from clust_df"   
)
confusion.matrix(iris_setosa$res, setosa$res, .5)
#akurat ta klasyfikacja przebieg�a bezb��dnie wi�c nie ma sensu
#pokazywa� jej te� dla klasyfikatora z inn� liczb� s�siad�w

#macierz b��du dla versicolor
iris_versicolor <- sqldf(
    "select case 
                when class like 'iris_setosa' then 0
                when class like 'iris_versicolor' then 1
                when class like'iris_virginica' then 0
            end res
            from test_set_2"
)
versicolor <- sqldf(
    "select case
    when clust like 'iris_setosa' then 0
    when clust like 'iris_versicolor' then 1
    when clust like 'iris_virginica' then 0
end res
from clust_df"   
)
confusion.matrix(iris_versicolor$res, versicolor$res, .5)
#jeden b��d - false positive dla 3-s�siad�w
versicolor2 <- sqldf(
    "select case
    when clust2 like 'iris_setosa' then 0
    when clust2 like 'iris_versicolor' then 1
    when clust2 like 'iris_virginica' then 0
end res
from clust2_df"   
)
confusion.matrix(iris_versicolor$res, versicolor2$res, .5)
#tym razem dwa b��dy - po jednym false positive i negative

#macierz b��du dla virginica
iris_virginica <- sqldf(
    "select case 
                when class like 'iris_setosa' then 0
                when class like 'iris_versicolor' then 0
                when class like'iris_virginica' then 1
            end res
            from test_set_2"
)
virginica <- sqldf(
    "select case
    when clust like 'iris_setosa' then 0
    when clust like 'iris_versicolor' then 0
    when clust like 'iris_virginica' then 1
end res
from clust_df"   
)
confusion.matrix(iris_virginica$res, virginica$res, .5)
#jeden b��d. Na podstawie poprzedniej macierzy mo�na stwierdzi�
#�e klasyfikator dla k=3 #jednego virginica zakwalifikowa� jako versicolora

#dla k = 2
virginica2 <- sqldf(
    "select case
    when clust2 like 'iris_setosa' then 0
    when clust2 like 'iris_versicolor' then 0
    when clust2 like 'iris_virginica' then 1
end res
from clust2_df"   
)
confusion.matrix(iris_virginica$res, virginica2$res, .5)
#i analogicznie tutaj s� dwa �le zakwalifikowane 
#raz virginica jako versicolor i raz na odwr�t

print(sprintf("Skuteczno�� klasyfikatora o k=3: %.3f", 44/45))
print(sprintf("Skuteczno�� klasyfikatora o k=2: %.3f", 43/45))