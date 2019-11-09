#import packietów
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
    print(sprintf("Œrednia: %.4f", mean(data_variable)))
    print(sprintf("Wariancja: %.4f", var(data_variable)))
    print(sprintf("Mediana: %.4f", median(data_variable)))
    medi_temp = median(data_variable)
    print(sprintf("Œrednie odchylenie bezwglêdne: %.4f", mad(data_variable, medi_temp))) 
    library(moments)
    print(sprintf("Skoœnoœæ rozk³adu: %.4f", skewness(data_variable)))
    print(sprintf("Kurtoza rozk³adu: %.4f", kurtosis(data_variable)))
    par(mfrow=c(1,3))
    hist(data_variable)
    boxplot(data_variable)
    plot(data_variable)
}

#---------------------zadanie 1-----------------------------
#---------------------a-----------------------------

#wczytanie danych
t_cars = MASS::Cars93

#tworzenie przeliczników:
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
#statystyki próbkowe
average_price   = mean(t_cars$price_PLN)
variance        = var(t_cars$price_PLN)
median_price    = median(t_cars$price_PLN)
mad_factor      = mad(t_cars$price_PLN, median_price) #œrednie odchylenie bezwglêdne
library(moments)
skew_price      = skewness(t_cars$price_PLN)
kurt_price      = kurtosis(t_cars$price_PLN)
quant           = quantile(t_cars$price_PLN, .95)     # 95-ty percentyl

#wypisanie cen i modeli dro¿szych ni¿ .95 percentyl
library(sqldf)
fn$sqldf(
"select Model, price_PLN
from t_cars
where price_PLN > $quant"
)

#---------------------c---------------------
#rysowanie wykresów
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
    #amerykañsie ogó³em pal¹ wiêcej
    # a¿ 1/4 amerykañskich pali powy¿ej 14l
    # a¿ 1/4 nieamerykañskich pali poni¿ej 10l

#---------------------e---------------------
#stosujê wspó³czynniki korelacji liniowej w obu przypadkach
corr_1 = cor(t_cars$km_per_l_1, t_cars$price_PLN)
corr_2 = cor(t_cars$km_per_l_2, t_cars$km_per_l_1)
par(mfrow=c(1,2))
plot(t_cars$km_per_l_1, t_cars$price_PLN, main = "Cena samochodu a zu¿ycie paliwa",
     xlab = "Zu¿ycie paliwa w l/100km", ylab = "Cena w PLN", 
     sub = sprintf("R = %.4f", corr_1))
plot(t_cars$km_per_l_2, t_cars$km_per_l_1, main = "Zu¿ycie na autostradzie a spalanie w mieœcie",
     xlab = "Spalanie na autostradzie", ylab = "Spalanie w mieœcie", 
     sub = sprintf("R = %.4f", corr_2))

#---------------------f---------------------
#histogram masy samochodu
par(mfrow=c(1,1))
hist(t_cars$Weight_kg, main="Rozk³ad masy samochodów", xlab="Masa samochodu w kg")

#---------------------zadanie 2---------------------
#---------------------a---------------------
path = "E:/9sem/DM/dane/dane/airpollution.txt"
airpollution <- read.delim(path)
#statystyka opisowa
print("Statystyki dot. œmiertelnoœci")
univariate_statistics(airpollution$Mortality)
print("Statystyki dot. lat kszta³cenia")
univariate_statistics(airpollution$Education)
print("Statystyki dot. ludnoœci nie-bia³ej")
univariate_statistics(airpollution$X.NonWhite)
print("Statystyki dot. zarobków")
univariate_statistics(airpollution$income)
print("Statystyki dot. temperatury w styczniu")
univariate_statistics(airpollution$JanTemp)
print("Statystyki dot. temperatury w lipcu")
univariate_statistics(airpollution$JulyTemp)
print("Statystyki dot. stê¿enia tlenków azotu")
univariate_statistics(airpollution$NOx)

#regresja liniowa
linear_model = lm(airpollution$Mortality ~ airpollution$NOx)

#---------------------b---------------------
print(summary.lm(linear_model)) #slope = -0.1043
#PR(>|t|) jest zbli¿one do |t|. Aby jednoznacznie oceniæ jakoœæ regresji wspomogê siê grafik¹
par(mfrow=c(1,1))
plot(airpollution$NOx, airpollution$Mortality)
abline(linear_model)
#wniosek: Dwie obserwacje dla miast o olbrzymim stê¿eniu NOX
        #sprawiaj¹, ¿e niemo¿liwe jest dopasowanie prostej do tych dancyh
        #miasta o olbrzymim stê¿eniu nale¿a³oby analizowaæ odrêbnie od reszty
        #analiza tak jak jest teraz prowadzi do mylnego wniosku ¿e 
        #wysokie stê¿enie NOx obni¿a œmiertelnoœæ


#nowy model z przetworzonymi danych
#---------------------c---------------------
linear_log_model <- lm(airpollution$Mortality ~ log(airpollution$NOx))
print(summary.lm(linear_log_model)) #slope = 15.1
R2 = summary.lm(linear_log_model)$r.squared
#test Pr(>|t|) wskazuje ¿e model dobrze opisuje dane, a grafika to potwierdza
par(mfrow=c(1,1))
plot(log(airpollution$NOx), airpollution$Mortality, 
     xlab = "Stê¿enie NOXów", ylab = "Smiertelnoœæ na 1e6 mieszkañców",
     main = "Œmiertelnoœæ w miastach a stê¿enie tlenków azotu", 
     sub = sprintf("R2 = %.4f", R2))
abline(linear_log_model)
#po poprawkach R2 jest du¿o wiêksze ni¿ przed

#---------------------d---------------------
#znalezienie i usuniêcie danych o du¿ym residuum
abs_res <- abs(studres(linear_log_model))
indices <- abs_res >= 0.8
airpollution_proc <- airpollution
airpollution_proc$is_res_big <- indices
#nie wiem dlaczego delete from nie dzia³a 
#https://stackoverflow.com/questions/58670664/r-sqldf-delete-rows-using-filter-on-logic-column
airpollution_proc <- sqldf(
    "select * from airpollution_proc where is_res_big = 0"
)
linear_log_model2 <- lm(Mortality ~ log(NOx), data=airpollution_proc)
R2 = summary.lm(linear_log_model2)$r.squared
par(mfrow=c(1,1))
plot(log(airpollution_proc$NOx), airpollution_proc$Mortality, 
     xlab = "Stê¿enie NOXów", ylab = "Smiertelnoœæ na 1e6 mieszkañców",
     main = "Œmiertelnoœæ w miastach a stê¿enie tlenków azotu", 
     sub = sprintf("R2 = %.4f", R2))
abline(linear_log_model2)
#uzyskaliœmy jeszcze lepszy wynik regresji
#R2 = 0.57
#---------------------zadanie 3---------------------
#---------------------a---------------------
path = "E:/9sem/DM/dane/dane/savings.txt"
savings_table <- read.csv(path, sep="")
#statystyka opisowa
print("Statystyki dot. oszczêdnoœci w odniesieniu do dochodu netto")
univariate_statistics(savings_table$Savings)
print("Statystyki dot. odsetka populacji do 15 roku ¿ycia")
univariate_statistics(savings_table$Pop15)
print("Statystyki dot. odsetka populacji powy¿ej 75 roku ¿ycia")
univariate_statistics(savings_table$Pop75)
print("Statystyki dot. dochodu netto na osobnika")
univariate_statistics(savings_table$dpi)
print("Statystyki dot. tempa wzrostu dochodów")
univariate_statistics(savings_table$ddpi)

#model liniowy
savings_lm <- lm(savings_table$Savings ~ savings_table$Pop15 + 
                     savings_table$Pop75 + savings_table$ddpi + savings_table$dpi)
#---------------------b---------------------
#wartoœci reszt, chyba zwyk³ych + area plot
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
#najwiêksze reszty: Chile, Islandia, Zambia, Korea, Paragwaj, Filipiny, Peru
#---------------------c---------------------
#leveragesy wed³ug gotowej funkcji - wykresy
leveragePlots(savings_lm)
#nie znalaz³em wbudowanego pola/metody do zbierania indeksów dŸwigniarzy, wiêc spiszê je rêcznie
#obserwacje wysokiej dŸwigni:
#Kanada, Chile, Irlandia, Japonia, Rodezja, USA, Zambia, Libia

#wykres reszt studentyzowanych
ols_plot_resid_stud(savings_lm)

#identyfikacja krajów o najmniejszej i najwiêkszej reszcie studentyzowanej
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
#najwiêksze reszty studentyzowane: Chile, Zambia

#---------------------d---------------------
#wyznaczenie miary DFFITs
ols_plot_dffits(savings_lm) #interpretacja: obecnoœæ próbek 49 - Libya, 46 - Zambia, 23 Japonia
#najbardziej zmieni³y prost¹ najlepszego dopasowania
#Zambia i Libya pojawia³y siê przy analizie reszt jako wyró¿niaj¹ce siê du¿ymi resztami
#Ca³a ta trójk¹ zosta³y sklasyfikowane jako obserwacje o du¿ej dŸwigni

#wyznaczenie miary DFBETAs
ols_plot_dfbetas(savings_lm)
#dane z których krajów najbardziej wp³ywaj¹ na wspó³czynniki przy poszczególnych wejœciach:
#wyraz wolny: Libia, Japonia, Irlandia
#Pop75: Libia, Zambia, Japonia, Irlandia
#Pop15: Kostaryka, Irlandia, Japonia, Libya
#ddpi: Japonia, Peru, Jamajka, Libya

#wyznaczenie odleg³oœci Cooka
ols_plot_cooksd_bar(savings_lm)
#znowu Libia, Zambia i Japonia usuniêcie tych próbek przed dopasowaniem najbardziej zmienia resztê dla nich

#okreœlenie obserwacji wp³ywowych: Japonia, Libia, Zambia, Irlandia, Kostaryka, Peru, Jamajka

#---------------------e---------------------
#regresja dla danych bez libii
saving_Libia_out <- sqldf("select * from savings_table where Country not like 'Libya'")
saving_lm_libia <- lm(saving_Libia_out$Savings ~ saving_Libia_out$Pop15 +
                          saving_Libia_out$Pop75 + saving_Libia_out$dpi + saving_Libia_out$dpi)

#porównanie modeli - porównanie reszt
indices <- 1:50
res <- savings_lm$residuals
res_frame <- data.frame(indices, res)

indices2 <- 1:49
res2 <- saving_lm_libia$residuals
res_frame2 <- data.frame(indices2, res2)

p1 <- ggplot(res_frame, aes(indices, res)) + geom_area() + ggtitle("Reszty pocz¹tkowego modelu")
p2 <- ggplot(res_frame2, aes(indices2, res2)) + geom_area() + ggtitle("Reszty zmodyfikowanego modelu")

grid.arrange(p1, p2, nrow=1)
print(summary(saving_lm_libia))
print(summary(savings_lm))
#usuniêcie obserwacji o najwiêkszej wartoœci odleg³oœci Cooka zwiêkszy³o nieznacznie reszty dopasowania
# z 3.8 standardowy b³¹d resztowy na 3.96

#ocena istotnoœci zmiennej dpi i w oryginalnym modelu
print(summary(savings_lm))
#waga wspó³czynnika dla tej wielkoœci wynosi 3e-4 i jest o 3 rzêdy mniejsza
#ni¿ drugi najmniejszy wspó³czynnik, wiêc zmienna ta jest w ogólnie istotna

#---------------------f---------------------
#takie wykresy to nic innego jak wskaŸniki DFBETAs (Difference of Betas)
ols_plot_dfbetas(savings_lm)
#Najwiêkszy wp³yw ma Libia - indeks 49

#---------------------zadanie 4---------------------
#wczytanie datasetu
realest_base <- read.csv("E:/9sem/DM/dane/dane/realest.txt", sep="")

#---------------------a---------------------
est_lm_base <- lm(Price ~ Bedroom + Space +Room + Lot + Tax + 
                  Bathroom + Garage + Condition, data = realest_base)

print(summary(est_lm_base))
#wspó³czynnik dla wejœcie Bedroom wynosi -7.75
#wiêc cena zmaleje o 7.75 (nie wiem jakiego przedrostka)
#potencjalne wyt³umaczenie: przy okreœlonym metra¿u i liczbie pokoi
#te¿ nie chcia³bym zamieniæ jednego z pokoi na dodatkow¹ sypialniê

est_lm_bedroom <- lm(realest_base$Price ~ realest_base$Bedroom)
print(summary(est_lm_bedroom))
#gdy uzwglêdniamy tylko liczê wypialni to liczba wypialni ogólnie podnosi cenê o prawie 4czegosie
#ale warto zwróciæ uwagê ¿e wspó³czynnik determinacji R tego modelu
#osi¹ga porywaj¹c¹ wartoœæ 0.17, wiêc nie jest to chyba najlepszy model

#---------------------b---------------------
#predykcja dla podanego zestawy danych:
test_data <- data.frame(Bedroom = 3, Space = 1500, Room = 8,
                            Lot = 40, Tax = 1000, Bathroom = 5,
                            Garage = 1, Condition = 1)

prediction <- predict.lm(est_lm_base, test_data)
#spodziewamy siê sprzedaæ za 91.2

#---------------------zadanie 5---------------------
#---------------------a---------------------
gala_data <- read.csv("E:/9sem/DM/dane/dane/gala_data.txt", sep="")
gala_lm <- lm(Species ~ Area + Elevation + Nearest + 
                  Scruz + Adjacent, data = gala_data)

#diagnostyka modelu
print(summary(gala_lm))
#WskaŸnik nearest ma bardzo ma³¹ wartoœæ wspó³czynnika beta
#co wiêcej test Pr(>|t|) daje wynik o dwa rzêdy wielkoœci wiêkszy od t
#b³ad standardowy dla tego wspó³czynnika jest duzy
#wiêc ta zmienna tylko przeszkadza w modelu

mean_res = mean(gala_lm$residuals)
#œrednia b³êdów jest bliska zeru podstawowe za³o¿enie regresji spe³nione

par(mfrow=c(1,1))
hist(gala_lm$residuals)
ols_plot_resid_qq(gala_lm)
#histogram wykazuje skoœnoœc prawostronn¹ i jest du¿e odstêpstwo
#zwi¹zane z wysp¹ Santa Cruz, za³o¿enie o normalnoœci mozê byæ niespe³nione
#choæ moim zdaniem to po rpostu tak wygl¹da przez obserwacjê odstaj¹c¹

ols_plot_resid_stand(gala_lm)
#obserwacje odstaj¹ce o indeksach 16, 19, 25 ale nie wiadomo czy s¹ one wp³ywowe

gala_data$ind <- 1:30
leveragePlots(gala_lm) #mo¿e nie dzia³aæ w ma³ym oknie plots w RStudio
#mamy du¿o obserwacji wysokiej dŸwignii 8, 12, 15, 19, 23, 24, 25, 30
#widzimy ¿e 19 i 25 jest dziwniarzem i odstajacy wiêc prawie na pewno bêdzie wp³ywowy

ols_plot_dfbetas(gala_lm)
#du¿y wp³yw na wspó³czynniki kierunkowe maj¹ - wielkoœci wp³ywowe wg DFBETAs
#Scruz 16, 23
#Adjacent 12, 16, 19, 25, 27
#Elevation 16, 19, 25, 27
#Area 16
#NEarest 15, 16, 25, 26 - parametr który moim zdaniem nie powinien byæ w modelu

#W podanych danych s¹ obserwacje, które wp³ywaj¹ na wiele wspó³czynników kierunowych
#s¹ to obserwacje wp³ywowe - 16(odstaj¹cy), 19 i 25 (przed chwil¹ wspomniane), 27

#weryfikacja czy wariancja b³êdów zale¿y od podanych wartoœci wejœciowych
ols_plot_resid_fit(gala_lm)
#charkater heteroskedastyczny - dla ma³ych wartoœci mniejszy b³¹d
#mamy niesta³¹ wariancjê b³êdów za³o¿enie o sta³ej wariancji jest niespe³nione

#---------------------b---------------------
#spierwiastkowanie zmiennej objaœnianej i stworzenie nowego modelu
gala_data2 <- gala_data
gala_data2$Species <- sqrt(gala_data$Species)
gala_lm2 <- lm(Species ~ Area + Elevation + Nearest + 
                  Scruz + Adjacent, data = gala_data2)

#diagnostyka nowego modelu
print(summary(gala_lm2))
#znowu wejœcie Nearest ma P(t) > |t| i najwiêksz¹ wartoœæ P(t)
#moze zwiekszyæ jakoœc modelu

mean_res2 = mean(gala_lm2$residuals)
#œrednia reszt bardzo bliska zeru

ols_plot_resid_fit(gala_lm2)
#tym razem spe³nione za³o¿enie o sta³ej wariancji reszt
#œrednia wartoœæ b³êdu raczej te¿ nie zalê¿y od y_i 
#za³o¿enie od niezale¿noœci spe³nione

ols_plot_resid_qq(gala_lm2)
hist(gala_lm2$residuals)
#niewielka skoœnoœæ, która moim zdaniem wynika z ma³ej liczby danch
#w na wykresie norm-qq dane uk³adaj¹ siê wzd³¿ prostej
#za³o¿enie o normalnoœci reszt spe³nione

ols_plot_dfbetas(gala_lm2)
#mamy kilka obserwacji, które maj¹ du¿y wp³yw na wspó³czynniki kierunkowe
#Scruz: 11, 13,15, 16, 23
#Adjacent: 12, 16, 19, 25, 27
#Elevation: 15, 16, 19, 25, 27
#Area: 16,
#Nearest: 11, 13, 15, 23
#mamy obserwacje wp³ywowe

#usuniêcie zmiennej Nearest
gala_lm3 <- lm(Species ~ Area + Elevation +  Scruz + 
                   Adjacent, data = gala_data2)

print(summary(gala_lm)) #R2 = 0.76
print(summary(gala_lm2)) #R2 = 0.78
print(summary(gala_lm3)) #R2 = 0.78 bety te¿ ma³o siê zmieni³y
#powierdziliœy ¿e zmienan Nearest by³a bezu¿yteczna

#---------------------zadanie 6---------------------
#---------------------a---------------------
iris <- read.csv("E:/9sem/DM/dane/dane/iris.txt", row.names=NULL)

#podzia³ danych na treningowe i testowe - losowy 75% - 25%
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
#powiêkszyæ okno wykresu ¿eby siê poprawnie narysowa³
par(mfrow=c(1,1))
plot(tree)
text(tree)
#wyjaœnienie regu³
#jeœli mamy d³ugoœc p³atka mniejsz¹ ni¿ 2.6 to jest iris-setosa
#jeœli jest równe b¹dŸ wiêksze ni¿ 2.6 to idziemy dalej
#jeœli d³ugoœc p³atka jest mniejsza od 4.95 to iris-versicolor
#w przeciwynym razie to iris-virginica

#macierz b³êdu dla iris-setosa
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
#bezb³êdnie

#macierz b³êdu dla versicolor
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
#cztery b³êdy - czterokrotnie false positive

#macierz b³êdu dla virginica
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
#cztery b³êdy - czterokrotnie false negative

#drzewo cztery razy zakwalifikowa³o virginicê jako versicolora
print(sprintf("Mamy %.2f procent poprawnych wskazañ", 41/45*100))

#---------------------zadanie 7---------------------
#---------------------a---------------------
iris <- read.csv("E:/9sem/DM/dane/dane/iris.txt", row.names=NULL)
#processing <- preProcess(iris, methos = 'range')
#iris.norm <- predict(processing, iris)
#nie wiem czemu ale zwraca bzdury

#sensowniej mog³by by byæ najpierw podzieliæ dane a potem normalizowaæ
#zgodnie z parametrami obrnaych dla zbioru treningowego, ale robiê zgodnie z instrukcj¹
#normalizacja na przedzia³ [0,1]
iris.norm <- normalize(iris, method = "range", range = c(0, 1), margin = 2)
classes <- iris.norm$class
iris.norm <- subset(iris.norm, select = -5)

#losowy podzia³ danych w proporcjach 70 : 30
#podzia³ jest ten sam jak w poprzednim zadaniu
iris_count = nrow(iris)
sample_size <- floor(0.70 * nrow(iris))
set.seed(1)
train_indices <- sample(seq_len(iris_count), size = sample_size)
train_set <- iris.norm[train_indices, ]
test_set <- iris.norm[-train_indices, ]

classes <- classes[train_indices]

clust <- knn(train_set, test_set, classes, 3)
clust_df <- data.frame(clust)
#wyniki klasyfikacji dla 3 s¹siadów. Klasyfikacja zale¿y od k, ale poka¿e pro forma
print(clust)

clust2 <- knn(train_set, test_set, classes, 2)
clust2_df <- data.frame(clust2)

#---------------------b---------------------
#tu powielam, bo jest ma³e zamieszanie z etykietami
#klasyfikator p³acze ¿e ich nie chce a s¹ mi potrzebne
#do wygenerowania macierzy b³êdu

test_set_2 <- iris.norm[-train_indices, ]
test_set_2$class <- iris$class[-train_indices]
#macierz b³êdu dla iris-setosa

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
#akurat ta klasyfikacja przebieg³a bezb³êdnie wiêc nie ma sensu
#pokazywaæ jej te¿ dla klasyfikatora z inn¹ liczb¹ s¹siadów

#macierz b³êdu dla versicolor
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
#jeden b³¹d - false positive dla 3-s¹siadów
versicolor2 <- sqldf(
    "select case
    when clust2 like 'iris_setosa' then 0
    when clust2 like 'iris_versicolor' then 1
    when clust2 like 'iris_virginica' then 0
end res
from clust2_df"   
)
confusion.matrix(iris_versicolor$res, versicolor2$res, .5)
#tym razem dwa b³êdy - po jednym false positive i negative

#macierz b³êdu dla virginica
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
#jeden b³¹d. Na podstawie poprzedniej macierzy mo¿na stwierdziæ
#¿e klasyfikator dla k=3 #jednego virginica zakwalifikowa³ jako versicolora

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
#i analogicznie tutaj s¹ dwa Ÿle zakwalifikowane 
#raz virginica jako versicolor i raz na odwrót

print(sprintf("Skutecznoœæ klasyfikatora o k=3: %.3f", 44/45))
print(sprintf("Skutecznoœæ klasyfikatora o k=2: %.3f", 43/45))