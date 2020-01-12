#za³adowanie potrzebnych zale¿noœci
setwd("E:/9sem/DM/zep/repo")
library(sqldf)
library(data.table)
library(plyr)
library(SDMTools)
library(caret)
#za³adowanie bazowych danych

#--------------------Zadanie 1--------------------
basedata <- read.csv("E:/9sem/DM/zep/bank/bank.csv", row.names=NULL, sep=";", stringsAsFactors=FALSE)

#--------------------Zadanie 3--------------------
#analiza eksploracyjna
y_quot <- sqldf(
  "select y, count(y) as liczba_wynikow
   from basedata
   group by y"
)
y_quot

age_no = sqldf("
select age, count(y) as no
from basedata
where y = 'no'
group by age
order by age")

age_yes = sqldf("
select age, count(y) as yes
from basedata
where y = 'yes'
group by age
order by age")

age_y = sqldf("
select age, count(y) liczba_prob
from basedata
group by age
order by age")

windows()
par(mfrow = c(1,3))
plot(age_no$age, age_no$no, xlab = "Wiek",
     ylab = "Liczba odmów", main = "Rozk³ad wieku odmawiaj¹cych")
plot(age_yes$age, age_yes$yes, xlab = "Wiek", ylab = "Liczba zgód",
     main="Rozk³ad wieku zgadzajacych siê")
plot(age_y$age, age_y$liczba_prob, xlab = "Wiek", ylab = "Liczba prób",
     main = "Rozk³ad wieku ogólnie")

job_no = sqldf("
select job, count(y) as no
from basedata
where y = 'no'
group by job
order by job")

job_yes = sqldf("
select job, count(y) as yes
from basedata
where y = 'yes'
group by job
order by job")

job_y <- data.frame(row.names = job_yes$job)
job_y$skutecznosc <- job_yes$yes/(job_yes$yes + job_no$no)

windows()
par(mfrow = c(1,1))
barplot(job_y$skutecznosc, names.arg = rownames(job_y), 
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu na zawód")

marital = sqldf(
  "select q1.stan as stan,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select marital as stan,
              count(y) as liczba_prob
       from basedata
       group by marital
       order by marital) q1
  left join
      (select marital as stan,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by marital
       order by marital) q2
  on q1.stan = q2.stan"
)

marital$skutecznosc <- marital$udane / marital$liczba_prob

windows()
par(mfrow = c(1,1))
barplot(marital$skutecznosc, names.arg = marital$stan, 
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu na status matrymonialny")

education <- sqldf(
  "select q1.wyksztalcenie as wyksztalcenie,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select education as wyksztalcenie,
              count(y) as liczba_prob
       from basedata
       group by wyksztalcenie
       order by wyksztalcenie) q1
  left join
      (select education as wyksztalcenie,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by wyksztalcenie
       order by wyksztalcenie) q2
  on q1.wyksztalcenie = q2.wyksztalcenie"
)

education$skutecznosc <- education$udane / education$liczba_prob

windows()
par(mfrow = c(1,1))
barplot(education$skutecznosc, names.arg = education$wyksztalcenie,
        ylab = "Skutecznioœæ marketingu",
        main = "Skutecznioœæ marketingu ze wzglêdu na wykszta³cenie")

default <- sqldf(
  "select q1.bankrut as bankrut,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select [default] as bankrut,
              count(y) as liczba_prob
       from basedata
       group by bankrut
       order by bankrut) q1
  left join
      (select [default] as bankrut,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by bankrut
       order by bankrut) q2
  on q1.bankrut = q2.bankrut"
)

default$skutecznosc <- default$udane / default$liczba_prob

windows()
par(mfrow = c(1,1))
barplot(default$skutecznosc, names.arg = default$bankrut,
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu na kredyt niemo¿liwy do sp³acenia")

balance_yes <- sqldf(
  "select balance
   from basedata
   where y = 'yes'
  "
)
balance_all <- sqldf(
  "select balance
   from basedata
  "
)

balance_yes$balance <- round(balance_yes$balance / 500) * 500
balance_all$balance <- round(balance_all$balance / 500) * 500

balance_yes <- sqldf(
  "select balance, count(balance) as czestosc
   from balance_yes
   group by balance
   order by balance
  "
)
balance_all <- sqldf(
  "select balance, count(balance) as czestosc
   from balance_all
   group by balance
   order by balance
  "
)
balance_all <- sqldf(
  "select t1.balance, 
          t1.czestosc as liczba_prob,
          t2.czestosc as liczba_zgod
   from balance_all as t1
   left join balance_yes as t2 on t1.balance = t2.balance
  "
)
balance_all$liczba_zgod[is.na(balance_all$liczba_zgod) == 1] <- 0
balance_all$skutecznosc <- balance_all$liczba_zgod / balance_all$liczba_prob

windows()
par(mfrow=c(1,2))
barplot(balance_all$skutecznosc, names.arg = balance_all$balance,
        xlab = "Roczny zysk w euro", ylab = "Skutecznoœæ telemarketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu na zysk roczny")
barplot(balance_all$liczba_prob, names.arg = balance_all$balance,
        xlab = "Roczny zysk w euro", ylab =  "Liczba prób telemarketingowych",
        main = "Rozk³ad prób marketingu ze wzglêdu na zysk roczny")

housing <- sqldf(
  "select q1.hipoteka as hipoteka,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select [housing] as hipoteka,
              count(y) as liczba_prob
       from basedata
       group by hipoteka
       order by hipoteka) q1
  left join
      (select [housing] as hipoteka,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by hipoteka
       order by hipoteka) q2
  on q1.hipoteka = q2.hipoteka"
)
housing$skutecznosc <- housing$udane / housing$liczba_prob

windows()
par(mfrow = c(1,1))
barplot(housing$skutecznosc, names.arg = housing$hipoteka,
        main = "Skutecznoœæ marketingu ze wzglêdu na posiadanie kredytu hipotecznego",
        ylab = "Skutecznoœc marketingu")

housing_balance <- sqldf(
  "select case
          when balance < 0 then 'debt'
          when balance between 0 and 1000 then 'low'
          when balance between 1001 and 2500 then 'mid-low'
          when balance between 2501 and 5000 then 'mid'
          when balance between 5001 and 10000 then 'mid-high'
          when balance > 10000 then 'high'
          end balance_level,
          housing,
          y
    from basedata
    order by balance_level
  "
)
housing_balance <- sqldf(
  "select balance_level,
          housing,
          y,
          count(y)
   from housing_balance
   group by balance_level,
            housing,
            y
   order by balance_level,
            housing,
            y"
)

housing_balance_no <- sqldf(
  "select *
   from housing_balance
   where y = 'no'"
)
housing_balance_yes <- sqldf(
  "select *
   from housing_balance
   where y = 'yes'"
)
housing_balance_yes$skutecznosc <- housing_balance_yes$`count(y)`/ 
                                   (housing_balance_yes$`count(y)`+ 
                                   housing_balance_no$`count(y)`)

windows()
par(mfrow = c(1, 1))
barplot(housing_balance_yes$skutecznosc, 
        names.arg = paste(housing_balance_yes$housing, housing_balance_yes$balance_level, sep="-"),
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu posiadanie hipoteki i poziom zysku rocznego")

loan <- sqldf(
  "select q1.pozyczka as pozyczka,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select [loan] as pozyczka,
              count(y) as liczba_prob
       from basedata
       group by pozyczka
       order by pozyczka) q1
  left join
      (select [loan] as pozyczka,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by pozyczka
       order by pozyczka) q2
  on q1.pozyczka = q2.pozyczka"
)
loan$skutecznosc <- loan$udane / loan$liczba_prob

windows()
par(mfrow = c(1,1))
barplot(loan$skutecznosc, names.arg = loan$pozyczka,
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu na posiadanie po¿yczki")

loan_default <- sqldf(
  "select loan, [default], y as wynik, count(y) as liczba_wynikow
   from basedata
   group by loan, [default], wynik
   order by loan, [default], wynik"
)
loan_default$loan <- paste("loan", loan_default$loan, sep = "-")
loan_default$default <- paste("def", loan_default$default, sep = "-")

loan_default_no <- sqldf(
  "select *
   from loan_default
   where wynik = 'no'"
)
loan_default_yes <- sqldf(
  "select *
   from loan_default
   where wynik = 'yes'"
)
loan_default_yes$skutecznosc <- loan_default_yes$liczba_wynikow/ 
  (loan_default_yes$liczba_wynikow + 
     loan_default_no$liczba_wynikow)

windows()
par(mfrow = c(1, 1))
barplot(housing_balance_yes$skutecznosc, 
        names.arg = paste(housing_balance_yes$housing, housing_balance_yes$balance_level, sep="-"),
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu na posiadanie po¿yczki lub utopionego kredytu")

contact <- sqldf(
  "select q1.kontakt as kontakt,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select [contact] as kontakt,
              count(y) as liczba_prob
       from basedata
       group by kontakt
       order by kontakt) q1
  left join
      (select [contact] as kontakt,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by kontakt
       order by kontakt) q2
  on q1.kontakt = q2.kontakt"
)
contact$skutecznosc <- contact$udane / contact$liczba_prob

windows()
par(mfrow=c(1,1))
barplot(contact$skutecznosc, names.arg = contact$kontakt,
        ylab = "Skutecznoœæ marketingu", 
        main = "Skutecznoœæ marketingu ze wzglêdu na formê kontaktu")

day <- sqldf(
  "select q1.dzien as dzien,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select [day] as dzien,
              count(y) as liczba_prob
       from basedata
       group by dzien
       order by dzien) q1
  left join
      (select [day] as dzien,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by dzien
       order by dzien) q2
  on q1.dzien = q2.dzien"
)
day$skutecznosc <- day$udane / day$liczba_prob

windows()
pie(day$skutecznosc, labels = day$dzien,
    main = "Skutecznoœæ marketingu w zale¿noœci od dnia miesi¹ca")

sqldf(
  "select * from day
  where liczba_prob >= 100
  order by skutecznosc desc"
)

month <- sqldf(
  "select q1.miesiac as miesiac,
          cast(q1.liczba_prob as real) as liczba_prob,
          cast(q2.udane as real) as udane
   from
       (select [month] as miesiac,
              count(y) as liczba_prob
       from basedata
       group by miesiac
       order by miesiac) q1
  left join
      (select [month] as miesiac,
              count(y) as udane
       from basedata
       where y = 'yes'
       group by miesiac
       order by miesiac) q2
  on q1.miesiac = q2.miesiac"
)

windows()
month$skutecznosc <- month$udane / month$liczba_prob
pie(month$skutecznosc, labels = month$miesiac)

sqldf(
  "select * from month
  where liczba_prob >= 100
  order by skutecznosc desc"
)

day_month_yes <- sqldf(
  "select month as miesiac,
          day as dzien,
          count(y) as liczba_udanych
   from basedata
   where y = 'yes'
   group by miesiac,
            dzien
   order by miesiac,
            dzien
   "
)
day_month_yes <- sqldf(
  "select t1.miesiac,
          t1.dzien,
          t1.liczba_udanych,
          t2.liczba_prob
   from day_month_yes as t1
   left join (
        select month as miesiac,
               day as dzien,
               count(y) as liczba_prob
        from basedata
        group by miesiac,
                 dzien
   )as t2
   on t1.dzien = t2.dzien and t1.miesiac = t2.miesiac
  "
)
day_month_yes$skutecznosc <- day_month_yes$liczba_udanych / day_month_yes$liczba_prob

sqldf(
  "select *
   from day_month_yes
   where liczba_prob >= 15
         and skutecznosc >= .1
   order by skutecznosc desc"
)
sqldf(
  "select *
   from day_month_yes
   where liczba_prob >= 15
         and skutecznosc <= .05
   order by skutecznosc asc"
)

windows()
par(mfrow=c(1,2))
hist(basedata$duration[basedata$y == 'no'], xlab = "D³ugoœæ rozmowy w sekundach",
     ylab = "Liczba odmów", main = "Rozk³ad czasu trawnia rozmowy w razue odmowy")
hist(basedata$duration[basedata$y == 'yes'], xlab = "D³ugoœæ rozmowy w sekundach",
     ylab = "Liczba zgód", main = "Rozk³ad czasu trwania rozmowy w razie zgody")

campaign <- sqldf(
  "select campaign as liczba_kontaktow,
          count(y) as liczba_prób
   from basedata
   group by liczba_kontaktow
   order by liczba_kontaktow"
)
campaign <- sqldf(
  "select t1.*,
          t2.liczba_udanych
   from campaign as t1
   left join (select campaign as liczba_kontaktow,
                     count(y) as liczba_udanych
              from basedata
              where y = 'yes'
              group by liczba_kontaktow) as t2
        on t1.liczba_kontaktow = t2.liczba_kontaktow
   order by liczba_kontaktow
")
campaign$liczba_udanych[is.na(campaign$liczba_udanych) == 1] <- 0
campaign$skutecznosc <- campaign$liczba_udanych / campaign$liczba_prób

windows()
par(mfrow=c(1,1))
plot(campaign$liczba_kontaktow[campaign$liczba_prób >= 15], 
     campaign$skutecznosc[campaign$liczba_prób >= 15],
     xlab = "Liczba kontaktów w ramach kampanii marketingowej",
     ylab = "Skutecznoœæ",
     main = "Skutecznoœæ marketingu w zale¿noœci od liczy kontaktów w ramach kampanii")

pdays <- sqldf(
  "select pdays,
          y
   from basedata
   where pdays <> -1"
)

windows()
par(mfrow=c(1,2))
hist(pdays$pdays[pdays$y == 'no'], 
     xlab = "liczba dni pomiedzy kampaniami marketingowymi",
     ylab = "czêstoœæ odmów",
     main = "Rozk³ad odmów w zale¿noœci od d³ugoœci przerwy miedzy kampaniami")
hist(pdays$pdays[pdays$y == 'yes'], 
     xlab = "liczba dni pomiedzy kampaniami marketingowymi",
     ylab = "czêstoœæ zgód",
     main = "Rozk³ad zgód w zale¿noœci od d³ugoœci przerwy miedzy kampaniami")

poutcome <- sqldf(
  "select poutcome,
          pdays,
          y
   from basedata
   where pdays <>-1
         and poutcome <> 'other'"
)
windows()
par(mfrow=c(2,2))
hist(poutcome$pdays[poutcome$y == 'no' & poutcome$poutcome == 'failure'], 
     xlab = "liczba dni pomiêdzy kampaniami marketingowymi",
     ylab = "liczba odmów obecnie", 
     main = "Rozk³ad odmów ze wzglêdu na czas od poprzedniej odmowy")
hist(poutcome$pdays[poutcome$y == 'no' & poutcome$poutcome == 'success'], 
     xlab = "liczba dni pomiêdzy kampaniami marketingowymi",
     ylab = "liczba odmów obecnie", 
     main = "Rozk³ad odmów ze wzglêdu na czas od poprzedniej zgody")
hist(poutcome$pdays[poutcome$y == 'yes' & poutcome$poutcome == 'failure'], 
     xlab = "liczba dni pomiêdzy kampaniami marketingowymi",
     ylab = "liczba zgód obecnie", 
     main = "Rozk³ad zgód ze wzglêdu na czas od poprzedniej odmowy")
hist(poutcome$pdays[poutcome$y == 'yes' & poutcome$poutcome == 'success'], 
     xlab = "liczba dni pomiêdzy kampaniami marketingowymi",
     ylab = "liczba zgód obecnie", 
     main = "Rozk³ad zgód ze wzglêdu na czas od poprzedniej zgody")

changed <- sqldf(
  "select *
   from basedata
   where poutcome = 'failure'
         and y = 'yes'
         and pdays <> - 1"
)

sqldf(
  "select education as wyksztalcenie,
          y as wynik,
          count(y) as liczba_wynikow
   from changed
   group by wyksztalcenie, y
   order by liczba_wynikow desc"
)

changed$poziom_zysku <- sqldf(
  "select case when balance < 0 then 'debt'
               when balance between 0 and 500 then 'low'
               when balance between 501 and 1000 then 'med-low'
               when balance between 1001 and 2000 then 'med'
               when balance between 2001 and 3000 then 'med-high'
               when balance between 3000 and 4000 then 'high'
               when balance > 4000 then 'huge'
          end poziom_zysku
   from changed"
)$poziom_zysku

sqldf(
  "select poziom_zysku,
          y as wynik,
          count(y) as liczba_zgod
   from changed
   where education in ('secondary', 'tertiary')
   group by poziom_zysku, y
   order by liczba_zgod desc"
)

sqldf(
  "select housing as czy_hipoteka,
          count(*) as liczba_zgod
   from changed
   group by czy_hipoteka
   order by liczba_zgod desc"
)

prev <- sqldf(
  "select previous as liczba_kampanii,
          count(y) as liczba_prób
   from basedata
   where previous > 0
   group by liczba_kampanii
   order by liczba_kampanii"
)
prev <- sqldf(
  "select t1.*,
          t2.liczba_udanych
   from prev as t1
   left join (select previous as liczba_kampanii,
                     count(y) as liczba_udanych
              from basedata
              where y = 'yes'
              group by liczba_kampanii) as t2
        on t1.liczba_kampanii = t2.liczba_kampanii
   order by liczba_kampanii
")
prev$liczba_udanych[is.na(prev$liczba_udanych) == 1] <- 0
prev$skutecznosc <- prev$liczba_udanych/prev$liczba_prób

windows()
par(mfrow=c(1,2))
plot(prev$liczba_kampanii, prev$skutecznosc,
     xlab = "Liczba poprzedzaj¹cych kampanii",
     ylab = "Skutecznoœæ bie¿¹cej kampanii",
     main = "Analiza wp³ywu liczby kampanii poprzedzaj¹cych")
plot(prev$liczba_kampanii, prev$liczba_prób,
     xlab = "Liczba poprzedzaj¹cych kampanii",
     ylab = "Liczba bie¿¹cych prób przy x kampaniach poprzedzaj¹cych",
     main = "Rozk³ad liczby prób o danej liczbie kampanii poprzedzaj¹cych")

basedata_sorted <- sqldf(
  "select * from basedata
   order by y, poutcome, education, job, marital, housing, loan,
            [default], contact, balance, age, previous, campaign, pdays, day, month"
)

indices <- 1:length(basedata_sorted[, 1])
train_indices <- indices %% 2 == 1
val_indices <- indices %% 4 == 0
test_indices <- indices %% 4 == 2

train_data <- basedata_sorted[train_indices,]
val_data <- basedata_sorted[val_indices,]
test_data <- basedata_sorted[test_indices,]

#--------------------Zadanie 4--------------------
#stworzenie domyÅ›lnego drzewa klasyfikujÄ…cego
library(rpart)
library(rpart.plot)

model1 = rpart(y ~ .-duration, train_data, method = 'class')
windows()
rpart.plot(model1)

#--------------------Zadanie 5--------------------
prediction1 = predict(model1, train_data, type = "class")
meanAcc = mean(prediction1 == train_data$y)
trainAn = train_data$y
trainAn[trainAn == 'no'] = 0
trainAn[trainAn == 'yes'] = 1
trainAn = as.numeric(trainAn)
confusion.matrix(trainAn, as.numeric(prediction1) - 1, 0.5)


prediction2 = predict(model1, test_data, type = 'class')
mean(prediction2 == test_data$y)

testAn = test_data$y
testAn[testAn == 'no'] = 0
testAn[testAn == 'yes'] = 1
testAn = as.numeric(testAn)
confusion.matrix(testAn, as.numeric(prediction2) - 1, 0.5)

#--------------------Zadanie 6 i 7--------------------
library(rpart)
library(rpart.plot)

basedata_sorted <- sqldf(
  "select * from basedata
   order by y, poutcome, education, job, marital, housing, loan,
            [default], contact, balance, age, previous, campaign, pdays, day, month"
)

indices <- 1:length(basedata_sorted[, 1])
train_indices <- indices %% 2 == 1
train_data <- basedata_sorted[train_indices,]
val_indices <- indices %% 4 == 0
val_data <- basedata_sorted[val_indices,]
test_indices <- indices %% 4 == 2
test_data <- basedata_sorted[test_indices,]

val_an <- val_data$y
val_an[val_an == 'no'] <- 0
val_an[val_an == 'yes'] <- 1
val_an <- as.numeric(val_an)
test_an <- test_data$y
test_an[test_an == 'no'] <- 0
test_an[test_an == 'yes'] <- 1
test_an <- as.numeric(test_an)

weights <- seq(1, 4, by = 0.25)
cp_vec <- seq(0, 0.02, by =0.0025)
minsplits <- seq(1, 31, by = 3)

best_score <- 0
best_i <- 0
best_j <- 0
best_k <- 0

sink('log.txt')
for (i in weights) {
  for (j in cp_vec) {
    for (k in minsplits) {
      weights_vec <- rep(1, length(train_data[, 1]))
      weights_vec[train_data$y == 'yes'] <- i
      
      model <- rpart(y ~ . -duration, train_data, method = 'class', weights = weights_vec,
                     control = rpart.control(cp=j, minsplit = k, xval = 20))
      
      prediction_t <- predict(model, train_data, type = 'class')
      prediction <- predict(model, val_data, type = 'class')
      score_t <- mean(prediction_t == train_data$y)
      score0 <- mean(prediction == val_data$y)
      
      score1 = 0 #TP
      for (z in 1:length(val_data[, 1])) {
        if (prediction[z] == 'yes' & val_data[z, 'y'] == 'yes') {
          score1 = score1 + 1
        }
      }
      score2 = 0 #FP
      for (z in 1:length(val_data[, 1])) {
        if (prediction[z] == 'yes' & val_data[z, 'y'] == 'no') {
          score2 = score2 + 1
        }
      }
      score3 = 0 #FN
      for (z in 1:length(val_data[, 1])) {
        if (prediction[z] == 'no' & val_data[z, 'y'] == 'yes') {
          score3 = score3 + 1
        }
      }
      sensitivity = score1 / (score1 + score3)
      specificity = score1 / (score1 + score2)
      Gmean = sqrt(specificity * sensitivity)
      score = Gmean
      
      if (score > best_score) {
        best_score <- score
        best_i <- i
        best_j <- j
        best_k <- k
        best_model <- model
      }
      
      cat(sprintf("Wynik na zbiorze treningowym dla wag %f, cp=%f i minsplit=%i: %f\n", i, j, k, score_t))
      cat(sprintf("Wynik na zbiorze walidacyjnym dla wag %f, cp=%f i minsplit=%i: %f\n", i, j, k, score))
      cat(confusion.matrix(val_an, as.numeric(prediction) - 1, .5))
      cat("\n")
    }
  }
}  
sink()

weights_vec <- rep(1, length(train_data[, 1]))
weights_vec[train_data$y == 'yes'] <- best_i
model <- rpart(y ~ . -duration, train_data, method = 'class', weights = weights_vec,
               control = rpart.control(cp=best_j, minsplit = best_k))

#ad hoc model
weights_vec <- rep(1, length(train_data[, 1]))
weights_vec[train_data$y == 'yes'] <- 2.25
model <- rpart(y ~ . -duration, train_data, method = 'class', weights = weights_vec,
               control = rpart.control(cp=0.0075, minsplit = 25))


prediction2 <- predict(model, test_data, type = 'class')
mean(prediction2 == test_data$y)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)

#score = 1.414

windows()
rpart.plot(model)

divisor = 3
train_data$rowNo <- seq.int(nrow(train_data))
train_data <- train_data[(train_data$rowNo %% divisor  == 0 & train_data$y == 'no') | train_data$y == 'yes', ]
train_data$rowNo <- NULL


weights <- seq(1, 4, by = 0.25)
cp_vec <- seq(0, 0.02, by =0.0025)
minsplits <- seq(1, 31, by = 3)

best_score <- 0
best_i <- 0
best_j <- 0
best_k <- 0


sink('logDiv4.txt')
for (i in weights) {
  for (j in cp_vec) {
    for (k in minsplits) {
      weights_vec <- rep(1, length(train_data[, 1]))
      weights_vec[train_data$y == 'yes'] <- i
      
      model <- rpart(y ~ . -duration, train_data, method = 'class', weights = weights_vec,
                     control = rpart.control(cp=j, minsplit = k, xval = 20))
      
      prediction_t <- predict(model, train_data, type = 'class')
      prediction <- predict(model, val_data, type = 'class')
      score_t <- mean(prediction_t == train_data$y)
      score0 <- mean(prediction == val_data$y)
    
      score1 = 0 #TP
      for (z in 1:length(val_data[, 1])) {
        if (prediction[z] == 'yes' & val_data[z, 'y'] == 'yes') {
          score1 = score1 + 1
        }
      }
      score2 = 0 #FP
      for (z in 1:length(val_data[, 1])) {
        if (prediction[z] == 'yes' & val_data[z, 'y'] == 'no') {
          score2 = score2 + 1
        }
      }
      score3 = 0 #FN
      for (z in 1:length(val_data[, 1])) {
        if (prediction[z] == 'no' & val_data[z, 'y'] == 'yes') {
          score3 = score3 + 1
        }
      }
      sensitivity = score1 / (score1 + score3)
      specificity = score1 / (score1 + score2)
      Gmean = sqrt(specificity * sensitivity)
      score = Gmean
      
      if (score > best_score) {
        best_score <- score
        best_i <- i
        best_j <- j
        best_k <- k
        best_model <- model
      }
      
      cat(sprintf("Wynik na zbiorze treningowym dla wag %f, cp=%f i minsplit=%i: %f\n", i, j, k, score_t))
      cat(sprintf("Wynik na zbiorze walidacyjnym dla wag %f, cp=%f i minsplit=%i: %f\n", i, j, k, score))
      cat(confusion.matrix(val_an, as.numeric(prediction) - 1, .5))
      cat("\n")
    }
  }
}  
# sink()

weights_vec <- rep(1, length(train_data[, 1]))
weights_vec[train_data$y == 'yes'] <- best_i
model <- rpart(y ~ . -duration, train_data, method = 'class', weights = weights_vec,
               control = rpart.control(cp=best_j, minsplit = best_k))

#ad hoc model
weights_vec <- rep(1, length(train_data[, 1]))
weights_vec[train_data$y == 'yes'] <- 1
model <- rpart(y ~ . -duration, train_data, method = 'class', weights = weights_vec,
               control = rpart.control(cp=0.0075, minsplit = 9))

prediction2 <- predict(model, test_data, type = 'class')
mean(prediction2 == test_data$y)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)

#score = 1.414

windows()
rpart.plot(model)

printcp(model)
model <- prune(model, model$cptable[which.min(model$cptable[,"xerror"]),"CP"])

prediction2 <- predict(model, test_data, type = 'class')
mean(prediction2 == test_data$y)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)

#score = 1.414

windows()
rpart.plot(model)

