#załadowanie potrzebnych zależności
setwd("E:/9sem/DM/zep/repo")
library(sqldf)
library(data.table)
library(plyr)
#załadowanie bazowych danych



#--------------------Zadanie 1--------------------
basedata <- read.csv("E:/9sem/DM/zep/bank/bank.csv", row.names=NULL, sep=";", stringsAsFactors=FALSE)



#--------------------Zadanie 2--------------------
#opis danych i zagadnienia klasyfikacyjnego:
#zaczerpnięty z MCI Machine Learning Repository:

# Input variables:
#   # bank client data:
#   1 - age (numeric)
# 2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
#                        "blue-collar","self-employed","retired","technician","services") 
# 3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
# 4 - education (categorical: "unknown","secondary","primary","tertiary")
# 5 - default: has credit in default? (binary: "yes","no")
# 6 - balance: average yearly balance, in euros (numeric) 
# 7 - housing: has housing loan? (binary: "yes","no")
# 8 - loan: has personal loan? (binary: "yes","no")
# # related with the last contact of the current campaign:
# 9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
# 10 - day: last contact day of the month (numeric)
# 11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
# 12 - duration: last contact duration, in seconds (numeric)
# # other attributes:
# 13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
# 14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
# 15 - previous: number of contacts performed before this campaign and for this client (numeric)
# 16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
# 
# Output variable (desired target):
#   17 - y - has the client subscribed a term deposit? (binary: "yes","no")

#The classification goal is to predict if the client will subscribe a term deposit (variable y).

#warto zauważyć że dane te są już obrobione. Nie zawierają wartości pustych
#a wartości nieznane, np zawodów rozmówców są oznaczone




#--------------------Zadanie 3--------------------
#analiza eksploracyjna
y_quot <- sqldf(
  "select y, count(y) as liczba_wynikow
   from basedata
   group by y"
)
y_quot
#mamy 8 razy mniej zgód na pożyczkę niż odmów. Może być trochę problem z wytrenowaniem
#drzwa przewidującego zgody
#badanie wypływu wieku ankietowanych

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
     ylab = "Liczba odmów", main = "Rozkład wieku odmawiających")
plot(age_yes$age, age_yes$yes, xlab = "Wiek", ylab = "Liczba zgód",
     main="Rozkład wieku zgadzajacych się")
plot(age_y$age, age_y$liczba_prob, xlab = "Wiek", ylab = "Liczba prób",
     main = "Rozkład wieku ogólnie")
#strasznie mało danych dla ludzi skrajnie młodych i skrajnie starych
#dlatego nie ma wykresu skutecznośći w ułamku, bo dla tych ludzi
#wynik byłby niewiarygodny i zawyżony

#liczba zgód jest nieproporcjonalnie większa dla ludzi
#w wieku pomiędzy 45-60 rokiem życia

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
        ylab = "Skuteczność marketingu",
        main = "Skuteczność marketingu ze względu na zawód")
#zdecydowanie duża większa skuteczność dla studentów/uczniów i emerytów

#analiza statusu matrymonialnego
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

#robię w ten sposób bo backend od RSqLite nie radzi sobie z konwersją
marital$skutecznosc <- marital$udane / marital$liczba_prob

windows()
par(mfrow = c(1,1))
barplot(marital$skutecznosc, names.arg = marital$stan, 
        ylab = "Skuteczność marketingu",
        main = "Skuteczność marketingu ze względu na status matrymonialny")
#widać że married rzadziej decydują się na pozyczke

#analiza wpływu poziomu wykształcenia
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
        ylab = "Skuteczniość marketingu",
        main = "Skuteczniość marketingu ze względu na wykształcenie")
#ludzie z wykształceniem wyższym częściej zgadzali się na pożyczkę
#można się było tego spodziewać, powinni mieć większa zdolność kredytowa

#analiza defaultu - posiadania kredytu, któego nie jest się w stanie spłacić
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
        ylab = "Skuteczność marketingu",
        main = "Skuteczność marketingu ze względu na kredyt niemożliwy do spłacenia")

#tu też jestem zdziwiony, spodziewałbym się, że ludzie którzy już
#wcześniej zawiedli w spłacaniu kredytu będą się mniej decydować
#a tu jednak rozkłada się to bardzo identycznie

#analiza zamożności
#przetwarzamy i wizualizujemy bardziej ręcznie
#gdyż wbudowane histogramy ciężko razem porównywać
#ze względu na trudną manipulację przedziałami na osi X
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
#aby łatwiej porównywać ucinamy każdy zysk do 5-setek
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
        xlab = "Roczny zysk w euro", ylab = "Skuteczność telemarketingu",
        main = "Skuteczność marketingu ze względu na zysk roczny")
barplot(balance_all$liczba_prob, names.arg = balance_all$balance,
        xlab = "Roczny zysk w euro", ylab =  "Liczba prób telemarketingowych",
        main = "Rozkład prób marketingu ze względu na zysk roczny")

#prób dla ludzi z olbrzymim zyskiem 10000+ jest malo dlatego sądzimy, że duża skueczność dla nich jest przypadkowa
#warto zwrócić uwagę, że jest dużo prób dla ludzi z małą sratą lub zyskiem do 5 tysięcy
#warto zauwazyć że ludzie z małym długiem cześciej decydują się na pożyczkę niż ludzie
#z minimalnym rocznym zyskiem 0-500 euro, prawdopodobnie próbują ratować się pożyczkami
#warto zauwazyć dużą skuteczność dla ludzi z zyskiem 3000 - 5000 euro rocznie
#prawdopodobnie ludzie Ci biorą pozyczki na np rozwój swoich działaności gospodarczych
#lub mają dużą zdolnośc kredytową się kupują sobie mieszkania/samochody

#analiza posiadania obecnie kredytu hipotecznego
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
        main = "Skuteczność marketingu ze względu na posiadanie kredytu hipotecznego")
#widać że skuteczność marketingu jest o wiele większa w przypadku
#rozmówcóW, którzy nie mają na głwoie kredytu hipotecznego

#sprawdzmy jak hipoteka się przedstawia przy uwzglednieniu zyskow/dlugow
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
#tutaj przy okazji widać, że zdecydowana większośc ludzi którzy rok kończą z długiem
#posiada kredyt hipoteczny
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
        ylab = "Skuteczność marketingu",
        main = "Skuteczność marketingu ze względu na posiadanie hipoteki i poziom zysku rocznego")

#warto zwrócić uwagę, że chętnymi klientami są ludzie
#o średnim (w tym mid-low i mid-high) poziomie bilansu rocznego
#i nieposiadający kredytów hipotecznych
#drugą klasą chętnych klientów są klienci o niskim bilansie rocznym
#nie posiadających hipoteki - możę właśnie im ją wcisneli?

#sprawdzenie wpływu posiadania pożyczki
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
        ylab = "Skuteczność marketingu",
        main = "Skuteczność marketingu ze względu na posiadanie pożyczki")
#nie ma niespodzianki, że ludzie którzy obecnie nie mają żadnej pożyczki, biorą 1. chetniej

#sprawdźmy uwzgledniając niemożliwośc spłaty (defaulty)
loan_default <- sqldf(
  "select loan, [default], y as wynik, count(y) as liczba_wynikow
   from basedata
   group by loan, [default], wynik
   order by loan, [default], wynik"
)
loan_default$loan <- paste("loan", loan_default$loan, sep = "-")
loan_default$default <- paste("def", loan_default$default, sep = "-")
#mamy bardzo małą próbę dla ludzi z kredytem i z defaultem
#więc wskazanie dla nich może być mało wiarygodne

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
        ylab = "Skuteczność marketingu",
        main = "Skuteczność marketingu ze względu na posiadanie pożyczki lub utopionego kredytu")

#niestety nie można wyciągnąć żadnego wniosku na temat wyróżnienia szczególnej
#grupy rozmówców na podstawie par loan-default

#badanie wpływu typu kontaktu
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
        ylab = "Skuteczność marketingu", 
        main = "Skuteczność marketingu ze względu na formę kontaktu")
#nie wygląda na to aby forma kontaktu miałą wpływ na wynik telemarketingu

#analiza wpływu daty kontaktów
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
    main = "Skuteczność marketingu w zależności od dnia miesiąca")
#1, 10, 24 i 30 dzień miesiąca wyróżniają się większą skutecznością telemarketingu
#1 dnia było mało kontaktów, więc prognoza moze być zbyt optymistyczna
#możę to być związane z cyklem wypłacania wynagrodzeń
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
#skuteczność jest zdecydowanie większa w marcu, wrześniu, październiku i grudniu
#w grudniu mamy tylko 20 prób, więc to może być przyczyną dużej skuteczności w tym miesiący
#duża skuteczność w marcu: być może pożyczki związane z karnawałem (ja bym nie brał, ale kto wie)
#duża skuteczność we wrześniu i październiku: powrót po urlopach, może kredyty studenckie

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

#tych różnych dni jest dużo, więc raport będzie w postaci selecta
#sprawdzamy dni, dla których kontaktów było conajmniej 15 i skutecznosc >= 10%
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

#istnieją dni które wyróżniają się wysoką skutecznością i takie dla których skuteczność jest znikoma.
#sprawdzenie jak rozkładają się zawody ludzi biorących pożyczki we wrześniu i październiku:
job_month <- sqldf(
  "select miesiac,
          zawod,
          count(*) as liczba_wynikow
   from
  (  select month as miesiac,
            job as zawod
     from basedata
     where month in ('sep', 'oct')
           and y = 'yes') t1
   group by miesiac, zawod"
)

windows()
par(mfrow=c(1,2))
pie(job_month$liczba_wynikow[job_month$miesiac == 'sep'], 
    labels = job_month$zawod[job_month$miesiac == 'sep'],
    main = "Rozkład zawodów ludzi biorących pożyczki we wrześniu")
pie(job_month$liczba_wynikow[job_month$miesiac == 'oct'], 
    labels = job_month$zawod[job_month$miesiac == 'oct'],
    main = "Rozkład zawodów ludzi biorących pożyczki w październiku")
#okazuje się że strzał że brane są pożyczki studenckie był nieuzasadniony
#większością w obu przypadkach są ludzie pracujacy przy zarządzaniu

#analiza wpływu czasu trwania ostatniej rozmowy

windows()
par(mfrow=c(1,2))
hist(basedata$duration[basedata$y == 'no'], xlab = "Długość rozmowy w sekundach",
     ylab = "Liczba odmów", main = "Rozkład czasu trawnia rozmowy w razie odmowy")
hist(basedata$duration[basedata$y == 'yes'], xlab = "Długość rozmowy w sekundach",
     ylab = "Liczba zgód", main = "Rozkład czasu trwania rozmowy w razie zgody")

#widać, że w przypadku wzięcia pożyczki rozmowy z konsultantem były dłuższe
#ludzie nie zgadzali się szybciej niz w 5 minut długie rozmowy przynosiły zgody
#ludzie odmawiający starali się jak najszybciej odmówić. Rozmowy powyżej 20 minut nie pomogły

#analiza wpływu liczby kontaktów w ramach kampanii
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
     xlab = "Liczba kontaktóW w ramach kampanii marketingowej",
     ylab = "Skuteczność",
     main = "Skuteczność marketingu w zależności od liczy kontaktów w ramach kampanii")

#można dostrzec małą tendencję, że skuteczność maleje wraz ze wzrostem liczby kontaktów
#męczenie klienta telefonami nie pomaga

#analiza wpływu czasu pomiędzy, który upłynął od ostatniego kontaktu z poprzedniej kampanii
#zbijamy przerwy na przedziały kategoryczne w celu łatwiejszej analizy
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
     ylab = "częstość odmów",
     main = "Rozkład odmów w zależności od długości przerwy miedzy kampaniami")
hist(pdays$pdays[pdays$y == 'yes'], 
     xlab = "liczba dni pomiedzy kampaniami marketingowymi",
     ylab = "częstość zgód",
     main = "Rozkład zgód w zależności od długości przerwy miedzy kampaniami")
#zdecydowana większość ludzi zgadzających się na pożyczkę decyduje się w odstępie mniejszym niż 200 dni
#zestawmy to z wynikiem poprzedniej kampanii
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
     xlab = "liczba dni pomiędzy kampaniami marketingowymi",
     ylab = "liczba odmów obecnie", 
     main = "Rozkład odmów ze względu na czas od poprzedniej odmowy")
hist(poutcome$pdays[poutcome$y == 'no' & poutcome$poutcome == 'success'], 
     xlab = "liczba dni pomiędzy kampaniami marketingowymi",
     ylab = "liczba odmów obecnie", 
     main = "Rozkład odmów ze względu na czas od poprzedniej zgody")
hist(poutcome$pdays[poutcome$y == 'yes' & poutcome$poutcome == 'failure'], 
     xlab = "liczba dni pomiędzy kampaniami marketingowymi",
     ylab = "liczba zgód obecnie", 
     main = "Rozkład zgód ze względu na czas od poprzedniej odmowy")
hist(poutcome$pdays[poutcome$y == 'yes' & poutcome$poutcome == 'success'], 
     xlab = "liczba dni pomiędzy kampaniami marketingowymi",
     ylab = "liczba zgód obecnie", 
     main = "Rozkład zgód ze względu na czas od poprzedniej zgody")
#widać że klienci są dość statyczni: mało klientów zmieniło stanowisko od czasu następnej kampanii
#zdecydowana większość klientów którzy zdecydowali się odmówić (przy poprzedniej zgodzie)
#zgodziła się po kontakcie po przerwie 50-200 dni lub! 300-350 dni
#najwięcej ludzi którzy ponownie wzieli pożyczkę zrobiło to po kontakcie po 50-100 dniach lub 150-200 dniach
#nie biorą też pożyczek od razu, najwyraźniej próbują się zorientować w promocjach itd
#nakładniając ludzi do wzięcia pożyczki mimo uprzedniej odmowy kampanie po przerwie 400 dni nie przyniosły efektów
#najwięcej ludzi zmieniło zdanie jeśli ponowny kontakt wystącił pomiędzy 100 a 200 dniach
#przy tej wartości przerwy ogólnie najwięcej klientów zgadza się na pożyczkę

#spróbujmy znaleźc do cechuje klientów którzy zmienili zdanie i zgodzili się na pożyczkę
changed <- sqldf(
  "select *
   from basedata
   where poutcome = 'failure'
         and y = 'yes'
         and pdays <> - 1"
)
#analizując tę tabelę w BROWSERZE rzuca się w oczy że: 
#   prawie nikt z nich nie ma pożyczki - loan = 'no'
#   prawie każdy kontakt był na komórkę 'cellular'
#   nikt z nich nie ma utopionego kredytu
sqldf(
  "select education as wyksztalcenie,
          y as wynik,
          count(y) as liczba_wynikow
   from changed
   group by wyksztalcenie, y
   order by liczba_wynikow desc"
)
#widzimy że najczęściej zmieniają zdanie ludzie wykształceni, moze pozostali nie mają zdolności kredytowej
#sprawdzmy poziom zarobków wśród tych ludzi
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
#a jednak pożyczki biorą raczej ludzie którzy są wykształceni ale mają niski poziom zysku rocznego
sqldf(
  "select housing as czy_hipoteka,
          count(*) as liczba_zgod
   from changed
   group by czy_hipoteka
   order by liczba_zgod desc"
)
#nieznaczna większość z nich ma hipotekę
#jednak nie można jednoznacznie wskazać dominujący wąski typ człowieka który zmienił zdanie i wziął pożyczkę

#sprawdzmy jeszcze wpływ liczby kampanii 
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
     xlab = "Liczba poprzedzających kampanii",
     ylab = "Skuteczność bieżącej kampanii",
     main = "Analiza wpływu liczby kampanii poprzedzających")
plot(prev$liczba_kampanii, prev$liczba_prób,
     xlab = "Liczba poprzedzających kampanii",
     ylab = "Liczba bieżących prób przy x kampaniach poprzedzających",
     main = "Rozkład liczby prób o danej liczbie kampanii poprzedzających")


#do 6. kampanii poprzedzających mamy dużo próbek, więc analiza może być wiarygodna
#można zauważyć małą tendencję wzrostową - klieci są lojalni, a przynajmniej Ci od 6 kampanii
#a im więcej ich przebyli tym większa skuteczność marketingu
#dla wiekszych liczb kampanii mamy mało prób, ale raczej spodziewalibyśmy się, że nie skutkują
#i własnie dlatego nie były podejmowane

#podział danych na treningowe, walidacyjne i testowe

#aby dane mogłby być reprezentatywne poszeregujemy dany i wybierzemy zbiory okresowo
#w sekwencji treningowy - walidacyjny - treningowy - testowy
#ustawiamy dane pokolei według kryteriów najpierw kategorycznych, potem liczbowych
#kolejnosc sortowania: y, poutcome, education, job, marital, housing, loan, default, contact, balance, age
#previous, campaing, pdays, day, month. W 1 kolejnosci zmienne może bardziej istotne
basedata_sorted <- sqldf(
  "select * from basedata
   order by y, poutcome, education, job, marital, housing, loan,
            [default], contact, balance, age, previous, campaign, pdays, day, month"
)

indices <- 1:length(basedata_sorted[, 1])
train_indices <- indices %% 2 == 1
val_indices <- indices %% 4 == 2
test_indices <- indices %% 4 == 0

train_data <- basedata_sorted[train_indices,]
val_data <- basedata_sorted[val_indices,]
test_data <- basedata_sorted[test_indices,]

#--------------------Zadanie 4--------------------
#stworzenie domyślnego drzewa klasyfikującego
library(rpart)
library(rpart.plot)
library(SDMTools)

model1 = rpart(y ~ ., train_data, method = 'class')
windows()
rpart.plot(model1)
text(model1)



#--------------------Zadanie 5--------------------
#opis drzewa będzie w sprawozdaniu
#analiza drzewa
#wyniki na zbiorze uczącym i testowym

prediction1 = predict(model1, train_data, type = "class")
mean_acc = mean(prediction1 == train_data$y)
train_an = train_data$y
train_an[train_an == 'no'] = 0
train_an[train_an == 'yes'] = 1
train_an = as.numeric(train_an)
confusion.matrix(train_an, as.numeric(prediction1) - 1, 0.5)
#nawet na zbiorze uczącym jest więcej false-negative'ów niż yes-yes

prediction2 = predict(model1, test_data, type = 'class')
mean(prediction2 == test_data$y)
#0.893% skuteczności @@> aż za dobrze bym powiedział

test_an = test_data$y
test_an[test_an == 'no'] = 0
test_an[test_an == 'yes'] = 1
test_an = as.numeric(test_an)
confusion.matrix(test_an, as.numeric(prediction2) - 1, 0.5)
#widać, że  jeśli chodzi o przyjęcie pożyczki to model więcej nie trafił (false-negative) niż trafił
#model swoją skuteczność nabija na odmowach, których jest pełno,
#ale nie jest przydatny do przewidywania wzięcia pożyczki

#domyślne drzewo spisuje się słabo, nie ma zdolności do przewidywania zgód
#spodziewamy się poprawić jakoś klasyfikacji drzewami z dobieranymi parametrami
#i z ustaleniem większych wag dla próbek o y = 'yes'

#wwartosc funkcji celu w zadaniu optymalizacji: 0.893 + 0.01*(43-33) + 0.005*43 = 1.423

#--------------------Zadanie 6 i 7--------------------
#walidacja - analiza wypływu hiperparametrów na jakość klasyfikacji
#wpływ zwiększonych wag

basedata_sorted <- sqldf(
  "select * from basedata
   order by y, poutcome, education, job, marital, housing, loan,
            [default], contact, balance, age, previous, campaign, pdays, day, month"
)

indices <- 1:length(basedata_sorted[, 1])
train_indices <- indices %% 2 == 1
val_indices <- indices %% 4 == 2
test_indices <- indices %% 4 == 0

train_data <- basedata_sorted[train_indices,]
val_data <- basedata_sorted[val_indices,]
test_data <- basedata_sorted[test_indices,]

val_an <- val_data$y
val_an[val_an == 'no'] <- 0
val_an[val_an == 'yes'] <- 1
val_an <- as.numeric(val_an)

test_an <- test_data$y
test_an[test_an == 'no'] <- 0
test_an[test_an == 'yes'] <- 1
test_an <- as.numeric(test_an)

weights <- seq(1, 2, by = 0.25)
cp_vec <- seq(0.00, 0.02, by = 0.0025)
minsplits <- seq(1, 22, by = 3)

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
      
      model <- rpart(y ~., train_data, method = 'class', weights = weights_vec,
                     control = rpart.control(cp=j, minsplit = k))
      
      prediction_t <- predict(model, train_data, type = 'class')
      prediction <- predict(model, val_data, type = 'class')
      score_t <- mean(prediction_t == train_data$y)
      score0 <- mean(prediction == val_data$y)
      
      #wpływ TP
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
      score = score0 + 0.01*(score1 - score2) + 0.01*score1 #premia za pewnośc TP vs FP
      
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
weights_vec[train_data$y == 'yes'] <- 1.5
model <- rpart(y ~., train_data, method = 'class', weights = weights_vec,
               control = rpart.control(cp=0.005, minsplit = 16))

prediction2 <- predict(model, test_data, type = 'class')
mean(prediction2 == test_data$y)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)

#score = 1.189

windows()
rpart.plot(model)

printcp(model)

model <- prune(model, cp=0.019157)

windows()
rpart.plot(model)

prediction2 <- predict(model, test_data, type = 'class')
mean(prediction2 == test_data$y)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)

#score = 1.676