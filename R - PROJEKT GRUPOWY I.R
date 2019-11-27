#za³adowanie potrzebnych zale¿noœci
setwd("E:/9sem/DM/zep/repo")
library(sqldf)
library(data.table)
library(plyr)
library(rpart)
library(rpart.plot)
library(SDMTools)
#za³adowanie bazowych danych



#--------------------Zadanie 1--------------------
basedata <- read.csv("E:/9sem/DM/zep/bank/bank.csv", row.names=NULL, sep=";", stringsAsFactors=FALSE)



#--------------------Zadanie 2--------------------
#opis danych i zagadnienia klasyfikacyjnego:
#zaczerpniêty z MCI Machine Learning Repository:

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

#warto zauwa¿yæ ¿e dane te s¹ ju¿ obrobione. Nie zawieraj¹ wartoœci pustych
#a wartoœci nieznane, np zawodów rozmówców s¹ oznaczone




#--------------------Zadanie 3--------------------
#analiza eksploracyjna
y_quot <- sqldf(
  "select y, count(y) as liczba_wynikow
   from basedata
   group by y"
)
y_quot
#mamy 8 razy mniej zgód na po¿yczkê ni¿ odmów. Mo¿e byæ trochê problem z wytrenowaniem
#drzwa przewiduj¹cego zgody
#badanie wyp³ywu wieku ankietowanych

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
#strasznie ma³o danych dla ludzi skrajnie m³odych i skrajnie starych
#dlatego nie ma wykresu skutecznoœæi w u³amku, bo dla tych ludzi
#wynik by³by niewiarygodny i zawy¿ony

#liczba zgód jest nieproporcjonalnie wiêksza dla ludzi
#w wieku pomiêdzy 45-60 rokiem ¿ycia

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
#zdecydowanie du¿a wiêksza skutecznoœæ dla studentów/uczniów i emerytów

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

#robiê w ten sposób bo backend od RSqLite nie radzi sobie z konwersj¹
marital$skutecznosc <- marital$udane / marital$liczba_prob

windows()
par(mfrow = c(1,1))
barplot(marital$skutecznosc, names.arg = marital$stan, 
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu na status matrymonialny")
#widaæ ¿e married rzadziej decyduj¹ siê na pozyczke

#analiza wp³ywu poziomu wykszta³cenia
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
#ludzie z wykszta³ceniem wy¿szym czêœciej zgadzali siê na po¿yczkê
#mo¿na siê by³o tego spodziewaæ, powinni mieæ wiêksza zdolnoœæ kredytowa

#analiza defaultu - posiadania kredytu, któego nie jest siê w stanie sp³aciæ
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

#tu te¿ jestem zdziwiony, spodziewa³bym siê, ¿e ludzie którzy ju¿
#wczeœniej zawiedli w sp³acaniu kredytu bêd¹ siê mniej decydowaæ
#a tu jednak rozk³ada siê to bardzo identycznie

#analiza zamo¿noœci
#przetwarzamy i wizualizujemy bardziej rêcznie
#gdy¿ wbudowane histogramy ciê¿ko razem porównywaæ
#ze wzglêdu na trudn¹ manipulacjê przedzia³ami na osi X
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
#aby ³atwiej porównywaæ ucinamy ka¿dy zysk do 5-setek
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

#prób dla ludzi z olbrzymim zyskiem 10000+ jest malo dlatego s¹dzimy, ¿e du¿a skuecznoœæ dla nich jest przypadkowa
#warto zwróciæ uwagê, ¿e jest du¿o prób dla ludzi z ma³¹ srat¹ lub zyskiem do 5 tysiêcy
#warto zauwazyæ ¿e ludzie z ma³ym d³ugiem czeœciej decyduj¹ siê na po¿yczkê ni¿ ludzie
#z minimalnym rocznym zyskiem 0-500 euro, prawdopodobnie próbuj¹ ratowaæ siê po¿yczkami
#warto zauwazyæ du¿¹ skutecznoœæ dla ludzi z zyskiem 3000 - 5000 euro rocznie
#prawdopodobnie ludzie Ci bior¹ pozyczki na np rozwój swoich dzia³anoœci gospodarczych
#lub maj¹ du¿¹ zdolnoœc kredytow¹ siê kupuj¹ sobie mieszkania/samochody

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
        main = "Skutecznoœæ marketingu ze wzglêdu na posiadanie kredytu hipotecznego")
#widaæ ¿e skutecznoœæ marketingu jest o wiele wiêksza w przypadku
#rozmówcóW, którzy nie maj¹ na g³woie kredytu hipotecznego

#sprawdzmy jak hipoteka siê przedstawia przy uwzglednieniu zyskow/dlugow
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
#tutaj przy okazji widaæ, ¿e zdecydowana wiêkszoœc ludzi którzy rok koñcz¹ z d³ugiem
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
        ylab = "Skutecznoœæ marketingu",
        main = "Skutecznoœæ marketingu ze wzglêdu posiadanie hipoteki i poziom zysku rocznego")

#warto zwróciæ uwagê, ¿e chêtnymi klientami s¹ ludzie
#o œrednim (w tym mid-low i mid-high) poziomie bilansu rocznego
#i nieposiadaj¹cy kredytów hipotecznych
#drug¹ klas¹ chêtnych klientów s¹ klienci o niskim bilansie rocznym
#nie posiadaj¹cych hipoteki - mo¿ê w³aœnie im j¹ wcisneli?

#sprawdzenie wp³ywu posiadania po¿yczki
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
#nie ma niespodzianki, ¿e ludzie którzy obecnie nie maj¹ ¿adnej po¿yczki, bior¹ 1. chetniej

#sprawdŸmy uwzgledniaj¹c niemo¿liwoœc sp³aty (defaulty)
loan_default <- sqldf(
  "select loan, [default], y as wynik, count(y) as liczba_wynikow
   from basedata
   group by loan, [default], wynik
   order by loan, [default], wynik"
)
loan_default$loan <- paste("loan", loan_default$loan, sep = "-")
loan_default$default <- paste("def", loan_default$default, sep = "-")
#mamy bardzo ma³¹ próbê dla ludzi z kredytem i z defaultem
#wiêc wskazanie dla nich mo¿e byæ ma³o wiarygodne

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
 
#niestety nie mo¿na wyci¹gn¹æ ¿adnego wniosku na temat wyró¿nienia szczególnej
#grupy rozmówców na podstawie par loan-default

#badanie wp³ywu typu kontaktu
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
#nie wygl¹da na to aby forma kontaktu mia³¹ wp³yw na wynik telemarketingu

#analiza wp³ywu daty kontaktów
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
#1, 14, 24 i 30 dzieñ miesi¹ca wyró¿niaj¹ siê wiêksz¹ skutecznoœci¹ telemarketingu
#1 dnia by³o ma³o kontaktów, wiêc prognoza moze byæ zbyt optymistyczna
#mo¿ê to byæ zwi¹zane z cyklem wyp³acania wynagrodzeñ
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
#skutecznoœæ jest zdecydowanie wiêksza w marcu, wrzeœniu, paŸdzierniku i grudniu
#w grudniu mamy tylko 20 prób, wiêc to mo¿e byæ przyczyn¹ du¿ej skutecznoœci w tym miesi¹cy
#du¿a skutecznoœæ w marcu: byæ mo¿e po¿yczki zwi¹zane z karnawa³em (ja bym nie bra³, ale kto wie)
#du¿a skutecznoœæ we wrzeœniu i paŸdzierniku: powrót po urlopach, mo¿e kredyty studenckie

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

#tych ró¿nych dni jest du¿o, wiêc raport bêdzie w postaci selecta
#sprawdzamy dni, dla których kontaktów by³o conajmniej 15 i skutecznosc >= 10%
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

#istniej¹ dni które wyró¿niaj¹ siê wysok¹ skutecznoœci¹ i takie dla których skutecznoœæ jest znikoma.
#sprawdzenie jak rozk³adaj¹ siê zawody ludzi bior¹cych po¿yczki we wrzeœniu i paŸdzierniku:
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
    main = "Rozk³ad zawodów ludzi bior¹cych po¿yczki we wrzeœniu")
pie(job_month$liczba_wynikow[job_month$miesiac == 'oct'], 
    labels = job_month$zawod[job_month$miesiac == 'oct'],
    main = "Rozk³ad zawodów ludzi bior¹cych po¿yczki w paŸdzierniku")
#okazuje siê ¿e strza³ ¿e brane s¹ po¿yczki studenckie by³ nieuzasadniony
#wiêkszoœci¹ w obu przypadkach s¹ ludzie pracujacy przy zarz¹dzaniu

#analiza wp³ywu czasu trwania ostatniej rozmowy

windows()
par(mfrow=c(1,2))
hist(basedata$duration[basedata$y == 'no'], xlab = "D³ugoœæ rozmowy w sekundach",
     ylab = "Liczba odmów", main = "Rozk³ad czasu trawnia rozmowy w razue odmowy")
hist(basedata$duration[basedata$y == 'yes'], xlab = "D³ugoœæ rozmowy w sekundach",
     ylab = "Liczba zgód", main = "Rozk³ad czasu trwania rozmowy w razie zgody")

#widaæ, ¿e w przypadku wziêcia po¿yczki rozmowy z konsultantem by³y d³u¿sze
#ludzie nie zgadzali siê szybciej niz w 5 minut d³ugie rozmowy przynosi³y zgody
#ludzie odmawiaj¹cy starali siê jak najszybciej odmówiæ. Rozmowy powy¿ej 20 minut nie pomog³y

#analiza wp³ywu liczby kontaktów w ramach kampanii
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
     ylab = "Skutecznoœæ",
     main = "Skutecznoœæ marketingu w zale¿noœci od liczy kontaktów w ramach kampanii")

#mo¿na dostrzec ma³¹ tendencjê, ¿e skutecznoœæ maleje wraz ze wzrostem liczby kontaktów
#mêczenie klienta telefonami nie pomaga

#analiza wp³ywu czasu pomiêdzy, który up³yn¹³ od ostatniego kontaktu z poprzedniej kampanii
#zbijamy przerwy na przedzia³y kategoryczne w celu ³atwiejszej analizy
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
#zdecydowana wiêkszoœæ ludzi zgadzaj¹cych siê na po¿yczkê decyduje siê w odstêpie mniejszym ni¿ 200 dni
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
#widaæ ¿e klienci s¹ doœæ statyczni: ma³o klientów zmieni³o stanowisko od czasu nastêpnej kampanii
#zdecydowana wiêkszoœæ klientów którzy zdecydowali siê odmówiæ (przy poprzedniej zgodzie)
#zgodzi³a siê po kontakcie po przerwie 50-200 dni lub! 300-350 dni
#najwiêcej ludzi którzy ponownie wzieli po¿yczkê zrobi³o to po kontakcie po 50-100 dniach lub 150-200 dniach
#nie bior¹ te¿ po¿yczek od razu, najwyraŸniej próbuj¹ siê zorientowaæ w promocjach itd
#nak³adniaj¹c ludzi do wziêcia po¿yczki mimo uprzedniej odmowy kampanie po przerwie 400 dni nie przynios³y efektów
#najwiêcej ludzi zmieni³o zdanie jeœli ponowny kontakt wyst¹ci³ pomiêdzy 100 a 200 dniach
#przy tej wartoœci przerwy ogólnie najwiêcej klientów zgadza siê na po¿yczkê

#spróbujmy znaleŸc do cechuje klientów którzy zmienili zdanie i zgodzili siê na po¿yczkê
changed <- sqldf(
  "select *
   from basedata
   where poutcome = 'failure'
         and y = 'yes'
         and pdays <> - 1"
)
#analizuj¹c tê tabelê w BROWSERZE rzuca siê w oczy ¿e: 
#   prawie nikt z nich nie ma po¿yczki - loan = 'no'
#   prawie ka¿dy kontakt by³ na komórkê 'cellular'
#   nikt z nich nie ma utopionego kredytu
sqldf(
  "select education as wyksztalcenie,
          y as wynik,
          count(y) as liczba_wynikow
   from changed
   group by wyksztalcenie, y
   order by liczba_wynikow desc"
)
#widzimy ¿e najczêœciej zmieniaj¹ zdanie ludzie wykszta³ceni, moze pozostali nie maj¹ zdolnoœci kredytowej
#sprawdzmy poziom zarobków wœród tych ludzi
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
#a jednak po¿yczki bior¹ raczej ludzie którzy s¹ wykszta³ceni ale maj¹ niski poziom zysku rocznego
sqldf(
  "select housing as czy_hipoteka,
          count(*) as liczba_zgod
   from changed
   group by czy_hipoteka
   order by liczba_zgod desc"
)
#nieznaczna wiêkszoœæ z nich ma hipotekê
#jednak nie mo¿na jednoznacznie wskazaæ dominuj¹cy w¹ski typ cz³owieka który zmieni³ zdanie i wzi¹³ po¿yczkê

#sprawdzmy jeszcze wp³yw liczby kampanii 
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


#do 6. kampanii poprzedzaj¹cych mamy du¿o próbek, wiêc analiza mo¿e byæ wiarygodna
#mo¿na zauwa¿yæ ma³¹ tendencjê wzrostow¹ - klieci s¹ lojalni, a przynajmniej Ci od 6 kampanii
#a im wiêcej ich przebyli tym wiêksza skutecznoœæ marketingu
#dla wiekszych liczb kampanii mamy ma³o prób, ale raczej spodziewalibyœmy siê, ¿e nie skutkuj¹
#i w³asnie dlatego nie by³y podejmowane

#podzia³ danych na treningowe, walidacyjne i testowe

#aby dane mog³by byæ reprezentatywne poszeregujemy dany i wybierzemy zbiory okresowo
#w sekwencji treningowy - walidacyjny - treningowy - testowy
#ustawiamy dane pokolei wed³ug kryteriów najpierw kategorycznych, potem liczbowych
#kolejnosc sortowania: y, poutcome, education, job, marital, housing, loan, default, contact, balance, age
#previous, campaing, pdays, day, month. W 1 kolejnosci zmienne mo¿e bardziej istotne
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
#stworzenie domyœlnego drzewa klasyfikuj¹cego
model1 <- rpart(y ~., train_data, method = 'class')
windows()
rpart.plot(model1)
text(model1)



#--------------------Zadanie 5--------------------
#opis drzewa bêdzie w sprawozdaniu
#analiza drzewa
#wyniki na zbiorze ucz¹cym i testowym

prediction1 <- predict(model1, train_data, type = "class")
mean(prediction1 == train_data$y)
train_an <- train_data$y
train_an[train_an == 'no'] <- 0
train_an[train_an == 'yes'] <- 1
train_an <- as.numeric(train_an)
confusion.matrix(train_an, as.numeric(prediction1) - 1, 0.5)
#nawet na zbiorze ucz¹cym jest wiêcej false-negative'ów ni¿ yes-yes

prediction2 <- predict(model1, test_data, type = 'class')
mean(prediction2 == test_data$y)
#0.893% skutecznoœci @@> a¿ za dobrze bym powiedzia³

test_an <- test_data$y
test_an[test_an == 'no'] <- 0
test_an[test_an == 'yes'] <- 1
test_an <- as.numeric(test_an)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)
#widaæ, ¿e  jeœli chodzi o przyjêcie po¿yczki to model wiêcej nie trafi³ (false-negative) ni¿ trafi³
#model swoj¹ skutecznoœæ nabija na odmowach, których jest pe³no,
#ale nie jest przydatny do przewidywania wziêcia po¿yczki

#domyœlne drzewo spisuje siê s³abo, nie ma zdolnoœci do przewidywania zgód
#spodziewamy siê poprawiæ jakoœ klasyfikacji drzewami z dobieranymi parametrami
#i z ustaleniem wiêkszych wag dla próbek o y = 'yes'



#--------------------Zadanie 6 i 7--------------------
#walidacja - analiza wyp³ywu hiperparametrów na jakoœæ klasyfikacji
#wp³yw zwiêkszonych wag

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
      
      #wp³yw TP
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
      score = score0 + 0.01*(score1 - score2) + 0.005*score1 #premia za pewnoœc TP vs FP
      
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
               control = rpart.control(cp=0.005, minsplit = 14))

prediction2 <- predict(model, test_data, type = 'class')
mean(prediction2 == test_data$y)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)

windows()
rpart.plot(model)

windows()
printcp(model)

model <- prune(model, cp=0.019)

windows()
rpart.plot(model)

prediction2 <- predict(model, test_data, type = 'class')
mean(prediction2 == test_data$y)
confusion.matrix(test_an, as.numeric(prediction2) - 1, .5)