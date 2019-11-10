#za³adowanie potrzebnych zale¿noœci
library(sqldf)
library(data.table)

#za³adowanie bazowych danych
basedata <- read.csv("E:/9sem/DM/zep/bank/bank.csv", row.names=NULL, sep=";", stringsAsFactors=FALSE)

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

par(mfrow = c(1,3))
plot(age_no$age, age_no$no, xlab = "Wiek",
     ylab = "Liczba odmów")
plot(age_yes$age, age_yes$yes, xlab = "Wiek", ylab = "Liczba zgód")
plot(age_y$age, age_y$liczba_prob, xlab = "Wiek", ylab = "Liczba prób")
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

par(mfrow = c(1,1))
barplot(job_y$skutecznosc, names.arg = rownames(job_y))
#zdecydowanie du¿a wiêksza skutecznoœæ dla studentów/uczniów i emerytów
#ta zmienna te¿ bêdzie istotna w modelowaniu

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

par(mfrow = c(1,1))
barplot(marital$skutecznosc, names.arg = marital$stan)
#widaæ ¿e married rzadziej decyduj¹ siê na pozyczke
#bierzemy stan cywilny do modelu jak najbardziej

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

par(mfrow = c(1,1))
barplot(education$skutecznosc, names.arg = education$wyksztalcenie)

#powiem szczerze tego siê nie spodziewa³em: 
#ludzie z wykszta³ceniem wy¿szym czêœciej zgadzali siê na po¿yczkê
#zmienna jaknajbardziej istotna przy tworzeniu modelu

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

par(mfrow = c(1,1))
barplot(default$skutecznosc, names.arg = default$bankrut)

#tu te¿ jestem zdziwiony, spodziewa³bym siê, ¿e ludzie którzy ju¿
#wczeœniej zawiedli w sp³acaniu kredytu bêd¹ siê mniej decydowaæ
#a tu jednak rozk³ada siê to bardzo identycznie
#nie wykorzystujemy poczatkowo tego wejœcia w modelu

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

par(mfrow=c(1,2))
barplot(balance_all$skutecznosc, names.arg = balance_all$balance,
        xlab = "Roczny zysk w euro", ylab = "Skutecznoœæ telemarketingu")
barplot(balance_all$liczba_prob, names.arg = balance_all$balance,
        xlab = "Roczny zysk w euro", ylab =  "Liczba prób telemarketingowych")

#prób dla ludzi z olbrzymim zyskiem 10000+ jest malo dlatego s¹dzimy, ¿e du¿a skuecznoœæ dla nich jest przypadkowa
#warto zwróciæ uwagê, ¿e jest du¿o prób dla ludzi z ma³¹ srat¹ lub zyskiem do 5 tysiêcy
#warto zauwazyæ ¿e ludzie z ma³ym d³ugiem czeœciej decyduj¹ siê na po¿yczkê ni¿ ludzie
#z minimalnym rocznym zyskiem 0-500 euro, prawdopodobnie próbuj¹ ratowaæ siê po¿yczkami
#warto zauwazyæ du¿¹ skutecznoœæ dla ludzi z zyskiem 3000 - 5000 euro rocznie
#prawdopodobnie ludzie Ci bior¹ pozyczki na np rozwój swoich dzia³anoœci gospodarczych
#lub maj¹ du¿¹ zdolnoœc kredytow¹ siê kupuj¹ sobie mieszkania/samochody
#zmienna jak najbardziej istotna i wziêta do modelu

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

par(mfrow = c(1,1))
barplot(housing$skutecznosc, names.arg = housing$hipoteka)
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
par(mfrow = c(1, 1))
barplot(housing_balance_yes$skutecznosc, 
        names.arg = paste(housing_balance_yes$housing, housing_balance_yes$balance_level, sep="-"))

#warto zwróciæ uwagê, ¿e chêtnymi klientami s¹ ludzie
#o œrednim (w tym mid-low i mid-high) poziomie bilansu rocznego
#i nieposiadaj¹cy kredytów hipotecznych
#drug¹ klas¹ chêtnych klientów s¹ klienci o niskim bilansie rocznym
#nie posiadaj¹cych hipoteki - mo¿ê w³aœnie im j¹ wcisneli?
#naturalnie weŸmiemy do modelu

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

par(mfrow = c(1,1))
barplot(loan$skutecznosc, names.arg = loan$pozyczka)
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
par(mfrow = c(1, 1))
barplot(housing_balance_yes$skutecznosc, 
        names.arg = paste(housing_balance_yes$housing, housing_balance_yes$balance_level, sep="-"))
 
#niestety nie mo¿na wyci¹gn¹æ ¿adnego wniosku na temat wyró¿nienia szczególnej
#grupy rozmówców na podstawie par loan-default