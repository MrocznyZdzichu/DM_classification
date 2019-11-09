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

