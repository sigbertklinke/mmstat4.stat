library("mmstat4")
#
data(data13211)
# Zeile   1-156 Deutschland
# Zeile 157-312 Alte Bundesländer
# Zeile 313-468 Neue Bundesländer
ger <- data13211[1:156,]
#
ts <- ts(ger$Arbeitslose/1e6, start=c(2005,1), frequency=12)
plot(ts, main="Arbeitslose in Deutschland (in Mio)", xlab="")
#
ts <- ts(ger$"Gemeldete Stellen"/1e6, start=c(2005,1),
         frequency=12)
plot(ts, main="Gemeldete Arbeitsstellen (in Mio)", xlab="")
