data(Boston, package="MASS")
# Haeufigkeitstabelle (absolut & relativ)
table(Boston$rad)
prop.table(table(Boston$rad))
# Haeufigkeitsttabellen fuer stetige Daten
table(Boston$crim)
# Abhilfe kann Klassierung leisten:
table(cut(Boston$crim, c(0,0.1,0.2,0.3,0.4,0.5,1,2,10,100)))
