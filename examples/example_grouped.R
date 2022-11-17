data(Boston, package="MASS")
# Haeufigkeitstabelle einer stetigen Variablen
table(Boston$crim)
# Klassierung
ccrim <- cut(Boston$crim, c(0,0.1,0.2,0.3,0.4,0.5,1,2,10,100))
# Haeufigkeitstabelle klassierter Variablen
table(ccrim)
