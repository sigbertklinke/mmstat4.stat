library("MASS")
tab <- table(Boston$chas, Boston$rad)
tab
# Gestapeltes Balkendiagramm
barplot(tab, legend=TRUE)
# Gruppiertes Balkendiagramm
barplot(tab, beside=TRUE, legend=TRUE)
# Bedingte Verteilung bzgl. CHAS
ctab <- prop.table(tab, 1)
ctab
barplot(ctab, beside=TRUE, legend=TRUE)
# Bedingte Verteilung bzgl. RAD
ctab <- prop.table(tab, 2)
ctab
barplot(ctab, legend=TRUE)