library("MASS")
# Spearmansche Rangkorrelation
cor(Boston$crim, Boston$tax, method="spearman")
# Kendalls Rangkorrelation
cor(Boston$crim, Boston$tax, method="kendall")