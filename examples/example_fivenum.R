library("MASS")
# Minimum, lower-hinge (1. Quartil), median, 
# upper-hinge (3. Quartil), Maximum
fivenum(Boston$crim)
# Minimum, 1., 2., und 3. Quartil, Maximum
quantile(Boston$crim)