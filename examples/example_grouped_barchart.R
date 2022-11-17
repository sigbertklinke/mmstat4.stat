# Studierende nach FS (HU Berlin, WS 13/14)
# https://www.hu-berlin.de/de/studium/statistik
bsc_bwl <- c(171, 9, 174, 16, 115, 30, 90, 10, 46, 4, 8, 3, 21)
bsc_vwl <- c(135, 2, 111, 2, 75, 24, 60, 8, 22, 4, 7, 1, 18)
#
counts <- rbind(bsc_bwl, bsc_vwl)
colnames(counts) <- c(1:12, ">12")
barplot(counts, beside=T,
        legend=c("B.Sc. BWL", "B.Sc. VWL"),
        main="Studierende im WS 2013/14",
        xlab="Fachsemester")
