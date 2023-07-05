
library(readr)
data_tutorial360711_2023_06_06_12_40 <- read_csv("data_tutorial360711_2023-06-06_12-40.csv")
dataset <- data_tutorial360711_2023_06_06_12_40
View(dataset)


#Aussortieren: CASE 280, 17 wegen zu hohem Alter --> Über Dienstalter

dataset1 <- dataset[-3,]
View(dataset1)
datasetbereinigt <- dataset1[-160,]
View(datasetbereinigt)

#Verteilung der UTB
utb <- c(datasetbereinigt$A014)
table(utb)
################################################################################
#Heer: 186, Luftwaffe: 99, Marine: 132

#Altersdurchschnitt
mean(datasetbereinigt$A002_01)
################################################################################
#M = 24.07 Jahre

#Geschlechterverteilung
table(datasetbereinigt$A001)
geschlecht <- c(datasetbereinigt$A001)
################################################################################
#W: 102, M: 311, D: 4

#Vordienstzeit
table(datasetbereinigt$A015)
################################################################################
#Ja: 150, Nein: 267

#UTB-Wechsler
table(datasetbereinigt$A016)
################################################################################
#Kein Wechsel: 376, Zuvor Heer: 18, Zuvor Luftwaffe: 15, Zuvor Marine: 8

#Bildungsabschluss
table(datasetbereinigt$A010)
table(datasetbereinigt$A010_09)
################################################################################
#ohne: 2, Real: 7, Fachabi: 2, Abitur: 276, Hochschulabschluss: 125, Anderer: 6

#Graphische Darstellung
#UTB
barplot(table(utb), xlab = "Uniformträgerbereiche", ylab = "absolute Häufigkeit", main = "Verteilung der UTB", col = c("lightgrey", "darkgrey", "black"), names.arg = c("Heer", "Luftwaffe", "Marine"), ylim = c(0,200), las = 1)
#Geschlechterverteilung nach UTB
verteilunggeschlecht <- data.frame(geschlecht, utb)
barplot(table(verteilunggeschlecht), ylim = c(0,150), legend.text = c("weiblich", "männlich", "divers"), xlab = "Uniformträgerbereiche", ylab = "absolute Häufigkeit", names.arg = c("Heer", "Luftwaffe", "Marine"), beside = TRUE, las = 1)


#Mittelwerte als neue Variable

#Verträglichkeit
verträglichkeit <- datasetbereinigt[,c("A101_04","A101_09","A101_14","A101_19","A101_24","A101_29","A101_34","A101_39","A101_44","A101_49","A101_54","A101_59")]
View(verträglichkeit)
verträglichkeitmittel <- rowMeans(verträglichkeit)
View(verträglichkeitmittel)
?cbind
datasetbereinigt1 <- cbind(datasetbereinigt, verträglichkeitmittel)
View(datasetbereinigt1)

#Motive
#Motiv Macht
macht <- datasetbereinigt[,c("A201_01","A201_02","A201_03","A201_04","A201_05","A201_06","A201_07","A201_08","A201_09","A201_10","A201_11","A201_12")]
View(macht)
machtmittel <- rowMeans(macht)
datasetbereinigt2 <- cbind(datasetbereinigt1, machtmittel)
View(datasetbereinigt2)

#Motiv Leistung
leistung <- datasetbereinigt[,c("A202_01","A202_02","A202_03","A202_04","A202_05","A202_06","A202_07","A202_08","A202_09","A202_10","A202_11")]
View(leistung)
leistungsmittel <- rowMeans(leistung)
datasetbereinigt3 <- cbind(datasetbereinigt2, leistungsmittel)
View(datasetbereinigt3)

#Motiv Abhängigkeit
abhängigkeit <- datasetbereinigt[,c("A203_01","A203_02","A203_03","A203_04","A203_05","A203_06","A203_07","A203_08","A203_09","A203_10","A203_11")]
View(abhängigkeit)
abhängigkeitsmittel <- rowMeans(abhängigkeit)
datasetbereinigt4 <- cbind(datasetbereinigt3, abhängigkeitsmittel)
View(datasetbereinigt4)

datasetbereinigt4[,c("SERIAL","REF","QUESTNNR","MODE","STARTED","TIME001","TIME002","TIME003","TIME004","TIME005","TIME006","TIME007","TIME_SUM","MAILSENT","LASTDATA","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI","DEG_TIME")] <- list(NULL)
data_final <- datasetbereinigt4
View(data_final)
write.csv2(data_final, file="data_final.csv")

#Regressionsanalyse

#Annahmen prüfen: Mithilfe von Residuenanalyse

#1. Linearität (Rainbow-Test): H0 = Linearität

library(lmtest)

#macht --> gegeben
raintest(machtregression)
#leistung --> nicht gegeben
raintest(leistungregression)
#abhängigkeit --> gegeben
raintest(abhängigkeitregression)
################################################################################
#Gegeben bei einem p>0.05

#2. Homoskedastizität (Breusch-Pagan-Test):

install.packages("lmtest")
library(lmtest)

#Macht: --> gegeben
plot(fitted.values(machtregression), rstandard(machtregression))
bptest(machtregression) 
#Leistung: --> nicht gegeben (aber vorsicht bei großen Stichproben Test überempfindlich)
plot(fitted.values(leistungregression), rstandard(leistungregression))
bptest(leistungregression)
#Abhängigkeit: --> gegeben
plot(fitted.values(abhängigkeitregression), rstandard(abhängigkeitregression))
bptest(abhängigkeitregression)
################################################################################
#gegeben bei einem p>0.05

#3. Normalverteilung (Shapiro-Wilk-Test):

#Macht: --> gegeben
hist(residuals(machtregression))
shapiro.test(rstandard(machtregression)) 
#Leistung --> nicht gegeben
hist(residuals(leistungregression))
shapiro.test(rstandard(leistungregression))
#Abhängigkeit --> gegeben
hist(residuals(abhängigkeitregression))
shapiro.test(rstandard(abhängigkeitregression))
################################################################################
#gegeben bei einem p>0.05

#4. Unabhängigkeit der Fehlerterme (Durbin-Watson-Test): 

library(car)

#Macht --> nicht gegeben (Problem)
durbinWatsonTest(machtregression)
#Leistung --> gegeben
durbinWatsonTest(leistungregression)
#Abhängigkeit --> gegeben
durbinWatsonTest(abhängigkeitregression)
################################################################################
#gegeben bei einem p>0.05


#Regression von Verträglichkeit auf Macht 
machtregression <- lm(data_final$machtmittel~data_final$verträglichkeitmittel, data = data_final)
summary(machtregression)
plot(data_final$verträglichkeitmittel, data_final$machtmittel, xlab = "Ausprägung Verträglichkeit", ylab = "Ausprägung Machtmotiv", ylim = c(0,7), xlim = c(0,6), las = 1)
abline(lm(data_final$machtmittel~data_final$verträglichkeitmittel, data = data_final), col = "red")
legend("bottomleft", bty="n", 
       legend = paste0 ("R² = ", format(summary(machtregression)$r.squared)))
################################################################################
#Signifikante Regressionsgewichte: intercept: p<2e-16, slope: p<2e-16
#y=5.66 - 0.55 x + e
#Negative Regression von Machtmotiv auf Verträglichkeit: je größer die Ausprägung des Machtmotivs, desto geringer die Verträglichkeit
#R2 = 0.21 --> 21% erklärte Varianz

#Regression von Verträglichkeit auf Leistung
leistungregression <- lm(data_final$leistungsmittel~data_final$verträglichkeitmittel, data = data_final)
summary(leistungregression)
plot(data_final$verträglichkeitmittel, data_final$leistungsmittel, xlab = "Ausprägung Verträglichkeit", ylab = "Ausprägung Leistungsmotiv", ylim = c(0,7), xlim = c(0,6), las = 1)
abline(lm(data_final$leistungsmittel~data_final$verträglichkeitmittel, data = data_final), col = "red")
legend("bottomleft", bty="n", 
       legend = paste0 ("R² = ", format(summary(leistungregression)$r.squared)))
################################################################################
#Nicht signifikante Regressionsgewichte: intercept: p<2e-16, slope: p=0.458
#y=4.36 + 0.05 x + e
#Keine signifikante Steigung
#R2 = 0.001 --> 0.1% erklärte Varianz

#Regression von Verträglichkeit auf Abhängigkeit
abhängigkeitregression <- lm(data_final$abhängigkeitsmittel~data_final$verträglichkeitmittel, data = data_final)
summary(abhängigkeitregression)
plot(data_final$verträglichkeitmittel, data_final$abhängigkeitsmittel, xlab = "Ausprägung Verträglichkeit", ylab = "Ausrprägung Abhängigkeitsmotiv", ylim = c(0,7), xlim = c(0,6), las = 1)
abline(lm(data_final$abhängigkeitsmittel~data_final$verträglichkeitmittel, data = data_final), col = "red")
legend("bottomleft", bty="n", 
       legend = paste0 ("R² = ", format(summary(abhängigkeitregression)$r.squared)))
################################################################################
#Signifikante Regressionsgewichte: intercept: p<2e-16, slope: p=5.92e-08
#y=2.93 + 0.32 x + e
#Positive Regression von Abhängigkeitsmotiv auf Verträglichkeit: je größer die Ausprägung des Abhängigkeitsmotivs, desto höher die Verträglichkeit
#R2 = 0.07 --> 7% erklärte Varianz



#ANOVA zum Vergleich der Motive zwischen den UTBs und der Verträglichkeit zwischen den UTBs

library(psych)
library(car)
install.packages("DescTools")
library(DescTools)

#Annahmen prüfen: 
# 1. mehr als zwei voneinander unabhängige Stichproben (hier noch Überprüfung/Kontrolle der UTB Wechsler)

library(psych)
ohnewechsler <- subset(data_final, A016 == -1)
View(ohnewechsler)
describeBy(ohnewechsler)
################################################################################
#gegeben bei Herausnehmen der UTB-Wechsler

# 2. metrisch skalierte AV
################################################################################
#gegeben

# 3. Normalverteilung der Fehlerterme innerhalb der Gruppe
################################################################################
#Siehe Test bei der Regression

# 4. Homogene Varianzen
utbfactor <- as.factor(data_final$A001)

#Macht --> gegeben
leveneTest(y=machtmittel, group=utbfactor, center = median)
#Leistung --> gegeben
leveneTest(y=leistungsmittel, group=utbfactor, center = median)
#Abhängigkeit --> nicht gegeben
leveneTest(y=verträglichkeitmittel, group=utbfactor, center = median)
################################################################################
#Trotz unterschiedlicher Gruppengrößen ist Varianzhomogenität gegeben bei Machtmotiv und Leistungsmotiv

#Mittelwerte der UTB

heer <- subset(data_final, A001 == 1)
luftwaffe <- subset(data_final, A001 == 2)
marine <- subset(data_final, A001 == 3)

#Verträglichkeit
mean(heer$verträglichkeitmittel)
mean(luftwaffe$verträglichkeitmittel)
mean(marine$verträglichkeitmittel)
################################################################################
#Heer: 3.68, Luftwaffe: 3.53, Marine: 2.71

#Macht
mean(heer$machtmittel)
mean(luftwaffe$machtmittel)
mean(marine$machtmittel)
################################################################################
#Heer: 3.48, Luftwaffe: 3.78, Marine: 4.02

#Leistung
mean(heer$leistungsmittel)
mean(luftwaffe$leistungsmittel)
mean(marine$leistungsmittel)
################################################################################
#Heer: 4.60, Luftwaffe: 4.52, Marine: 3.43

#Abhängigkeit
mean(heer$abhängigkeitsmittel)
mean(luftwaffe$abhängigkeitsmittel)
mean(marine$abhängigkeitsmittel)
################################################################################
#Heer: 4.27, Luftwaffe: 4.02, Marine: 3.59

#ANOVA Macht 
anova_macht <- aov(data_final$machtmittel ~ data_final$A001)
summary(anova_macht)
pairwise.t.test(data_final$machtmittel, data_final$A001, p.adjust = "bonferroni")
EtaSq(anova_macht)
################################################################################
#Unterschiede in der Machtmotiv-Ausprägung sind signifikant auf Faktorstufen zurückzuführen mit p=7.59e-05
#Signifikanter Unterschied nur zwischen Heer und Luftwaffe (Kann das sein, wenn bei den Gesamtmittelwerten zwischen Heer und Marine ein größerer Unterschied besteht?)
#Gesamtunterschiedlichkeit in der AV ist zu 3.7% auf die Faktorstufen zurückzuführen

#Kontrolle Wechsler
anova_macht_kontrolle <- aov(ohnewechsler$machtmittel ~ ohnewechsler$A001)
summary(anova_macht_kontrolle)
pairwise.t.test(ohnewechsler$machtmittel, ohnewechsler$A001, p.adjust = "bonferroni")
EtaSq(anova_macht_kontrolle)
################################################################################
#Unterschiede in der Machtmotiv-Ausprägung sind signifikant auf Faktorstufen zurückzuführen mit p=0.000576
#Signifikanter Unterschied nur zwischen Heer und Luftwaffe (Kann das sein, wenn bei den Gesamtmittelwerten zwischen Heer und Marine ein größerer Unterschied besteht?)
#Gesamtunterschiedlichkeit in der AV ist zu 3.1% auf die Faktorstufen zurückzuführen

#ANOVA Leistung
anova_leistung <- aov(data_final$leistungsmittel ~ data_final$A001)
summary(anova_leistung)
pairwise.t.test(data_final$leistungsmittel, data_final$A001, p.adjust = "bonferroni")
EtaSq(anova_leistung)
################################################################################
#Unterschiede in der Leistungsmotiv-Ausprägung sind nicht signifikant auf Faktorstufen zurückzuführen mit p=0.0945
#Signifikanter Unterschied zwischen Heer und Marine und Luftwaffe und Marine
#Gesamtunterschiedlichkeit in der AV ist zu 0.7% auf die Faktorstufen zurückzuführen

#Kontrolle Wechsler
anova_leistung_kontrolle <- aov(ohnewechsler$leistungsmittel ~ ohnewechsler$A001)
summary(anova_leistung_kontrolle)
pairwise.t.test(ohnewechsler$leistungsmittel, ohnewechsler$A001, p.adjust = "bonferroni")
EtaSq(anova_leistung_kontrolle)
################################################################################
#Unterschiede in der Leistungsmotiv-Ausprägung sind signifikant auf Faktorstufen zurückzuführen mit p=0.0455
#Signifikanter Unterschied zwischen Heer und Marine und Luftwaffe und Marine
#Gesamtunterschiedlichkeit in der AV ist zu 1.1% auf die Faktorstufen zurückzuführen

#ANOVA Abhängigkeit
anova_abhängigkeit <- aov(data_final$abhängigkeitsmittel ~ data_final$A001)
summary(anova_abhängigkeit)
pairwise.t.test(data_final$abhängigkeitsmittel, data_final$A001, p.adjust = "bonferroni")
EtaSq(anova_abhängigkeit)
################################################################################
#Unterschiede in der Abhängigkeitsmotiv-Ausprägung sind signifikant auf Faktorstufen zurückzuführen mit p=0.000652
#Signifikanter Unterschied zwischen Heer und Luftwaffe (Kann das sein, wenn bei den Gesamtmittelwerten zwischen Heer und Marine ein größerer Unterschied besteht?)
#Gesamtunterschiedlichkeit in der AV ist zu 2.8% auf die Faktorstufen zurückzuführen

#Kontrolle Wechsler
anova_abhängigkeit_kontrolle <- aov(ohnewechsler$abhängigkeitsmittel ~ ohnewechsler$A001)
summary(anova_abhängigkeit_kontrolle)
pairwise.t.test(ohnewechsler$abhängigkeitsmittel, ohnewechsler$A001, p.adjust = "bonferroni")
EtaSq(anova_abhängigkeit_kontrolle)
################################################################################
#Unterschiede in der Abhängigkeitsmotiv-Ausprägung sind signifikant auf Faktorstufen zurückzuführen mit p=0.00521
#Signifikanter Unterschied zwischen Heer und Luftwaffe (Kann das sein, wenn bei den Gesamtmittelwerten zwischen Heer und Marine ein größerer Unterschied besteht?)
#Gesamtunterschiedlichkeit in der AV ist zu 2.1% auf die Faktorstufen zurückzuführen

#ANOVA Verträglichkeit
anova_verträglichkeit <- aov(data_final$verträglichkeitmittel ~ data_final$A001)
summary(anova_verträglichkeit)
pairwise.t.test(data_final$verträglichkeitmittel, data_final$A001, p.adjust = "bonferroni")
EtaSq(anova_verträglichkeit)
#Da auch hier ein signifikanter Unterschied zwischen den Faktorstufen besteht, kann als explorative Auswertung eine ANCOVA durchgeführt werden

#ANCOVA 

#macht 
ancova_macht <- aov(data_final$machtmittel ~ data_final$A001 + data_final$verträglichkeitmittel, data = data_final)
Anova(ancova_macht, type=3)

#leistung
ancova_leistung <- aov(data_final$leistungsmittel ~ data_final$A001 + data_final$verträglichkeitmittel, data = data_final)
Anova(ancova_leistung, type=3)

#abhängigkeit 
ancova_abhängigkeit <- aov(data_final$abhängigkeitsmittel ~ data_final$A001 + data_final$verträglichkeitmittel, data = data_final)
Anova(ancova_abhängigkeit, type=3)

