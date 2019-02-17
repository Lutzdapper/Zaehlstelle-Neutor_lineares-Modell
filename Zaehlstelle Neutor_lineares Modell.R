# =====================================================================================
#                     Anwendung eines linearen Regressionsmodells zur
#                     Prädiktion des Verkehrsaufkommens an der Radverkehr-Zählstelle
#                     Neutor der Stadt Münster
# ====================================================================================

#Entfernen der bislang definierten Objekte aus der Arbeitsumgebung. 
#Falls Objekte bereits definiert waren, stehen diese nun nicht mehr zur Verfuegung.
rm(list = ls())

# Installieren des Paketes pheatmap
install.packages("pheatmap")

# Laden der benötigten Pakete
library(dplyr)
library(lubridate)
library(readr)
library(pheatmap)

# Einlesen der Daten
df <- read_csv("/home/..../DataScience/Daten/zaehlstelle_neutor_2017.csv")


################ Aufgabe 1 #####################

# Berechnung von Monat, Wochentag, Tag und Stunde
# Speicherung der Variablen als ungeordnete factor im Data Frame "df".
# Der Tag wird nicht als ungeordneter factor gespeichert.

df$Monat <- factor(month(df$Datum), ordered = FALSE)

df$Wochentag <- factor(wday(df$Datum, label = TRUE, 
                            abbr = FALSE, locale = "de_DE.UTF-8"), ordered = FALSE)

df$Stunde <- factor(hour(df$Datum), ordered = FALSE)

df$Tag <- yday(df$Datum)

# In diesem Fall kann die Nullhypothese klar abgelehnt werden,
# d.h. es gibt einen signifikanten Zusammenhang.
# Das bedeutet, dass sich das Verkehrsaufkommen je nach 
# Uhrzeit an verschiedenen Wochentagen unterscheidet.

# Laden der Feiertagsdaten
load("/home/.../DataScience/Daten/feiertage.RData")

# Zusammenführung des bestehenden Data Frames und den Feiertagsdaten
# Join geschieht über die Variable "Tag", die in beiden Data sets vorhanden ist.
df <- df %>% left_join(feiertage, by = "Tag")

# Erstelle Variable mit Typ des Tages
df$Tag_Typ <- ifelse(!is.na(df$Feiertag), "Feiertag", 
                     ifelse(df$Wochentag %in% c("Samstag", "Sonntag"), 
                            "Wochenende", "Werktag"))

# Erstelle neue Variable mit Feiertag Ja oder Nein
df$Feiertag_bool <- ifelse(!is.na(df$Feiertag), "Ja", 
                           "Nein")

################ Aufgabe 2 #####################
# Anzeigen der Summary des Datensatzes
summary(df)

# Berechnung geeigneter Kennzahlen
## Mittelwert der Anzahl Radfahrer
mean(df$Anzahl)

### Der Mittelwert der Anzahl Radfahrer wird gebildet, da er als Erwartungswert
### in den Kontingenztafel verwendet wird. Diese Kennzahl wird vorwiegend für 
### den Bericht verwendet.

## Mittelwert der Radfahrer je Stunde
summary_Stunde <- df %>% group_by(Stunde) %>%
  summarise(Anzahl = mean(Anzahl),
            Anzahl_stadteinwaerts = mean(Anzahl_stadteinwaerts),
            Anzahl_stadtauswaerts = mean(Anzahl_stadtauswaerts))
summary_Stunde

## Anzahl der Gesamtradfahrer im Jahr 2017
Gesamtradfahrer <- sum(df$Anzahl)

## Radfahrer im Jahr einwaerts
Gesamtradfahrer_einwaerts <- sum(df$Anzahl_stadteinwaerts)

## Radfahrer im Jahr auswaerts
Gesamtradfahrer_auswaerts <- sum(df$Anzahl_stadtauswaerts)

# Einflüsse der Variablen
## Korrelationsmatrix
cor(df[c("Anzahl",
         "Temperatur",
         "Windstaerke")],
    use = "pairwise")

## Einfluss Temperatur auf Anzahl
cor(df[c('Anzahl', 
         'Temperatur')], 
    use = 'pairwise')

### Die Korrelation zwischen Temperatur und der Anzahl
### Radfahrern ist mit 0.36 relativ hoch. 


## Visualisierung des Einflusses der Temperatur auf die Anzahl
png(filename = "Einfluss der Temperatur auf die Anzahl Radfahrer.png", width = 1000, height = 369)
plot( df$Temperatur,
      df$Anzahl,
      main="Einfluss der Temperatur auf die Anzahl Radfahrer",
      xlab="Temperatur in °Celsius",
      ylab="Anzahl Radfahrer",
      pch=19)
dev.off()

png(filename = "Histogramm der Temperatur.png", width = 1000, height = 369)
hist(df$Temperatur,
     xlab = "Temperatur",
     ylab = "Häufigkeit",
     main = "Histogramm der Temperatur" )
dev.off()
## Einfluss Windstaerke auf Anzahl
cor(df[c('Anzahl', 
         'Windstaerke')],
    use = 'pairwise')

### Wind scheint den Radfahrern wenig auszumachen,
### hier ist die Korrelation nur ca. 0.11. 

## Visualisierung des Einflusses der Windstaerke auf die Anzahl
png(filename = "Einfluss der Windstaerke auf die Anzahl Radfahrer.png", width = 1000, height = 369)
plot( df$Windstaerke,
      df$Anzahl, 
      main="Einfluss der Windstaerke auf die Anzahl Radfahrer", 
      xlab="Windstaerke in km/h",
      ylab="Anzahl Radfahrer",
      pch=19)
dev.off()

png(filename = "Histogramm der Windstaerke.png", width = 1000, height = 369)
hist(df$Windstaerke,
     xlab = "Windstaerke",
     ylab = "Häufigkeit",
     main = "Histogramm der Windstaerke" )
dev.off()

## Einfluss Stunde auf Anzahl
ktafel_StundeAnzahl <- tapply(df$Anzahl, 
                              INDEX = df$Stunde,
                              mean)
ktafel_StundeAnzahl
chisq.test(ktafel_StundeAnzahl)

### In diesem Fall kann die Nullhypothese klar abgelehnt werden,
### d.h. es gibt einen signifikanten Zusammenhang.
### Das bedeutet, dass sich das Verkehrsaufkommen je nach 
### Uhrzeit unterscheidet.

## Einfluss Wochentag auf Anzahl
ktafel_WochentagAnzahl <- tapply(df$Anzahl, 
                                 INDEX = df$Wochentag,
                                 mean)
ktafel_WochentagAnzahl
write.csv2(ktafel_WochentagAnzahl, file = "ktafel_WochentagAnzahl.csv")
chisq.test(ktafel_WochentagAnzahl)

### In diesem Fall kann die Nullhypothese klar abgelehnt werden,
### d.h. es gibt einen signifikanten Zusammenhang.
### Das bedeutet, dass sich das Verkehrsaufkommen je nach 
### Wochentag unterscheidet.

## Einfluss Feiertag auf Anzahl
ktafel_FeiertagboolAnzahl <- tapply(df$Anzahl, 
                                    INDEX = df$Feiertag_bool,
                                    mean)
ktafel_FeiertagboolAnzahl
chisq.test(ktafel_FeiertagboolAnzahl)

### In diesem Fall kann die Nullhypothese klar abgelehnt werden,
### d.h. es gibt einen signifikanten Zusammenhang.
### Das bedeutet, dass sich das Verkehrsaufkommen je nach 
### Feiertag(Ja/Nein) unterscheidet.

## Einfluss Monat auf Anzahl
ktafel_MonatAnzahl <- tapply(df$Anzahl, 
                             INDEX = df$Monat,
                             mean)
ktafel_MonatAnzahl
chisq.test(ktafel_MonatAnzahl)

### In diesem Fall kann die Nullhypothese klar abgelehnt werden,
### d.h. es gibt einen signifikanten Zusammenhang.
### Das bedeutet, dass sich das Verkehrsaufkommen je nach 
### Monat unterscheidet.

## Einfluss Wetter auf Anzahl
ktafel_WetterAnzahl <- tapply(df$Anzahl, 
                              INDEX = df$Wetter,
                              mean)
ktafel_WetterAnzahl 
chisq.test(ktafel_WetterAnzahl)

### In diesem Fall kann die Nullhypothese klar abgelehnt werden,
### d.h. es gibt einen signifikanten Zusammenhang.
### Das bedeutet, dass sich das Verkehrsaufkommen je nach 
### Wetter unterscheidet.

# Visualisierung der Variablen
## Histogramm der Gesamtanzahl Radfahrer
png(filename = "Verteilung der Häufigkeite.png", width = 1000, height = 369)
hist(df$Anzahl,
     main = "Verteilung der Häufigkeiten",
     ylab = "Häufigkeit",
     xlab = "Anzahl Radfahrer")
dev.off()

## Durchschnittliche Fahrradfahrer an Feiertagen und Nicht-Feiertagen nach Monaten
png(filename = "Durchschnittliche Anzahl Radfahrer pro Stunde an FeiertagenJaNein1.png", width = 1000, height = 369)
barplot(tapply(df$Anzahl, INDEX = list(df$Feiertag_bool, 'Monat' = df$Monat), mean),
        main = "Durchschnittliche Anzahl Radfahrer pro Stunde an Feiertagen(Ja/Nein)",
        xlab = "Monat", 
        ylab = "Anzahl Radfahrer",
        col = c("gray10","gray60"),
        beside = TRUE,
        legend = c("Feiertag", "kein Feiertag"),
        ylim = c(0,1200),
        args.legend=list(x = 40, y =1200))
dev.off()

## Saeulendiagramm nach Feiertag(Ja/Nein) und Anzahl
png(filename = "Durchschnittliche Anzahl Radfahrer pro Stunde an Feiertagen JaNein2.png", width = 1000, height = 369)
barplot(tapply(df$Anzahl, INDEX = df$Feiertag_bool, mean),
        main = "Durchschnittliche Anzahl Radfahrer pro Stunde an Feiertagen(Ja/Nein)",
        xlab = "Feiertag(Ja/Nein)", 
        ylab = "Anzahl Radfahrer",
        col = c("gray10","gray60"))
dev.off()

## Saeulendiagramm nach Monaten und Anzahl
png(filename = "Anzahl Radfahrer gesamt je Monat.png", width = 1000, height = 369)
barplot(tapply(df$Anzahl, INDEX = df$Monat, sum), 
        main = "Anzahl Radfahrer gesamt je Monat",
        xlab = "Monat", 
        ylab = "Anzahl Radfahrer")
dev.off()

## Saeulendiagramm nach Wochentagen und Anzahl
png(filename = "Durchschnittliche Anzahl Radfahrer pro Stunde nach Wochentagen.png", width = 1000, height = 369)
barplot(tapply(df$Anzahl, INDEX = df$Wochentag, mean), 
        main = "Durchschnittliche Anzahl Radfahrer pro Stunde nach Wochentagen",
        xlab = "Wochentag", 
        ylab = "Anzahl Radfahrer")
dev.off()

## Verlauf der durchschnittlichen Anzahl der Radfahrer je Stunde
png(filename = "Durchschnittliche Anzahl Radfahrer pro Stunde.png", width = 1000, height = 369)
plot(tapply(df$Anzahl, INDEX = df$Stunde, mean), type = "l", 
     main = "Durchschnittliche Anzahl Radfahrer pro Stunde",
     xlab = "Stunde", 
     ylab = "Anzahl Radfahrer")
dev.off()

## Verlauf der Anzahl Radfahrer im Jahr
png(filename = "Verlauf der Anzahl Radfahrer im Jahr.png", width = 1000, height = 369)
plot(df$Datum, df$Anzahl, type = "l",
     main = "Verlauf der Anzahl Radfahrer im Jahr",
     xlab = "Monat",
     ylab = "Anzahl Radfahrer")
dev.off()

## Verlauf der Anzahl Radfahrer im Monat Dezember
png(filename = "Durchschnittliche Anzahl Radfahrer pro Tag im Dezember1.png", width = 1000, height = 369)
plot(tapply(df$Anzahl[df$Monat == 12], INDEX = df$Tag[df$Monat == 12], sum), type = "l", 
     main = "Durchschnittliche Anzahl Radfahrer pro Tag im Dezember",
     xlab = "Tag", 
     ylab = "Anzahl Radfahrer")
dev.off()

png(filename = "Durchschnittliche Anzahl Radfahrer pro Tag im Dezember2.png", width = 1000, height = 369)
barplot(tapply(df$Anzahl[df$Monat == 12], INDEX = df$Tag[df$Monat == 12], sum), 
        main = "Durchschnittliche Anzahl Radfahrer pro Tag im Dezember",
        xlab = "Tag", 
        ylab = "Anzahl Radfahrer")
dev.off()
## Boxplot Anzahl
png(filename = "Verteilung der Anzahl Radfahrer.png", width = 1000, height = 369)
boxplot(df$Anzahl,data=df,
        main="Verteilung der Anzahl Radfahrer", 
        xlab="",
        ylab="Anzahl Radfahrer")
dev.off()

## Boxplot Anzahl stadteinwaerts vs. stadtauswaerts
png(filename = "Verteilung der Anzahl Radfahrer stadteinwaerts und - auswaerts.png", width = 1000, height = 369)
boxplot(df$Anzahl_stadteinwaerts, df$Anzahl_stadtauswaerts,data=df,
        main="Verteilung der Anzahl Radfahrer stadteinwaerts und - auswaerts",
        ylab="Anzahl Radfahrer")
dev.off()

## Boxplot Anzahl nach Monat
png(filename = "Anzahl Radfahrer im Monatsvergleich.png", width = 1000, height = 369)
boxplot(df$Anzahl ~ df$Monat, data=df, 
        main="Anzahl Radfahrer im Monatsvergleich", 
        xlab="Monat", 
        ylab="Anzahl Radfahrer")
dev.off()

## Boxplot Anzahl nach Wochentag
png(filename = "Anzahl Radfahrer im Tagesvergleich.png", width = 1000, height = 369)
boxplot(df$Anzahl ~ df$Wochentag,data=df,
        main="Anzahl Radfahrer im Tagesvergleich", 
        xlab="Wochentag",
        ylab="Anzahl Radfahrer")
dev.off()

## Boxplot Anzahl nach Stunden
png(filename = "Anzahl Radfahrer im Stundenvergleich.png", width = 1000, height = 369)
boxplot(df$Anzahl ~ df$Stunde,data=df,
        main="Anzahl Radfahrer im Stundenvergleich", 
        xlab="Uhrzeit",
        ylab="Anzahl Radfahrer")
dev.off()

## Boxplot Anzahl nach Feiertagbool
png(filename = "Anzahl Radfahrer verglichen nach Feiertag(Ja/Nein).png", width = 1000, height = 369)
boxplot(df$Anzahl ~ df$Feiertag_bool,data=df,
        main="Anzahl Radfahrer verglichen nach Feiertag(Ja/Nein)", 
        xlab="Feiertag(Ja/Nein)",
        ylab="Anzahl Radfahrer")
dev.off()

## Heatmap Stunden und Wochentage anhand der Anzahl inkl. Chi-Quadrat-Test
ktafel <- tapply(df$Anzahl,
                 INDEX = list(Wochentag = df$Wochentag,
                              Stunde = df$Stunde),
                 mean)

chisq.test(ktafel)
png(filename = "Einfluss der Wochentage und Stunden auf die Anzahl Radfahrer.png", width = 1000, height = 369)
pheatmap(ktafel,
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         border_col = NA,
         main = "Einfluss der Wochentage und Stunden auf die Anzahl Radfahrer")
dev.off()


### In diesem Fall kann die Nullhypothese klar abgelehnt werden,
### d.h. es gibt einen signifikanten Zusammenhang.
### Das bedeutet, dass sich das Verkehrsaufkommen je nach 
### Uhrzeit und Wochentag unterscheidet.

################ Aufgabe 3 #####################
# Aufstellen des linearen Modells
# Wechselwirkungseffekt wird berücksichtigt (Wochentag*Stunde)
Regressionsmodell <- lm(log(Anzahl) ~ Wochentag*Stunde + Feiertag_bool + Temperatur + Windstaerke + Wetter + Monat,
                        data = df)

# Varianzanalyse zur Untersuchung der Signifikanz der unabhängigen Variablen zur Erklärung
# der Zielvariablen.
anova(Regressionsmodell)

### Alle Merkmale haben einen signifikanten Einfluss auf die Zielvariable Einfluss.

# Ausgeben der Summary des Regressionsmodell zur Kontrolle des Gütemaßes anhand des
# korrigierten R-Quadrats (Adjusted R-squared)
summary(Regressionsmodell)

### Das korrigierte R-Quadrat hat einen Wert von 0,9156. Dies bedeutet, dass 
### das Modell die Varianz der Zielvariable zu 91,56 % erklärt.
### Auch hier ist die Signifikanz durch einen P-Wert < 2.2e-16 gegeben.

# ausgeben der Regressionskoeffizienten zur Weiterverarbeitung in Excel
write.table(Regressionsmodell$coefficients, "Koeffizientenmatrix2.csv")

################ Aufgabe 4 #####################
# Erstellen eines neuen Data Frames mit den 3 Szenarien, auf die das Modell
# angewendet werden soll.
# Die gleichen Variablennamen wie im Modell werden verwendet.
# Heiligabend ist kein gesetzlicher Feiertag und somit auch als kein
# Feiertag gekennzeichnet.
df_new <- data.frame(Datum = rep(ymd_hms("2018-12-24 00:00:00") + (0:23)*60*60, 3),
                     Temperatur = rep(c(-5,0,15), each = 24),
                     Windstaerke = rep(c(10,25,0), each = 24),
                     Wetter = rep(c("Regen","Schneeregen","Klarer Himmel"), each = 24),
                     Feiertag_bool = rep("Nein", 72),
                     Szenario = rep(1:3, each = 24))

# Erstellen neuer Variablen, wie bereits im Trainingsdatensatz.
# Monat, Wochentag und Stunde werden als ungeordnete factor erstellt.
# Der Tag wird nicht als ungeordneter factor gespeichert.
df_new$Monat <- factor(month(df_new$Datum),
                       ordered = FALSE)

df_new$Wochentag <- factor(wday(df_new$Datum, label = TRUE, 
                                abbr = FALSE, locale = "de_DE.UTF-8"), ordered = FALSE)

df_new$Stunde <- factor(hour(df_new$Datum), ordered = FALSE)

df_new$Tag <- yday(df_new$Datum)

# Anwendung des Regressionsmodells auf den soeben erstellten Data Frame und somit
# auf die Szenarien.
# Speichern der Vorhersage in einer Variablen.
Vorhersage <- predict(Regressionsmodell, newdata = df_new)

# Auflösung des Logarithmus durch exp() und speichern des Ergebnisses in der
# neuen Variable des Datensatzes df_new "pred".
df_new$pred <- exp(Vorhersage)

# Darstellung der Ergebnisse des Szenario 1 in einem lineplot.
png(filename = "Ergebnisse der Simulation der Szenarien.png", width = 1000, height = 369)
plot(df_new$Datum[df_new$Szenario == 1],
     df_new$pred[df_new$Szenario == 1], 
     type = "l", 
     lty = 1, 
     ylim = c(0,1200),
     xlab = "Uhrzeit", 
     ylab = "Anzahl Radfahrer",
     col = 1,
     main = "Ergebnisse der Simulation der Szenarien")

# Hinzufügen des Szenario 2 zum bestehenden lineplot.
lines(df_new$Datum[df_new$Szenario == 2],
      df_new$pred[df_new$Szenario == 2], 
      type = "l",
      lty = 2,
      col = 2)

# Hinzufügen des Szenario 3 zum bestehenden lineplot.
lines(df_new$Datum[df_new$Szenario == 3],
      df_new$pred[df_new$Szenario == 3],
      type = "l",
      lty = 3,
      col = 3)


# Hinzufügen einer Legende zum bestehenden lineplot.
legend("topleft",
       legend = c("Szenario 1", "Szenario 2", "Szenario 3"),
       lty = 1:3,
       col = 1:3,
       cex = 0.6)
dev.off()

# Erstellung eines lineplot des Verlaufs der Anzahl Radfahrer am 24.12.2017 

png(filename = "Anzahl Radfahrer am 24.12.2017.png", width = 1000, height = 369)
plot(df$Datum[df$Tag == 358], df$Anzahl[df$Tag == 358],
     type = "l", 
     xlab = "Uhrzeit",
     ylab = "Anzahl Radfahrer",
     lty = 4, col = 4,
     main = "Anzahl Radfahrer am 24.12.2017")
dev.off()