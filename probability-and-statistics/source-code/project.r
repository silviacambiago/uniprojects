# Lettura dei dati dal file di testo
revenues <- read.csv("/Users/silviacambiago/Desktop/R/revenues.csv", header = TRUE, sep = "\t")

# Assegna i nomi delle colonne corretti
column7 <- c(21, 21, 14, 33, 31, 34, 31, 22, 33, 31, 28, 32, 25, 41, 38, 31, 38, 54, 55, 68, 57, 70, 61, 69, 59, 56, 51, 44, 55, 56, 56, 61, 51, 76, 53, 63, 38, 46, 39, 38, 43, 29, 42, 30, 29, 30, 35, 28, 36, 44, 52, 31)
column8 <- c(57, 55, 69, 56, 58, 40, 62, 39, 54, 52, 61, 76, 55, 87, 85, 64, 95, 105, 117, 123, 118, 126, 123, 112, 125, 123, 113, 101, 107, 119, 130, 116, 116, 121, 103, 123, 107, 96, 89, 96, 95, 84, 97, 75, 62, 55, 61, 69, 79, 72, 93, 59)

# Test di ipotesi
result <- t.test(column7, column8, paired = TRUE)
result2 <- t.test(column7, column8, paired = TRUE, alternative = "two.sided", conf.level = 0.01)

data <- data.frame(column7, column8)

boxplot(data, names = c("Componenti", "Componenti con assistenza"), ylab = "Vendite", col = c("#81F9CE", "#3A8283"), main = "Confronto vendite per l'anno 2022")

par(mfrow = c(2, 1))  # Set up a 2x1 layout for the plots
hist(column7, main = "Istogramma componenti", xlab = "Valori", ylab = "Frequenza")
hist(column8, main = "Istogramma componenti con assistenza", xlab = "Valori", ylab = "Frequenza")

var(column7)
var(column8)

sd(column7)
sd(column8)

# Stampa
print(result)
print(result2)





