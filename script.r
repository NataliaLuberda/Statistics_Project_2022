library(ggplot2)
library(knitr)
library(tibble)
library(e1071)
library(dplyr)

#importowanie danych
data <- read.csv("C:\\Users\\zilla\\Downloads\\archive (3)\\insurance.csv")

#wyświetlanie danych
View(data)
summary(data)

#pozbywam się wartości null
data <-na.omit(data)

#dodawanie i usuwanie kolumn
data<-data.frame(data)
#usunięcie niepotrzebnych kolumn
data=subset(data,select = -c(sex,bmi,region,children,smoker))


#wyświetlenie prostych informacji o przekształconych danych
dim(data)
summary(data)

# Tworzymy kolumnę z grupami wiekowymi
data$age_group <- cut(data$age, breaks = c(18, 25, 35, 45, 55, 65, 100), labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66+"))


#Boxplot
ggplot(data, aes(x=age, y=charges, group=age)) +
  geom_boxplot(fill = "green", color = "black") +
  xlab("Wiek") +
  ylab("Wielkość składki ubezpieczeniowej") +
  ggtitle("Wielkość składki ubezpieczeniowej w zależności od wieku")

# Tworzymy violinplot dla każdej grupy wiekowej
ggplot(data, aes(x=age_group, y=charges)) +
  geom_violin(fill = "green", color = "black") +
  xlab("Grupa wiekowa") +
  ylab("Wielkość składki ubezpieczeniowej") +
  ggtitle("Wielkość składki ubezpieczeniowej w zależności od grupy wiekowej")


# Tworzymy histogram dla średniej wysokości składek ubezpieczeniowych dla każdej grupy wiekowej
ggplot(data, aes(x=charges, fill=age_group)) +
  geom_histogram(color = "black", binwidth = 1000) +
  xlab("Średnia wysokość składki ubezpieczeniowej") +
  ylab("Liczba klientów") +
  ggtitle("Histogram rozkładu średniej wysokości składek ubezpieczeniowych dla poszczególnych grup wiekowych")



# Miary centralne
mean_charges <- mean(data$charges)
median_charges <- median(data$charges)
mode_charges <-  as.numeric(names(sort(table(data$charges),decreasing = T))[1])

# Miary rozproszenia
sd_charges <- sd(data$charges)
var_charges <- var(data$charges)
quantile_charges <- quantile(data$charges, probs = c(0.25, 0.5, 0.75))

# Miary kształtu
skewness_charges <- skewness(data$charges)
kurtosis_charges <- kurtosis(data$charges)


#tu obliczam sobie ogólne miary poszczególnych zmiennych
parametry <- tibble(
  Mean = mean_charges,
  Median = median_charges,
  Mode = mode_charges,
  Standard_Deviation = sd_charges,
  Variance = var_charges,
  Quantiles = quantile_charges,
  Skewness = skewness_charges,
  Kurtosis = kurtosis_charges
)


#tu obliczam sobie  miary poszczególnych zmiennych dla grup wiekowych
estimates <- data %>% 
  group_by(age_group) %>% 
  summarise(
    median = median(charges),
    q0.5 = quantile(charges, 0.5),
    sd = sd(charges),
    IQR = IQR(charges),
    min = min(charges),
    max = max(charges)
)









