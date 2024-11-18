# uts_analisis_data
UTS ANALISIS DATA
#Mila Nurul Hikmah
#22106010012
#UTS ANDAT

#read data
getwd()
setwd("C:/Users/ASUS/OneDrive/Documents/semester 5/prak andat")

library(readr)
kepuasan = read.csv("Customer-survey-data.csv", sep =";", header = TRUE)
head(kepuasan, 15)
tail(kepuasan, 15)

#check the packaging
nrow(kepuasan)
ncol(kepuasan)
str(kepuasan)

#look at the top and the bottom data
head(kepuasan[, c(2, 3)])
tail(kepuasan[, c(2, 3)])

#always checking "n" s
head(table(kepuasan$satisfied.quality..food))
table(kepuasan$satisfied.quality..food)

#Menyaring kepuasan dengan satisfied quality food >= 4
result <- filter(kepuasan, satisfied.quality..food >= 4)%>%
  select(satisfied.overall.delivery)
print(head(result,10))

#Menyaring kepuasan dengan satisfied overal deliveri < 3
result2 <- filter(kepuasan, satisfied.overall.delivery < 3)%>%
  select(satisfied.quality..food)
print(head(result2,10))

select(kepuasan, satisfied.overall.delivery) %>% unique %>% nrow
unique(na.omit(kepuasan$satisfied.overall.delivery))

#Validate With at Least One External Data Source
summary(kepuasan$satisfied.overall.delivery, na.rm = TRUE)
quantile(kepuasan$satisfied.overall.delivery, seq(0, 1, 0.1), 
         na.rm = TRUE)

#plot
# Pastikan ggplot2 sudah diinstal dan dimuat
library(ggplot2)
ggplot(kepuasan, aes(x =satisfied.quality..food , 
                     y = satisfied.overall.delivery)) 
                    + geom_boxplot()


#try the Easy Solution First
kepuasan$seluruh <- ifelse(kepuasan$satisfied.overall.delivery >= 4, "Puas", "tidak puas")
kepuasan$seluruh


kepuasan %>%
group_by(seluruh) %>%
  summarize(
    mean_keseluruhan = mean(satisfied.overall.delivery, na.rm =TRUE),
    median_keseluruhan = median(satisfied.overall.delivery, na.rm = TRUE),
    jumlah = n()
  )



#Challenge Your Solution
filter(kepuasan, satisfied.quality..food  < 4 ) %>%
  summarize(
      mean_keseluruhan = mean(satisfied.overall.delivery, na.rm =TRUE),
      median_keseluruhan = median(satisfied.overall.delivery, na.rm = TRUE),
      jumlah = n()
    )

filter(kepuasan, satisfied.quality..food >= 4 ) %>%
  summarize(
    mean_keseluruhan = mean(satisfied.overall.delivery, na.rm =TRUE),
    median_keseluruhan = median(satisfied.overall.delivery, na.rm = TRUE),
    jumlah = n()
  )


library(ggplot2)
library(dplyr)

# Misalkan data sudah tersedia dalam variabel 'kepuasan'
kepuasan %>%
  mutate(food_quality_group = ifelse(satisfied.quality..food < 4, "Food Quality < 4", "Food Quality >= 4")) %>%
  ggplot(aes(x = food_quality_group, y = satisfied.overall.delivery, fill = food_quality_group)) +
  geom_boxplot() +
  labs(title = "Perbandingan Kepuasan Pengiriman Berdasarkan Kualitas Makanan",
       x = "Kelompok Kualitas Makanan",
       y = "Kepuasan Pengiriman") +
  theme_minimal() +
  scale_fill_manual(values = c("Food Quality < 4" = "red", "Food Quality >= 4" = "green"))



###################### ANALISIS DATA NOMER 3 UTS
#Mila Nurul Hikmah (22106010012)

getwd()
setwd("C:/Users/ASUS/OneDrive/Documents/semester 5/prak andat")

library(readr)
kepuasan = read.csv("Customer-survey-data.csv", sep =";", header = TRUE)
kepuasan
head(kepuasan)

#Menghitung mean dan standar deviation untuk variabel kualitas.makanan
mean_kualitas <- mean(kepuasan$satisfied.quality..food, na.rm = TRUE)
mean_kualitas
sd_kualitas <- sd(kepuasan$satisfied.quality..food, na.rm = TRUE)
sd_kualitas

#Membuat kurva distribusi normal
curve(dnorm(x, mean=mean_kualitas, sd=sd_kualitas),
      from = min(kepuasan$satisfied.quality..food, na.rm = TRUE),
      to = max(kepuasan$satisfied.quality..food, na.rm = TRUE),
      xlab = " quality of the food",
      ylab = "satisfied overall delivery ", 
      main = "Normal Model untuk Kualitas Makanan",
      col = "blue",
      lwd = 2)

#Comparing Model Expectations to Reality
#histogram variabel dependen (overall delivery)
# Histogram variabel dependen (kualitas makanan)
# Histogram berdistribusi normal
hist(
  kepuasan$satisfied.overall.delivery, 
  probability = TRUE,  # Mengubah sumbu y ke probabilitas
  main = "Histogram Tingkat Kepuasan Pengalaman Pengiriman Secara Keseluruhan", 
  xlab = "Tingkat Kepuasan Secara Keseluruhan", 
  col = "lightblue", 
  border = "black"
)

# Tambahkan kurva distribusi normal
curve(
  dnorm(x, 
        mean = mean(kepuasan$satisfied.overall.delivery, na.rm = TRUE), 
        sd = sd(kepuasan$satisfied.overall.delivery, na.rm = TRUE)),
  add = TRUE, 
  col = "blue", 
  lwd = 2
)

# Tambahkan garis vertikal pada rata-rata
abline(
  v = mean(kepuasan$satisfied.overall.delivery, na.rm = TRUE), 
  col = "red", 
  lwd = 2
)


#REACTING TO DATA
y = kepuasan$satisfied.overall.delivery
x = kepuasan$satisfied.quality..food
#Model Regression
model = lm(y~x, data=kepuasan) ; model
summary(model)
