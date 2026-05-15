# Memanggil library
library(readxl)
library(psych)

# Membaca data
data <- read_excel("D:/SEMESTER 4/TEKSAM/data survei kepuasan fasilitas 32 sampel.xlsx")

# Melihat data
View(data)

# ======================================
# Uji Validitas
# ======================================

hasil <- alpha(data)

# Menampilkan nilai validitas item
hasil$item.stats

# ======================================
# Uji Reliabilitas
# ======================================

# Menampilkan nilai Cronbach Alpha
hasil$total

# =========================================
# Perhitungan Jumlah Sampel dengan Slovin
# =========================================

# Jumlah populasi
N <- 50

# Tingkat kesalahan (error)
e <- 0.11

# Menghitung jumlah sampel
n <- N / (1 + N * (e^2))

# Menampilkan hasil
cat("Jumlah sampel minimum =", round(n), "responden")

# ======================================
# Naive Estimation
# ======================================

# Jumlah responden
n <- 32

# Jumlah responden berdasarkan gender
laki_laki <- 10
perempuan <- 22

# Menghitung proporsi sampel
p_laki <- laki_laki / n
p_perempuan <- perempuan / n

# Menampilkan hasil
cat("Naive Estimation Laki-laki =", round(p_laki,3), "\n")
cat("Naive Estimation Perempuan =", round(p_perempuan,3))

# ======================================
# Weighted Estimation
# ======================================

# Populasi
N <- 50

# Jumlah populasi berdasarkan gender
pop_laki <- 18
pop_perempuan <- 32

# Proporsi populasi
prop_pop_laki <- pop_laki / N
prop_pop_perempuan <- pop_perempuan / N

# Proporsi sampel
prop_sampel_laki <- laki_laki / n
prop_sampel_perempuan <- perempuan / n

# Menghitung bobot
w_laki <- prop_pop_laki / prop_sampel_laki
w_perempuan <- prop_pop_perempuan / prop_sampel_perempuan

# Menampilkan hasil
cat("Bobot Laki-laki =", round(w_laki,3), "\n")
cat("Bobot Perempuan =", round(w_perempuan,3))

# ======================================
# Perbandingan Estimasi
# ======================================

hasil_estimasi <- data.frame(
  Gender = c("Laki-laki", "Perempuan"),
  Naive_Estimation = c(round(p_laki,3),
                       round(p_perempuan,3)),
  Weighted_Estimation = c(round(prop_pop_laki,3),
                          round(prop_pop_perempuan,3))
)

hasil_estimasi

# ======================================
# Grafik Perbandingan Estimasi
# ======================================

barplot(
  t(as.matrix(hasil_estimasi[,2:3])),
  beside = TRUE,
  names.arg = hasil_estimasi$Gender,
  col = c("skyblue", "orange"),
  main = "Perbandingan Hasil Estimasi",
  ylab = "Proporsi",
  legend.text = c("Naive", "Weighted")
)