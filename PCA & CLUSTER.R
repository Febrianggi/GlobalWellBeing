# =============================================================================
# ANALISIS MULTIVARIAT: PCA & K-MEANS CLUSTERING
# Data: World Happiness Report 2026 – Figure 2.1 (147 Negara, Tahun 2025)
# Variabel: Log GDP per capita, Social Support, Healthy Life Expectancy,
#            Freedom to Make Life Choices, Generosity, Perceptions of Corruption
# =============================================================================

# -----------------------------------------------------------------------------
# TAHAP 1: PERSIAPAN DATA
# -----------------------------------------------------------------------------

# Load semua library yang dibutuhkan
library(tidyverse)       # Manipulasi data & visualisasi (ggplot2, dplyr, dll.)
library(factoextra)      # Visualisasi PCA & Clustering (scree plot, biplot, fviz_*)
library(FactoMineR)      # Analisis PCA dengan fungsi PCA()
library(psych)           # Uji KMO dan Bartlett's Test of Sphericity
library(cluster)         # Analisis silhouette untuk penentuan k optimal

# Set seed agar hasil konsisten setiap kali dijalankan ulang
set.seed(42)

# -----------------------------------------------------------------------------
# 1.1 Load dan Bersihkan Data
# -----------------------------------------------------------------------------

# Baca file CSV – dialog pemilihan file akan muncul otomatis
# Pastikan file CSV menggunakan separator ";" dan desimal ","
df_raw <- read.csv(file.choose(), header = TRUE, dec = ",", sep = ";")
df_raw
df_raw$Generosity<-NULL
df_raw

# Tampilkan struktur dan 5 baris pertama untuk pengecekan awal
cat("\n=== STRUKTUR DATA AWAL ===\n")
glimpse(df_raw)
cat("\n5 Baris Pertama:\n")
print(head(df_raw, 5))

# Cek nama kolom aktual setelah dibaca R (kadang spasi jadi titik)
cat("\n=== NAMA KOLOM AKTUAL ===\n")
print(colnames(df_raw))

# Simpan nama negara sebagai identitas baris (row labels)
# Kolom ke-3 = Country name
negara <- df_raw[[3]]

# Pilih 6 variabel numerik berdasarkan POSISI kolom (4 s/d 9)
# Lebih aman daripada nama string jika readxl mengubah spasi/karakter
df_analisis <- df_raw[, 4:9]

# Pastikan nama kolom konsisten untuk analisis selanjutnya
colnames(df_analisis) <- c(
  "Log_GDP",
  "Social_support",
  "Healthy_life_exp",
  "Freedom",
  "Generosity",
  "Corruption"
)

# Simpan label panjang untuk keperluan plot
label_asli <- c(
  "Log_GDP"          = "Log GDP per capita",
  "Social_support"   = "Social support",
  "Healthy_life_exp" = "Healthy life expectancy",
  "Freedom"          = "Freedom to make life choices",
  "Generosity"       = "Generosity",
  "Corruption"       = "Perceptions of corruption"
)

# Cek dan hapus baris dengan nilai NA (2 negara memiliki NA pada 2 variabel)
cat("\n=== CEK MISSING VALUES ===\n")
print(colSums(is.na(df_analisis)))

# Simpan indeks baris yang lengkap (tidak ada NA)
baris_lengkap <- complete.cases(df_analisis)
df_bersih    <- df_analisis[baris_lengkap, ]       # Data tanpa NA (145 negara)\
df_bersih$Generosity<-NULL
df_bersih
negara_bersih <- negara[baris_lengkap]             # Label negara yang sesuai

cat(paste("\nJumlah negara setelah hapus NA:", nrow(df_bersih), "\n"))

# -----------------------------------------------------------------------------
# 1.2 Standardisasi Data
# -----------------------------------------------------------------------------

# Standardisasi menggunakan scale(): mean=0, sd=1 (z-score)
# Penting agar variabel dengan skala berbeda tidak mendominasi analisis
df_scaled <- scale(df_bersih)
df_scaled
rownames(df_scaled) <- negara_bersih   # Tempelkan nama negara ke baris

cat("\n=== STATISTIK SETELAH STANDARDISASI ===\n")
print(round(apply(df_scaled, 2, function(x) c(Mean=mean(x), SD=sd(x))), 4))


# -----------------------------------------------------------------------------
# TAHAP 1.3: MATRIKS KORELASI ANTAR VARIABEL
# -----------------------------------------------------------------------------

library(corrplot)   # Visualisasi matriks korelasi dengan corrplot()

# Hitung matriks korelasi dari data numerik asli (sebelum standardisasi)
# use="complete.obs" agar baris dengan NA diabaikan secara otomatis
korelasi <- cor(df_bersih, use = "complete.obs")

# Ganti nama baris & kolom menjadi X1-X6 agar konsisten dengan laporan
nama_xi_label <- paste0("X", 1:ncol(df_bersih))
rownames(korelasi) <- nama_xi_label
colnames(korelasi) <- nama_xi_label

# Tampilkan tabel matriks korelasi di konsol
cat("\n=== MATRIKS KORELASI ANTAR VARIABEL ===\n")
print(round(korelasi, 2))

# Visualisasi matriks korelasi dengan corrplot (upper triangle, method=color)
corrplot(
  korelasi,
  method      = "color",        # Warna sel berdasarkan nilai korelasi
  type        = "upper",        # Tampilkan hanya segitiga atas
  tl.col      = "black",        # Warna label variabel
  tl.srt      = 45,             # Rotasi label 45 derajat
  addCoef.col = "black",        # Tampilkan angka korelasi berwarna hitam
  number.cex  = 0.7,            # Ukuran angka korelasi
  title       = "Matriks Korelasi Variabel",
  mar         = c(0, 0, 1, 0)  # Margin: bawah, kiri, atas, kanan
)


# -----------------------------------------------------------------------------
# TAHAP 2: UJI ASUMSI PCA
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 2.1 Uji KMO (Kaiser-Meyer-Olkin) – Kecukupan Sampel
# -----------------------------------------------------------------------------
# KMO > 0.6 = layak untuk PCA; > 0.8 = sangat baik

cat("\n=== UJI KMO (Kaiser-Meyer-Olkin) ===\n")
kmo_hasil <- KMO(df_scaled)
print(kmo_hasil)
cat(paste("Nilai KMO Keseluruhan:", round(kmo_hasil$MSA, 3), "\n"))

if (kmo_hasil$MSA >= 0.8) {
  cat("Interpretasi: KMO sangat baik (>= 0.8) → data sangat layak untuk PCA\n")
} else if (kmo_hasil$MSA >= 0.6) {
  cat("Interpretasi: KMO cukup baik (>= 0.6) → data layak untuk PCA\n")
} else {
  cat("Peringatan: KMO < 0.6 → pertimbangkan kembali variabel yang digunakan\n")
}

# -----------------------------------------------------------------------------
# 2.2 Uji Bartlett's Test of Sphericity – Korelasi Antar Variabel
# -----------------------------------------------------------------------------
# H0: matriks korelasi = matriks identitas (tidak ada korelasi)
# p-value < 0.05 → tolak H0 → ada korelasi → PCA sesuai

cat("\n=== UJI BARTLETT'S TEST OF SPHERICITY ===\n")
bartlett_hasil <- cortest.bartlett(cor(df_scaled), n = nrow(df_scaled))
cat(paste("Chi-square:", round(bartlett_hasil$chisq, 3), "\n"))
cat(paste("df:", bartlett_hasil$df, "\n"))
cat(paste("p-value:", format(bartlett_hasil$p.value, scientific = TRUE), "\n"))

if (bartlett_hasil$p.value < 0.05) {
  cat("Interpretasi: p < 0.05 → Tolak H0 → Terdapat korelasi signifikan → PCA valid\n")
} else {
  cat("Peringatan: p >= 0.05 → Tidak cukup bukti korelasi antar variabel\n")
}

# -----------------------------------------------------------------------------
# 2.3 Deteksi Outlier – Jarak Mahalanobis & Boxplot
# -----------------------------------------------------------------------------

# --- Boxplot untuk melihat sebaran dan outlier visual ---
cat("\n=== DETEKSI OUTLIER: BOXPLOT ===\n")

# Ubah data scaled ke format panjang untuk ggplot
df_long <- as.data.frame(df_scaled) %>%
  pivot_longer(cols = everything(), names_to = "Variabel", values_to = "Nilai")

# Nama variabel lebih pendek untuk label grafik
label_pendek <- c(
  "Log_GDP"          = "GDP",
  "Social_support"   = "Sosial",
  "Healthy_life_exp" = "Kesehatan",
  "Freedom"          = "Kebebasan",
  "Generosity"       = "Kedermawanan",
  "Corruption"       = "Korupsi"
)
df_long$Variabel <- recode(df_long$Variabel, !!!label_pendek)

p_boxplot <- ggplot(df_long, aes(x = Variabel, y = Nilai, fill = Variabel)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(
    title = "Boxplot Variabel WHR 2025 (Data Terstandardisasi)",
    subtitle = "Titik merah = potensi outlier (di luar 1.5×IQR)",
    x = "Variabel", y = "Z-Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

print(p_boxplot)

# --- Jarak Mahalanobis untuk deteksi outlier multivariat ---
cat("\n=== DETEKSI OUTLIER: JARAK MAHALANOBIS ===\n")

# Hitung Jarak Mahalanobis menggunakan matriks kovarians
maha_dist <- mahalanobis(
  x     = df_scaled,
  center = colMeans(df_scaled),
  cov    = cov(df_scaled)
)

# Batas kritis: distribusi chi-square dengan df = jumlah variabel, alpha = 0.001
batas_kritis <- qchisq(0.999, df = ncol(df_scaled))
outlier_idx  <- which(maha_dist > batas_kritis)

cat(paste("Batas kritis chi-square (p=0.001, df=6):", round(batas_kritis, 3), "\n"))
cat(paste("Jumlah outlier terdeteksi:", length(outlier_idx), "\n"))

if (length(outlier_idx) > 0) {
  cat("Negara dengan jarak Mahalanobis tinggi:\n")
  print(data.frame(
    Negara = negara_bersih[outlier_idx],
    Jarak  = round(maha_dist[outlier_idx], 3)
  ) %>% arrange(desc(Jarak)))
}

# Plot Mahalanobis Distance
df_maha <- data.frame(
  Index  = 1:length(maha_dist),
  Jarak  = maha_dist,
  Negara = negara_bersih,
  Outlier = maha_dist > batas_kritis
)

p_maha <- ggplot(df_maha, aes(x = Index, y = Jarak, color = Outlier, label = ifelse(Outlier, Negara, ""))) +
  geom_point(size = 2) +
  geom_hline(yintercept = batas_kritis, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_text(vjust = -0.5, size = 3, color = "darkred") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                     labels = c("Normal", "Outlier")) +
  labs(
    title    = "Deteksi Outlier: Jarak Mahalanobis (145 Negara)",
    subtitle = "Garis merah putus-putus = batas kritis chi² (p=0.001, df=6)",
    x        = "Indeks Observasi",
    y        = "Jarak Mahalanobis",
    color    = "Status"
  ) +
  theme_minimal(base_size = 12)

print(p_maha)


# -----------------------------------------------------------------------------
# TAHAP 3: ANALISIS PCA
# -----------------------------------------------------------------------------

cat("\n=== ANALISIS PCA ===\n")

# Jalankan PCA menggunakan FactoMineR
# scale.unit=FALSE karena data sudah distandardisasi
# graph=FALSE agar plot dikelola manual
pca_hasil <- PCA(df_scaled, scale.unit = FALSE, ncp = 6, graph = FALSE)

# -----------------------------------------------------------------------------
# 3.1 Tabel Eigenvalues dan Cumulative Proportion
# -----------------------------------------------------------------------------

eigen_tabel <- pca_hasil$eig
colnames(eigen_tabel) <- c("Eigenvalue", "Var. Explained (%)", "Cumulative (%)")

cat("\n=== TABEL EIGENVALUES ===\n")
print(round(eigen_tabel, 3))

# Hitung PC optimal berdasarkan kriteria (untuk K-Means & skor clustering)
n_pc_eigen <- sum(eigen_tabel[, "Eigenvalue"] > 1)    # Kaiser criterion
n_pc_cum   <- which(eigen_tabel[, "Cumulative (%)"] >= 70)[1]  # 70% variance
n_pc       <- max(n_pc_eigen, n_pc_cum)                # Dipakai untuk K-Means

# n_pc_total = semua PC (ditampilkan di model akhir & tabel loading)
n_pc_total <- ncol(df_scaled)                          # = 6 (semua PC)

cat(paste("\nJumlah PC dengan Eigenvalue > 1 :", n_pc_eigen))
cat(paste("\nJumlah PC untuk Cumulative > 70%:", n_pc_cum))
cat(paste("\nKeputusan K-Means : Gunakan", n_pc, "PC pertama\n"))
cat(paste("Model Akhir PCA   : Tampilkan semua", n_pc_total, "PC\n"))

# Variance explained oleh PC yang dipilih untuk K-Means
var_explained <- eigen_tabel[n_pc, "Cumulative (%)"]
cat(paste("Total variansi (PC signifikan):", round(var_explained, 1), "%\n"))
cat(paste("Total variansi (semua PC)     : 100.0 %\n"))

# -----------------------------------------------------------------------------
# 3.2 Scree Plot
# -----------------------------------------------------------------------------

p_scree <- fviz_eig(
  pca_hasil,
  addlabels  = TRUE,          # Tambahkan label persentase di atas bar
  ylim       = c(0, 60),
  barfill    = "steelblue",
  barcolor   = "steelblue",
  linecolor  = "darkred"
) +
  labs(
    title    = "Scree Plot – PCA World Happiness Report 2025",
    subtitle = paste("Garis merah = batas Eigenvalue=1 | PC dipilih:", n_pc),
    x        = "Principal Component",
    y        = "Persentase Variansi (%)"
  ) +
  geom_hline(yintercept = 100/ncol(df_scaled),  # Batas rata-rata (Eigenvalue=1 setara)
             linetype = "dashed", color = "red", linewidth = 0.8) +
  theme_minimal(base_size = 12)

print(p_scree)

# -----------------------------------------------------------------------------
# 3.3 Loading Plot (Kontribusi Variabel ke PC)
# -----------------------------------------------------------------------------

p_loading <- fviz_pca_var(
  pca_hasil,
  col.var    = "contrib",          # Warna berdasarkan kontribusi
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel      = TRUE                # Hindari label tumpang tindih
) +
  labs(
    title    = "Loading Plot – Kontribusi Variabel ke PC1 & PC2",
    subtitle = "Panjang panah = kekuatan kontribusi; Sudut = korelasi antar variabel",
    color    = "Kontribusi (%)"
  ) +
  theme_minimal(base_size = 12)

print(p_loading)

# -----------------------------------------------------------------------------
# 3.4 Biplot (Observasi + Variabel)
# -----------------------------------------------------------------------------

# Biplot dengan label hanya negara teratas & terbawah agar tidak penuh
biplot_label <- ifelse(
  rank(-pca_hasil$ind$coord[,1]) <= 10 | rank(pca_hasil$ind$coord[,1]) <= 10,
  rownames(pca_hasil$ind$coord), ""
)

p_biplot <- fviz_pca_biplot(
  pca_hasil,
  label        = "var",            # Tampilkan label variabel saja
  col.ind      = "steelblue",      # Warna titik observasi
  col.var      = "darkred",        # Warna panah variabel
  alpha.ind    = 0.5,
  repel        = TRUE,
  labelsize    = 4
) +
  labs(
    title    = "Biplot PCA – Negara & Variabel WHR 2025",
    subtitle = "Titik biru = negara; Panah merah = variabel"
  ) +
  theme_minimal(base_size = 12)

print(p_biplot)


# -----------------------------------------------------------------------------
# TAHAP 4: EKSTRAKSI SKOR PCA
# -----------------------------------------------------------------------------

# Ambil skor PC (koordinat) dari hasil PCA
# Minimal 3 PC agar tabel skor & K-Means memakai PC1, PC2, PC3
n_pc_kmeans <- max(n_pc, 3)                                    # Minimal 3 PC
n_pc_kmeans
pc_scores   <- as.data.frame(pca_hasil$ind$coord[, 1:n_pc_kmeans])
pc_scores

# Beri nama kolom yang deskriptif
colnames(pc_scores) <- paste0("PC", 1:n_pc_kmeans)
rownames(pc_scores) <- negara_bersih

cat("\n=== SKOR PCA (5 NEGARA PERTAMA) ===\n")
print(round(head(pc_scores, 5), 3))
cat(paste("\nDimension skor PCA:", nrow(pc_scores), "negara ×", ncol(pc_scores), "PC\n"))

# -----------------------------------------------------------------------------
# 4.1 Model Akhir PCA – Persamaan Loading (Seperti Contoh di Laporan)
# -----------------------------------------------------------------------------

cat("\n=== MODEL AKHIR PCA ===\n")

# Ambil matriks loading (eigenvectors) dari hasil PCA
# Gunakan n_pc_total agar SEMUA PC ditampilkan (bukan hanya PC signifikan)
loading_matrix <- pca_hasil$svd$V[, 1:n_pc_total]
rownames(loading_matrix) <- colnames(df_scaled)        # Nama variabel sebagai baris
colnames(loading_matrix) <- paste0("PC", 1:n_pc_total) # Nama PC sebagai kolom

# Tampilkan tabel loading lengkap
cat("\nTabel Loading (Eigenvectors):\n")
print(round(loading_matrix, 4))

# Fungsi untuk memformat satu persamaan PC secara otomatis
# Menampilkan hanya koefisien yang signifikan (|loading| > 0.1)
format_persamaan_pc <- function(pc_idx, loading_mat, var_names, threshold = 0.1) {
  koef  <- loading_mat[, pc_idx]                        # Koefisien PC ke-i
  bagian <- c()
  
  for (i in seq_along(koef)) {
    if (abs(koef[i]) >= threshold) {                    # Filter koefisien kecil
      tanda <- ifelse(koef[i] >= 0 & length(bagian) > 0, "+ ", 
                      ifelse(koef[i] < 0, "- ", ""))
      nilai <- sprintf("%.3f", abs(koef[i]))
      bagian <- c(bagian, paste0(tanda, nilai, var_names[i]))
    }
  }
  paste(bagian, collapse = " ")                         # Gabung jadi satu string
}

# Nama variabel pendek untuk persamaan (X1–X6 sesuai urutan kolom)
nama_xi <- paste0("X", 1:ncol(df_scaled))

cat("\n--- PERSAMAAN MODEL AKHIR PCA (SEMUA PC) ---\n")
for (i in 1:n_pc_total) {
  persamaan <- format_persamaan_pc(i, loading_matrix, nama_xi)
  cat(paste0("PC", i, " = ", persamaan, "\n\n"))
}

# Keterangan variabel (X1 = apa, X2 = apa, dst.)
cat("Keterangan Variabel:\n")
for (i in seq_along(colnames(df_scaled))) {
  cat(paste0("  X", i, " = ", colnames(df_scaled)[i], "\n"))
}

# Tampilkan juga tabel loading dalam format rapi per PC
cat("\nRingkasan Loading per PC (diurutkan dari terbesar):\n")
for (i in 1:n_pc_total) {
  cat(paste0("\n  PC", i, " (Var. explained: ",
             round(eigen_tabel[i, "Var. Explained (%)"], 1), "%):\n"))
  loading_sort <- sort(abs(loading_matrix[, i]), decreasing = TRUE)
  for (nm in names(loading_sort)) {
    nilai_asli <- loading_matrix[nm, i]
    tanda_str  <- ifelse(nilai_asli >= 0, " (+)", " (-)")
    cat(sprintf("    %-20s : %6.4f %s\n", nm, abs(nilai_asli), tanda_str))
  }
}

# Visualisasi Loading Heatmap per PC yang dipilih
loading_df <- as.data.frame(loading_matrix) %>%
  rownames_to_column("Variabel") %>%
  pivot_longer(-Variabel, names_to = "PC", values_to = "Loading") %>%
  mutate(Variabel = recode(Variabel, !!!label_pendek))  # Label pendek

p_loading_heat <- ggplot(loading_df, aes(x = PC, y = Variabel, fill = Loading)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Loading, 3),
                color = abs(Loading) > 0.3), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#FC4E07", mid = "white", high = "#2196F3",
                       midpoint = 0, limit = c(-1, 1), name = "Loading") +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "black"), guide = "none") +
  labs(
    title    = paste0("Heatmap Loading PCA – Semua ", n_pc_total, " PC"),
    subtitle = paste0("Nilai = bobot variabel | PC1–PC", n_pc, " dipakai untuk K-Means"),
    x        = "Principal Component",
    y        = "Variabel"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 11))

print(p_loading_heat)


# -----------------------------------------------------------------------------
# TAHAP 5: ANALISIS KLUSTER K-MEANS
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 5.1 Metode Elbow – Tentukan k Optimal
# -----------------------------------------------------------------------------

cat("\n=== METODE ELBOW (k=2 sampai 10) ===\n")

# Hitung total within-cluster sum of squares (WCSS) untuk setiap k
wcss <- sapply(2:10, function(k) {
  kmeans(pc_scores, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

df_elbow <- data.frame(k = 2:10, WCSS = wcss)

p_elbow <- ggplot(df_elbow, aes(x = k, y = WCSS)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 3, color = "steelblue") +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title    = "Metode Elbow – Penentuan k Optimal",
    subtitle = "Cari 'siku' (titik penurunan WCSS mulai melambat)",
    x        = "Jumlah Kluster (k)",
    y        = "Total Within-Cluster SS (WCSS)"
  ) +
  theme_minimal(base_size = 12)

print(p_elbow)

# -----------------------------------------------------------------------------
# 5.2 Silhouette Plot – Konfirmasi k Optimal
# -----------------------------------------------------------------------------

cat("\n=== ANALISIS SILHOUETTE (k=2 sampai 8) ===\n")

sil_avg <- sapply(2:8, function(k) {
  km      <- kmeans(pc_scores, centers = k, nstart = 25, iter.max = 100)
  sil     <- silhouette(km$cluster, dist(pc_scores))
  mean(sil[, 3])
})

df_sil <- data.frame(k = 2:8, Silhouette = sil_avg)
k_optimal <- df_sil$k[which.max(df_sil$Silhouette)]

cat(paste("k dengan Silhouette rata-rata tertinggi:", k_optimal, "\n"))
print(df_sil)

p_sil <- ggplot(df_sil, aes(x = k, y = Silhouette)) +
  geom_line(color = "darkorange", linewidth = 1.2) +
  geom_point(size = 3, color = "darkorange") +
  geom_vline(xintercept = k_optimal, linetype = "dashed", color = "red", linewidth = 0.8) +
  scale_x_continuous(breaks = 2:8) +
  labs(
    title    = "Silhouette Plot – Penentuan k Optimal",
    subtitle = paste("Garis merah = k optimal =", k_optimal),
    x        = "Jumlah Kluster (k)",
    y        = "Rata-rata Silhouette Width"
  ) +
  theme_minimal(base_size = 12)

print(p_sil)

# -----------------------------------------------------------------------------
# 5.3 Jalankan K-Means dengan k Optimal
# -----------------------------------------------------------------------------

cat(paste("\n=== K-MEANS CLUSTERING (k =", k_optimal, ") ===\n"))

# Jalankan K-Means dengan nstart=25 untuk stabilitas hasil
kmeans_hasil <- kmeans(
  x        = pc_scores,
  centers  = k_optimal,
  nstart   = 25,           # 25 inisialisasi acak, ambil yang terbaik
  iter.max = 100           # Maksimum iterasi
)

cat(paste("Ukuran tiap kluster:\n"))
print(table(kmeans_hasil$cluster))
cat(paste("Total WCSS:", round(kmeans_hasil$tot.withinss, 3), "\n"))
cat(paste("Between-cluster SS:", round(kmeans_hasil$betweenss, 3), "\n"))
cat(paste("Rasio (between/total):", round(kmeans_hasil$betweenss/kmeans_hasil$totss, 3), "\n"))

# Tambahkan label kluster ke data skor PCA
pc_scores$Cluster <- as.factor(kmeans_hasil$cluster)

# -----------------------------------------------------------------------------
# 5.4 Visualisasi Kluster di Ruang PC1-PC2
# -----------------------------------------------------------------------------

p_cluster <- fviz_cluster(
  kmeans_hasil,
  data           = pc_scores[, -ncol(pc_scores)],   # Tanpa kolom Cluster
  geom           = "point",
  ellipse.type   = "convex",      # Lingkaran konveks per kluster
  palette        = "jco",
  ggtheme        = theme_minimal(base_size = 12),
  pointsize      = 1.8,
  alpha          = 0.7
) +
  labs(
    title    = paste0("Hasil K-Means Clustering (k=", k_optimal, ") – WHR 2025"),
    subtitle = "Axis: PC1 (kesejahteraan umum) & PC2; Titik = negara; Warna = kluster",
    x        = paste0("PC1 (", round(eigen_tabel[1, "Var. Explained (%)"], 1), "% variansi)"),
    y        = paste0("PC2 (", round(eigen_tabel[2, "Var. Explained (%)"], 1), "% variansi)")
  )

print(p_cluster)

# Silhouette plot detail per observasi untuk k optimal
sil_final <- silhouette(kmeans_hasil$cluster, dist(pc_scores[, -ncol(pc_scores)]))

p_sil_final <- fviz_silhouette(sil_final, palette = "jco", ggtheme = theme_minimal()) +
  labs(
    title    = paste0("Silhouette Plot per Negara (k=", k_optimal, ")"),
    subtitle = "Nilai > 0 = terkategorisasi baik; < 0 = ambigu"
  )

print(p_sil_final)


# -----------------------------------------------------------------------------
# TAHAP 6: INTERPRETASI & PROFILING KLUSTER
# -----------------------------------------------------------------------------

cat("\n=== PROFILING KLUSTER – RATA-RATA VARIABEL ASLI ===\n")

# Gabungkan data asli (tidak terstandardisasi) dengan label kluster
df_profil <- df_bersih
df_profil$Cluster <- as.factor(kmeans_hasil$cluster)
df_profil$Negara  <- negara_bersih

# Hitung rata-rata setiap variabel per kluster
profil_tabel <- df_profil %>%
  group_by(Cluster) %>%
  summarise(
    Jumlah_Negara           = n(),
    GDP_rata                = round(mean(Log_GDP), 3),
    Sosial_rata             = round(mean(Social_support), 3),
    Kesehatan_rata          = round(mean(Healthy_life_exp), 3),
    Kebebasan_rata          = round(mean(Freedom), 3),
    Kedermawanan_rata       = round(mean(Generosity), 3),
    Korupsi_rata            = round(mean(Corruption), 3),
    .groups = "drop"
  )

print(as.data.frame(profil_tabel))

# -----------------------------------------------------------------------------
# 6.1 Visualisasi Profil Kluster (Radar-style Barplot)
# -----------------------------------------------------------------------------

# Ubah ke format panjang untuk facet plot
profil_long <- profil_tabel %>%
  pivot_longer(cols = -c(Cluster, Jumlah_Negara),
               names_to  = "Variabel",
               values_to = "Rata_Rata") %>%
  mutate(Variabel = recode(Variabel,
                           "GDP_rata"          = "Log GDP",
                           "Sosial_rata"       = "Dukungan Sosial",
                           "Kesehatan_rata"    = "Harapan Hidup",
                           "Kebebasan_rata"    = "Kebebasan",
                           "Kedermawanan_rata" = "Kedermawanan",
                           "Korupsi_rata"      = "Persepsi Korupsi"
  ))

p_profil <- ggplot(profil_long, aes(x = Variabel, y = Rata_Rata, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Cluster, labeller = label_both, ncol = 2) +
  coord_flip() +
  labs(
    title    = "Profil Rata-Rata Variabel per Kluster – WHR 2025",
    subtitle = "Berdasarkan nilai variabel asli (sebelum standardisasi)",
    x        = "Variabel",
    y        = "Rata-Rata Nilai"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set1")

print(p_profil)

# -----------------------------------------------------------------------------
# 6.2 Daftar Negara per Kluster
# -----------------------------------------------------------------------------

cat("\n=== DAFTAR NEGARA PER KLUSTER ===\n")

for (k in sort(unique(kmeans_hasil$cluster))) {
  negara_cluster <- df_profil %>%
    filter(Cluster == k) %>%
    arrange(desc(Log_GDP)) %>%
    pull(Negara)
  
  cat(paste0("\nKluster ", k, " (", length(negara_cluster), " negara):\n"))
  cat(paste(negara_cluster, collapse = ", "), "\n")
}

# -----------------------------------------------------------------------------
# 6.3 Heatmap Korelasi Antar Variabel (Bonus)
# -----------------------------------------------------------------------------

cor_matrix <- cor(df_scaled)

# Ubah ke format panjang untuk ggplot
cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Korelasi") %>%
  mutate(
    Var1 = recode(Var1, !!!label_pendek),
    Var2 = recode(Var2, !!!label_pendek)
  )

p_heatmap <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Korelasi)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Korelasi, 2)), size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#FC4E07", mid = "white", high = "#00AFBB",
                       midpoint = 0, limit = c(-1, 1)) +
  labs(
    title = "Heatmap Korelasi Antar Variabel WHR 2025",
    x = "", y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_heatmap)

# =============================================================================
# RINGKASAN AKHIR
# =============================================================================

cat("\n")
cat("=============================================================\n")
cat("                    RINGKASAN ANALISIS\n")
cat("=============================================================\n")
cat(paste("Data          : World Happiness Report 2025 (WHR 2026)\n"))
cat(paste("Jumlah negara :", nrow(df_bersih), "\n"))
cat(paste("Variabel      : 6 indikator kebahagiaan\n"))
cat(paste("\nUji KMO       : MSA =", round(kmo_hasil$MSA, 3),
          ifelse(kmo_hasil$MSA >= 0.6, "→ Layak PCA ✓", "→ Perlu evaluasi\n")))
cat(paste("Bartlett Test : p =", format(bartlett_hasil$p.value, scientific=TRUE),
          "→ Korelasi signifikan ✓\n"))
cat(paste("\nPCA           : Dipilih", n_pc, "PC, menjelaskan",
          round(var_explained, 1), "% variansi\n"))
cat(paste("K-Means       : k optimal =", k_optimal, "kluster\n"))
cat(paste("\nSilhouette avg:", round(max(sil_avg), 3), "(mendekati 1 = kluster baik)\n"))
cat("=============================================================\n")