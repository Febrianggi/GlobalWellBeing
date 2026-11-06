[Read in English](README.md)
# Analisis Well-Being Global dengan PCA dan K-Means Clustering

Proyek analisis multivariat dengan R untuk mengelompokkan negara berdasarkan indikator kesejahteraan dari World Happiness Report. Datanya cross-sectional tahun 2025, mencakup 147 negara. Data dikurangi menjadi 145 negara setelah baris yang memiliki nilai hilang dibuang.

## Isi proyek

Skrip `PCA&CLUSTER UHUYY.R` menjalankan seluruh alur analisis dari awal sampai profil klaster:

1. Pembersihan data: membuang baris dengan nilai hilang dan standardisasi z-score.
2. Uji asumsi: KMO, Bartlett's test of sphericity, boxplot, dan jarak Mahalanobis untuk outlier.
3. PCA: tabel eigenvalue, scree plot, loading plot, biplot, dan model akhir tiap komponen.
4. K-Means pada skor PCA: penentuan k dengan metode Elbow dan Silhouette.
5. Profil klaster: rata-rata variabel asli per klaster dan daftar negara per klaster.

## Bahasa

Paper dan slide presentasi proyek ini ditulis dalam bahasa Indonesia. Komentar dan keluaran di skrip R juga berbahasa Indonesia, sedangkan nama variabel dan istilah teknis (misalnya well-being, clustering, eigenvalue) tetap memakai istilah aslinya dalam bahasa Inggris.

## Variabel

Analisis awal memakai enam variabel dari World Happiness Report:

- Log GDP per capita
- Social support
- Healthy life expectancy
- Freedom to make life choices
- Generosity
- Perceptions of corruption

Generosity dikeluarkan karena nilai KMO-nya hanya 0,36, di bawah batas kelayakan 0,5. Analisis utama berjalan dengan lima variabel.

## Ringkasan hasil

- Bartlett's test signifikan (chi-square 271,634; p ≈ 1,5 × 10⁻⁵²), jadi korelasi antarvariabel cukup untuk PCA.
- Tiga komponen utama menjelaskan 88,172% keragaman total. Eigenvalue PC1 sebesar 2,821.
- PC1 menggambarkan kesejahteraan umum (Log GDP, Social support, Healthy life expectancy). PC2 didominasi Perceptions of corruption, dan PC3 didominasi Freedom to make life choices.
- Metode Elbow menunjuk k = 4, sedangkan Silhouette menunjuk k = 2 dengan rata-rata 0,386. Hasil akhir memakai k = 2.
- Klaster 1 (67 negara) memiliki rata-rata indikator lebih rendah. Klaster 2 (78 negara) memiliki rata-rata lebih tinggi.

## Cara menjalankan

Butuh R dengan paket berikut:

```r
install.packages(c(
  "tidyverse", "factoextra", "FactoMineR",
  "psych", "cluster", "corrplot"
))
```

Data tidak disertakan di repositori ini. Unduh data World Happiness Report dari https://data.worldhappiness.report, lalu simpan sebagai CSV dengan pemisah titik koma (`;`) dan desimal koma (`,`). Urutan kolom yang dibaca skrip:

| Kolom | Isi |
|-------|-----|
| 1 | Year |
| 2 | Rank |
| 3 | Country name |
| 4 sampai 9 | Log GDP, Social support, Healthy life expectancy, Freedom, Generosity, Perceptions of corruption |

Setelah itu buka skrip di RStudio dan jalankan. Dialog `file.choose()` akan muncul untuk memilih file CSV. Seed diset ke 42 supaya hasil K-Means bisa diulang.

## Sumber data

Helliwell, J., Layard, R., Sachs, J., De Neve, J.-E., Aknin, L., & Wang, S. (2026). World Happiness Report. University of Oxford: Wellbeing Research Centre.

Data asli milik penerbitnya. Cek ketentuan penggunaan di situs World Happiness Report sebelum membagikan ulang file datanya.

## Keterbatasan

- Datanya satu titik waktu, jadi tidak ada analisis perubahan antartahun.
- Hanya PCA dan K-Means yang dipakai. Metode lain seperti hierarchical clustering atau DBSCAN tidak dibandingkan.
- Nilai silhouette 0,386 tergolong sedang, jadi batas antarklaster tidak tegas.