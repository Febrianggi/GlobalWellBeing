[Baca dalam Bahasa Indonesia](README.id.md)
# Global Well-Being Analysis with PCA and K-Means Clustering

A multivariate analysis in R that groups countries by well-being indicators from the World Happiness Report. The data are cross-sectional (2025) and cover 147 countries. Two countries with missing values are dropped, which leaves 145 countries.

## What is in this project

The script `PCA & CLUSTER.R` runs the whole workflow, from data cleaning to cluster profiles:

1. Data preparation: remove rows with missing values and standardize with z-scores.
2. Assumption checks: KMO, Bartlett's test of sphericity, boxplots, and Mahalanobis distance for outliers.
3. PCA: eigenvalue table, scree plot, loading plot, biplot, and the final equation for each component.
4. K-Means on the PCA scores: choosing k with the Elbow and Silhouette methods.
5. Cluster profiles: average of the original variables per cluster and the list of countries in each cluster.

## Language

The paper and the presentation slides for this project are written in Indonesian. Comments and console output in the R script are also in Indonesian. Variable names and technical terms (well-being, clustering, eigenvalue) are kept in English.

## Variables

The analysis starts with six variables from the World Happiness Report:

- Log GDP per capita
- Social support
- Healthy life expectancy
- Freedom to make life choices
- Generosity
- Perceptions of corruption

Generosity is removed because its KMO value is only 0.36, below the 0.5 threshold. The main analysis runs on the remaining five variables.

## Summary of results

- Bartlett's test is significant (chi-square 271.634, p about 1.5 × 10⁻⁵²), so the variables are correlated enough for PCA.
- Three principal components explain 88.172% of the total variance. The first eigenvalue is 2.821.
- PC1 is a general well-being dimension (Log GDP, Social support, Healthy life expectancy). PC2 is dominated by Perceptions of corruption, and PC3 by Freedom to make life choices.
- The Elbow method suggests k = 4, while the Silhouette method suggests k = 2 with an average width of 0.386. The final result uses k = 2.
- Cluster 1 (67 countries) has lower average indicators. Cluster 2 (78 countries) has higher average indicators.

## How to run

You need R and the following packages:

```r
install.packages(c(
  "tidyverse", "factoextra", "FactoMineR",
  "psych", "cluster", "corrplot"
))
```

The data are not included in this repository. Download the World Happiness Report data from https://data.worldhappiness.report and save it as a CSV file with a semicolon (`;`) separator and a decimal comma (`,`). The script reads the columns in this order:

| Column | Content |
|--------|---------|
| 1 | Year |
| 2 | Rank |
| 3 | Country name |
| 4 to 9 | Log GDP, Social support, Healthy life expectancy, Freedom, Generosity, Perceptions of corruption |

Then open the script in RStudio and run it. A `file.choose()` dialog will ask for the CSV file. The seed is set to 42 so the K-Means results are reproducible.

## Data source

Helliwell, J., Layard, R., Sachs, J., De Neve, J.-E., Aknin, L., & Wang, S. (2026). World Happiness Report. University of Oxford: Wellbeing Research Centre.

The data belong to the publisher. Check the terms of use on the World Happiness Report website before redistributing the data file.

## Limitations

- The data cover a single point in time, so changes over the years are not analyzed.
- Only PCA and K-Means are used. Other methods such as hierarchical clustering or DBSCAN are not compared.
- A silhouette width of 0.386 is moderate, so the boundaries between clusters are not sharp.
