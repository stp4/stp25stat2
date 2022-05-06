stp25settings
================

# stp25stat2

Bietet ein einfaches und intuitives Formula und Pipe-freundliches
Framework, um grundlegende statistische Tests durchzuführen, zum
Beispiel t-Test, Wilcoxon-Test, ANOVA, Kruskal-Wallis und
Korrelationsanalysen.

Die Ausgabe jedes Tests wird automatisch in einen APA-Style conforme
Tabelle umgewandelt.

Zusätzliche Funktionen zum Umformen, Neuordnen, Manipulieren sind in
*spp25tools* enthalten. Funktionen zur Visualisierung sind in
*stp25plot* enthalten.

## Key functions

``` r
require(stp25settings)
require(stp25stat2)

which_output()
# [1] "markdown_html"
```

### Descriptive statistics

-   `Tbll_desc`, `Tbll_desc_long()`, `Tbll_desc_item()` und
    `Tbll_desc_multi` : Berechnen deskriptive Statistiken für eine oder
    mehrere numerische Variablen. Kann mit gruppierten Daten umgehen.
-   `Tbll_xtabs()`: Berechnet Häufigkeitstabelle kategorialer Variablen.

``` r
Tbll_desc(~ breaks + wool, warpbreaks)
# # A tibble: 5 x 2
#   Item            m              
#   <chr>           <chr>          
# 1 "(N) "          "54"           
# 2 "breaks (mean)" "28.15 (13.20)"
# 3 "wool "         ""             
# 4 "    A"         "50% (27)"     
# 5 "    B"         "50% (27)"

warpbreaks %>% Tbll_desc(~breaks + wool)
# # A tibble: 5 x 2
#   Item            m              
#   <chr>           <chr>          
# 1 "(N) "          "54"           
# 2 "breaks (mean)" "28.15 (13.20)"
# 3 "wool "         ""             
# 4 "    A"         "50% (27)"     
# 5 "    B"         "50% (27)"

warpbreaks %>% Tbll_desc(breaks, wool)
# # A tibble: 5 x 2
#   Item            m              
#   <chr>           <chr>          
# 1 "(N) "          "54"           
# 2 "breaks (mean)" "28.15 (13.20)"
# 3 "wool "         ""             
# 4 "    A"         "50% (27)"     
# 5 "    B"         "50% (27)"
```

``` r
warpbreaks %>% Tbll_desc(
  "H1",
  breaks,
  tension,
  by = ~ wool,
  include.total = TRUE,
  include.n = FALSE,
  include.test = TRUE) %>% kable()
```

| Item          | Total         | A             | B            | statistics            |
|:--------------|:--------------|:--------------|:-------------|:----------------------|
| H1            |               |               |              |                       |
| breaks (mean) | 28.15 (13.20) | 31.04 (15.85) | 25.26 (9.30) | F(1, 52)=1.33, p=.253 |
| tension       |               |               |              | X2(2)=0.00, p=1.000   |
| L             | 33% (18)      | 33% (9)       | 33% (9)      |                       |
| M             | 33% (18)      | 33% (9)       | 33% (9)      |                       |
| H             | 33% (18)      | 33% (9)       | 33% (9)      |                       |

``` r
mtcars %>% Tbll_desc_long(
  mpg[mean, 1],  cyl[median],  disp[0],
  hp[0],  drat,  wt,  qsec[1],  vs[freq],
  am[freq],  gear,  carb,
  include.range = TRUE,  include.n = TRUE
) %>% kable(caption ="summary statistics" )
```

| Item         |   n | m                                   |
|:-------------|----:|:------------------------------------|
| mpg (mean)   |  32 | 20.1 (SD 6.0, range 10.4 to 33.9)   |
| cyl (median) |  32 | 6.00 (IQR 4.00, range 4.00 to 8.00) |
| disp (mean)  |  32 | 231 (SD 124, range 71 to 472)       |
| hp (mean)    |  32 | 147 (SD 69, range 52 to 335)        |
| drat (mean)  |  32 | 3.60 (SD 0.53, range 2.76 to 4.93)  |
| wt (mean)    |  32 | 3.22 (SD 0.98, range 1.51 to 5.42)  |
| qsec (mean)  |  32 | 17.8 (SD 1.8, range 14.5 to 22.9)   |
| vs (0/1)     |  32 | 18/14                               |
| am (0/1)     |  32 | 19/13                               |
| gear (mean)  |  32 | 3.69 (SD 0.74, range 3.00 to 5.00)  |
| carb (mean)  |  32 | 2.81 (SD 1.62, range 1.00 to 8.00)  |

summary statistics

### Correlation analysis

-   `Tbll_corr()()`: Korrelationstest zwischen zwei oder mehr Variablen
    mit Pearson-, Spearman- oder Kendall-Methoden.

-   `Tbll_corr()`: Compute the mode of a vector, that is the most
    frequent values.

-   `Tbll_desc_long()`: Detect univariate outliers using boxplot
    methods.

-   `Tbll_desc_item()`: Compute Mahalanobis Distance and Flag
    Multivariate Outliers.

-   `Tbll_likert()` and `mshapiro_test()`: Univariate and multivariate
    Shapiro-Wilk normality test.
