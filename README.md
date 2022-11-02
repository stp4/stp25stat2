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
# # A tibble: 5 × 2
#   Item            m              
# * <chr>           <chr>          
# 1 "(N) "          "54"           
# 2 "breaks (mean)" "28.15 (13.20)"
# 3 "wool "         ""             
# 4 "    A"         "50% (27)"     
# 5 "    B"         "50% (27)"

warpbreaks %>% Tbll_desc(~breaks + wool)
# # A tibble: 5 × 2
#   Item            m              
# * <chr>           <chr>          
# 1 "(N) "          "54"           
# 2 "breaks (mean)" "28.15 (13.20)"
# 3 "wool "         ""             
# 4 "    A"         "50% (27)"     
# 5 "    B"         "50% (27)"

warpbreaks %>% Tbll_desc(breaks, wool)
# # A tibble: 5 × 2
#   Item            m              
# * <chr>           <chr>          
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

| Item          | Total         | A             | B            | Statistics            |
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

| Item         |   n | m                                  |
|:-------------|----:|:-----------------------------------|
| mpg (mean)   |  32 | 20.1 (SD 6.0, range 10.4 to 33.9)  |
| cyl (median) |  32 | (6.004)                            |
| disp (mean)  |  32 | 231 (SD 124, range 71 to 472)      |
| hp (mean)    |  32 | 147 (SD 69, range 52 to 335)       |
| drat (mean)  |  32 | 3.60 (SD 0.53, range 2.76 to 4.93) |
| wt (mean)    |  32 | 3.22 (SD 0.98, range 1.51 to 5.42) |
| qsec (mean)  |  32 | 17.8 (SD 1.8, range 14.5 to 22.9)  |
| vs (0/1)     |  32 | 18/14                              |
| am (0/1)     |  32 | 19/13                              |
| gear (mean)  |  32 | 3.69 (SD 0.74, range 3.00 to 5.00) |
| carb (mean)  |  32 | 2.81 (SD 1.62, range 1.00 to 8.00) |

summary statistics

``` r
set_opt(
  caption=TRUE,
  median = list(digits = 0, style=1),
  prozent = list(style=2, 
                 digits=1,
                 percentage_str="%",
                 
                 null_percent_sign =  ' . ',
                 
                 include_level_multi = TRUE)
) 
```

``` r
DF %>%
  Tbll_desc(
    sex[ratio],
    group[freq],
    age[median],
    alc, smoker,
    "Clinical/Pretreatment",
    diabet,
    cardo,
    metabolic,
    infect[multi],
    bili[median,3],
    
    by = ~ group,
    include.total = TRUE,
    
    include.custom = stp25stat2:::effect_size
    # include.custom =  function(x ,
    #                            by, ...) {
    #   x <- scale(as.numeric(x))
    #   round(diff(sapply(split(x, by), mean, na.rm = TRUE)), 2)
    # },
    # 
    # include.value = c(group = "OR = .0256", sex = "swx = 26")
  ) %>% stp25output2::Output()
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 1: group (N=47)
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Item
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Total
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Control
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Treat
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
SDM/OR \[95% CI\]
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(N) 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
47
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
22
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
25
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Sex (male:female)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
25:22
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10:12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
15:10
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.56 \[0.17, 1.77\] OR
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Group 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n.a.
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 Control
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
22 (46.8%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
22 (100.0%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 . 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 Treat
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
25 (53.2%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 . 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
25 (100.0%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Age (median)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
43 (35, 59)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
51 (29, 62)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
41 (36, 55)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.24 \[-0.82, 0.33\] ES
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Alcohol Consumption 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n.a.
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 non
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
17 (36.2%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
8 (36.4%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
9 (36.0%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 Occasionally
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
12 (25.5%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
9 (40.9%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
3 (12.0%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 Abuse
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
18 (38.3%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5 (22.7%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
13 (52.0%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Tobacco Smoker true 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
9 (19%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4 (18%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5 (20%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.12 \[0.26, 4.85\] OR
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Clinical/Pretreatment 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Diabetes true 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
22 (47%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10 (45%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
12 (48%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.11 \[0.35, 3.49\] OR
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Cardiovascular Disease true 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
3 (6%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1 (5%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2 (8%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.83 \[0.15, \>20\] OR
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Chronic Metabolic Disease true 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
18 (38%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
7 (32%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
11 (44%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.68 \[0.51, 5.56\] OR
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Chronic Infections yes 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
11 (23.4%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4 (18.2%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
7 (28.0%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.57 \[0.14, 2.30\] OR
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
Serum Bilirubin (mg/dl) (median)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.735 (0.590, 1.026)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.708 (0.593, 1.044)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.786 (0.488, 1.024)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.10 \[-0.47, 0.68\] ES
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="5">
Categorical date: count (percent), ratio; Continuous date: median
(quantile)
</td>
</tr>
</tfoot>
</table>

``` r
Info <- data.frame( var_name=c("age", "sex"), y=1:2)
rownames(Info) <- c("age", "sex" )
DF %>%
  Tbll_desc(
    sex[ratio],
    group[freq],
    "Zwichen Ueberschrift",
    age[median],
    by = ~ group,
    include.total = TRUE,
    include.value = Info,
    include.nr = TRUE
  ) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Item
</th>
<th style="text-align:left;">
Total
</th>
<th style="text-align:left;">
Control
</th>
<th style="text-align:left;">
Treat
</th>
<th style="text-align:left;">
var_name
</th>
<th style="text-align:right;">
y
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">

14) </td>
    <td style="text-align:left;">
    47
    </td>
    <td style="text-align:left;">
    22
    </td>
    <td style="text-align:left;">
    25
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:right;">
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    Sex (male:female)
    </td>
    <td style="text-align:left;">
    25:22
    </td>
    <td style="text-align:left;">
    10:12
    </td>
    <td style="text-align:left;">
    15:10
    </td>
    <td style="text-align:left;">
    sex
    </td>
    <td style="text-align:right;">
    2
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    Group
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:right;">
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    Control
    </td>
    <td style="text-align:left;">
    22 (46.8%)
    </td>
    <td style="text-align:left;">
    22 (100.0%)
    </td>
    <td style="text-align:left;">
    .
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:right;">
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    Treat
    </td>
    <td style="text-align:left;">
    25 (53.2%)
    </td>
    <td style="text-align:left;">
    .
    </td>
    <td style="text-align:left;">
    25 (100.0%)
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:right;">
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    Zwichen Ueberschrift
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:left;">
    </td>
    <td style="text-align:right;">
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    Age (median)
    </td>
    <td style="text-align:left;">
    43 (35, 59)
    </td>
    <td style="text-align:left;">
    51 (29, 62)
    </td>
    <td style="text-align:left;">
    41 (36, 55)
    </td>
    <td style="text-align:left;">
    age
    </td>
    <td style="text-align:right;">
    1
    </td>
    </tr>
    </tbody>
    </table>

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
