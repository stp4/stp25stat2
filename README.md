stp25stat2
================

Bietet ein einfaches und intuitives Formula und Pipe-freundliches
Framework, um grundlegende statistische Tests durchzuführen, zum
Beispiel t-Test, Wilcoxon-Test, ANOVA, Kruskal-Wallis und
Korrelationsanalysen.

Die Ausgabe jedes Tests wird automatisch in einen APA-Style conforme
Tabelle umgewandelt.

Zusätzliche Funktionen zum Umformen, Neuordnen, Manipulieren sind in
*spp25tools* enthalten. Funktionen zur Visualisierung sind in
*stp25plot* enthalten.

``` r
require(stp25stat2)
# which_output()
```

## Descriptive statistics

- `Tbll_desc`, `Tbll_desc_long()`, `Tbll_desc_item()` und
  `Tbll_desc_multi` : Berechnen deskriptive Statistiken für eine oder
  mehrere numerische Variablen. Kann mit gruppierten Daten umgehen.
- `Tbll_xtabs()`: Berechnet Kreuztabelle kategorialer Variablen.
- `Tbll_corr()`: Korrelationstest zwischen zwei oder mehr Variablen mit
  Pearson-, Spearman- oder Kendall-Methoden.

### Tbll_desc

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
# 
#  in stp25stat2/R/tbll_desc.R:246:6 
# $x
# # A tibble: 6 × 3
#   h__1__h breaks tension
#   <lgl>    <dbl> <fct>  
# 1 NA          26 L      
# 2 NA          30 L      
# 3 NA          54 L      
# 4 NA          25 L      
# 5 NA          70 L      
# 6 NA          52 L      
# 
# $digits
# h__1__h  breaks tension 
#       0       2       0 
# 
# $measure
#   h__1__h    breaks   tension 
#  "header" "numeric"  "factor" 
# 
# $row_name
#   h__1__h    breaks   tension 
#      "H1"  "breaks" "tension" 
# 
# $use.level
# [1] 1
# 
# $include.single.value
# [1] TRUE
# 
# 
#  in stp25stat2/R/tbll_desc.R:246:6 
# $x
# # A tibble: 6 × 3
#   h__1__h breaks tension
#   <lgl>    <dbl> <fct>  
# 1 NA          27 L      
# 2 NA          14 L      
# 3 NA          29 L      
# 4 NA          19 L      
# 5 NA          29 L      
# 6 NA          31 L      
# 
# $digits
# h__1__h  breaks tension 
#       0       2       0 
# 
# $measure
#   h__1__h    breaks   tension 
#  "header" "numeric"  "factor" 
# 
# $row_name
#   h__1__h    breaks   tension 
#      "H1"  "breaks" "tension" 
# 
# $use.level
# [1] 1
# 
# $include.single.value
# [1] TRUE
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

in stp25stat2/R/tbll_desc.R:246:6 \$x \# A tibble: 6 × 11 sex group age
alc smoker h\_\_1\_\_h diabet cardo <fct> <fct> <dbl> <fct> <lgl> <lgl>
<lgl> <lgl> 1 male Control 60.4 Occasion… FALSE NA TRUE FALSE 2 male
Control 25.3 Occasion… FALSE NA FALSE FALSE 3 female Control 30.2
Occasion… TRUE NA FALSE FALSE 4 male Control 25.2 Abuse FALSE NA FALSE
FALSE 5 male Control 27.7 Abuse FALSE NA FALSE FALSE 6 male Control 28.6
non FALSE NA TRUE FALSE \# … with 3 more variables: metabolic <lgl>,
infect <fct>, \# bili <dbl>

\$digits sex group age alc smoker h\_\_1\_\_h 0 1 0 1 0 0 diabet cardo
metabolic infect bili 0 0 0 1 3

\$measure sex group age alc smoker h\_\_1\_\_h “ratio” “factor” “median”
“factor” “logical” “header” diabet cardo metabolic infect bili “logical”
“logical” “logical” “multi” “median”

\$row_name sex group “Sex” “Group” age alc “Age” “Alcohol Consumption”
smoker h\_\_1\_\_h “Tobacco Smoker” “Clinical/Pretreatment” diabet cardo
“Diabetes” “Cardiovascular Disease” metabolic infect “Chronic Metabolic
Disease” “Chronic Infections” bili “Serum Bilirubin (mg/dl)”

\$use.level \[1\] 1

\$include.single.value \[1\] TRUE

in stp25stat2/R/tbll_desc.R:246:6 \$x \# A tibble: 6 × 11 sex group age
alc smoker h\_\_1\_\_h diabet cardo <fct> <fct> <dbl> <fct> <lgl> <lgl>
<lgl> <lgl> 1 female Treat 44.3 Occasional… FALSE NA FALSE FALSE 2
female Treat 19.5 Abuse FALSE NA FALSE FALSE 3 male Treat 38.5 non TRUE
NA TRUE FALSE 4 female Treat 35.7 Abuse TRUE NA FALSE FALSE 5 female
Treat 45.9 Abuse FALSE NA TRUE TRUE 6 male Treat 69.1 Abuse FALSE NA
FALSE FALSE \# … with 3 more variables: metabolic <lgl>, infect <fct>,
\# bili <dbl>

\$digits sex group age alc smoker h\_\_1\_\_h 0 1 0 1 0 0 diabet cardo
metabolic infect bili 0 0 0 1 3

\$measure sex group age alc smoker h\_\_1\_\_h “ratio” “factor” “median”
“factor” “logical” “header” diabet cardo metabolic infect bili “logical”
“logical” “logical” “multi” “median”

\$row_name sex group “Sex” “Group” age alc “Age” “Alcohol Consumption”
smoker h\_\_1\_\_h “Tobacco Smoker” “Clinical/Pretreatment” diabet cardo
“Diabetes” “Cardiovascular Disease” metabolic infect “Chronic Metabolic
Disease” “Chronic Infections” bili “Serum Bilirubin (mg/dl)”

\$use.level \[1\] 1

\$include.single.value \[1\] TRUE

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
# 
#  in stp25stat2/R/tbll_desc.R:246:6 
# $x
# # A tibble: 6 × 4
#   sex    group   h__1__h   age
#   <fct>  <fct>   <lgl>   <dbl>
# 1 male   Control NA       60.4
# 2 male   Control NA       25.3
# 3 female Control NA       30.2
# 4 male   Control NA       25.2
# 5 male   Control NA       27.7
# 6 male   Control NA       28.6
# 
# $digits
#     sex   group h__1__h     age 
#       0       1       0       0 
# 
# $measure
#      sex    group  h__1__h      age 
#  "ratio" "factor" "header" "median" 
# 
# $row_name
#                    sex                  group 
#                  "Sex"                "Group" 
#                h__1__h                    age 
# "Zwichen Ueberschrift"                  "Age" 
# 
# $use.level
# [1] 1
# 
# $include.single.value
# [1] TRUE
# 
# 
#  in stp25stat2/R/tbll_desc.R:246:6 
# $x
# # A tibble: 6 × 4
#   sex    group h__1__h   age
#   <fct>  <fct> <lgl>   <dbl>
# 1 female Treat NA       44.3
# 2 female Treat NA       19.5
# 3 male   Treat NA       38.5
# 4 female Treat NA       35.7
# 5 female Treat NA       45.9
# 6 male   Treat NA       69.1
# 
# $digits
#     sex   group h__1__h     age 
#       0       1       0       0 
# 
# $measure
#      sex    group  h__1__h      age 
#  "ratio" "factor" "header" "median" 
# 
# $row_name
#                    sex                  group 
#                  "Sex"                "Group" 
#                h__1__h                    age 
# "Zwichen Ueberschrift"                  "Age" 
# 
# $use.level
# [1] 1
# 
# $include.single.value
# [1] TRUE
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

### Tbll_likert

- `Tbll_likert()`: Univariate

``` r
Lik <- Tbll_likert(DF2,
                   Magazines, Comic.books, Fiction, Newspapers,
                   ReferenceZero=2)

Lik
# # A tibble: 4 × 5
#   Item        `low(1:1)` `neutral(2)` `high(3:5)` `M(SD)`   
# * <fct>       <chr>      <chr>        <chr>       <chr>     
# 1 Comic.books 4 (4.0%)   15 (15.0%)   81 (81.0%)  3.34 (1.0…
# 2 Newspapers  2 (2.0%)   19 (19.0%)   79 (79.0%)  3.21 (0.9…
# 3 Magazines   8 (8.0%)   22 (22.0%)   70 (70.0%)  3.11 (1.1…
# 4 Fiction     19 (19.0%) 36 (36.0%)   45 (45.0%)  2.46 (1.0…
```

    stp25plot::likertplot(
         Item  ~ . , 
         data = Lik)

### Tbll_xtabs

``` r
Tbll_xtabs( ~ induced + education + case,
            DF,
            margin = "case",
            include.count = FALSE)
# $xtab
# # A tibble: 9 × 4
#   induced education case_case case_control
# * <chr>   <chr>     <chr>     <chr>       
# 1 0       0-5yrs    " . "     "2.4%"      
# 2 0       6-11yrs   "25.3%"   "34.5%"     
# 3 0       12+ yrs   "31.3%"   "21.2%"     
# 4 1       0-5yrs    "2.4%"    " . "       
# 5 1       6-11yrs   "13.3%"   "9.7%"      
# 6 1       12+ yrs   "12.0%"   "17.6%"     
# 7 2       0-5yrs    "2.4%"    "2.4%"      
# 8 2       6-11yrs   "9.6%"    "4.2%"      
# 9 2       12+ yrs   "3.6%"    "7.9%"
```

``` r
Tbll_xtabs(
  ~ induced + education + case,
  DF,
  margin = "case",
  add.margins =1:3,
  include.count = FALSE,
  include.test =TRUE
)
# $xtab
# # A tibble: 16 × 5
#    induced education case_case case_control case_Sum
#  * <chr>   <chr>     <chr>     <chr>        <chr>   
#  1 0       0-5yrs    " . "     "2.4%"       2.4%    
#  2 0       6-11yrs   "25.3%"   "34.5%"      59.8%   
#  3 0       12+ yrs   "31.3%"   "21.2%"      52.5%   
#  4 0       Sum       "56.6%"   "58.2%"      114.8%  
#  5 1       0-5yrs    "2.4%"    " . "        2.4%    
#  6 1       6-11yrs   "13.3%"   "9.7%"       22.9%   
#  7 1       12+ yrs   "12.0%"   "17.6%"      29.6%   
#  8 1       Sum       "27.7%"   "27.3%"      55.0%   
#  9 2       0-5yrs    "2.4%"    "2.4%"       4.8%    
# 10 2       6-11yrs   "9.6%"    "4.2%"       13.9%   
# 11 2       12+ yrs   "3.6%"    "7.9%"       11.5%   
# 12 2       Sum       "15.7%"   "14.5%"      30.2%   
# 13 Sum     0-5yrs    "4.8%"    "4.8%"       9.7%    
# 14 Sum     6-11yrs   "48.2%"   "48.5%"      96.7%   
# 15 Sum     12+ yrs   "47.0%"   "46.7%"      93.7%   
# 16 Sum     Sum       "100.0%"  "100.0%"     200.0%  
# 
# $chisq_tests
# # A tibble: 1 × 3
#   Chisq    df p    
#   <chr> <dbl> <chr>
# 1 29.40    12 .003
```

### Tbll_corr

``` r
Tbll_corr( ~ a + b + c, dat)
# # A tibble: 3 × 4
#   Source    `(1)` `(2)`   `(3)`   
# * <chr>     <chr> <chr>   <chr>   
# 1 (1) Alpha "1"   ".17, " ".25, " 
# 2 (2) Beta  ""    "1"     "-.20, "
# 3 (3) Gamma ""    ""      "1"
Tbll_corr(a ~ c, dat)
# # A tibble: 1 × 4
#   Characteristics   c_N c_r   c_p.value
# * <chr>           <dbl> <chr> <chr>    
# 1 Alpha              40 .25   .119
Tbll_corr(a + b + c ~ d, dat)
# # A tibble: 3 × 4
#   Characteristics   d_N d_r   d_p.value
# * <chr>           <dbl> <chr> <chr>    
# 1 Alpha              40 .39   .012     
# 2 Beta               40 .83   <.001    
# 3 Gamma              40 -.15  .343
Tbll_corr(a + b + c ~ d, 
          dat, 
          groups = ~ g, 
          include.n=FALSE) %>%  kable
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Characteristics
</th>
<th style="text-align:left;">
Control_r
</th>
<th style="text-align:left;">
Control_p.value
</th>
<th style="text-align:left;">
Treat_r
</th>
<th style="text-align:left;">
Treat_p.value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Alpha
</td>
<td style="text-align:left;">
.50
</td>
<td style="text-align:left;">
.025
</td>
<td style="text-align:left;">
.13
</td>
<td style="text-align:left;">
.587
</td>
</tr>
<tr>
<td style="text-align:left;">
Beta
</td>
<td style="text-align:left;">
.69
</td>
<td style="text-align:left;">
.001
</td>
<td style="text-align:left;">
.88
</td>
<td style="text-align:left;">
\<.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Gamma
</td>
<td style="text-align:left;">
-.11
</td>
<td style="text-align:left;">
.651
</td>
<td style="text-align:left;">
-.36
</td>
<td style="text-align:left;">
.114
</td>
</tr>
</tbody>
</table>

    stp25plot::corr_plot( ~ a + b + c + d, 
                           dat,
                           resize=TRUE
     )

## Reliability

- `Tbll_desc_item()`, `Tbll_reliability()` and `Tbll_item_analysis()`:

``` r
Tbll_desc_item( ~ A + B + C + D + E, df)
# # A tibble: 5 × 8
#   variable n     M     SD    Range     Skew  Kurtosi Shapi…¹
# * <chr>    <chr> <chr> <chr> <chr>     <chr> <chr>   <chr>  
# 1 A        15    2.53  1.77  [1.00, 5… 0.46  -1.69   W=0.76…
# 2 B        15    2.67  1.68  [1.00, 5… 0.41  -1.64   W=0.80…
# 3 C        15    3.07  1.58  [1.00, 5… -0.10 -1.59   W=0.86…
# 4 D        15    3.47  1.77  [1.00, 5… -0.46 -1.69   W=0.76…
# 5 E        15    2.67  1.72  [1.00, 5… 0.33  -1.71   W=0.80…
# # … with abbreviated variable name ¹​Shapiro.Test
```

``` r
Tbll_reliability( ~ A + B + C + D + E, df,
                 include.item_statistics = TRUE)
# $item_statistics
# # A tibble: 5 × 5
#   Items     n M     SD    Alpha.if.Deleted
# * <chr> <int> <chr> <chr> <chr>           
# 1 A        15 2.53  1.77  0.90            
# 2 B        15 2.67  1.68  0.95            
# 3 C        15 3.07  1.58  0.95            
# 4 D        15 3.47  1.77  0.77            
# 5 E        15 2.67  1.72  0.92            
# 
# $scale_statistics
# # A tibble: 1 × 9
#   Items     n M     SD    Range  Skew  Kurtosi Shapi…¹ Alpha
#   <int> <int> <chr> <chr> <chr>  <chr> <chr>   <chr>   <chr>
# 1     5    15 2.88  1.59  1.00;… 0.21  -1.59   W=0.86… 0.96 
# # … with abbreviated variable name ¹​Shapiro.Test
# 
# $index
#  [1] 1.0 2.0 3.0 4.2 5.0 2.6 2.6 2.2 2.6 1.0 1.0 1.0 5.0 5.0
# [15] 5.0
# 
# $keys
# [1] 1 1 1 1 1
# 
# attr(,"class")
# [1] "list"        "psych_alpha"
```

``` r
Tbll_item_analysis(  A + B + C + D + E ~ index, df,
                     include.Sample.SD =  TRUE,
                     include.Item.total = TRUE,   #  (unkorrigierte) Trennschärfe
           
                     include.Item.Tot.woi = TRUE, # korrigierte Trennschärfe Alpha if Item Deleted
                     include.Difficulty = TRUE,    # Itemschwierigkeit
                     include.Discrimination = TRUE,
                     
                     include.Item.Reliab = TRUE,
                     include.Item.Rel.woi = TRUE
                     )
#   Items Difficulty Sample.SD Item.total Alpha.if.Deleted
# 1     A       0.38      0.44       0.94             0.90
# 2     B       0.42      0.42       0.97             0.95
# 3     C       0.52      0.39       0.97             0.95
# 4     D       0.62      0.44       0.85             0.77
# 5     E       0.42      0.43       0.95             0.92
#   Discrimination Item.Reliab Item.Rel.woi Item.Criterion
# 1           0.90        0.40         0.39           0.94
# 2           0.90        0.39         0.38           0.97
# 3           0.90        0.37         0.36           0.97
# 4           0.95        0.36         0.33           0.85
# 5           0.90        0.40         0.38           0.95
#   Item.Validity
# 1          0.40
# 2          0.39
# 3          0.37
# 4          0.36
# 5          0.40
```

### Síg. Test

``` r
t1 <- wilcox.test(mpg ~ vs, mtcars)
t1
# 
#   Wilcoxon rank sum test with continuity correction
# 
# data:  mpg by vs
# W = 22.5, p-value = 9.034e-05
# alternative hypothesis: true location shift is not equal to 0
Tbll(t1)
# # A tibble: 1 × 3
#   Source    W     p.value
# * <chr>     <chr> <chr>  
# 1 mpg by vs 22.50 <.001
APA(t1)
# [1] "W=22.50, p<.001"
```

### Regression

``` r

 lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
 lm2 <- lm(breaks ~ wool * tension, data = warpbreaks)


 Tbll_reg(
  lm1,
  lm2,
  include.p = FALSE,
  include.ci = TRUE,
  include.se=FALSE
)
# # A tibble: 12 × 5
#    term                   lm1_b      lm1_conf  lm2_b lm2_c…¹
#    <chr>                  <chr>      <chr>     <chr> <chr>  
#  1 (Intercept)            "39.3***"  [45.6, 3… "44.… [51.9,…
#  2 wool[T.B]              "-5.78"    [0.573, … "-16… [-5.96…
#  3 tension[T.M]           " -10*"    [-2.22, … "-20… [-10.2…
#  4 tension[T.H]           "-14.7***" [-6.94, … " -2… [-9.63…
#  5 wool[T.B]:tension[T.M]  <NA>      <NA>      "21.… [35.8,…
#  6 wool[T.B]:tension[T.H]  <NA>      <NA>      "10.… [25.2,…
#  7 R2                     "0.27"     <NA>      "0.3… <NA>   
#  8 adj. R2                "0.23"     <NA>      "0.3… <NA>   
#  9 AIC                    "424.0"    <NA>      "419… <NA>   
# 10 BIC                    "433.9"    <NA>      "433… <NA>   
# 11 RMSE                   "11.18"    <NA>      "10.… <NA>   
# 12 Obs                    "54"       <NA>      "54"  <NA>   
# # … with abbreviated variable name ¹​lm2_conf


 Tbll_reg(
   lm2,
   include.p = TRUE,
   include.beta = TRUE,
   include.gof = FALSE,
   include.statistic = TRUE,
   include.stars = TRUE
 )
# # A tibble: 6 × 6
#   term                   b         conf  beta  stati…¹ p    
#   <chr>                  <chr>     <chr> <chr> <chr>   <chr>
# 1 (Intercept)            "44.6***" [51.… ""    12.22   <.001
# 2 wool[T.B]              "-16.3**" [-5.… "-0.… -3.17   .003 
# 3 tension[T.M]           "-20.6**… [-10… "-0.… -3.99   <.001
# 4 tension[T.H]           " -20***" [-9.… "-0.… -3.88   <.001
# 5 wool[T.B]:tension[T.M] "21.1**"  [35.… "0.6… 2.89    .006 
# 6 wool[T.B]:tension[T.H] "10.6"    [25.… "0.3… 1.45    .154 
# # … with abbreviated variable name ¹​statistic
```

``` r
 counts <- c(18,17,15,20,10,20,25,13,12)
 outcome <- gl(3,1,9)
 treatment <- gl(3,3)
 data.frame(treatment, outcome, counts) # showing data
#   treatment outcome counts
# 1         1       1     18
# 2         1       2     17
# 3         1       3     15
# 4         2       1     20
# 5         2       2     10
# 6         2       3     20
# 7         3       1     25
# 8         3       2     13
# 9         3       3     12
 lm.D93 <- lm(counts ~ outcome + treatment )
 glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
 Tbll_reg(lm.D93, glm.D93, digits=2)
# # A tibble: 14 × 5
#    term           lm.D93_b lm.D93_conf     glm.D93_b glm.D…¹
#    <chr>          <chr>    <chr>           <chr>     <chr>  
#  1 (Intercept)    21.00**  [30.45, 11.55]  3.04***   [3.37,…
#  2 outcome[T.2]   -7.67    [2.68, -18.01]  -0.45*    [-0.06…
#  3 outcome[T.3]   -5.33    [5.01, -15.68]  -0.29     [0.08,…
#  4 treatment[T.2] 0.00     [10.35, -10.35] 0.00      [0.39,…
#  5 treatment[T.3] 0.00     [10.35, -10.35] 0.00      [0.39,…
#  6 R2             0.53     <NA>            <NA>      <NA>   
#  7 adj. R2        0.05     <NA>            <NA>      <NA>   
#  8 AIC            57.6     <NA>            56.8      <NA>   
#  9 BIC            58.8     <NA>            57.7      <NA>   
# 10 RMSE           3.04     <NA>            0.19      <NA>   
# 11 McFadden       <NA>     <NA>            0.10      <NA>   
# 12 CoxSnell       <NA>     <NA>            0.45      <NA>   
# 13 Nagelkerke     <NA>     <NA>            0.46      <NA>   
# 14 Obs            9        <NA>            9         <NA>   
# # … with abbreviated variable name ¹​glm.D93_conf
 Tbll_reg(lm.D93, glm.D93, digits=c(0,2,2,1,1))
# # A tibble: 14 × 5
#    term           lm.D93_b lm.D93_conf    glm.D93_b glm.D9…¹
#    <chr>          <chr>    <chr>          <chr>     <chr>   
#  1 (Intercept)    21**     [30, 12]       3***      [3, 3]  
#  2 outcome[T.2]   -7.67    [2.68, -18.01] -0.45*    [-0.06,…
#  3 outcome[T.3]   -5.33    [5.01, -15.68] -0.29     [0.08, …
#  4 treatment[T.2] 0.0      [10.3, -10.3]  0.0       [0.4, -…
#  5 treatment[T.3] 0.0      [10.3, -10.3]  0.0       [0.4, -…
#  6 R2             0.53     <NA>           <NA>      <NA>    
#  7 adj. R2        0.05     <NA>           <NA>      <NA>    
#  8 AIC            57.6     <NA>           56.8      <NA>    
#  9 BIC            58.8     <NA>           57.7      <NA>    
# 10 RMSE           3.04     <NA>           0.19      <NA>    
# 11 McFadden       <NA>     <NA>           0.10      <NA>    
# 12 CoxSnell       <NA>     <NA>           0.45      <NA>    
# 13 Nagelkerke     <NA>     <NA>           0.46      <NA>    
# 14 Obs            9        <NA>           9         <NA>    
# # … with abbreviated variable name ¹​glm.D93_conf
```
