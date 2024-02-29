stp25stat2
================

This library provides a simple and intuitive formula and pipe-based
framework for performing basic statistical tests such as t-test,
Wilcoxon test, ANOVA, Kruskal-Wallis and correlation analysis.

The output of each test is automatically converted into an APA-style
table.

Additional functions for transforming, rearranging and manipulating are
included in *spp25tools*. Visualisation functions are included in
*stp25plot*.

``` r
require(stp25stat2)
# which_output()
```

## Descriptive statistics

- `Tbll_desc`, `Tbll_desc_long()`, `Tbll_desc_item()` und
  `Tbll_desc_multi`: Calculate descriptive statistics for one or more
  numerical variables. Can deal with grouped data.
- `Tbll_xtabs()`: Calculates crosstabs of categorical variables.
- `Tbll_corr()`: Correlation test between two or more variables using
  Pearson, Spearman or Kendall methods.

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

warpbreaks |> Tbll_desc(~breaks + wool)
# # A tibble: 5 × 2
#   Item            m              
# * <chr>           <chr>          
# 1 "(N) "          "54"           
# 2 "breaks (mean)" "28.15 (13.20)"
# 3 "wool "         ""             
# 4 "    A"         "50% (27)"     
# 5 "    B"         "50% (27)"

warpbreaks |> Tbll_desc(breaks, wool)
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
warpbreaks |> Tbll_desc(
  "H1",
  breaks,
  tension,
  by = ~ wool,
  include.total = TRUE,
  include.n = FALSE,
  include.test = TRUE) |> kable()
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
mtcars |> Tbll_desc_long(
  mpg[mean, 1],  cyl[median],  disp[0],
  hp[0],  drat,  wt,  qsec[1],  vs[freq],
  am[freq],  gear,  carb,
  include.range = TRUE,  include.n = TRUE
) |> kable(caption ="summary statistics" )
```

| Item        |   n | m                                   |
|:------------|----:|:------------------------------------|
| mpg(mean)   |  32 | 20.1 (SD 6.0, range 10.4 to 33.9)   |
| cyl(median) |  32 | 6.00 (IQR 4.00, range 4.00 to 8.00) |
| disp(mean)  |  32 | 231 (SD 124, range 71 to 472)       |
| hp(mean)    |  32 | 147 (SD 69, range 52 to 335)        |
| drat(mean)  |  32 | 3.60 (SD 0.53, range 2.76 to 4.93)  |
| wt(mean)    |  32 | 3.22 (SD 0.98, range 1.51 to 5.42)  |
| qsec(mean)  |  32 | 17.8 (SD 1.8, range 14.5 to 22.9)   |
| vs (0/1)    |  32 | 18/14                               |
| am (0/1)    |  32 | 19/13                               |
| gear(mean)  |  32 | 3.69 (SD 0.74, range 3.00 to 5.00)  |
| carb(mean)  |  32 | 2.81 (SD 1.62, range 1.00 to 8.00)  |

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
DF |>
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
  ) |> stp25output2::Output()
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 1: group (N=47) (N=47)
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
0 ( . )
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
0 ( . )
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

Hinzufügen von zusätzliche Spalten mit **include.value**. Vector oder
data.frame in exact der Reihenfolge wie die meassure-variablen.

``` r
Info <- data.frame( var_name=c("age", "sex"), y=1:2)
rownames(Info) <- c("age", "sex" )

DF |>
  Tbll_desc(
    sex[ratio],
    group[freq],
    "Zwichen Ueberschrift",
    age[median],
    by = ~ group,
    include.total = TRUE,
    include.value = Info,
    include.nr = TRUE
  ) |> kable()
```

| Item                 | Total       | Control     | Treat       | var_name |   y |
|:---------------------|:------------|:------------|:------------|:---------|----:|
| \(N\)                | 47          | 22          | 25          |          |     |
| Sex (male:female)    | 25:22       | 10:12       | 15:10       | sex      |   2 |
| Group                |             |             |             |          |     |
| Control              | 22 (46.8%)  | 22 (100.0%) | 0 ( . )     |          |     |
| Treat                | 25 (53.2%)  | 0 ( . )     | 25 (100.0%) |          |     |
| Zwichen Ueberschrift |             |             |             |          |     |
| Age (median)         | 43 (35, 59) | 51 (29, 62) | 41 (36, 55) | age      |   1 |

### Tbll_likert

- `Tbll_likert()`: Univariate

``` r
Lik <- Tbll_likert(DF2,
                   Magazines, Comic.books, Fiction, Newspapers,
                   ReferenceZero=2)

Lik
# # A tibble: 4 × 5
#   Item        `low(1:1)` `neutral(2)` `high(3:5)` `M(SD)`   
#   <fct>       <chr>      <chr>        <chr>       <chr>     
# 1 Magazines   8 (8.0%)   22 (22.0%)   70 (70.0%)  3.11 (1.1…
# 2 Comic.books 4 (4.0%)   15 (15.0%)   81 (81.0%)  3.34 (1.0…
# 3 Fiction     19 (19.0%) 36 (36.0%)   45 (45.0%)  2.46 (1.0…
# 4 Newspapers  2 (2.0%)   19 (19.0%)   79 (79.0%)  3.21 (0.9…
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
# # A tibble: 9 × 3
#   induced_education case_case case_control
# * <chr>             <chr>     <chr>       
# 1 0_0-5yrs          " . "     "2.4%"      
# 2 0_6-11yrs         "25.3%"   "34.5%"     
# 3 0_12+ yrs         "31.3%"   "21.2%"     
# 4 1_0-5yrs          "2.4%"    " . "       
# 5 1_6-11yrs         "13.3%"   "9.7%"      
# 6 1_12+ yrs         "12.0%"   "17.6%"     
# 7 2_0-5yrs          "2.4%"    "2.4%"      
# 8 2_6-11yrs         "9.6%"    "4.2%"      
# 9 2_12+ yrs         "3.6%"    "7.9%"
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
# # A tibble: 16 × 4
#    induced_education case_case case_control case_Sum
#  * <chr>             <chr>     <chr>        <chr>   
#  1 0_0-5yrs          " . "     "2.4%"       2.4%    
#  2 0_6-11yrs         "25.3%"   "34.5%"      59.8%   
#  3 0_12+ yrs         "31.3%"   "21.2%"      52.5%   
#  4 0_Sum             "56.6%"   "58.2%"      114.8%  
#  5 1_0-5yrs          "2.4%"    " . "        2.4%    
#  6 1_6-11yrs         "13.3%"   "9.7%"       22.9%   
#  7 1_12+ yrs         "12.0%"   "17.6%"      29.6%   
#  8 1_Sum             "27.7%"   "27.3%"      55.0%   
#  9 2_0-5yrs          "2.4%"    "2.4%"       4.8%    
# 10 2_6-11yrs         "9.6%"    "4.2%"       13.9%   
# 11 2_12+ yrs         "3.6%"    "7.9%"       11.5%   
# 12 2_Sum             "15.7%"   "14.5%"      30.2%   
# 13 Sum_0-5yrs        "4.8%"    "4.8%"       9.7%    
# 14 Sum_6-11yrs       "48.2%"   "48.5%"      96.7%   
# 15 Sum_12+ yrs       "47.0%"   "46.7%"      93.7%   
# 16 Sum_Sum           "100.0%"  "100.0%"     200.0%  
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
          include.n=FALSE) |>  kable()
```

| Characteristics | Control_r | Control_p.value | Treat_r | Treat_p.value |
|:----------------|:----------|:----------------|:--------|:--------------|
| Alpha           | .50       | .025            | .13     | .587          |
| Beta            | .69       | .001            | .88     | \<.001        |
| Gamma           | -.11      | .651            | -.36    | .114          |

    stp25plot::corr_plot( ~ a + b + c + d, 
                           dat,
                           resize=TRUE
     )

## Reliability

- `Tbll_desc_item()`, `Tbll_reliability()` and `Tbll_item_analysis()`:

``` r
Tbll_desc_item( ~ A + B + C + D + E, df)
# # A tibble: 5 × 8
#   variable n     M     SD    Range        Skew  Kurtosi
#   <chr>    <chr> <chr> <chr> <chr>        <chr> <chr>  
# 1 A        15    2.53  1.77  [1.00, 5.00] 0.46  -1.69  
# 2 B        15    2.67  1.68  [1.00, 5.00] 0.41  -1.64  
# 3 C        15    3.07  1.58  [1.00, 5.00] -0.10 -1.59  
# 4 D        15    3.47  1.77  [1.00, 5.00] -0.46 -1.69  
# 5 E        15    2.67  1.72  [1.00, 5.00] 0.33  -1.71  
# # ℹ 1 more variable: Shapiro.Test <chr>
```

``` r
Tbll_reliability( ~ A + B + C + D + E, df,
                 include.item_statistics = TRUE)
# $item_statistics
# # A tibble: 5 × 5
#   Items     n M     SD    Trennschaerfe
# * <chr> <int> <chr> <chr> <chr>        
# 1 A        15 2.53  1.77  0.90         
# 2 B        15 2.67  1.68  0.95         
# 3 C        15 3.07  1.58  0.95         
# 4 D        15 3.47  1.77  0.77         
# 5 E        15 2.67  1.72  0.92         
# 
# $scale_statistics
# # A tibble: 1 × 9
#   Items     n M     SD    Range   Skew  Kurtosi Shapiro.Test
#   <int> <int> <chr> <chr> <chr>   <chr> <chr>   <chr>       
# 1     5    15 2.88  1.59  1.00; … 0.21  -1.59   W=0.86, p=.…
# # ℹ 1 more variable: Alpha <chr>
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
#   Items Difficulty Sample.SD Item.total Trennschaerfe
# 1     A       0.38      0.44       0.94          0.90
# 2     B       0.42      0.42       0.97          0.95
# 3     C       0.52      0.39       0.97          0.95
# 4     D       0.62      0.44       0.85          0.77
# 5     E       0.42      0.43       0.95          0.92
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

### Sig. Test

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

## Regression

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
#    term                   lm1_b      lm1_conf lm2_b lm2_conf
#    <chr>                  <chr>      <chr>    <chr> <chr>   
#  1 (Intercept)            "39.3***"  [45.6, … "44.… [51.9, …
#  2 wool[T.B]              "-5.78"    [0.573,… "-16… [-5.96,…
#  3 tension[T.M]           " -10*"    [-2.22,… "-20… [-10.2,…
#  4 tension[T.H]           "-14.7***" [-6.94,… " -2… [-9.63,…
#  5 wool[T.B]:tension[T.M]  <NA>      <NA>     "21.… [35.8, …
#  6 wool[T.B]:tension[T.H]  <NA>      <NA>     "10.… [25.2, …
#  7 R2                     "0.27"     <NA>     "0.3… <NA>    
#  8 adj. R2                "0.23"     <NA>     "0.3… <NA>    
#  9 AIC                    "424.0"    <NA>     "419… <NA>    
# 10 BIC                    "433.9"    <NA>     "433… <NA>    
# 11 RMSE                   "11.18"    <NA>     "10.… <NA>    
# 12 Obs                    "54"       <NA>     "54"  <NA>


 Tbll_reg(
   lm2,
   include.p = TRUE,
   include.beta = TRUE,
   include.gof = FALSE,
   include.statistic = TRUE,
   include.stars = TRUE
 )
# # A tibble: 6 × 6
#   term                   b       conf  beta  statistic p    
#   <chr>                  <chr>   <chr> <chr> <chr>     <chr>
# 1 (Intercept)            "44.6*… [51.… ""    12.22     <.001
# 2 wool[T.B]              "-16.3… [-5.… "-0.… -3.17     .003 
# 3 tension[T.M]           "-20.6… [-10… "-0.… -3.99     <.001
# 4 tension[T.H]           " -20*… [-9.… "-0.… -3.88     <.001
# 5 wool[T.B]:tension[T.M] "21.1*… [35.… "0.6… 2.89      .006 
# 6 wool[T.B]:tension[T.H] "10.6"  [25.… "0.3… 1.45      .154
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
#    term          lm.D93_b lm.D93_conf glm.D93_b glm.D93_conf
#    <chr>         <chr>    <chr>       <chr>     <chr>       
#  1 (Intercept)   21.00**  [30.45, 11… 3.04***   [3.37, 2.70]
#  2 outcome[T.2]  -7.67    [2.68, -18… -0.45*    [-0.06, -0.…
#  3 outcome[T.3]  -5.33    [5.01, -15… -0.29     [0.08, -0.6…
#  4 treatment[T.… 0.00     [10.35, -1… 0.00      [0.39, -0.3…
#  5 treatment[T.… 0.00     [10.35, -1… 0.00      [0.39, -0.3…
#  6 R2            0.53     <NA>        <NA>      <NA>        
#  7 adj. R2       0.05     <NA>        <NA>      <NA>        
#  8 AIC           57.6     <NA>        56.8      <NA>        
#  9 BIC           58.8     <NA>        57.7      <NA>        
# 10 RMSE          3.04     <NA>        0.19      <NA>        
# 11 McFadden      <NA>     <NA>        0.10      <NA>        
# 12 CoxSnell      <NA>     <NA>        0.45      <NA>        
# 13 Nagelkerke    <NA>     <NA>        0.46      <NA>        
# 14 Obs           9        <NA>        9         <NA>
 Tbll_reg(lm.D93, glm.D93, digits=c(0,2,2,1,1))
# # A tibble: 14 × 5
#    term          lm.D93_b lm.D93_conf glm.D93_b glm.D93_conf
#    <chr>         <chr>    <chr>       <chr>     <chr>       
#  1 (Intercept)   21**     [30, 12]    3***      [3, 3]      
#  2 outcome[T.2]  -7.67    [2.68, -18… -0.45*    [-0.06, -0.…
#  3 outcome[T.3]  -5.33    [5.01, -15… -0.29     [0.08, -0.6…
#  4 treatment[T.… 0.0      [10.3, -10… 0.0       [0.4, -0.4] 
#  5 treatment[T.… 0.0      [10.3, -10… 0.0       [0.4, -0.4] 
#  6 R2            0.53     <NA>        <NA>      <NA>        
#  7 adj. R2       0.05     <NA>        <NA>      <NA>        
#  8 AIC           57.6     <NA>        56.8      <NA>        
#  9 BIC           58.8     <NA>        57.7      <NA>        
# 10 RMSE          3.04     <NA>        0.19      <NA>        
# 11 McFadden      <NA>     <NA>        0.10      <NA>        
# 12 CoxSnell      <NA>     <NA>        0.45      <NA>        
# 13 Nagelkerke    <NA>     <NA>        0.46      <NA>        
# 14 Obs           9        <NA>        9         <NA>
```
