require(stp25stat2)

# DATA --------------------------------------------------------------------


DF <-  stp25tools::get_data("
id    Group Outcome    sex age    Cys   Glu    Gln HCys   His  Kyn
1      Case     Bad   male   9 23.357 301.7  594.9 2.52 103.1 3.23
2      Case    Good   male  22 33.128 354.1  348.3 4.42  90.7 3.89
3      Case    Good   male  25 27.131 317.7  415.2 3.03  84.3 2.98
4      Case    Good   male   9 21.654 301.7  371.7 2.67  79.2 3.06
5      Case     Bad female  16 41.243 359.1  324.5 6.92  76.5 2.03
6      Case    Good   male  23 31.525 367.8  355.0 6.06  85.0 3.25
7      Case    Good female  21 23.627 304.1  302.0 3.03  80.6 2.06
8      Case     Bad female  20 29.300 249.1  388.5 5.04  69.3 2.17
9      Case    Good   male  13 21.625 302.9  435.3 2.75  98.2 2.64
10     Case    Good female  29 29.324 438.1  298.5 5.57  84.2 2.59
11     Case    Good   male  14 22.166 271.0  301.4 3.16  79.6 2.02
12     Case    Good   male  22 24.492 249.4  516.8 4.02  75.9 2.67
13     Case    Good female  26 30.906 213.1  419.3 4.19  95.1 1.83
14     Case    Good   male  12 24.696 380.3  163.0 2.78  57.6 1.36
15     Case    Good   male  30 23.026 537.4  305.8 2.22 105.2 3.80
16     Case     Bad female  10 20.668 347.1  246.4 2.29  72.6 3.63
17     Case     Bad   male  38 33.610 421.1  329.3 4.94 102.5 3.89
18     Case    Good   male  32 27.527 330.1  440.6 3.77  80.0 2.06
19     Case     Bad female  27 32.750 364.1  284.3 4.64  79.6 2.03
20     Case     Bad female  12 30.272 221.1  434.7 3.47  66.3 2.05
21     Case     Bad female  18 24.520 342.1  316.2 2.55  74.0 2.12
22     Case    Good female  10 25.552 342.1  323.3 3.35  71.3 2.38
23     Case    Good female  17 29.008 243.1  569.7 6.01  97.1 1.84
24     Case    Good   male  19 28.963 296.0  429.7 2.84  94.9 2.94
25     Case    Good female  22 35.257 176.5  531.3 3.70 103.0 2.37
26     Case    Good   male  26 35.591 273.3  549.2 5.50  87.8 3.58
27     Case    Good   male  30 38.154 260.7  625.1 5.12  94.1 2.76
28     Case    Good female  33 31.823 375.7  270.1 4.61  92.5 1.83
29     Case    Good   male   6 25.893 214.1  445.3 2.90  89.3 3.07
30     Case    Good   male  17 30.518 194.1  742.6 4.40  86.1 2.90
31     Case    Good   male  10 33.443 153.0  458.4 5.95  81.1 2.55
32     Case    Good   male  13 21.793 459.1  496.3 2.46  75.6 1.36
33     Case    Good   male  15 30.816 197.0  541.4 3.46  82.9 2.78
34     Case    Good   male  17 31.589 279.1  273.6 5.68  82.8 3.10
35     Case    Good female  29 32.798 111.2  675.3 5.04  86.1 2.24
36     Case    Good   male  19 30.817 167.0  710.6 5.41  70.7 2.17
37     Case     Bad   male  13 26.466 121.8  946.6 2.90 108.7 2.24
38     Case     Bad   male  26 27.532 306.1  514.0 5.34  98.5 2.95
39     Case    Good female   8 31.329 124.1  910.8 3.34 104.1 2.36
40     Case    Good female  15 24.086 171.9  495.6 2.60  77.4 2.05
41     Case    Good   male  14 23.153 205.1  503.4 3.42  80.3 2.51
42     Case    Good   male  18 25.495 249.1  624.2 4.97  74.9 2.64
43     Case    Good female  16 30.048 137.8  672.0 4.47  82.3 3.06
44     Case    Good female  22 21.523 164.0  654.1 3.65  97.5 2.27
45     Case    Good   male  20 37.550 105.0  697.6 7.38  94.4 3.24
46     Case    Good   male   9 26.348 117.0  761.6 3.43  84.7 3.47
47     Case    Good female   7 17.926 113.9  735.6 2.24  94.4 2.08
48     Case    Good female  10 20.850  62.8  732.0 3.91  93.5 3.06
49     Case    Good female   4 26.424  85.2  728.9 3.22  71.8 1.89
50     Case    Good female  18 28.085  49.1  670.8 8.35  72.5 3.17
51     Case    Good female  21 34.440  52.1  592.7 5.42  71.2 2.57
52     Case    Good   male  22 44.021  57.6  813.7 5.67  95.2 1.99
53  Control Control   male   9 49.399  52.1  817.1 2.83 105.1 2.06
54  Control Control   male  22 74.876  51.0  816.0 6.59 108.3 3.05
55  Control Control   male  25 80.335  56.1  651.9 8.09  86.5 2.23
56  Control Control   male   9 54.171  55.1  725.5 4.97  96.8 3.08
57  Control Control female  16 60.658  42.4 1128.7 8.42 118.8 2.28
58  Control Control   male  23 66.186  78.0  755.7 4.08  88.9 2.36
59  Control Control female  21 37.190  44.2  633.7 2.05  84.7 2.69
60  Control Control female  20 61.454  37.4  726.0 5.20  78.3 1.47
61  Control Control   male  13 54.981  94.0  851.7 5.94  94.7 2.46
62  Control Control female  29 65.322  22.6  752.1 5.04 100.7 2.28
63  Control Control   male  14 54.756  43.4  684.2 4.30 109.8 2.49
64  Control Control   male  22 73.412  75.8 1053.7 8.54 127.1 2.79
65  Control Control female  26 53.085  38.1  601.7 5.56  85.6 1.91
66  Control Control   male  12 64.649  65.4  548.1 5.82  99.6 2.71
67  Control Control   male  30 56.920  54.2  909.7 4.90 143.4 1.85
68  Control Control female  10 57.845  39.8  830.3 3.86  99.8 2.41
69  Control Control   male  38 59.096  71.2  696.4 6.51  79.5 1.26
70  Control Control   male  32 62.638  57.6  636.0 5.78  76.0 1.66
71  Control Control female  27 69.087  91.4  756.8 9.22 103.3 1.90
72  Control Control female  12 55.659  58.4  777.0 4.43  91.5 2.27
73  Control Control female  18 49.532  28.5  666.8 5.97  83.8 2.32
74  Control Control female  10 56.143  74.8  819.6 4.32  95.6 1.89
75  Control Control female  17 52.690  75.1  633.7 4.72 106.5 1.65
76  Control Control   male  19 64.502  41.4  759.0 5.21  81.9 2.77
77  Control Control female  22 61.302  46.8  570.4 5.10  94.2 1.84
78  Control Control   male  26 62.612  33.4  878.5 8.86 102.5 1.79
79  Control Control   male  30 55.807  36.8  763.5 5.33  93.9 1.99
80  Control Control female  33 73.272  96.6  706.6 6.52  92.7 2.01
81  Control Control   male   6 41.234  46.6  857.5 2.65  99.7 2.05
82  Control Control   male  17 59.038  55.3  797.1 8.69 103.8 2.20
83  Control Control   male  10 65.016  52.0  798.3 6.14 100.0 2.50
84  Control Control   male  13 63.769  82.1  569.7 7.03  80.3 2.51
85  Control Control   male  15 84.185  56.5  656.3 9.32  89.9 3.54
86  Control Control   male  17 61.014  50.1  652.6 6.33  88.4 2.09
87  Control Control female  29 80.554  55.7 1032.5 5.56 122.4 2.00
88  Control Control   male  19 61.019 100.0  715.4 8.42  87.0 2.98
89  Control Control   male  13 64.220  55.5  788.0 6.96 104.5 2.41
90  Control Control   male  26 60.162  48.7  977.1 7.87  97.0 2.64
91  Control Control female   8 61.590  78.1  774.7 3.91 105.2 2.33
92  Control Control female  15 53.914  56.6  750.1 5.26  86.2 2.04
93  Control Control   male  14 71.041  56.5  700.0 6.90  81.2 2.68
94  Control Control   male  18 79.614  56.0  759.2 8.87  91.4 2.20
95  Control Control female  16 59.368  41.6  650.8 5.09 105.4 2.10
96  Control Control female  22 64.576  48.8  740.1 6.76  92.9 2.73
97  Control Control   male  20 66.421  54.9  829.1 6.25  99.6 2.67
98  Control Control   male   9 49.966  49.4  774.6 3.49  85.3 1.80
99  Control Control female   7 41.872  74.9  898.6 2.39  98.8 2.74
100 Control Control female  10 55.919  76.6  782.9 5.53  88.1 2.61
101 Control Control female  4  37.257  83.9  708.8 2.24  86.9 2.13
102 Control Control female  18 68.435  72.0  889.6 6.99 152.8 2.60
103 Control Control female  21 74.301  35.1  676.4 5.86  94.4 1.96
104 Control Control  male   22 58.237  39.2 1090.8 6.13  98.3 1.69")

DF$Group <- factor(DF$Group, c("Control", "Case"))
DF$Outcome <- factor(DF$Outcome, c("Control", "Good", "Bad"))
# MANOVA ------------------------------------------------------------------
require(effects)
fit_lm <-
  lm(cbind(Cys, Glu, Gln ,HCys, His, Kyn) ~ Group, DF)
fit_aov <-
  manova(cbind(Cys, Glu, Gln, HCys, His, Kyn) ~ Group , DF)
 Tbll(fit_aov)


require(MASS)
require(ggplot2)
fit_lda <- MASS::lda(Group ~ Cys + Glu + Gln + HCys + His + Kyn, data=DF)
#fit_lda <- MASS::lda(Outcome ~  HCys + His + Kyn, data=DF)
Tbll(fit_lda) |> stp25output2::Output()


rslt <-   Tbll(fit_lda)

#head(rslt$predict )


#pROC::roc(dat$Group,  dat$Control, plot=TRUE, legacy.axes = TRUE)


X <- DF[Cs(Cys, Glu, Gln, HCys, His, Kyn)]
X <- apply(X , 2, scale)
rslt <- psych::principal(X , 1)
rslt  |>  Tbll_pca_loadings()

#BiocManager::install('mixOmics')
library(mixOmics)

result.pca.multi <- pca(X)   # run the method
plotIndiv(result.pca.multi,
          comp = c(1, 2),   # Specify components to plot
          ind.names = TRUE, # Show row names of samples
          group = DF$Outcome,
          title = 'ABC transporters, PCA comp 1 - 2',
          legend = TRUE, legend.title = 'Cell line')
plotVar(result.pca.multi)    # plot the variables

biplot(result.pca.multi,
       group = DF$Outcome,
       col.per.group = as.vector(farbe(n=3)),
       legend.title = 'Cell line')

result.pca.multi
selectVar(result.pca.multi, comp = 1)$value


# result.spca.multi <- spca(X )  # run the method
# plotIndiv(result.spca.multi)  # plot the samples
# plotVar(result.spca.multi)    # plot the variables
