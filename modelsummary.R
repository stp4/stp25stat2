library(modelsummary)
require(stp25output2)
require(stp25stat2)
url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv'
dat <- read.csv(url)
Projekt("html")
# rescale mm -> cm
dat$bill_length_cm <- dat$bill_length_mm / 10
dat$flipper_length_cm <- dat$flipper_length_mm / 10

mod <- lm(bill_length_cm ~ flipper_length_cm + species, data = dat)
 
 modelsummary(mod, output="data.frame") %>% Output()
Output.modelsummary_string <- function(x, ...) stp25output2:::HTML_default( x )
 modelsummary(mod, output="html")  %>% Output()
 
End()