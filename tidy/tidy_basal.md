Tidy
================
L
20231101

``` r
# libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(haven)
library(purrr)
library(skimr)
library(compareGroups)
library(corrr)
library(Hmisc)
library(labelled)
library(writexl)
library(tibble)
```

    ## # A tibble: 6 × 23
    ##   sexo   educa agecat site  windex  frqbalcoh const…¹ bmicat diet   trig psist…²
    ##   <fct>  <dbl> <fct>  <fct> <fct>   <fct>     <fct>   <fct>  <fct> <dbl>   <dbl>
    ## 1 Female    11 45-54  Lima  Middle  Never     Never … Obese  High…   132     119
    ## 2 Male       9 55-64  Lima  Middle  <=1/month Curren… Normal High…   162     106
    ## 3 Male      11 <45    Lima  Highest <=1/month Curren… Overw… High…   166     114
    ## 4 Male      12 55-64  Lima  Lowest  Never     Smoked… <NA>   Pota…    NA      NA
    ## 5 Male      14 55-64  Lima  Highest 2-4/month Smoked… Obese  Pota…   242     117
    ## 6 Male      11 45-54  Lima  Highest <=1/month Smoked… Overw… High…   257     133
    ## # … with 12 more variables: psisto_m2 <dbl>, psisto_m3 <dbl>, pdiast_m1 <dbl>,
    ## #   pdiast_m2 <dbl>, pdiast_m3 <dbl>, hdlc <dbl>, glucose <dbl>, f2crp <dbl>,
    ## #   crp <dbl>, cintur_m1 <dbl>, cintur_m2 <dbl>, cintur_m3 <dbl>, and
    ## #   abbreviated variable names ¹​constabac, ²​psisto_m1

    ## tibble [3,619 × 23] (S3: tbl_df/tbl/data.frame)
    ##  $ sexo     : Factor w/ 2 levels "Female","Male": 1 2 2 2 2 2 1 1 2 1 ...
    ##   ..- attr(*, "label")= chr "Sex"
    ##  $ educa    : num [1:3619] 11 9 11 12 14 11 11 0 6 5 ...
    ##   ..- attr(*, "label")= chr "Nº years of education"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ agecat   : Factor w/ 4 levels "<45","45-54",..: 2 3 1 3 3 2 3 4 3 1 ...
    ##   ..- attr(*, "label")= chr "Age (strata)"
    ##  $ site     : Factor w/ 4 levels "Lima","Urban Puno",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..- attr(*, "label")= chr "Study site"
    ##  $ windex   : Factor w/ 3 levels "Lowest","Middle",..: 2 2 3 1 3 3 3 2 2 3 ...
    ##   ..- attr(*, "label")= chr "3 quantiles of index"
    ##  $ frqbalcoh: Factor w/ 5 levels "Never","<=1/month",..: 1 2 2 1 3 2 1 2 2 2 ...
    ##   ..- attr(*, "label")= chr "Currently, how often consume a drink with alcohol"
    ##  $ constabac: Factor w/ 3 levels "Never smoke",..: 1 3 3 2 2 2 2 1 3 2 ...
    ##   ..- attr(*, "label")= chr "Self-reported smoking history"
    ##  $ bmicat   : Factor w/ 3 levels "Normal","Overweight",..: 3 1 2 NA 3 2 3 3 2 2 ...
    ##   ..- attr(*, "label")= chr "BMI categorized"
    ##  $ diet     : Factor w/ 4 levels "Potato & Vegetables",..: 3 3 3 2 2 3 3 3 3 3 ...
    ##   ..- attr(*, "label")= chr "Diet patterns"
    ##  $ trig     : num [1:3619] 132 162 166 NA 242 257 151 112 83 51 ...
    ##   ..- attr(*, "label")= chr "Triglycerides"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ psisto_m1: num [1:3619] 119 106 114 NA 117 133 99 177 133 101 ...
    ##   ..- attr(*, "label")= chr "BSP: Measure 1"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ psisto_m2: num [1:3619] 120 108 102 NA 115 120 93 151 114 97 ...
    ##   ..- attr(*, "label")= chr "BSP: Measure 2"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ psisto_m3: num [1:3619] 103 137 100 NA 115 122 97 152 124 104 ...
    ##   ..- attr(*, "label")= chr "BSP: Measure 3"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ pdiast_m1: num [1:3619] 79 64 56 NA 75 85 64 92 81 59 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 1"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ pdiast_m2: num [1:3619] 71 64 56 NA 77 81 61 80 77 67 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 2"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ pdiast_m3: num [1:3619] 79 71 57 NA 74 82 64 75 72 71 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 3"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ hdlc     : num [1:3619] 43 41 42 NA 57 34 55 48 52 55 ...
    ##   ..- attr(*, "label")= chr "HDL-cholesterol"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ glucose  : num [1:3619] 86 90 89 NA 87 88 90 111 88 103 ...
    ##   ..- attr(*, "label")= chr "Fasting glucose"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f2crp    : num [1:3619] 0.7 0.44 0.43 NA NA ...
    ##   ..- attr(*, "label")= chr "C-reactive protein"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ crp      : num [1:3619] 0.26 1.53 0.36 NA 3.76 ...
    ##   ..- attr(*, "label")= chr "C-reactive protein"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ cintur_m1: num [1:3619] 98.8 86.8 86.8 NA 113.5 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 1"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ cintur_m2: num [1:3619] 98.3 87 86.8 NA 112.3 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 2"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ cintur_m3: num [1:3619] 98.9 86.9 86.7 NA 112.3 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 3"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"

    ## Rows: 3,619
    ## Columns: 23
    ## $ sexo      <fct> Female, Male, Male, Male, Male, Male, Female, Female, Male, …
    ## $ educa     <dbl> 11, 9, 11, 12, 14, 11, 11, 0, 6, 5, 16, 1, 15, 3, 11, 11, 0,…
    ## $ agecat    <fct> 45-54, 55-64, <45, 55-64, 55-64, 45-54, 55-64, 65+, 55-64, <…
    ## $ site      <fct> Lima, Lima, Lima, Lima, Lima, Lima, Lima, Lima, Lima, Lima, …
    ## $ windex    <fct> Middle, Middle, Highest, Lowest, Highest, Highest, Highest, …
    ## $ frqbalcoh <fct> Never, <=1/month, <=1/month, Never, 2-4/month, <=1/month, Ne…
    ## $ constabac <fct> Never smoke, Currently smoke, Currently smoke, Smoked before…
    ## $ bmicat    <fct> Obese, Normal, Overweight, NA, Obese, Overweight, Obese, Obe…
    ## $ diet      <fct> "High diversity, animal & processed food", "High diversity, …
    ## $ trig      <dbl> 132, 162, 166, NA, 242, 257, 151, 112, 83, 51, NA, 213, 115,…
    ## $ psisto_m1 <dbl> 119, 106, 114, NA, 117, 133, 99, 177, 133, 101, NA, 148, 109…
    ## $ psisto_m2 <dbl> 120, 108, 102, NA, 115, 120, 93, 151, 114, 97, NA, 140, 101,…
    ## $ psisto_m3 <dbl> 103, 137, 100, NA, 115, 122, 97, 152, 124, 104, NA, 145, 100…
    ## $ pdiast_m1 <dbl> 79, 64, 56, NA, 75, 85, 64, 92, 81, 59, NA, 84, 69, 63, 99, …
    ## $ pdiast_m2 <dbl> 71, 64, 56, NA, 77, 81, 61, 80, 77, 67, NA, 86, 70, 61, 106,…
    ## $ pdiast_m3 <dbl> 79, 71, 57, NA, 74, 82, 64, 75, 72, 71, NA, 93, 71, 68, 106,…
    ## $ hdlc      <dbl> 43, 41, 42, NA, 57, 34, 55, 48, 52, 55, NA, 37, 31, 30, 30, …
    ## $ glucose   <dbl> 86, 90, 89, NA, 87, 88, 90, 111, 88, 103, NA, 93, 102, 100, …
    ## $ f2crp     <dbl> 0.70, 0.44, 0.43, NA, NA, 1.28, 8.25, 6.25, 0.80, 0.57, NA, …
    ## $ crp       <dbl> 0.26, 1.53, 0.36, NA, 3.76, 2.09, 1.85, 7.59, 2.60, 0.85, NA…
    ## $ cintur_m1 <dbl> 98.8, 86.8, 86.8, NA, 113.5, 96.5, 95.6, 112.0, 97.4, 77.7, …
    ## $ cintur_m2 <dbl> 98.3, 87.0, 86.8, NA, 112.3, 96.4, 95.5, 112.1, 96.5, 77.7, …
    ## $ cintur_m3 <dbl> 98.9, 86.9, 86.7, NA, 112.3, 96.5, 95.5, 112.1, 96.5, 77.8, …

``` r
#data2$sexo <- factor(data$sexo, levels = c("Female", "Male"), labels = c(0, 1))

data %>% skim() %>%
  mutate(label = label(data)) %>%
  writexl::write_xlsx(path = "table.xlsx")
```

# Crear variable SM

## Medidas recolectadas en triplicado en el basal (CC, PA)

- Data 2 = data

``` r
# crear variable SM 
# c.cintura

data2 <- data
data2$cintur_mean_f0 <- rowMeans(data2[, c("cintur_m1", "cintur_m2", "cintur_m3")], na.rm = TRUE)
summary(data2$cintur_mean_f0)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   52.50   84.67   92.00   91.78   99.00  138.37     389

``` r
# missings
total_missing <- sum(is.na(data2$cintur_mean_f0))
total_non_missing <- sum(!is.na(data2$cintur_mean_f0))
total_observations <- nrow(data2)

# Print 
cat("No NAs para cintur_mean_f0:", total_non_missing, "\n")
```

    ## No NAs para cintur_mean_f0: 3230

``` r
cat("NAs cintur_mean_f0:", total_missing, "\n")
```

    ## NAs cintur_mean_f0: 389

``` r
cat("Total obs:", total_observations, "\n")
```

    ## Total obs: 3619

``` r
# NAs
data2$na_count <- rowSums(is.na(data2[, c("cintur_m1", "cintur_m2", "cintur_m3")]),na.rm = TRUE)

# 3 NA
all_na <- sum(data2$na_count == 3)

# 1 NA
one_na <- sum(data2$na_count == 1)

#2 NA
two_na <- sum(data2$na_count == 2)

# Print
cat("Todo NA:", all_na, "\n")
```

    ## Todo NA: 389

``` r
cat("Un NA:", one_na, "\n")
```

    ## Un NA: 1

``` r
cat("Dos NA:", two_na, "\n")
```

    ## Dos NA: 0

``` r
# PAS
data2$psisto_f0 <- rowMeans(data2[, c("psisto_m1", "psisto_m2", "psisto_m3")], na.rm = TRUE)
summary(data2$psisto_f0)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    68.0   106.3   115.7   118.9   128.3   222.0     388

``` r
# N. NAs y no NAs
total_missing <- sum(is.na(data2$psisto_f0))
total_non_missing <- sum(!is.na(data2$psisto_f0))
total_observations <- nrow(data2)

# Print
cat("No NAs para psisto_f0:", total_non_missing, "\n")
```

    ## No NAs para psisto_f0: 3231

``` r
cat("NAs psisto_f0:", total_missing, "\n")
```

    ## NAs psisto_f0: 388

``` r
cat("Total obs:", total_observations, "\n")
```

    ## Total obs: 3619

``` r
# NAs
data2$na_count <- rowSums(is.na(data2[, c("psisto_m1", "psisto_m2", "psisto_m3")]),na.rm = TRUE)

#3 NA
all_na <- sum(data2$na_count == 3)

# 1 NA
one_na <- sum(data2$na_count == 1)

# 2 NA
two_na <- sum(data2$na_count == 2)

# Print
cat("Todo NA:", all_na, "\n")
```

    ## Todo NA: 388

``` r
cat("un NA:", one_na, "\n")
```

    ## un NA: 0

``` r
cat("dos NA:", two_na, "\n")
```

    ## dos NA: 0

``` r
# PAD
data2$pad_f0 <- rowMeans(data2[, c("pdiast_m1", "pdiast_m2", "pdiast_m3")], na.rm = TRUE)
summary(data2$pad_f0)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   45.00   66.33   73.00   73.67   79.67  181.33     388

``` r
# N. NAs y no NAs
total_missing <- sum(is.na(data2$pad_f0))
total_non_missing <- sum(!is.na(data2$pad_f0))
total_observations <- nrow(data2)

# Print
cat("No NAs para pad_f0:", total_non_missing, "\n")
```

    ## No NAs para pad_f0: 3231

``` r
cat("NAs pad_f0:", total_missing, "\n")
```

    ## NAs pad_f0: 388

``` r
cat("Total obs:", total_observations, "\n")
```

    ## Total obs: 3619

``` r
# NAs x fila
data2$na_count <- rowSums(is.na(data2[, c("pdiast_m1", "pdiast_m2", "pdiast_m3")]),na.rm = TRUE)

# 3 NA
all_na <- sum(data2$na_count == 3)

# 1 NA
one_na <- sum(data2$na_count == 1)

# 2 NA
two_na <- sum(data2$na_count == 2)

# Print
cat("Número de observaciones con todos los valores como NA:", all_na, "\n")
```

    ## Número de observaciones con todos los valores como NA: 388

``` r
cat("un NA:", one_na, "\n")
```

    ## un NA: 0

``` r
cat("dos NA:", two_na, "\n")
```

    ## dos NA: 0

## Medidas de laboratorio, recolectadas una vez en el basal

``` r
# TG
summary(data2$trig)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    35.0    98.0   136.0   160.1   190.5  1299.0     484

``` r
# N. NAs y no NAs
total_missing <- sum(is.na(data2$trig))
total_non_missing <- sum(!is.na(data2$trig))
total_observations <- nrow(data2)

# Print
cat("No NAs para trig:", total_non_missing, "\n")
```

    ## No NAs para trig: 3135

``` r
cat("NAs trig:", total_missing, "\n")
```

    ## NAs trig: 484

``` r
cat("Total obs:", total_observations, "\n")
```

    ## Total obs: 3619

``` r
# TG
summary(data2$hdlc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   16.00   33.00   40.00   41.56   48.00  116.20     484

``` r
# N. NAs y no NAs
total_missing <- sum(is.na(data2$hdlc))
total_non_missing <- sum(!is.na(data2$hdlc))
total_observations <- nrow(data2)

# Print
cat("No NAs para hdlc:", total_non_missing, "\n")
```

    ## No NAs para hdlc: 3135

``` r
cat("NAs hdlc:", total_missing, "\n")
```

    ## NAs hdlc: 484

``` r
cat("Total obs:", total_observations, "\n")
```

    ## Total obs: 3619

``` r
# TG
summary(data2$glucose)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   46.00   85.00   91.00   98.16  100.00  537.00     484

``` r
# N. NAs y no NAs
total_missing <- sum(is.na(data2$glucose))
total_non_missing <- sum(!is.na(data2$glucose))
total_observations <- nrow(data2)

# Print
cat("No NAs para glucose:", total_non_missing, "\n")
```

    ## No NAs para glucose: 3135

``` r
cat("NAs glucose:", total_missing, "\n")
```

    ## NAs glucose: 484

``` r
cat("Total obs:", total_observations, "\n")
```

    ## Total obs: 3619

``` r
#### Existen 484 (13.4%) observaciones que no tienen medidas de laboratorio...
484/3619
```

    ## [1] 0.1337386

# Valoración de SM por sexo, debido a missings en sexo

``` r
# Presencia de síndrome metabólico en el basal
#data missing sexo
sum(is.na(data2$sexo))
```

    ## [1] 22

``` r
#22 datos perdidos

#2 funciones: 1. con sexo datos completos y 2. NAs en sexo.

#I. sexo =! NAs

calculate_sm_with_sex <- function(psisto_f0, pad_f0, cintur_mean_f0, trig, hdlc, glucose, sexo, colesalt, presart, diabetesm) {
  waist_threshold <- ifelse(sexo == 0, 80, 90)
  hdlc_threshold <- ifelse(sexo == 0, 50, 40)
  
  criteria_count <- sum(
    ifelse(!is.na(cintur_mean_f0), cintur_mean_f0 >= waist_threshold, FALSE),
    ifelse(!is.na(trig), trig >= 150, FALSE) || colesalt == 2,#actualmente toma meds para hcolesterol(2)
    ifelse(!is.na(hdlc), hdlc < hdlc_threshold, FALSE) || colesalt == 2,
    ifelse(!is.na(psisto_f0), psisto_f0 >= 130, FALSE) || ifelse(!is.na(pad_f0), pad_f0 >= 85, FALSE) || presart == 2,
    ifelse(!is.na(glucose), glucose >= 100, FALSE) || diabetesm == 2,
    na.rm = TRUE
  )
  
  return(list(criteria_count = criteria_count, sm = as.integer(criteria_count >= 3)))
}

#II. sexo = NA
calculate_sm_without_sex <- function(psisto_f0, pad_f0, trig, glucose, colesalt, presart, diabetesm) {
  criteria_count <- sum(
    ifelse(!is.na(trig), trig >= 150, FALSE) || colesalt == 2,
    ifelse(!is.na(psisto_f0), psisto_f0 >= 130, FALSE) || ifelse(!is.na(pad_f0), pad_f0 >= 85, FALSE) || presart == 2,
    ifelse(!is.na(glucose), glucose >= 100, FALSE) || diabetesm == 2,
    na.rm = TRUE
  )
  
  return(list(criteria_count = criteria_count, sm = as.integer(criteria_count >= 3)))
}

data2 <- data2 %>%
  mutate(across(where(is.labelled), ~ as.numeric(as_factor(.))))


data2$sm_with_sex <- pmap(data2[, c('psisto_f0', 'pad_f0', 'cintur_mean_f0', 'trig', 'hdlc', 'glucose', 'sexo', 'colesalt', 'presart', 'diabetesm')], calculate_sm_with_sex)
data2$sm_without_sex <- pmap(data2[, c('psisto_f0', 'pad_f0', 'trig', 'glucose', 'colesalt', 'presart', 'diabetesm')], calculate_sm_without_sex)

# Extraer componente 'sm' del pmap
data2$sm_with_sex <- map_dbl(data2$sm_with_sex, ~ .x$sm)
data2$sm_without_sex <- map_dbl(data2$sm_without_sex, ~ .x$sm)

table(data2$sm_without_sex)
```

    ## 
    ##    0    1 
    ## 3438  181

``` r
#hay 181 casos de SM en aquellos con NAs en sexo.

data2$sexo_nomissing <- !is.na(data2$sexo)

data2$sm <- ifelse(data2$sexo_nomissing, data2$sm_with_sex, data2$sm_without_sex)

table(data2$sm, data2$sexo)
```

    ##    
    ##     Female Male
    ##   0   1329 1114
    ##   1    523  631

## Función para información de variables de interés

``` r
get_variable_info <- function(variables, data) {
  info <- tibble(
    variable = variables,
    exists_in_data = variables %in% names(data),
    type = sapply(variables, function(var) {
      if (var %in% names(data)) {
        return(class(data[[var]]))
      } else {
        return(NA)
      }
    }),
    description = sapply(variables, function(var) {
      if (var %in% names(data)) {
        attr_label <- attr(data[[var]], "label")
        if (!is.null(attr_label)) {
          return(attr_label)
        } else {
          return("No label attribute")
        }
      } else {
        return("Variable not in data")
      }
    }),
    levels = sapply(variables, function(var) {
      if (var %in% names(data) && is.factor(data[[var]])) {
        return(levels(data[[var]]))
      } else {
        return(NA)
      }
    }, simplify = FALSE),
    numeric_levels = sapply(variables, function(var) {
      if (var %in% names(data) && is.factor(data[[var]])) {
        return(as.numeric(levels(data[[var]])))
      } else {
        return(NA)
      }
    }, simplify = FALSE)
  )

  # Convertir list a character
  info <- info %>%
    mutate(variable = as.character(variable),
           description = as.character(description),
           levels = ifelse(is.list(levels), sapply(levels, function(x) paste(x, collapse = "; ")), as.character(levels)),
           numeric_levels = ifelse(is.list(numeric_levels), sapply(numeric_levels, function(x) paste(x, collapse = "; ")), as.character(numeric_levels)))
  
  return(info)
}
```

``` r
# Data
#1. criterio trigliceridos
trigliceridos <- c("trig","f2trig")
variable_info <- get_variable_info(trigliceridos, data2)
print(variable_info)
```

    ## # A tibble: 2 × 6
    ##   variable exists_in_data type    description   levels numeric_levels
    ##   <chr>    <lgl>          <chr>   <chr>         <chr>  <chr>         
    ## 1 trig     TRUE           numeric Triglycerides NA     NA            
    ## 2 f2trig   TRUE           numeric Triglycerides NA     NA

``` r
#2. criterio HDL-c
hdl <- c("colesalt", "colesaltm", "choldrugs" ,"colesal_p" ,"colesal_v" , "colesal_h", "colesal_t", "colesal_o", "cholest","hdlc" , "f1colesalt","f1colesaltm","f2colesalt", "f2colesaltm", "f2vpsicolog" ,"f2cholest", "f2hdlc")
variable_info <- get_variable_info(hdl, data2)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 17 × 6
    ##    variable    exists_in_data type      description               levels numer…¹
    ##    <chr>       <lgl>          <chr>     <chr>                     <chr>  <chr>  
    ##  1 colesalt    TRUE           factor    Currently take medicatio… No; Y… NA; NA 
    ##  2 colesaltm   TRUE           character High cholesterol: medica… No; Y… NA; NA 
    ##  3 choldrugs   TRUE           factor    Cholesterol: treatment    No; Y… NA; NA 
    ##  4 colesal_p   TRUE           factor    High cholesterol: Parents No; Y… NA; NA 
    ##  5 colesal_v   TRUE           factor    High cholesterol: Grandp… No; Y… NA; NA 
    ##  6 colesal_h   TRUE           factor    High cholesterol: Siblin… No; Y… NA; NA 
    ##  7 colesal_t   TRUE           factor    High cholesterol: Uncles… No; Y… NA; NA 
    ##  8 colesal_o   TRUE           factor    High cholesterol: Others  No; Y… NA; NA 
    ##  9 cholest     TRUE           numeric   Total cholesterol         No; Y… NA; NA 
    ## 10 hdlc        TRUE           numeric   HDL-cholesterol           No; Y… NA; NA 
    ## 11 f1colesalt  TRUE           factor    High cholesterol: take d… No; Y… NA; NA 
    ## 12 f1colesaltm TRUE           character High cholesterol: drug n… No; Y… NA; NA 
    ## 13 f2colesalt  TRUE           factor    Medicine for lipids?      No; Y… NA; NA 
    ## 14 f2colesaltm TRUE           character Lipids treatment          No; Y… NA; NA 
    ## 15 f2vpsicolog TRUE           factor    Last 12 months for menta… No; Y… NA; NA 
    ## 16 f2cholest   TRUE           numeric   Total cholesterol         No; Y… NA; NA 
    ## 17 f2hdlc      TRUE           numeric   HDL-cholesterol           No; Y… NA; NA 
    ## # … with abbreviated variable name ¹​numeric_levels

``` r
#3. criterio PA: HTA
presion_art <- c("presart", "presartm", "depression", "htdrugs", "hts",         
 "htd"        ,  "ht1"        ,  "htdx"    ,     "presartmay" ,  "hosprespi" ,  
 "f1txpresart" , "f1mdcpresart", "f1md1presart", "f1md2presart", "f1md3presart",
 "f1md4presart", "f1presartmay", "f2txpresart",  "f2mdcpresart", "f2md1presart",
 "f2md2presart", "f2md3presart", "f2md4presart", "f2hosprespi",  "f2depression",
 "f2hts"  ,      "f2htd"    ,    "f2ht1")
variable_info <- get_variable_info(presion_art, data2)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 28 × 6
    ##    variable   exists_in_data type      description                levels numer…¹
    ##    <chr>      <lgl>          <chr>     <chr>                      <chr>  <chr>  
    ##  1 presart    TRUE           factor    Currently take medication… No; Y… NA; NA 
    ##  2 presartm   TRUE           character Blood pressure: medicatio… No; Y… NA; NA 
    ##  3 depression TRUE           numeric   CES-D total                No; Y… NA; NA 
    ##  4 htdrugs    TRUE           factor    Specific Anti-hypertensiv… No; Y… NA; NA 
    ##  5 hts        TRUE           factor    Systolic HT                No; Y… NA; NA 
    ##  6 htd        TRUE           factor    Diastolic HT               No; Y… NA; NA 
    ##  7 ht1        TRUE           factor    Hypertension (measured on… No; Y… NA; NA 
    ##  8 htdx       TRUE           factor    Hypertension diagnosed by… No; Y… NA; NA 
    ##  9 presartmay TRUE           numeric   No label attribute         No; Y… NA; NA 
    ## 10 hosprespi  TRUE           numeric   No label attribute         No; Y… NA; NA 
    ## # … with 18 more rows, and abbreviated variable name ¹​numeric_levels

``` r
#4. criterio glucosa: diabetes
dm <- c("diabetesm" , "diabetesmed"  ,"diabetes" ,"diabete_q"  ,"diabete_l"  , "diabete_a"  ,"diabete_p"  ,"diabete_v" ,"diabete_h"  ,"diabete_t"  ,
"diabete_o"  ,"gdmgrasa1"  ,"gdmgrasa2"  ,"f1insufcardm"  ,"f1txdiabetes" ,
"f1mddiabetes" ,"f1md1diabet"  ,"f1md2diabet" ,"f1md3diabet" ,"f1md4diabet" ,
"f1citasxdiab" ,"f1txdiabhora" ,"f1txdiabdosi" ,"f1gdmgrasa1" ,"f1gdmgrasa2" ,
"f2insufcardm" ,"f2txdiabetes" ,"f2mddiabetes" ,"f2md1diabet","f2md2diabet" ,
"f2md3diabet","f2md4diabet","f2p1mdmenq" ,"f2p2mdmenq" ,"f2p3mdmenq" ,
"f2cuidmastm","f2prdiabuen" ,"f2gdmgrasa1" ,"f2gdmgrasa2")
variable_info <- get_variable_info(dm, data2)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 39 × 6
    ##    variable    exists_in_data type      description               levels numer…¹
    ##    <chr>       <lgl>          <chr>     <chr>                     <chr>  <chr>  
    ##  1 diabetesm   TRUE           factor    Currently take medicatio… No; Y… NA; NA 
    ##  2 diabetesmed TRUE           character Diabetes: medication name No; Y… NA; NA 
    ##  3 diabetes    TRUE           factor    Diabetes                  No; Y… NA; NA 
    ##  4 diabete_q   TRUE           factor    Diabetes diagnosed by     No; Y… NA; NA 
    ##  5 diabete_l   TRUE           factor    Diabetes: Place of diagn… No; Y… NA; NA 
    ##  6 diabete_a   TRUE           numeric   Diabetes: years since di… No; Y… NA; NA 
    ##  7 diabete_p   TRUE           factor    Diabetes: Parents         No; Y… NA; NA 
    ##  8 diabete_v   TRUE           factor    Diabetes: Grandparents    No; Y… NA; NA 
    ##  9 diabete_h   TRUE           factor    Diabetes: Siblings        No; Y… NA; NA 
    ## 10 diabete_t   TRUE           factor    Diabetes: Uncles/Aunts    No; Y… NA; NA 
    ## # … with 29 more rows, and abbreviated variable name ¹​numeric_levels

``` r
#antecedente familiar - 

ant_fam <- c("nfamilias","ingrefam","probfamil","noantfami","f2ingrefam","dise", "f2prmolefam")
variable_info <- get_variable_info(ant_fam,data2)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 7 × 6
    ##   variable    exists_in_data type    description                  levels numer…¹
    ##   <chr>       <lgl>          <chr>   <chr>                        <chr>  <chr>  
    ## 1 nfamilias   TRUE           numeric Nº families cooking at hous… NA     NA     
    ## 2 ingrefam    TRUE           factor  Familial income per month    NA     NA     
    ## 3 probfamil   TRUE           factor  Last 3 months, family probl… NA     NA     
    ## 4 noantfami   TRUE           factor  History of familial diseases NA     NA     
    ## 5 f2ingrefam  TRUE           factor  What was the total family i… NA     NA     
    ## 6 dise        FALSE          <NA>    Variable not in data         NA     NA     
    ## 7 f2prmolefam TRUE           factor  Mis problemas respiratorios… NA     NA     
    ## # … with abbreviated variable name ¹​numeric_levels

``` r
#dieta

dieta <- c("diet","f1p_dieta", "f1d_dieta")
variable_info <- get_variable_info(dieta,data2)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 3 × 6
    ##   variable  exists_in_data type   description   levels                   numer…¹
    ##   <chr>     <lgl>          <chr>  <chr>         <chr>                    <chr>  
    ## 1 diet      TRUE           factor Diet patterns Potato & Vegetables; Po… NA; NA…
    ## 2 f1p_dieta TRUE           factor HT: diet      Potato & Vegetables; Po… NA; NA…
    ## 3 f1d_dieta TRUE           factor DB: diet      Potato & Vegetables; Po… NA; NA…
    ## # … with abbreviated variable name ¹​numeric_levels

### FFQ

<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7613217/> version of an
FFQ, to obtain information on consumption frequency of certain foods and
beverages, to report how many times per month or week they consumed
foods and beverages within twenty-three categories.

``` r
#table(data2$diet, data2$site)
table(data2$f1p_dieta)
```

    ## 
    ##   No  Yes 
    ## 2662  209

``` r
table(data2$f1d_dieta)
```

    ## 
    ##   No  Yes 
    ## 2755  116

### Usando grep() para retornar indices de nombres macheados

``` r
# 

#criterio trigliceridos
patron <- "trig|Trig"
trig <- data2[, grep(patron, names(data2), value = TRUE)]
print(names(trig))
```

    ## [1] "trig"   "f2trig"

``` r
#criterio HDL-c
patron <- "chol|col|hdlc|Col|Chol|hd"
hdlc <- data2[, grep(patron, names(data2), value = TRUE)]
print(names(hdlc))
```

    ##  [1] "tvcolor"     "colesalt"    "colesaltm"   "choldrugs"   "colesal_p"  
    ##  [6] "colesal_v"   "colesal_h"   "colesal_t"   "colesal_o"   "cholest"    
    ## [11] "hdlc"        "f1colesalt"  "f1colesaltm" "f2colesalt"  "f2colesaltm"
    ## [16] "f2vpsicolog" "f2cholest"   "f2hdlc"

``` r
#criterio PA

patron <- "hta|ht|hiper|Hip|hyp|hy|Hy|high|pres"
var_ht <- data2[, grep(patron, names(data2), value = TRUE)]
print(names(var_ht))
```

    ##  [1] "presart"      "presartm"     "depression"   "htdrugs"      "hts"         
    ##  [6] "htd"          "ht1"          "htdx"         "presartmay"   "hosprespi"   
    ## [11] "f1txpresart"  "f1mdcpresart" "f1md1presart" "f1md2presart" "f1md3presart"
    ## [16] "f1md4presart" "f1presartmay" "f2txpresart"  "f2mdcpresart" "f2md1presart"
    ## [21] "f2md2presart" "f2md3presart" "f2md4presart" "f2hosprespi"  "f2depression"
    ## [26] "f2hts"        "f2htd"        "f2ht1"

``` r
#criterio glucosa
patron <- "dm|dm2|diabetes|diab|Diab"
var_dm <- data2[, grep(patron, names(data2), value = TRUE)]
print(names(var_dm))
```

    ##  [1] "diabetesm"    "diabetesmed"  "diabetes"     "diabete_q"    "diabete_l"   
    ##  [6] "diabete_a"    "diabete_p"    "diabete_v"    "diabete_h"    "diabete_t"   
    ## [11] "diabete_o"    "gdmgrasa1"    "gdmgrasa2"    "f1insufcardm" "f1txdiabetes"
    ## [16] "f1mddiabetes" "f1md1diabet"  "f1md2diabet"  "f1md3diabet"  "f1md4diabet" 
    ## [21] "f1citasxdiab" "f1txdiabhora" "f1txdiabdosi" "f1gdmgrasa1"  "f1gdmgrasa2" 
    ## [26] "f2insufcardm" "f2txdiabetes" "f2mddiabetes" "f2md1diabet"  "f2md2diabet" 
    ## [31] "f2md3diabet"  "f2md4diabet"  "f2p1mdmenq"   "f2p2mdmenq"   "f2p3mdmenq"  
    ## [36] "f2cuidmastm"  "f2prdiabuen"  "f2gdmgrasa1"  "f2gdmgrasa2"

``` r
#antecedente familiar
patron <- "fam|antec|hist|padre|madre|moth|fath|dise"
hist_fam <- data2[, grep(patron, names(data2), value = TRUE)]
print(names(hist_fam))
```

    ## [1] "nfamilias"   "ingrefam"    "probfamil"   "noantfami"   "f2ingrefam" 
    ## [6] "f2prmolefam"

``` r
#antecedente familiar
patron <- "diet|grain|dairy|produc|meat|poul|fish|sea|egg|piz|veget|legum|nut|seed|food|pick|fried|deep|potato|salt|fruit|beverag|UPF|processed|carne|pescado|alimen|comid"
diet <- data2[, grep(patron, names(data2), value = TRUE)]
print(names(diet))
```

    ##  [1] "gastcomid"   "carnef"      "carnev"      "pescadof"    "pescadov"   
    ##  [6] "pizzasf"     "pizzasv"     "legumbref"   "legumbrev"   "otrcomid"   
    ## [11] "wgrain"      "rgrain"      "dproduct"    "meat"        "poultry"    
    ## [16] "seafood"     "egg"         "gvegeta"     "rvegeta"     "cvegeta"    
    ## [21] "legume"      "pickle"      "fried"       "cpotato"     "fruit"      
    ## [26] "wgraincat"   "rgraincat"   "dproducat"   "meatcat"     "poultrycat" 
    ## [31] "seafoodcat"  "eggcat"      "legumecat"   "picklecat"   "friedcat"   
    ## [36] "cpotatocat"  "fruitcat"    "diet"        "ffood"       "colesalt"   
    ## [41] "colesaltm"   "comidadia"   "f1p_dieta"   "f1d_dieta"   "f1colesalt" 
    ## [46] "f1colesaltm" "f2colesalt"  "f2colesaltm"

# Explorando criterios alternativos: DM e HT & medicamentos

``` r
#1. criterio trigliceridos
trigliceridos <- c("trig","f2trig")
variable_info <- get_variable_info(trigliceridos, data)
print(variable_info)
```

    ## # A tibble: 2 × 6
    ##   variable exists_in_data type    description   levels numeric_levels
    ##   <chr>    <lgl>          <chr>   <chr>         <chr>  <chr>         
    ## 1 trig     TRUE           numeric Triglycerides NA     NA            
    ## 2 f2trig   TRUE           numeric Triglycerides NA     NA

``` r
#2. criterio HDL-c
hdl <- c("colesalt", "colesaltm", "choldrugs" ,"colesal_p" ,"colesal_v" , "colesal_h", "colesal_t", "colesal_o", "cholest","hdlc" , "f1colesalt","f1colesaltm","f2colesalt", "f2colesaltm", "f2vpsicolog" ,"f2cholest", "f2hdlc")
variable_info <- get_variable_info(hdl, data)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 17 × 6
    ##    variable    exists_in_data type      description               levels numer…¹
    ##    <chr>       <lgl>          <chr>     <chr>                     <chr>  <chr>  
    ##  1 colesalt    TRUE           factor    Currently take medicatio… No; Y… NA; NA 
    ##  2 colesaltm   TRUE           character High cholesterol: medica… No; Y… NA; NA 
    ##  3 choldrugs   TRUE           factor    Cholesterol: treatment    No; Y… NA; NA 
    ##  4 colesal_p   TRUE           factor    High cholesterol: Parents No; Y… NA; NA 
    ##  5 colesal_v   TRUE           factor    High cholesterol: Grandp… No; Y… NA; NA 
    ##  6 colesal_h   TRUE           factor    High cholesterol: Siblin… No; Y… NA; NA 
    ##  7 colesal_t   TRUE           factor    High cholesterol: Uncles… No; Y… NA; NA 
    ##  8 colesal_o   TRUE           factor    High cholesterol: Others  No; Y… NA; NA 
    ##  9 cholest     TRUE           numeric   Total cholesterol         No; Y… NA; NA 
    ## 10 hdlc        TRUE           numeric   HDL-cholesterol           No; Y… NA; NA 
    ## 11 f1colesalt  TRUE           factor    High cholesterol: take d… No; Y… NA; NA 
    ## 12 f1colesaltm TRUE           character High cholesterol: drug n… No; Y… NA; NA 
    ## 13 f2colesalt  TRUE           factor    Medicine for lipids?      No; Y… NA; NA 
    ## 14 f2colesaltm TRUE           character Lipids treatment          No; Y… NA; NA 
    ## 15 f2vpsicolog TRUE           factor    Last 12 months for menta… No; Y… NA; NA 
    ## 16 f2cholest   TRUE           numeric   Total cholesterol         No; Y… NA; NA 
    ## 17 f2hdlc      TRUE           numeric   HDL-cholesterol           No; Y… NA; NA 
    ## # … with abbreviated variable name ¹​numeric_levels

``` r
#3. criterio PA: HTA
presion_art <- c("presart", "presartm", "depression", "htdrugs", "hts",         
 "htd"        ,  "ht1"        ,  "htdx"    ,     "presartmay" ,  "hosprespi" ,  
 "f1txpresart" , "f1mdcpresart", "f1md1presart", "f1md2presart", "f1md3presart",
 "f1md4presart", "f1presartmay", "f2txpresart",  "f2mdcpresart", "f2md1presart",
 "f2md2presart", "f2md3presart", "f2md4presart", "f2hosprespi",  "f2depression",
 "f2hts"  ,      "f2htd"    ,    "f2ht1")
variable_info <- get_variable_info(presion_art, data)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 28 × 6
    ##    variable   exists_in_data type      description                levels numer…¹
    ##    <chr>      <lgl>          <chr>     <chr>                      <chr>  <chr>  
    ##  1 presart    TRUE           factor    Currently take medication… No; Y… NA; NA 
    ##  2 presartm   TRUE           character Blood pressure: medicatio… No; Y… NA; NA 
    ##  3 depression TRUE           numeric   CES-D total                No; Y… NA; NA 
    ##  4 htdrugs    TRUE           factor    Specific Anti-hypertensiv… No; Y… NA; NA 
    ##  5 hts        TRUE           factor    Systolic HT                No; Y… NA; NA 
    ##  6 htd        TRUE           factor    Diastolic HT               No; Y… NA; NA 
    ##  7 ht1        TRUE           factor    Hypertension (measured on… No; Y… NA; NA 
    ##  8 htdx       TRUE           factor    Hypertension diagnosed by… No; Y… NA; NA 
    ##  9 presartmay TRUE           numeric   No label attribute         No; Y… NA; NA 
    ## 10 hosprespi  TRUE           numeric   No label attribute         No; Y… NA; NA 
    ## # … with 18 more rows, and abbreviated variable name ¹​numeric_levels

``` r
#4. criterio glucosa: diabetes
dm <- c("diabetesm" , "diabetesmed"  ,"diabetes" ,"diabete_q"  ,"diabete_l"  , "diabete_a"  ,"diabete_p"  ,"diabete_v" ,"diabete_h"  ,"diabete_t"  ,
"diabete_o"  ,"gdmgrasa1"  ,"gdmgrasa2"  ,"f1insufcardm"  ,"f1txdiabetes" ,
"f1mddiabetes" ,"f1md1diabet"  ,"f1md2diabet" ,"f1md3diabet" ,"f1md4diabet" ,
"f1citasxdiab" ,"f1txdiabhora" ,"f1txdiabdosi" ,"f1gdmgrasa1" ,"f1gdmgrasa2" ,
"f2insufcardm" ,"f2txdiabetes" ,"f2mddiabetes" ,"f2md1diabet","f2md2diabet" ,
"f2md3diabet","f2md4diabet","f2p1mdmenq" ,"f2p2mdmenq" ,"f2p3mdmenq" ,
"f2cuidmastm","f2prdiabuen" ,"f2gdmgrasa1" ,"f2gdmgrasa2")
variable_info <- get_variable_info(dm, data)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
print(variable_info)
```

    ## # A tibble: 39 × 6
    ##    variable    exists_in_data type      description               levels numer…¹
    ##    <chr>       <lgl>          <chr>     <chr>                     <chr>  <chr>  
    ##  1 diabetesm   TRUE           factor    Currently take medicatio… No; Y… NA; NA 
    ##  2 diabetesmed TRUE           character Diabetes: medication name No; Y… NA; NA 
    ##  3 diabetes    TRUE           factor    Diabetes                  No; Y… NA; NA 
    ##  4 diabete_q   TRUE           factor    Diabetes diagnosed by     No; Y… NA; NA 
    ##  5 diabete_l   TRUE           factor    Diabetes: Place of diagn… No; Y… NA; NA 
    ##  6 diabete_a   TRUE           numeric   Diabetes: years since di… No; Y… NA; NA 
    ##  7 diabete_p   TRUE           factor    Diabetes: Parents         No; Y… NA; NA 
    ##  8 diabete_v   TRUE           factor    Diabetes: Grandparents    No; Y… NA; NA 
    ##  9 diabete_h   TRUE           factor    Diabetes: Siblings        No; Y… NA; NA 
    ## 10 diabete_t   TRUE           factor    Diabetes: Uncles/Aunts    No; Y… NA; NA 
    ## # … with 29 more rows, and abbreviated variable name ¹​numeric_levels

## Hipertensos

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
table(data2$sm_positivo) 
```

    ## Warning: Unknown or uninitialised column: `sm_positivo`.

    ## < table of extent 0 >

``` r
#1412 SM+ en el basal sin considerar las variables extras por añadir (consumo de medicamentos)
#comprobando que se incluyan los hipertenso en SM
tabla <- table(data2$htdx,data2$sm)
dimnames(tabla) <- list(htdx = c("No", "Yes"), sm = c("No", "Yes"))
tabla_prop <- prop.table(tabla) * 100 
formatted_tabla <- apply(tabla, c(1, 2), function(x, y) paste0(x, " (", format(round(y[x], 1)), "%)"), y = tabla_prop)
print(formatted_tabla)
```

    ##      sm
    ## htdx  No           Yes        
    ##   No  "2132 (NA%)" "843 (NA%)"
    ##   Yes "313 (NA%)"  "311 (NA%)"

``` r
# Tabla & data frame
tabla <- table(data2$htdx, data2$sm)
tabla
```

    ##      
    ##          0    1
    ##   No  2132  843
    ##   Yes  313  311

``` r
data2 %>% 
  tabyl(htdx,sm) %>%  
  adorn_totals(c("row")) %>% 
  adorn_totals(c("col")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title()
```

    ##                    sm                             
    ##   htdx              0             1          Total
    ##     No 2,132  (71.7%)   843 (28.3%) 2,975 (100.0%)
    ##    Yes   313  (50.2%)   311 (49.8%)   624 (100.0%)
    ##   <NA>    20 (100.0%)     0  (0.0%)    20 (100.0%)
    ##  Total 2,465  (68.1%) 1,154 (31.9%) 3,619 (100.0%)

### Medicamentos antiht

``` r
sum(is.na(data2$sm))
```

    ## [1] 0

``` r
data2 %>% 
  tabyl(htdrugs,sm) %>%  
  adorn_totals(c("row")) %>% 
  adorn_totals(c("col")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title()
```

    ##                     sm                             
    ##  htdrugs             0             1          Total
    ##       No     0     (-)     0     (-)     0 (100.0%)
    ##      Yes   151 (43.8%)   194 (56.2%)   345 (100.0%)
    ##     <NA> 2,314 (70.7%)   960 (29.3%) 3,274 (100.0%)
    ##    Total 2,465 (68.1%) 1,154 (31.9%) 3,619 (100.0%)

``` r
data2 %>% 
  tabyl(htdrugs) %>%  #<<
  adorn_totals("row") %>% 
  adorn_pct_formatting()
```

    ##  htdrugs    n percent valid_percent
    ##       No    0    0.0%          0.0%
    ##      Yes  345    9.5%        100.0%
    ##     <NA> 3274   90.5%             -
    ##    Total 3619  100.0%        100.0%

# Data3: excluyendo missings crp basales

### Data3: 3135 obs

``` r
# missings exposure: C-Reactive Protein (CRP)...
sum(is.na((data2$crp)))
```

    ## [1] 484

``` r
#without missings in CRP_basal
data3 <- data2 %>% filter(!is.na(crp))

#porcentaje de datos perdidos
484/3619*100
```

    ## [1] 13.37386

``` r
count(data3)
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1  3135

``` r
# descripcion de lab f0
str(data3[, c("trig", "hdlc", "glucose", "crp")])
```

    ## tibble [3,135 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ trig   : num [1:3135] 132 162 166 242 257 151 112 83 51 213 ...
    ##   ..- attr(*, "label")= chr "Triglycerides"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ hdlc   : num [1:3135] 43 41 42 57 34 55 48 52 55 37 ...
    ##   ..- attr(*, "label")= chr "HDL-cholesterol"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ glucose: num [1:3135] 86 90 89 87 88 90 111 88 103 93 ...
    ##   ..- attr(*, "label")= chr "Fasting glucose"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ crp    : num [1:3135] 0.26 1.53 0.36 3.76 2.09 ...
    ##   ..- attr(*, "label")= chr "C-reactive protein"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"

``` r
summary(data3[, c("trig", "hdlc", "glucose", "crp")])
```

    ##       trig             hdlc           glucose            crp         
    ##  Min.   :  35.0   Min.   : 16.00   Min.   : 46.00   Min.   :  0.070  
    ##  1st Qu.:  98.0   1st Qu.: 33.00   1st Qu.: 85.00   1st Qu.:  0.775  
    ##  Median : 136.0   Median : 40.00   Median : 91.00   Median :  1.680  
    ##  Mean   : 160.1   Mean   : 41.56   Mean   : 98.16   Mean   :  3.497  
    ##  3rd Qu.: 190.5   3rd Qu.: 48.00   3rd Qu.:100.00   3rd Qu.:  3.605  
    ##  Max.   :1299.0   Max.   :116.20   Max.   :537.00   Max.   :146.240

``` r
summarise(data3, across(c("trig", "hdlc", "glucose", "crp"), ~sum(is.na(.))))
```

    ## # A tibble: 1 × 4
    ##    trig  hdlc glucose   crp
    ##   <int> <int>   <int> <int>
    ## 1     0     0       0     0

``` r
# descripcion de lab f2
str(data2[, c("f2trig", "f2hdlc", "f2glucose", "f2crp")])
```

    ## tibble [3,619 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ f2trig   : num [1:3619] 166 251 113 NA NA 223 118 151 88 112 ...
    ##   ..- attr(*, "label")= chr "Triglycerides"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f2hdlc   : num [1:3619] 48 50 43 NA NA 36 54 47 63 50 ...
    ##   ..- attr(*, "label")= chr "HDL-cholesterol"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f2glucose: num [1:3619] 98 86 88 NA NA 100 95 106 94 99 ...
    ##   ..- attr(*, "label")= chr "Fasting glucose"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f2crp    : num [1:3619] 0.7 0.44 0.43 NA NA ...
    ##   ..- attr(*, "label")= chr "C-reactive protein"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"

``` r
summary(data2[, c("f2trig", "f2hdlc", "f2glucose", "f2crp")])
```

    ##      f2trig           f2hdlc         f2glucose         f2crp        
    ##  Min.   :  31.0   Min.   : 18.00   Min.   : 63.0   Min.   :  0.150  
    ##  1st Qu.: 100.0   1st Qu.: 37.00   1st Qu.: 88.0   1st Qu.:  0.850  
    ##  Median : 135.0   Median : 44.00   Median : 95.0   Median :  1.690  
    ##  Mean   : 158.7   Mean   : 45.53   Mean   :104.1   Mean   :  3.749  
    ##  3rd Qu.: 192.0   3rd Qu.: 52.00   3rd Qu.:104.0   3rd Qu.:  3.400  
    ##  Max.   :1052.0   Max.   :123.00   Max.   :534.0   Max.   :304.760  
    ##  NA's   :1086     NA's   :1086     NA's   :1086    NA's   :1086

``` r
summarise(data2, across(c("f2trig", "f2hdlc", "f2glucose", "f2crp"), ~sum(is.na(.))))
```

    ## # A tibble: 1 × 4
    ##   f2trig f2hdlc f2glucose f2crp
    ##    <int>  <int>     <int> <int>
    ## 1   1086   1086      1086  1086

## Indagar diferencias de NAs en variables x triplicado

``` r
# descripcion de circunferencia de cintura en triplicado f0,f1,f2
str(data3[, c("cintur_m1", "cintur_m2", "cintur_m3", "f1cintur_m1", "f1cintur_m2", "f1cintur_m3", "f2cintur_m1", "f2cintur_m2", "f2cintur_m3")])
```

    ## tibble [3,135 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ cintur_m1  : num [1:3135] 98.8 86.8 86.8 113.5 96.5 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 1"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ cintur_m2  : num [1:3135] 98.3 87 86.8 112.3 96.4 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 2"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ cintur_m3  : num [1:3135] 98.9 86.9 86.7 112.3 96.5 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 3"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ f1cintur_m1: num [1:3135] 98.7 86.7 89.7 115.1 93.5 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 1"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ f1cintur_m2: num [1:3135] 98.7 86.8 89.8 115 93.2 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 2"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ f1cintur_m3: num [1:3135] 98.7 87 89.8 115.3 93.2 ...
    ##   ..- attr(*, "label")= chr "Waist: measure 3"
    ##   ..- attr(*, "format.stata")= chr "%12.0g"
    ##  $ f2cintur_m1: Factor w/ 467 levels "No answer","60.2999992370605",..: 320 119 195 NA 239 321 413 243 92 306 ...
    ##   ..- attr(*, "label")= chr "Cintura(cm) medición 1"
    ##  $ f2cintur_m2: Factor w/ 467 levels "No answer","60.2999992370605",..: 320 117 192 NA 237 321 413 241 93 306 ...
    ##   ..- attr(*, "label")= chr "Cintura(cm) medición 2"
    ##  $ f2cintur_m3: Factor w/ 477 levels "No answer","60.2000007629395",..: 329 123 200 NA 244 330 423 249 97 314 ...
    ##   ..- attr(*, "label")= chr "Cintura(cm) medición 3"

``` r
summary(data3[, c("cintur_m1", "cintur_m2", "cintur_m3", "f1cintur_m1", "f1cintur_m2", "f1cintur_m3", "f2cintur_m1", "f2cintur_m2", "f2cintur_m3")])
```

    ##    cintur_m1        cintur_m2        cintur_m3       f1cintur_m1    
    ##  Min.   : 52.50   Min.   : 52.50   Min.   : 52.50   Min.   : 57.20  
    ##  1st Qu.: 85.00   1st Qu.: 85.00   1st Qu.: 85.00   1st Qu.: 85.60  
    ##  Median : 92.00   Median : 92.00   Median : 92.00   Median : 92.50  
    ##  Mean   : 91.86   Mean   : 91.87   Mean   : 91.88   Mean   : 92.62  
    ##  3rd Qu.: 99.00   3rd Qu.: 99.00   3rd Qu.: 99.00   3rd Qu.: 99.47  
    ##  Max.   :138.30   Max.   :138.40   Max.   :138.40   Max.   :136.40  
    ##  NA's   :13       NA's   :13       NA's   :14       NA's   :293     
    ##   f1cintur_m2      f1cintur_m3      f2cintur_m1    f2cintur_m2    f2cintur_m3  
    ##  Min.   : 57.30   Min.   : 57.20   88     :  34   90     :  30   88     :  26  
    ##  1st Qu.: 85.60   1st Qu.: 85.60   96     :  29   88     :  27   90     :  24  
    ##  Median : 92.50   Median : 92.50   98     :  29   96     :  25   96     :  24  
    ##  Mean   : 92.66   Mean   : 92.69   90     :  28   85     :  24   85     :  23  
    ##  3rd Qu.: 99.50   3rd Qu.: 99.50   92     :  27   92     :  24   82     :  22  
    ##  Max.   :136.30   Max.   :136.40   (Other):2506   (Other):2523   (Other):2534  
    ##  NA's   :294      NA's   :295      NA's   : 482   NA's   : 482   NA's   : 482

``` r
summarise(data3, across(c("cintur_m1", "cintur_m2", "cintur_m3", "f1cintur_m1", "f1cintur_m2", "f1cintur_m3", "f2cintur_m1", "f2cintur_m2", "f2cintur_m3"), ~sum(is.na(.))))
```

    ## # A tibble: 1 × 9
    ##   cintur_m1 cintur_m2 cintur_m3 f1cint…¹ f1cin…² f1cin…³ f2cin…⁴ f2cin…⁵ f2cin…⁶
    ##       <int>     <int>     <int>    <int>   <int>   <int>   <int>   <int>   <int>
    ## 1        13        13        14      293     294     295     482     482     482
    ## # … with abbreviated variable names ¹​f1cintur_m1, ²​f1cintur_m2, ³​f1cintur_m3,
    ## #   ⁴​f2cintur_m1, ⁵​f2cintur_m2, ⁶​f2cintur_m3

``` r
# Codebook para psisto measurements
str(data3[, c("pdiast_m1", "pdiast_m2", "pdiast_m3", "f1pdiast_m1", "f1pdiast_m2", "f1pdiast_m3", "f2pdiast_m1", "f2pdiast_m2", "f2pdiast_m3")])
```

    ## tibble [3,135 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ pdiast_m1  : num [1:3135] 79 64 56 75 85 64 92 81 59 84 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 1"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ pdiast_m2  : num [1:3135] 71 64 56 77 81 61 80 77 67 86 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 2"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ pdiast_m3  : num [1:3135] 79 71 57 74 82 64 75 72 71 93 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 3"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f1pdiast_m1: num [1:3135] 66 61 69 82 91 56 79 69 60 68 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 1"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f1pdiast_m2: num [1:3135] 67 66 69 78 92 55 74 65 55 69 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 2"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f1pdiast_m3: num [1:3135] 65 61 67 77 95 57 76 67 59 69 ...
    ##   ..- attr(*, "label")= chr "BDP: Measure 3"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f2pdiast_m1: num [1:3135] 66 67 62 NA 88 60 73 78 60 64 ...
    ##   ..- attr(*, "label")= chr "Presión diastólica(mmHg) Medición 1"
    ##   ..- attr(*, "format.stata")= chr "%8.0g"
    ##  $ f2pdiast_m2: Factor w/ 75 levels "No answer","43",..: 26 26 19 NA 47 19 51 41 19 19 ...
    ##   ..- attr(*, "label")= chr "Presión diastólica(mmHg) Medición 2"
    ##  $ f2pdiast_m3: Factor w/ 76 levels "No answer","31",..: 25 25 20 NA 46 28 48 31 21 22 ...
    ##   ..- attr(*, "label")= chr "Presión diastólica(mmHg) Medición 3"

``` r
summary(data3[, c("pdiast_m1", "pdiast_m2", "pdiast_m3", "f1pdiast_m1", "f1pdiast_m2", "f1pdiast_m3", "f2pdiast_m1", "f2pdiast_m2", "f2pdiast_m3")])
```

    ##    pdiast_m1        pdiast_m2        pdiast_m3       f1pdiast_m1    
    ##  Min.   : 34.00   Min.   : 39.00   Min.   : 43.00   Min.   : 37.00  
    ##  1st Qu.: 66.00   1st Qu.: 66.00   1st Qu.: 65.50   1st Qu.: 66.00  
    ##  Median : 73.00   Median : 72.00   Median : 73.00   Median : 73.00  
    ##  Mean   : 74.12   Mean   : 73.38   Mean   : 73.48   Mean   : 73.43  
    ##  3rd Qu.: 81.00   3rd Qu.: 80.00   3rd Qu.: 80.00   3rd Qu.: 80.00  
    ##  Max.   :180.00   Max.   :182.00   Max.   :182.00   Max.   :144.00  
    ##  NA's   :12       NA's   :12       NA's   :12       NA's   :290     
    ##   f1pdiast_m2      f1pdiast_m3     f2pdiast_m1      f2pdiast_m2  
    ##  Min.   : 38.00   Min.   : 35.0   Min.   : 34.00   74     : 106  
    ##  1st Qu.: 65.00   1st Qu.: 65.0   1st Qu.: 66.00   68     : 103  
    ##  Median : 71.00   Median : 71.0   Median : 73.00   67     : 101  
    ##  Mean   : 72.31   Mean   : 72.4   Mean   : 73.46   72     : 100  
    ##  3rd Qu.: 79.00   3rd Qu.: 79.0   3rd Qu.: 80.00   78     :  97  
    ##  Max.   :139.00   Max.   :138.0   Max.   :125.00   (Other):2146  
    ##  NA's   :290      NA's   :290     NA's   :482      NA's   : 482  
    ##   f2pdiast_m3  
    ##  74     : 120  
    ##  70     : 107  
    ##  71     : 106  
    ##  68     : 104  
    ##  66     : 101  
    ##  (Other):2115  
    ##  NA's   : 482

``` r
summarise(data3, across(c("pdiast_m1", "pdiast_m2", "pdiast_m3", "f1pdiast_m1", "f1pdiast_m2", "f1pdiast_m3", "f2pdiast_m1", "f2pdiast_m2", "f2pdiast_m3"), ~ sum(is.na(.))))
```

    ## # A tibble: 1 × 9
    ##   pdiast_m1 pdiast_m2 pdiast_m3 f1pdia…¹ f1pdi…² f1pdi…³ f2pdi…⁴ f2pdi…⁵ f2pdi…⁶
    ##       <int>     <int>     <int>    <int>   <int>   <int>   <int>   <int>   <int>
    ## 1        12        12        12      290     290     290     482     482     482
    ## # … with abbreviated variable names ¹​f1pdiast_m1, ²​f1pdiast_m2, ³​f1pdiast_m3,
    ## #   ⁴​f2pdiast_m1, ⁵​f2pdiast_m2, ⁶​f2pdiast_m3

## Distribución de criterios de SM

``` r
#missing crp-ubasal: 484

table(data3$sm)
```

    ## 
    ##    0    1 
    ## 1981 1154

``` r
# 3135  

sum(is.na(data3$sm))
```

    ## [1] 0

``` r
# 0

#criterios de sm basal


vars_basal <- data3[,c("psisto_f0", "pad_f0", "cintur_mean_f0", "trig", "hdlc", "glucose")]


vars_basal_resumen <- vars_basal %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarise(
    n = sum(!is.na(valor)),
    minimo = min(valor, na.rm = TRUE),
    Q1 = quantile(valor, 0.25, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    media = mean(valor, na.rm = TRUE),
    Q3 = quantile(valor, 0.75, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    n_NA = sum(is.na(valor)),
    p1 = quantile(valor, 0.01, na.rm = TRUE),
    p99 = quantile(valor, 0.99, na.rm = TRUE),
    menor_p1 = sum(valor < quantile(valor, 0.01, na.rm = TRUE), na.rm = TRUE),
    mayor_p99 = sum(valor > quantile(valor, 0.99, na.rm = TRUE), na.rm = TRUE)
  )


print(vars_basal_resumen)
```

    ## # A tibble: 6 × 13
    ##   variable           n minimo    Q1 mediana media    Q3 maximo  n_NA    p1   p99
    ##   <chr>          <int>  <dbl> <dbl>   <dbl> <dbl> <dbl>  <dbl> <int> <dbl> <dbl>
    ## 1 cintur_mean_f0  3122   52.5  85       92   91.9   99    138.    13  64.4  118 
    ## 2 glucose         3135   46    85       91   98.2  100    537      0  70    292.
    ## 3 hdlc            3135   16    33       40   41.6   48    116.     0  22     77 
    ## 4 pad_f0          3123   45    66.3     73   73.7   80    181.    12  53    105.
    ## 5 psisto_f0       3123   68   106.     116. 119.   128.   222     12  86.7  180.
    ## 6 trig            3135   35    98      136  160.   190.  1299      0  46.3  547 
    ## # … with 2 more variables: menor_p1 <int>, mayor_p99 <int>

``` r
#"colesalt""presart", "diabetesm"
write_xlsx(vars_basal_resumen, path = "vars_basal_resumen.xlsx")
```
