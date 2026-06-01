rm(list = ls())
library(dplyr)
library(purrr)
library(readxl)
library(stringi)
library(openxlsx)


# save(data, file = "data-2026.RData")
# saveRDS(data, 'data-2026.rds')
data <- readRDS("data-2026.rds")


##################################
## PRIPRAVA DAT
##################################

# Nacteni dat
data <- read_excel("vstupni_data_2026.xlsx", sheet = 1) |> 
  select(-matches("^ZV_\\d{2}$"))

# Nazvy sloupcu
colnames(data) <- colnames(data) |>
  tolower() |>
  stringi::stri_trans_general("Latin-ASCII") |>  # odstranění diakritiky
  gsub("\\s*\\(.*?\\)", "", x = _) |>            # odstranění textu v závorkách
  gsub("\\s+", "_", x = _) |>                    # mezery -> _
  gsub("_+", "_", x = _) |>                      # vícenásobné _ -> _
  gsub("_$", "", x = _)                          # odstranění _ na konci

# Transformace: ANO -> ano, NE -> ne, NAs
data <- data |>
  mutate(
    across(everything(), ~ gsub("^(ANO?)$|^(Ano?)$", "ano", .x)),
    across(everything(), ~ gsub("^(NE?)$|^(Ne?)$|^(nee)$", "ne", .x)),
    across(everything(), ~ gsub("^(na)$", "NA", .x)),
    across(where(is.character), ~ na_if(.x, "NA"))
  )

# Nahrad NaN za NA
data <- data |> mutate_all(~replace(., is.nan(.), NA))

# Tridy sloupcu a pojmenovani
data[, c(4:6, 8:23)] <- lapply(data[, c(4:6, 8:23)], factor)
data <- data |> 
  rename(ucetnictvi = "prevazujici_ucetnictvi") |>
  mutate(vznik = lubridate::year(as.Date.character(vznik, format = "%Y")))
data[, c(3, 24:34)] <- lapply(data[, c(3, 24:34)], as.numeric)


# Odstraň sloupce  s malou vypovídací hodnotou, tj. většinové zasotupení nějaké kategorie
summary(data)
data <- data |> select(-c(zv_ucel, zv_funkce, zv_older))

# Kategorizace
data$f_mesto <- as.factor(cut(data$obyvatele, breaks = c(0, 10000, 50000, 100000, 1000000, 1500000),
                                   labels = c('Maloměsto do 10 t.','Střední město do 50 t.',
                                              'Větší město do 100 t.', 'Velkoměsto do 1 mil.',
                                              'Světové velkoměsto nad 1 mil.')))
data$f_mesto_dummy <- as.factor(cut(data$obyvatele, breaks = c(0, 50000, 1500000),
                                            labels = c('Město do 50 t.','Město nad 50 t.')))
data$f_vznik <- as.factor(cut(data$vznik, breaks = c(0, 1993, 2030), right = FALSE,
                              labels = c("před 1993", "po 1993")))
data$ucetnictvi <- factor(data$ucetnictvi, labels = c("jednoduché", "zjednodušené", "plné"),
                         levels = c("jednoduché účetnictví",
                                    "podvojné účetnictví ve zjednodušeném rozsahu",
                                    "podvojné účetnictví v plném rozsahu"))
data$f_zamestnanost <- as.factor(ifelse(data$zamestnanost == '1 - 5 zaměstnanců', 'do 5 zaměstnanců',
                                        'více než 5 zaměstnanců'))

roz <- c("vc","hc")

data$f_zadluzenost <- factor(cut(data$zadluzenost,
                                       breaks = c(min(data$zadluzenost, na.rm = T) - 0.1, 0.05,
                                                  max(data$zadluzenost, na.rm = T) + 0.1),
                                       c('do 5 %', 'nad 5 %')))
data$f_majetkova_struktura <- factor(cut(data$majetkova_struktura,
                                                breaks = c(min(data$majetkova_struktura, na.rm = T) - 0.1, 0.9,
                                                           max(data$majetkova_struktura, na.rm = T) + 0.1), 
                                                c('do 90 %', 'nad 90 %')))
data$f_hhi <- factor(cut(data$hc_hhi,
                               breaks = c(min(data$hc_hhi, na.rm = T) - 0.1, 0.6,
                                          max(data$hc_hhi, na.rm = T) + 0.1), c('do 0,6', 'nad 0,6')))

# kontakt - alespoň jednou ano
data <- data |>
  mutate(
    kontakty_obecne = as.factor(if_else(
      coalesce(kontakt_jmeno, "ne") == "ne" &
        coalesce(kontakt_e_mail, "ne") == "ne" &
        coalesce(kontakt_telefon, "ne") == "ne",
      "ne",
      'ano'
    ))
  )

# Smaz neporebna data
data[data$zv_vse != data$zv_kontinualni, c('zv_vse', 'zv_kontinualni')]
data$zv_vse <- NULL # shodne

table(data$zv_vse)
## ano  ne 
## 40 40 

table(data$zv_kontinualni)
## ano  ne 
## 40 40 
## stejné výsledky -> stačí použít jen jednu vysvětlovnaou proměnnou


##################################
## Kontingnencni tabulky
##################################

kvalitativni <- data |>
  select(where(is.factor)) |>
  select(-matches("^(zv_kontinualni|zv_kontinualni_minule|zv_vse|zv_vse_minule|f_mesto|zamestnanost)$")) |>
  names()

zv <- c('zv_kontinualni') # c('zv_vse', 'zv_kontinualni')

kontingencni_tabulky <- list()

for (j in zv) {
  for (i in kvalitativni) {
    
    tab <- table(data[[j]], data[[i]])
    nazev <- paste0(j, "_VS_", i)
    
    kontingencni_tabulky[[paste0("abs_", nazev)]] <- addmargins(tab)
    kontingencni_tabulky[[paste0("podm_rad_", nazev)]] <- prop.table(tab, margin = 1)
    kontingencni_tabulky[[paste0("rel_", nazev)]] <- addmargins(prop.table(tab))
  }
}

cat(capture.output(print(kontingencni_tabulky), file = "kontingencni_tabulky_2026.txt"))

# Vymazani promennych
rm(list = ls(pattern = "abs_"))
rm(list = ls(pattern = "podm_rad_"))
rm(list = ls(pattern = "rel_"))


##################################
## Testy zavislosti - kvalitativni promenne
##################################

kvalitativni <- data |>
  select(where(is.factor)) |>
  select(-matches("^(zv_kontinualni|zv_kontinualni_minule|zv_vse|zv_vse_minule|f_mesto|zamestnanost)$")) |>
  names()


# chi kvadrat testy, Fisher presny (exaktni) test pri poruseni podminek dobre aproximace
chisq_zavislost <- map(set_names(kvalitativni), \(var) {
  set.seed(123)
  foo <- chisq.test(data$zv_kontinualni, data[[var]], simulate.p.value = TRUE, B = 10000)

  podminka <- if (
    (sum(foo$expected >= 5) + sum(foo$expected < 5 & foo$expected >= 2)) /
    length(foo$observed) == 1
  ) "splneno" else "nesplneno"

  K <- foo$statistic
  n <- sum(foo$observed)
  m <- min(dim(foo$observed))
  cramer_v <- sqrt(K / (n * (m - 1)))
  cramer_v_str <- case_when(
    cramer_v < 0.1 ~ paste(round(cramer_v, 3), "zanedbatelna zavislost"),
    cramer_v < 0.3 ~ paste(round(cramer_v, 3), "slaba zavislost"),
    cramer_v < 0.7 ~ paste(round(cramer_v, 3), "stredni zavislost"),
    .default       ~ paste(round(cramer_v, 3), "silna zavislost")
  )

  foo2 <- chisq.test(data$zv_kontinualni, data[[var]], simulate.p.value = TRUE)

  tibble(
    X_vs_Y               = paste0("zv_kontinualni_VS_", var),
    neprazdne            = sum(complete.cases(data[[var]])),
    podminky_dobre_aprox = podminka,
    chisq_p_value        = foo2$p.value,
    chisq_zavisle        = foo2$p.value < 0.05,
    crameruv_index       = cramer_v_str,
    fisher_p_value       = fisher.test(data$zv_kontinualni, data[[var]])$p.value,
    fisher_zavisle       = fisher.test(data$zv_kontinualni, data[[var]])$p.value < 0.05
  )
}) |> list_rbind()

write.csv2(chisq_zavislost, "KVALITATIVNI_zavislost_2026.csv", fileEncoding = "UTF-8")
# 2024: Odstranila jsem zamestance - vsichni 1-5



##################################
## Ciselne charakteristiky - spojite promenne
##################################

library(nortest) #lillie.test()

spojite <- grep('^(obyvatele|majet|hc|vc|zadluzenost)', sort(names(data)), value = T)
spojite_vyber <- data[, spojite]

# zkouska struktury
str(spojite_vyber)
names(spojite_vyber)[sapply(spojite_vyber, is.character)]
names(spojite_vyber)[sapply(spojite_vyber, is.factor)]

zv_kontinualni_RUCNE <- data |>
  select(zv_kontinualni, all_of(spojite)) |>
  pivot_longer(
    cols = all_of(spojite),
    names_to = "promenna",
    values_to = "hodnota"
  ) |>
  group_by(zv_kontinualni, promenna) |>
  summarise(
    notNAs = sum(!is.na(hodnota)),
    NAs = sum(is.na(hodnota)),
    min = ifelse(all(is.na(hodnota)), NA_real_, round(min(hodnota, na.rm = TRUE), 2)),
    max = ifelse(all(is.na(hodnota)), NA_real_, round(max(hodnota, na.rm = TRUE), 2)),
    avg = round(mean(hodnota, na.rm = TRUE), 2),
    median = round(median(hodnota, na.rm = TRUE), 2),
    smodch = round(sd(hodnota, na.rm = TRUE), 3),
    .groups = "drop"
  ) |> arrange(promenna)

zv_kontinualni_FCE_SUMMARY <- 
  apply(X = data[,spojite], 2,
        function(x) 
          tapply(X = x, INDEX = data$zv_kontinualni, FUN = summary, digits = 3))

write.xlsx(
  zv_kontinualni_RUCNE,
  file = "KATEG_SPOJ_ciselne_charakteristiky_2026.xlsx",
  overwrite = TRUE
)

rm(list = ls(pattern = ".RUCNE$"))


##################################
## T-TESTY
##################################

# Prazdny dataframe na output
testy_strhod <- data.frame(X_vs_Y = NA, pozorovani = NA,
                           prumer = NA, rozdil_prumeru = NA, median = NA, rozdil_medianu = NA, smodch = NA,
                           normalita_metoda = NA, normalita_p_hodnota = NA, normalita = NA,
                           ttest_metoda = NA, ttest_p_hodnota = NA, ttest_IS = NA, shoda_str_hodn = NA,
                           cohenuv_koef_ucinku = NA,
                           wilcoxon_p_hodnota = NA, wilcoxon_teststat = NA, shoda_medianu = NA,
                           poruseni_symetrie = NA)

# Funkce na testy normality v zavislosti na velikosti vyberu
test_normality <- function(x,...){
  n <- sum(complete.cases(x))
  if (n < 3 | length(unique(x[complete.cases(x) == TRUE])) == 1) {
    return(NA)
  } else if (n < 50) {
    return(shapiro.test(x)[...])
  } else if (n >= 50 & n < 100) {
    return(nortest::ad.test(x)[...])
  } else {
    return(nortest::lillie.test(x)[...])
  }
}


# pocet vyplnenych radku
sapply(data, function(x) sum(complete.cases(x)))

# Funkce na cohenuv index ucinku skupiny
cohen  <- function(x, y) {
  m1 <- mean(x, na.rm = TRUE)
  m2 <- mean(y, na.rm = TRUE)
  n1 <- sum(complete.cases(x))
  n2 <- sum(complete.cases(y))
  s1 <- sd(x, na.rm = TRUE)
  s2 <- sd(y, na.rm = TRUE)
  s.vazeny <- ((n1 - 1) * s1 ^ 2 + (n2 - 1) * s2 ^ 2) / (n1 + n2 - 2)
  cohen.d <- abs(m1-m2)/s.vazeny
  if (cohen.d < 0.2) {
    return(paste(round(cohen.d, 3), 'zanedbatelny ucinek'))
  } else if (cohen.d < 0.5) {
    return(paste(round(cohen.d, 3), 'maly ucinek'))
  } else if (cohen.d < 0.8) {
    return(paste(round(cohen.d, 3), 'stredni ucinek'))
  } else {
    return(paste(round(cohen.d, 3), 'velky ucinek'))
  }
}


alpha <- 0.05
l <- 1
for (j in zv){
  for (i in spojite){
    x <- data[[i]][data[[j]] == "ano"]
    y <- data[[i]][data[[j]] == "ne"]
    # test symetrie
    ties <- ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                   tryCatch(expr = wilcox.test(y, x, alternative = "t", mu = 0, exact = T, correct = F),
                            warning = function(w) if (grepl("ties", conditionMessage(w))) TRUE else FALSE),
                   NA)
    testy_strhod[l,] <- c(paste(j,'_ANO_VS_',i, sep = ""),
                          sum(complete.cases(x)),
                          round(mean(x, na.rm = TRUE), 3),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1 & mean(y, na.rm = T) != 0,
                                 round(mean(x, na.rm = T)/mean(y, na.rm = T) - 1,3),
                                 NA),
                          round(median(x, na.rm = TRUE), 3),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1 & median(y, na.rm = T) != 0,
                                 round(median(x, na.rm = T) / median(y, na.rm = T) - 1,3),
                                 NA),
                          round(sd(x, na.rm = TRUE), 3),
                          test_normality(x, 'method'),
                          test_normality(x, 'p.value'), # male vybery < 50
                          test_normality(x, 'p.value') > alpha,
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 ifelse(var.test(x, y, ratio = 1)$p.value > alpha, 
                                        't-test (stejne rozptyly)', 't-test (ruzne rozptyly)'),
                                 NA), # t-test s welch. aproximaci vs klasicky t. test
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 ifelse(var.test(x, y, ratio = 1)$p.value > alpha,
                                        t.test(x, y, alternative = "t", mu = 0, 
                                               conf.level = (1 - alpha), var.equal = T)$p.value,
                                        t.test(x, y, alternative = "t", mu = 0, 
                                               conf.level = (1 - alpha), var.equal = F)$p.value),
                                 NA),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 ifelse(var.test(x, y, ratio = 1)$p.value > alpha,
                                        paste(round(c(t.test(x, y, alternative = "t", mu = 0, 
                                                             var.equal = T, conf.level = (1 - alpha))$conf), 3),
                                              collapse = ";"),
                                        paste(round(c(t.test(x, y, alternative = "t", mu = 0, 
                                                             var.equal = F, conf.level = (1 - alpha))$conf), 3),
                                              collapse = ";")),
                                 NA),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 ifelse(var.test(x, y, ratio = 1)$p.value > alpha,
                                        t.test(x, y, alternative = "t", mu = 0, 
                                               conf.level = (1 - alpha), var.equal = T)$p.value > alpha,
                                        t.test(x, y, alternative = "t", mu = 0, 
                                               conf.level = (1 - alpha), var.equal = F)$p.value > alpha),
                                 NA),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 cohen(x,y),
                                 NA),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 wilcox.test(x, y, alternative = "t", mu = 0, exact = F, correct = F)$p.value,
                                 NA),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 wilcox.test(x, y, alternative = "t", mu = 0, exact = F, correct = F)$statistic,
                                 NA),
                          ifelse(sum(complete.cases(x)) > 1 & sum(complete.cases(y)) > 1,
                                 wilcox.test(x, y, alternative = "t", mu = 0, exact = F, correct = F)$p.value > alpha,
                                 NA),
                          ifelse(is.logical(ties), ties, FALSE))
    testy_strhod[l+1,] <- c(paste(j,'_NE_VS_',i, sep = ""),
                            sum(complete.cases(y)),
                            round(mean(y, na.rm = TRUE), 3),
                            NA,
                            round(median(y, na.rm = TRUE), 3),
                            NA,
                            round(sd(y, na.rm = TRUE), 3),
                            test_normality(y, 'method'),
                            test_normality(y, 'p.value'), # male vybery < 50
                            test_normality(y, 'p.value') > alpha,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA)
    l <- l+2
  }
}

write.csv2(testy_strhod,"SPOJITE_testy_strednich_hodnot_2025.csv")




##################################
## LOGISTICKA REGRESE
##################################

apply(data, 2, \(x) sum(complete.cases(x)))
colSums(!is.na(data))

data_na <- na.omit(data[, c('zv_kontinualni', 'obyvatele', 'vznik', 'podrizene_urovne',
                           'organizace', 'uroven', 'web', 'kontakty_obecne','transparentni_bu',
                           'vedlejsi_cinnost', 'zv_stanovy',  'ucetnictvi', 'majetek',
                           'f_mesto_dummy', 'f_vznik', 'f_majetkova_struktura')]) # 'f_hhi'
# vyřadím kvůli hodně NAs... 'hc_naklady', 'hc_vynosy', 'hc_vh', 'hc_hhi', 'f_mesto', 'f_vznik', 'f_zadluzenost', 'f_majetkova_struktura'

dim(data_na)
data_na$ucetnictvi <- factor(data_na$ucetnictvi) # zmizelo jendoduché účetnictví
data_na$zv_kontinualni <- factor(data_na$zv_kontinualni, levels = c('ne', 'ano'))

lapply(data_na[sapply(data_na, is.factor)], table)

fullmod <- glm(zv_kontinualni ~ obyvatele + vznik + podrizene_urovne + 
                 vedlejsi_cinnost + kontakty_obecne + ucetnictvi + majetek + hc_vynosy + 
                 hc_vh + f_hhi + f_majetkova_struktura + hc_hhi + f_vznik + f_mesto_dummy, 
               data = data_na, family = binomial) # zv_ucel,  transparentni_bu 

performance::check_collinearity(fullmod)

nothing <- glm(zv_kontinualni ~ 1, data = data_na, family = binomial)

# Backwards selection is the default
#--------------------------
backwards <- step(fullmod)
summary(backwards)

# Coefficients:
#                               Estimate  Std. Error  z value   Pr(>|z|)  
# (Intercept)                   2.273e+02  1.289e+02   1.763   0.0779 .
# obyvatele                    -5.421e-06  4.363e-06  -1.243   0.2140  
# vznik                        -1.143e-01  6.453e-02  -1.771   0.0766 .
# podrizene_urovnene           -2.005e+00  1.260e+00  -1.591   0.1116  
# majetek                      -7.236e-04  3.607e-04  -2.006   0.0448 *
# hc_vynosy                     3.782e-04  2.954e-04   1.280   0.2005  
# hc_vh                        -4.037e-03  2.039e-03  -1.980   0.0477 *
# f_hhinad 0,6                  2.032e+00  1.354e+00   1.501   0.1334  
# f_mesto_dummyMěsto nad 50 t.  2.886e+00  1.482e+00   1.947   0.0516 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# AIC: 48.7

backwards$deviance ## [1] 30.70005(aby bylo co nejemnsi)



# Forwards
#--------------------------
forwards <- step(nothing, scope = list(lower = formula(nothing), upper = formula(fullmod)), direction = "forward")
summary(forwards)

# Coefficients:
#                                Estimate   Std. Error  z value Pr(>|z|)  
# (Intercept)                    1.324e+02  8.428e+01   1.571   0.1161  
# majetek                       -3.234e-04  1.408e-04  -2.298   0.0216 *
# f_majetkova_strukturanad 90 % -1.769e+00  9.462e-01  -1.870   0.0615 .
# vznik                         -6.620e-02  4.217e-02  -1.570   0.1165  
# f_mesto_dummyMěsto nad 50 t.   1.287e+00  8.957e-01   1.437   0.1507  
# 
# AIC: 48.291

forwards$deviance ## [1] 38.29069 ... vyssi
# Obecně platí, že nižší hodnota deviance naznačuje lepší shodu mezi modelem a daty, protože menší odchylka znamená, že model lépe vysvětluje pozorované jevy. 



# Stepwise
#--------------------------
bothways <- step(nothing, list(lower = formula(nothing), upper = formula(fullmod)),
                 direction = "both", trace = 0)

summary(bothways)
# Coefficients:
#                                Estimate   Std. Error  z value Pr(>|z|)  
# (Intercept)                    1.324e+02  8.428e+01   1.571   0.1161  
# majetek                       -3.234e-04  1.408e-04  -2.298   0.0216 *
# f_majetkova_strukturanad 90 % -1.769e+00  9.462e-01  -1.870   0.0615 .
# vznik                         -6.620e-02  4.217e-02  -1.570   0.1165  
# f_mesto_dummyMěsto nad 50 t.   1.287e+00  8.957e-01   1.437   0.1507  
# 
# AIC: 48.291

bothways$deviance ## [1] 38.29069 ... stejné jako u forward
# bothways == forwards

formula(backwards)
# zv_kontinualni ~ obyvatele + vznik + podrizene_urovne + majetek + hc_vynosy + hc_vh + f_hhi + f_mesto_dummy
formula(forwards)
# zv_kontinualni ~ majetek + f_majetkova_struktura + vznik + f_mesto_dummy
formula(bothways)
# zv_kontinualni ~ majetek + f_majetkova_struktura + vznik + f_mesto_dummy

cbind(backwards$aic, forwards$aic, bothways$aic)
# [1,] 48.70005 48.29069 48.29069

# Změna referenční hladiny
data_na$zv_kontinualni <- relevel(data_na$zv_kontinualni, ref = "ne")
model <- glm(formula = zv_kontinualni ~ majetek + f_majetkova_struktura + 
               f_mesto_dummy + f_vznik + organizace + podrizene_urovne, 
             family = binomial, data = data_na)
model$aic
summary(model)
exp(model$coefficients) # poměr šancí
exp(-model$coefficients) # inverzní poměr šancí

# Coefficients:
#                                 Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                    -5.0092710  2.2060285   2.271   0.0232 *
# majetek                        0.0005717   0.0002253  -2.538   0.0111 *
# f_majetkova_strukturanad 90 %  2.3209871   1.2125023  -1.914   0.0556 .
# f_mesto_dummyMěsto nad 50 t.   -1.0129157  1.0124864   1.000   0.3171  
# f_vznikpo 1993                 -0.2101772  1.1451482   0.184   0.8544  
# organizaceFAČR                 3.6700336   1.7560428  -2.090   0.0366 *
# organizacehasiči               5.3850629   2.1469128  -2.508   0.0121 *
# organizacesokoli               3.7389934   1.6472642  -2.270   0.0232 *
# podrizene_urovnene             -1.6862146  1.1903516   1.417   0.1566  

# Pozn.: Všechny uvedené interpretace platí při stejných hodnotách ostatních proměnných v modelu.
# -----------------------------------------------------
# Organizace po roce 1993 včetně má o -19 % nižší šanci na zveřejnění než ty starší organizace
# Každé zvýšení majetku o 1 tis. mírně zvyšuje šanci (o cca 0.05 %). Tento efekt je statisticky významný (p = 0.0111).
# Organizace ve větších obcích (nad 50 t.) mají přibližně o 63,7 % nižší šanci zveřejnit kontinuální záznam než organizace v menších obcích.
# Organizace s vyšší majetkovou strukturou (nad 90%) mají cca 10 krát vyšší šanci zveřejnit kontinuální záznam.
# Organizace s nižší majetkovou strukturou (do 90%) mají o cca 90 % nižší šanci zveřejnit kontinuální záznam.
# Organizace s pozřízenou úornví mají přibližně 5.4 nižší šanci zveřejnit kontinuální záznam.
# Červený kříž má šanci zveřejnit kontinuální záznam přibližně o 97,5 % nižší než FAČR, o 99,5 % nižší než hasiči a o 97,6 % nižší než sokoli



##################################
## PANEL - porovnání změn zvěřejňování v čase
##################################

data |> select(zv_kontinualni, zv_kontinualni_minule) |> mutate(zv_kontinualni == zv_kontinualni_minule) |> View()
# u 8 došlo k změně při zvěřejňování - 4 začaly zveřejňovat a 4 přestaly
                            
data <- data |> 
  mutate(zv_kontinualni_zmena = case_when(
    zv_kontinualni_minule == "ano" & zv_kontinualni == "ano" ~ "stabilně ano",
    zv_kontinualni_minule == "ne" & zv_kontinualni == "ne" ~ "stabilně ne",
    zv_kontinualni_minule == "ne" & zv_kontinualni == "ano" ~ "začali",
    zv_kontinualni_minule == "ano" & zv_kontinualni == "ne" ~ "přestali"
  ))

tab_text <- function(x, useNA = FALSE, prob = FALSE) {
  tab <- table(
    x,
    useNA = ifelse(useNA, "ifany", "no")
  )
  
  if (!prob) {
    paste(
      names(tab),
      as.vector(tab),
      collapse = ", "
    )
  } else if (prob) {
    paste(
      names(tab),
      scales::percent(as.numeric(prop.table(tab)), accuracy = 1),
      collapse = ", "
    )
  }

}

charakterisitky_zmen <- data |> group_by(zv_kontinualni_zmena) |>
  summarise(pocet = n(),
            organizace = tab_text(organizace, prob = TRUE),
            obyvatele_median = median(obyvatele, na.rm = TRUE),
            f_mesto_dummy = tab_text(f_mesto_dummy, prob = TRUE),
            vznik_median = median(vznik, na.rm = TRUE),
            f_vznik = tab_text(f_vznik, prob = TRUE),
            podrizene_urovne = tab_text(podrizene_urovne, prob = TRUE),
            uroven = tab_text(uroven, prob = TRUE),
            web = tab_text(web, prob = TRUE),
            kontakty_obecne = tab_text(kontakty_obecne, prob = TRUE),
            transparentni_bu = tab_text(transparentni_bu, prob = TRUE),
            zv_stanovy = tab_text(zv_stanovy, prob = TRUE),
            vedlejsi_cinnost = tab_text(vedlejsi_cinnost, prob = TRUE),
            ucetnictvi = tab_text(ucetnictvi, prob = TRUE),
            f_zamestnanost = tab_text(f_zamestnanost, prob = TRUE),
            majetek_median = median(majetek, na.rm = TRUE),
            hc_naklady_median = median(hc_naklady, na.rm = TRUE),
            hc_vynosy_median = median(hc_vynosy, na.rm = TRUE),
            hc_vh_median = median(hc_vh, na.rm = TRUE),
            hc_hhi_median = round(median(hc_hhi, na.rm = TRUE), 3),
            f_hhi = tab_text(f_hhi, prob = TRUE),
            majetkova_struktura_median = round(median(majetkova_struktura, na.rm = TRUE), 3),
            f_majetkova_struktura = tab_text(f_majetkova_struktura, prob = TRUE),
            zadluzenost = round(median(zadluzenost, na.rm = TRUE), 3),
            f_zadluzenost = tab_text(f_zadluzenost, prob = TRUE))

openxlsx::write.xlsx(
  charakterisitky_zmen,
  "charakterisitky_zmen_2026.xlsx",
  overwrite = TRUE
)

#wilcox.test jedno a dvouvyberovy Wilcoxonuv test
#kruskal.test Kruskal-Wallisuv test (neparametricka ANOVA s jednim faktorem)
#friedman.test Friedmanova neparametricka ANOVA s dvema faktory
#cor.test(method="kendall") test o nulovosti Kendallova
#cor.test(method="spearman") test o nulovosti Spearmanova
