# Transparentnost neziskového sektoru: kontinuální zveřejňování účetních závěrek

Analýza faktorů spojených s kontinuálním plněním zákonné povinnosti zveřejňovat účetní závěrky u poboček čtyř velkých českých NNO: Českého červeného kříže (ČČK), Fotbalové asociace ČR (FAČR), Sdružení hasičů Čech, Moravy a Slezska a České obce sokolské.

**Autoři:** Romana Pejcalová, Jakub Pejcal  
**Datum:** květen 2026

## Výzkumný vzorek

80 poboček ze čtyř zastřešujících organizací — záměrně vybraných tak, aby přesně polovina (40) kontinuálně splňovala zákonnou povinnost zveřejňovat účetní závěrky a polovina (40) tuto povinnost neplnila.

## Použité metody

- Kontingenční tabulky a χ² testy dobré shody (Cramérovo V)
- Dvouvýběrový Wilcoxonův test
- Logistická regrese se stepwise selekcí

## Popis proměnných (codebook)

### Vysvětlovaná proměnná

| Proměnná | Typ | Popis |
|---|---|---|
| `zv_kontinualni` | faktor (ano/ne) | Kontinuální zveřejňování účetních závěrek – klíčová vysvětlovaná proměnná |
| `zv_kontinualni_minule` | faktor (ano/ne) | Kontinuální zveřejňování v předchozím sledovaném období |

### Charakteristiky organizace

| Proměnná | Typ | Popis |
|---|---|---|
| `organizace` | faktor | Zastřešující organizace (ČČK, FAČR, hasiči, sokoli) |
| `uroven` | faktor | Organizační úroveň pobočky |
| `vznik` | číselná | Rok vzniku organizace |
| `obyvatele` | číselná | Počet obyvatel obce sídla |
| `web` | faktor (ano/ne) | Přítomnost webových stránek |
| `kontakt_jmeno` | faktor (ano/ne) | Zveřejněné jméno kontaktní osoby |
| `kontakt_e_mail` | faktor (ano/ne) | Zveřejněný e-mail |
| `kontakt_telefon` | faktor (ano/ne) | Zveřejněný telefon |
| `kontakty_obecne` | faktor (ano/ne) | Alespoň jeden kontakt (odvozená proměnná) |
| `ucetnictvi` | faktor | Typ účetnictví (jednoduché / zjednodušené / plné) |
| `podrizene_urovne` | faktor (ano/ne) | Existence podřízených organizačních jednotek |
| `vedlejsi_cinnost` | faktor (ano/ne) | Vedlejší (hospodářská) činnost |
| `zv_stanovy` | faktor (ano/ne) | Stanovy zveřejněny ve sbírce listin |
| `transparentni_bu` | faktor (ano/ne) | Transparentní bankovní účet |
| `zamestnanost` | faktor | Velikostní kategorie zaměstnanosti |

### Ekonomické charakteristiky

Průměr za dostupná účetní období, hodnoty v tis. Kč (není-li uvedeno jinak).

| Proměnná | Typ | Popis |
|---|---|---|
| `majetek` | číselná | Průměrný majetek |
| `hc_vynosy` | číselná | Výnosy z hlavní činnosti |
| `hc_naklady` | číselná | Náklady na hlavní činnost |
| `hc_vh` | číselná | Výsledek hospodaření z hlavní činnosti |
| `hc_hhi` | číselná (0–1) | Herfindahl-Hirschmanův index výnosové koncentrace (hlavní činnost) |
| `majetkova_struktura` | číselná (0–1) | Podíl oběžného majetku na celkovém majetku |
| `zadluzenost` | číselná (0–1) | Podíl krátkodobých závazků na majetku |

### Kategorizované proměnné (odvozené)

| Proměnná | Kategorie | Popis |
|---|---|---|
| `f_mesto` | 5 kategorií | Velikost sídla (do 10 t. / do 50 t. / do 100 t. / do 1 mil. / nad 1 mil.) |
| `f_mesto_dummy` | do 50 t. / nad 50 t. | Dummy pro velikost sídla |
| `f_vznik` | před 1993 / po 1993 | Dummy pro období vzniku |
| `f_majetkova_struktura` | do 90 % / nad 90 % | Kategorizovaná majetková struktura |
| `f_zadluzenost` | do 5 % / nad 5 % | Kategorizovaná zadluženost |
| `f_hhi` | do 0,6 / nad 0,6 | Kategorizovaný HHI |
| `f_zamestnanost` | do 5 / více než 5 | Kategorizovaná zaměstnanost |
| `zv_kontinualni_zmena` | 4 kategorie | Změna chování: stabilně ano / stabilně ne / začali / přestali |

## Soubory

| Soubor | Popis |
|---|---|
| `transparentnost_nno_2026.qmd` | Zdrojový Quarto dokument |
| `data_2026_chisq_t-test.R` | R skript s analýzami |
| `index.html` | Publikovaný HTML report |

## Report

Interaktivní report je dostupný na:  
https://ropej.github.io/transparentnost-nno-2026/
