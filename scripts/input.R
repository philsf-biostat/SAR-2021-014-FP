# setup -------------------------------------------------------------------
# library(data.table)
library(tidyverse)
library(readxl)
# library(lubridate)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
# data.raw <- tibble(id=gl(2, 10), group = gl(2, 10), outcome = rnorm(20))
data.raw <- read_excel("dataset/EXCEL ESPINO-PELVICO.xlsx") %>%
  janitor::clean_names()
n.orig <- data.raw %>% nrow
c.orig <- data.raw %>% ncol

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  select(-doencas, -diagnostico, -variacao) %>%
  mutate(
    hipermovel = str_detect(tipo, fixed("*")),
    tipo = str_remove(tipo, fixed("*")),
  ) %>%
  rename(
    id = pacientes,
    dor_t = dor_em_meses,
    dor = lado_da_dor,
    tilt = tilt_em_pe,
  )

# perfil epidemiológico em tabela separada
participantes <- data.raw %>%
  select(
    id,
    sexo,
    idade,
    imc,
    dor_t,
    lombalgia,
    hhs,
    tipo,
    mobilidade,
    tonnis,
  )
data.raw <- data.raw %>%
  select(
    # -sexo,
    # -idade,
    # -imc,
    -dor_t,
    -lombalgia,
    -hhs,
    # -tipo,
    -mobilidade,
    -hipermovel,
    # -tonnis,
  )

# reshape
data.raw <- data.raw %>%
  pivot_longer(ends_with(c("_d", "_e"))) %>%
  separate(name, c("name", "lado")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(lado = ifelse(lado == "d", 1, 2))

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    id = factor(id), # or as.character
    sexo = factor(sexo),
    tipo = factor(tipo),
    dor = case_when(
      dor == lado ~ 1,
      dor == 3 ~ 1,
      TRUE ~ 0,
    ),
    tonnis = factor(tonnis),
    group = ifelse(tonnis %in% 0:1, "A", "B"),
    group = factor(group, labels = c("Sadio", "Artrose")),
  )
participantes <- participantes %>%
  mutate(
    id = factor(id), # or as.character
    sexo = factor(sexo, labels = c("Feminino", "Masculino")),
    tonnis = factor(tonnis, labels = c("Normal", "Leve", "Moderada", "Grave")),
    tipo = factor(tipo, levels = c("1A", "1B", "2A", "2B")),
    lombalgia = ifelse(lombalgia == 2, 0, 1),
    mobilidade = factor(mobilidade, labels = c("Normal", "Hipermóvel", "Rígido"))
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    slope_em_pe = "Slope (em pé)",
    slope_sentado = "Slope (sentado)",
    tilt = "Tilt (em pé)",
    acb = "ACB",
    ia = "IA",
    alfa = "Alfa",
  )

participantes <- participantes %>%
  set_variable_labels(
    sexo = "Sexo",
    idade = "Idade (anos)",
    imc = "IMC (kg/m²)",
    lombalgia = "Ocorrência de lombalgia",
    hhs = "HHS",
    tipo = "Tipo",
    tonnis = "Classificação Tonnis",
    mobilidade = "Mobilidade",
    # variacao = "Variação",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  select(
    -tonnis
  )

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(nrow(analytical)) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
