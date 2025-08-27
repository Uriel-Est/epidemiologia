library(tidyverse)
library(geobr)
library(sidrar)

# Filtrando os municípios por RGI -----------------------------------------
# 1. Extrair os códigos municipais
jp_codes <- mun_jp$code_muni
sc_codes <- mun_sc$code_muni
rgi_codes <- c(jp_codes, sc_codes)

# 2. Filtrar para pb_(ano)
pb_1991_filt <- pb_1991 |>
  filter(cod7 %in% rgi_codes) |>
  mutate(
    region = case_when(
      cod7 %in% jp_codes ~ "João Pessoa",
      T ~ "Sousa-Cajazeiras"
    )
  )

pb_2000_filt <- pb_2000 |>
  filter(cod7 %in% rgi_codes) |>
  mutate(
    region = case_when(
      cod7 %in% jp_codes ~ "João Pessoa",
      T ~ "Sousa-Cajazeiras"
    )
  )

pb_2010_filt <- pb_2010 |>
  filter(cod7 %in% rgi_codes) |>
  mutate(
    region = case_when(
      cod7 %in% jp_codes ~ "João Pessoa",
      T ~ "Sousa-Cajazeiras"
    )
  )

pb_2022_filt <- pb_2022 |>
  filter(cod7 %in% rgi_codes) |>
  mutate(
    region = case_when(
      cod7 %in% jp_codes ~ "João Pessoa",
      T ~ "Sousa-Cajazeiras"
    )
  )


# Estimativa Populacional (função) -------------------------------------------------
# =========================
# AJUSTE PARA 1º DE JULHO
# =========================

#-----------------------------
# util: garante chaves válidas
#-----------------------------
._resolve_chaves <- function(df, chaves){
  intersect(chaves, names(df))
}

#---------------------------------------------------------
# 1) Taxa geométrica DIÁRIA entre 2 fotografias (df_A->df_B)
#    r_dia = exp( (ln(P_B*) - ln(P_A*)) / Δdias ) - 1
#    * com correção-epsilon p/ zeros
#---------------------------------------------------------
calcular_taxa_crescimento_diaria <- function(
    df_A, df_B, data_A, data_B,
    pop_var = "pop",
    chaves = c("cod7","municipio","sex","age_group","region"),
    tratamento_zero = c("epsilon","clip0","drop"),
    epsilon = 0.5
){
  tratamento_zero <- match.arg(tratamento_zero)
  # padroniza tipos p/ evitar fator vs char
  conv <- intersect(c("sex","age_group","region"), intersect(names(df_A), names(df_B)))
  df_A <- df_A |> dplyr::mutate(dplyr::across(dplyr::all_of(conv), as.character))
  df_B <- df_B |> dplyr::mutate(dplyr::across(dplyr::all_of(conv), as.character))
  
  keys <- intersect(chaves, intersect(names(df_A), names(df_B)))
  if (!length(keys)) stop("Nenhuma chave em comum p/ o join.")
  
  joined <- df_A |>
    dplyr::select(dplyr::all_of(keys), pop_A = dplyr::all_of(pop_var)) |>
    dplyr::inner_join(
      df_B |> dplyr::select(dplyr::all_of(keys), pop_B = dplyr::all_of(pop_var)),
      by = keys
    )
  
  if (tratamento_zero == "epsilon") {
    joined <- joined |>
      dplyr::mutate(
        pop_A = ifelse(is.na(pop_A) | pop_A <= 0, epsilon, pop_A),
        pop_B = ifelse(is.na(pop_B) | pop_B <= 0, epsilon, pop_B)
      )
  } else if (tratamento_zero == "clip0") {
    joined <- joined |>
      dplyr::mutate(
        pop_A = pmax(0, pop_A),
        pop_B = pmax(0, pop_B)
      )
  } else { # drop
    joined <- joined |>
      dplyr::filter(!is.na(pop_A), !is.na(pop_B), pop_A > 0, pop_B > 0)
  }
  
  delta_dias <- as.numeric(difftime(data_B, data_A, units = "days"))
  if (!is.finite(delta_dias) || delta_dias == 0) stop("Datas inválidas ou iguais.")
  
  joined |>
    dplyr::mutate(
      r_diaria = (pop_B / pop_A)^(1 / delta_dias) - 1,
      delta_dias = delta_dias,
      data_A = as.Date(data_A),
      data_B = as.Date(data_B)
    )
}

#---------------------------------------------------------
# 2) Leva uma fotografia p/ uma data alvo
#    P_alvo = P_ref * (1 + r_diaria) ^ t
#    (t em DIAS; pode ser negativo)
#---------------------------------------------------------
ajustar_para_data_alvo <- function(
    df_ano_ref, data_ref, data_alvo,
    taxas_diarias,
    pop_var = "pop",
    chaves = c("cod7","municipio","sex","age_group","region")
){
  # harmoniza tipos p/ chaves
  conv <- intersect(c("sex","age_group","region"), names(df_ano_ref))
  df_ano_ref <- df_ano_ref |> dplyr::mutate(dplyr::across(dplyr::all_of(conv), as.character))
  conv2 <- intersect(c("sex","age_group","region"), names(taxas_diarias))
  taxas_diarias <- taxas_diarias |> dplyr::mutate(dplyr::across(dplyr::all_of(conv2), as.character))
  
  keys <- intersect(chaves, intersect(names(df_ano_ref), names(taxas_diarias)))
  if (!length(keys)) stop("Nenhuma chave em comum p/ o join.")
  
  dias_ajuste <- as.numeric(difftime(data_alvo, data_ref, units = "days"))
  
  df_ano_ref |>
    dplyr::left_join(
      taxas_diarias |> dplyr::select(dplyr::all_of(keys), r_diaria),
      by = keys
    ) |>
    tidyr::replace_na(list(r_diaria = 0)) |>
    dplyr::mutate(
      dias_ajuste = dias_ajuste,
      pop_ajust = .data[[pop_var]] * (1 + r_diaria)^dias_ajuste
    )
}

#---------------------------------------------------------
# 3) Atalho: ajustar censo de um ANO para 1º de julho
#    Você escolhe de qual par de anos usa a r_diaria.
#    Ex.: para 1991 (ref=01/09), use r de 1991->2000 e t ≈ -62 dias.
#---------------------------------------------------------
ajustar_censo_para_1_julho <- function(
    df_ano_ref, data_ref, taxas_diarias,
    pop_var = "pop",
    chaves = c("cod7","municipio","sex","age_group","region")
){
  ano <- as.integer(format(as.Date(data_ref), "%Y"))
  data_alvo <- as.Date(sprintf("%d-07-01", ano))
  ajustar_para_data_alvo(
    df_ano_ref = df_ano_ref,
    data_ref   = as.Date(data_ref),
    data_alvo  = data_alvo,
    taxas_diarias = taxas_diarias,
    pop_var = pop_var,
    chaves  = chaves
  )
}


# Aplicação para 2000, 2010 e 2022 ----------------------------------------
# --- harmonizador simples p/ evitar fator vs char nos joins
._padroniza_tipos <- function(df) {
  conv <- intersect(c("sex","age_group","region"), names(df))
  df |>
    dplyr::mutate(dplyr::across(dplyr::all_of(conv), as.character))
}

# dados (use os objetos *_filt que você já tem)
pb_1991h <- ._padroniza_tipos(pb_1991_filt)
pb_2000h <- ._padroniza_tipos(pb_2000_filt)
pb_2010h <- ._padroniza_tipos(pb_2010_filt)
pb_2022h <- ._padroniza_tipos(pb_2022_filt)

# chaves (usa region se existir; se não tiver na base, a função ignora)
chaves <- c("cod7","municipio","sex","age_group","region")

# datas oficiais de referência (IBGE)
d91 <- as.Date("1991-09-01")
d00 <- as.Date("2000-08-01")
d10 <- as.Date("2010-08-01")
d22 <- as.Date("2022-08-01")

# 1) taxas geométricas DIÁRIAS entre censos (por célula cod7×sexo×faixa×região)
r_91_00 <- calcular_taxa_crescimento_diaria(
  pb_1991h, pb_2000h, d91, d00,
  pop_var = "pop", chaves = chaves,
  tratamento_zero = "epsilon", epsilon = 0.5
)

r_00_10 <- calcular_taxa_crescimento_diaria(
  pb_2000h, pb_2010h, d00, d10,
  pop_var = "pop", chaves = chaves
)

r_10_22 <- calcular_taxa_crescimento_diaria(
  pb_2010h, pb_2022h, d10, d22,
  pop_var = "pop", chaves = chaves
)

# 2) levar cada censo para 01 de julho do MESMO ano
pb_1991_jul1 <- ajustar_censo_para_1_julho(pb_1991h, d91, r_91_00, pop_var="pop", chaves=chaves)
pb_2000_jul1 <- ajustar_censo_para_1_julho(pb_2000h, d00, r_00_10, pop_var="pop", chaves=chaves)
pb_2010_jul1 <- ajustar_censo_para_1_julho(pb_2010h, d10, r_10_22, pop_var="pop", chaves=chaves)
pb_2022_jul1 <- ajustar_censo_para_1_julho(pb_2022h, d22, r_10_22, pop_var="pop", chaves=chaves)

# sanidade rápida (deve dar ~1.00 ± pouquinho)
dplyr::summarise(pb_2000_jul1, media_fator = mean(pop_ajust / pop, na.rm = TRUE))


# 3) (opcional) cola tudo em um tibble só
pb_midyear <- dplyr::bind_rows(
  dplyr::mutate(pb_1991_jul1, ano = 1991),
  dplyr::mutate(pb_2000_jul1, ano = 2000),
  dplyr::mutate(pb_2010_jul1, ano = 2010),
  dplyr::mutate(pb_2022_jul1, ano = 2022)
)

# 4) (opcional) checagens rápidas
#   - mesma contagem de linhas?
stopifnot(nrow(pb_2000h) == nrow(pb_2000_jul1))
#   - médias de ajuste perto de 1 (sinal de que só andamos ~1 mês)
pb_2000_jul1 |>
  dplyr::summarise(media_fator = mean(pop_ajust / pop, na.rm = TRUE))
