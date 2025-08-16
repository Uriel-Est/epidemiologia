install.packages("read.dbc", repos = "https://packagemanager.posit.co/cran/2024-07-05")
# install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")
library(PNADcIBGE)
library(geobr)
library(sidrar)
library(microdatasus)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(arrow)
library(purrr)


# Dados iniciais ----------------------------------------------------------
# Baixar dados datasus
obitos <- fetch_datasus(year_start = 1996, year_end = 2023, uf = "PB", information_system = "SIM-DO")
obitos <- process_sim(obitos)


# ------------------------------------------------------------
# Municípios por RGI (Sousa–Cajazeiras e João Pessoa), com polígonos
# ------------------------------------------------------------
sf::sf_use_s2(FALSE)  # evita avisos de planaridade para operações simples

# 1) Malha municipal da PB (IBGE 2020)
mun_pb <- geobr::read_municipality(code_muni = "PB", year = 2020, simplified = TRUE)

# helper: detecção robusta do nome das RGIs (tolerando acentos/traços)
pat_sc <- regex("sousa\\s*[-–]?\\s*cajazeiras", ignore_case = TRUE)
pat_jp <- regex("jo(ã|a)o\\s*pessoa",           ignore_case = TRUE)

# 2) Caminho A: se os campos de RGI já vierem na malha municipal
has_rgi_cols <- all(c("code_intermediate", "name_intermediate") %in% names(mun_pb))

if (has_rgi_cols) {
  mun_sc <- mun_pb %>%
    filter(str_detect(name_intermediate, pat_sc))
  
  mun_jp <- mun_pb %>%
    filter(str_detect(name_intermediate, pat_jp))
  
} else {
  # 2') Caminho B (fallback): usar a malha de RGIs e filtrar por nome,
  # depois selecionar municípios por centroid WITHIN RGI (mantendo o polígono original)
  rgi_pb <- geobr::read_intermediate_region(code_intermediate = "PB", year = 2017)
  
  rgi_sc <- rgi_pb %>%
    dplyr::filter(str_detect(name_intermediate, pat_sc))
  
  rgi_jp <- rgi_pb %>%
    dplyr::filter(str_detect(name_intermediate, pat_jp))
  
  # seleção espacial por centroid -> mantém o polígono municipal original
  in_sc <- st_within(st_centroid(mun_pb), st_union(rgi_sc), sparse = FALSE)[, 1]
  in_jp <- st_within(st_centroid(mun_pb), st_union(rgi_jp), sparse = FALSE)[, 1]
  
  mun_sc <- mun_pb[in_sc, ]
  mun_jp <- mun_pb[in_jp, ]
}

# 3) Seleciona campos úteis (mantendo geometria). Alguns campos podem não existir em versões antigas;
# então usamos select(where()) + intersect para não quebrar.
keep_cols <- intersect(
  c("code_muni", "name_muni", "abbrev_state", "code_intermediate", "name_intermediate"),
  names(mun_pb)
)

mun_sc <- mun_sc %>% select(all_of(keep_cols), geometry = geom)
mun_jp <- mun_jp %>% select(all_of(keep_cols), geometry = geom)

# 4) (Opcional) imprimir a lista de nomes
cat("\nRGI Sousa–Cajazeiras (municípios):\n")
print(sort(mun_sc$name_muni))
cat("\nRGI João Pessoa (municípios):\n")
print(sort(mun_jp$name_muni))

# 5) (Opcional) mapinha de verificação rápida
p_check <- ggplot() +
  geom_sf(data = mun_pb, fill = "grey95", color = "white", size = 0.2) +
  geom_sf(data = mun_sc, fill = "#F9D976", color = "white", size = 0.25) +   # amarelo
  geom_sf(data = mun_jp, fill = "#7CB7FF", color = "white", size = 0.25) +   # azul
  labs(title = "PB — Municípios por RGI",
       subtitle = "Amarelo: Sousa–Cajazeiras | Azul: João Pessoa") +
  theme_void()

print(p_check)

# 6) (Opcional) exportar como GeoPackage
# sf::st_write(mun_sc, "rgi_sousa_cajazeiras.gpkg", delete_dsn = TRUE)
# sf::st_write(mun_jp, "rgi_joao_pessoa.gpkg",      delete_dsn = TRUE)


# População Residente -----------------------------------------------------
options(scipen = 999)

# ------------------ helpers ------------------

# 7 dígitos do SIDRA e lista de municípios-PB
mun_pb <- geobr::read_municipality("PB", year = 2020, simplified = TRUE) |>
  transmute(cod7 = as.integer(code_muni), municipio = name_muni) |>
  distinct()
pb_codes <- mun_pb$cod7

# Acha a coluna textual de idade
# Detecta a coluna textual de idade (ex.: "Grupo de idade", "Idade"),
# Ignorando "…(Código)" / "code" e variações com acento.
find_idade_col <- function(df){
  nm <- names(df)
  nm_ascii <- stringi::stri_trans_general(nm, "Latin-ASCII") |> tolower()
  
  # candidatos com "grupo … idade" OU começando por "idade"
  cand <- nm[ grepl("grupo.*idade|^idade\\b", nm_ascii) ]
  # remove qualquer coluna de código
  cand <- cand[ !grepl("codigo|code", nm_ascii[match(cand, nm)], ignore.case = TRUE) ]
  
  if (length(cand) == 0) stop("Não encontrei coluna de idade.")
  if (length(cand) > 1) {
    # escolhe a com mais categorias únicas (geralmente a textual)
    uc <- sapply(cand, \(x) length(unique(na.omit(df[[x]]))))
    cand <- cand[ which.max(uc) ]
  }
  cand
}

# converte rótulo do SIDRA em idade "base": 0,5,10,…,100
parse_idade_base <- function(x){
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[grepl("menos de 1", x, TRUE)] <- 0
  out[grepl("\\b100\\b.*mais", x, TRUE)] <- 100
  int <- is.na(out) & grepl("\\d+\\s*a\\s*\\d+", x)
  out[int] <- as.numeric(sub(" .*", "", x[int]))
  num <- is.na(out) & grepl("\\d+", x)
  out[num] <- as.numeric(stringr::str_extract(x[num], "\\d+"))
  out
}

# corta em faixas 5 anos padrão ONU
cut_5y <- function(age){
  brks <- c(seq(0, 100, by = 5), Inf)
  labs <- c(paste0(seq(0,95,5),"–",seq(4,99,5)), "100+")
  cut(age, breaks = brks, labels = labs, right = TRUE, include.lowest = TRUE)
}

# baixa 1 município em 1 tabela/ano (censo)
fetch_one_city_census <- function(table_id, year, cod7){
  df <- sidrar::get_sidra(
    x = table_id,
    variable   = 93,               # População residente (censos)
    period     = as.character(year),
    geo        = "City",
    geo.filter = list(City = cod7),
    format     = 3,
    header     = TRUE
  )
  
  # filtra forma de declaração quando existir (9514)
  if ("Forma de declaração da idade" %in% names(df)) {
    df <- df |> dplyr::filter(`Forma de declaração da idade` == "Total")
  }
  
  idade_col <- find_idade_col(df)
  
  df |>
    dplyr::filter(Sexo %in% c("Homens","Mulheres")) |>
    dplyr::filter(!grepl("^Total$", .data[[idade_col]], TRUE),
                  !grepl("ignorada|não declarad", .data[[idade_col]], TRUE)) |>
    transmute(
      cod7        = as.integer(substr(as.character(`Município (Código)`), 1, 7)),
      municipio   = `Município`,
      sex         = ifelse(Sexo=="Homens","H","M"),
      idade_base  = parse_idade_base(.data[[idade_col]]),
      pop         = as.numeric(Valor)
    ) |>
    mutate(age_group = cut_5y(idade_base)) |>
    group_by(cod7, municipio, sex, age_group) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
}

# baixa TODOS os municípios-PB em 1 tabela/ano (censo)
fetch_pb_census <- function(table_id, year, codes){
  purrr::map_dfr(codes, function(cod){
    Sys.sleep(0.12) # gentileza com API
    fetch_one_city_census(table_id, year, cod)
  })
}

# shares conjuntos sexo×idade (sobre o total municipal)
shares_joint <- function(df){
  totals <- df |> group_by(cod7, municipio) |> summarise(T = sum(pop), .groups = "drop")
  df |> left_join(totals, by = c("cod7","municipio")) |>
    mutate(share_joint = ifelse(T > 0, pop / T, 0)) |>
    select(cod7, municipio, sex, age_group, share_joint)
}

# totais censitários (para geométrico quando faltar 6579)
census_totals <- function(df){
  df |> group_by(cod7, municipio) |> summarise(T = sum(pop), .groups = "drop")
}

# tenta descobrir id da variável da 6579; se falhar, usa 9324
detect_var_6579 <- function(){
  v <- NA_integer_
  try({
    info <- sidrar::info_sidra(6579)
    # heurística: pega o primeiro código de variável numérico nas linhas de "Variável"
    cand <- info |> dplyr::filter(stringr::str_detect(tolower(classe), "vari")) |> dplyr::pull(código)
    cand <- suppressWarnings(as.integer(cand))
    cand <- cand[!is.na(cand)]
    if (length(cand) > 0) v <- cand[1]
  }, silent = TRUE)
  if (is.na(v)) v <- 9324L
  v
}

# baixa totais 6579 (mid-year) por City/ano (loop; robusto)
fetch_6579_totals <- function(years, codes){
  var_pop <- detect_var_6579()
  purrr::map_dfr(years, function(y){
    purrr::map_dfr(codes, function(cod){
      Sys.sleep(0.08)
      df <- tryCatch(
        sidrar::get_sidra(
          x = 6579,
          variable   = var_pop,
          period     = as.character(y),
          geo        = "City",
          geo.filter = list(City = cod),
          format     = 3,
          header     = TRUE
        ),
        error = function(e) NULL
      )
      if (is.null(df) || nrow(df)==0) return(tibble())
      tibble(
        cod7      = as.integer(substr(as.character(df$`Município (Código)`), 1, 7)),
        municipio = df$`Município`,
        year      = as.integer(y),
        T6579     = as.numeric(df$Valor)
      )
    })
  })
}

# interpola linearmente entre dois anos-âncora y0->y1 (shares conjuntos)
interp_interval <- function(sh0, sh1, totals_y0, totals_y1, years_seq, totals_6579){
  # junta shares (y0,y1)
  sh <- sh0 |>
    rename(share0 = share_joint) |>
    inner_join(sh1 |> rename(share1 = share_joint),
               by = c("cod7","municipio","sex","age_group"))
  
  # totais censitários y0,y1
  T0 <- totals_y0 |> rename(T0 = T)
  T1 <- totals_y1 |> rename(T1 = T)
  
  base <- sh |>
    left_join(T0, by = c("cod7","municipio")) |>
    left_join(T1, by = c("cod7","municipio"))
  
  purrr::map_dfr(years_seq, function(y){
    w <- (y - years_seq[1] + 0) / (tail(years_seq,1) - years_seq[1] + 1) # fração ~0..1
    # share conjunto linear
    tmp <- base |>
      mutate(share_t = (1 - w) * share0 + w * share1) |>
      select(cod7, municipio, sex, age_group, share_t)
    
    # total municipal para o ano y:
    Ty <- totals_6579 |> filter(year == y) |> select(cod7, T6579)
    # se não houver 6579, usa geométrico entre T0 e T1
    geomT <- base |>
      distinct(cod7, municipio, T0, T1) |>
      mutate(r = ifelse(T0>0 & T1>0, log(T1/T0)/ (length(years_seq)+1), 0),
             # distância em anos desde y0 (aprox: 1,2,...)
             k = which(years_seq == y),
             Tgeom = ifelse(T0>0, T0 * exp(r * k), T1))
    
    tot_y <- geomT |>
      select(cod7, Tgeom) |>
      left_join(Ty, by = "cod7") |>
      transmute(cod7, T_final = ifelse(!is.na(T6579), T6579, Tgeom))
    
    tmp |>
      left_join(tot_y, by = "cod7") |>
      mutate(year = y,
             pop = pmax(0, share_t) * pmax(0, T_final)) |>
      select(cod7, municipio, year, sex, age_group, pop)
  })
}

# ------------------ pipeline completo PB 1991–2023 ------------------

# 1) Censos
pb_1991 <- fetch_pb_census(205,  1991, pb_codes)
pb_2000 <- fetch_pb_census(1518, 2000, pb_codes)
pb_2010 <- fetch_pb_census(1378, 2010, pb_codes)
pb_2022 <- fetch_pb_census(9514, 2022, pb_codes)

# garante todas as faixas em todos os anos
all_groups <- levels(cut_5y(0:100))
pad5 <- function(df){
  df |>
    mutate(age_group = factor(as.character(age_group), levels = all_groups)) |>
    complete(cod7, municipio, sex, age_group, fill = list(pop = 0))
}
pb_1991 <- pad5(pb_1991); pb_2000 <- pad5(pb_2000)
pb_2010 <- pad5(pb_2010); pb_2022 <- pad5(pb_2022)

# shares conjuntos (sexo×idade sobre o total)
sh_1991 <- shares_joint(pb_1991)
sh_2000 <- shares_joint(pb_2000)
sh_2010 <- shares_joint(pb_2010)
sh_2022 <- shares_joint(pb_2022)

# totais censos (fallback geométrico)
T1991 <- census_totals(pb_1991)
T2000 <- census_totals(pb_2000)
T2010 <- census_totals(pb_2010)
T2022 <- census_totals(pb_2022)

# 2) Totais 6579 para 1991–2023 (onde existir)
years_all <- 1991:2023
tot6579 <- fetch_6579_totals(years_all, pb_codes)
# pode não haver 6579 para anos 1990s; tudo bem — o script cairá no geométrico

# 3) Interpolar intervalos
# 1991->2000: anos alvo 1992..1999
est_91_00 <- interp_interval(
  sh0 = sh_1991, sh1 = sh_2000,
  totals_y0 = T1991, totals_y1 = T2000,
  years_seq = 1992:1999,
  totals_6579 = tot6579
)

# 2000->2010: anos alvo 2001..2009
est_00_10 <- interp_interval(
  sh0 = sh_2000, sh1 = sh_2010,
  totals_y0 = T2000, totals_y1 = T2010,
  years_seq = 2001:2009,
  totals_6579 = tot6579
)

# 2010->2022: anos alvo 2011..2021
est_10_22 <- interp_interval(
  sh0 = sh_2010, sh1 = sh_2022,
  totals_y0 = T2010, totals_y1 = T2022,
  years_seq = 2011:2021,
  totals_6579 = tot6579
)

# 4) Monta anos âncora + 2022, e 2023 (extrapola shares de 2022; total 6579)
anchors <- bind_rows(
  pb_1991 |> mutate(year = 1991),
  pb_2000 |> mutate(year = 2000),
  pb_2010 |> mutate(year = 2010),
  pb_2022 |> mutate(year = 2022)
) |>
  transmute(cod7, municipio, year, sex, age_group, pop)

# 2023: usa shares 2022 e total 6579 (ou mantém 2022 se faltar 6579)
sh_2022_joint <- sh_2022
T2023 <- tot6579 |> filter(year == 2023) |> select(cod7, T6579)
if (nrow(T2023) > 0){
  est_2023 <- sh_2022_joint |>
    left_join(T2023, by = "cod7") |>
    group_by(cod7, municipio) |>
    mutate(Tfill = ifelse(is.na(T6579),
                          sum(pop, na.rm = TRUE), # fallback: total 2022 daquele muni
                          T6579)) |>
    ungroup() |>
    transmute(cod7, municipio, year = 2023, sex, age_group,
              pop = pmax(0, share_joint) * pmax(0, Tfill))
} else {
  est_2023 <- pb_2022 |> mutate(year = 2023) |>
    transmute(cod7, municipio, year, sex, age_group, pop)
}

# 5) Consolida tudo e salva
out_all <- bind_rows(
  anchors,
  est_91_00, est_00_10, est_10_22, est_2023
) |>
  arrange(cod7, year, sex, age_group)

dir.create("data", FALSE, TRUE)
arrow::write_parquet(out_all, "data/PB_muni_sex_age_1991_2023.parquet")

# pronto: 'data/PB_muni_sex_age_1991_2023.parquet'
# colunas: cod7, municipio, year, sex (H/M), age_group (0–4,...,100+), pop (mid-year)
