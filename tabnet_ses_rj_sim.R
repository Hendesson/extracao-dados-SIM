# =============================================================================
# RASPAGEM do TabNet da SES-RJ (Secretaria de ESTADO de Saúde) — SIM
#   ESTADO DO RIO DE JANEIRO: todos os municípios e BAIRROS de residência
#   Óbitos por doenças do aparelho circulatório (Cap. IX, I00–I99)
#   e do aparelho respiratório (Cap. X, J00–J99), 2006 a 2026
#
# Site (formulário "Mortalidade Geral - RJ"):
#   https://sistemas.saude.rj.gov.br/tabnetbd/dhx.exe?sim/tf_sim_do_geral.def
#
# POR QUE ESTE SITE?
#   - Microdados do DATASUS (microdatasus): têm todos os municípios, mas NÃO
#     têm bairro (o campo não é divulgado nos arquivos públicos).
#   - TabNet da SMS-Rio (script tabnet_rio_sim.R): tem bairro, mas só da
#     CAPITAL.
#   - TabNet da SES-RJ (este): tem "Bairro de residência" para TODOS os
#     municípios do estado, e anos até o corrente (preliminares).
#
# O QUE ESTA BASE CONTÉM (segundo as notas do próprio site):
#   - óbitos de residentes no RJ + óbitos de residentes de outras UFs que
#     morreram no RJ. Usamos o indicador "Óbitos não fetais de residentes RJ",
#     que JÁ exclui os fetais e os não residentes.
#   - até 2010: base nacional (Ministério da Saúde);
#     a partir de 2011: base ESTADUAL (pode diferir um pouco do DATASUS,
#     porque a SES-RJ corrige/inclui registros depois do fechamento nacional).
#
# CUIDADO COM O BAIRRO:
#   - 2006 a 2010: o bairro vem 100% "~Não especificado/ignorado" (a base
#     nacional usada nesses anos não tem bairro). Município e demais
#     variáveis estão ok. Para bairros da CAPITAL nesses anos, use o
#     TabNet da SMS-Rio (tabnet_rio_sim.R).
#   - 2011 em diante: ~8% a 19% dos óbitos ficam com bairro ignorado.
#   O bairro vem do texto digitado na Declaração de Óbito, NÃO de uma tabela
#   oficial de bairros. Então o mesmo lugar pode aparecer escrito de formas
#   diferentes ("ABRAAO", "VILA DO ABRAAO"; "B PARAISO", "BELO PARAISO"...).
#   Antes de analisar por bairro, será preciso PADRONIZAR os nomes.
#
# DIFERENÇA PARA O TABNET DA SMS-RIO:
#   Este TabNet é uma versão mais nova, que monta uma consulta SQL por trás.
#   Por isso os VALORES das opções do formulário não são códigos curtos
#   ("9"), e sim textos longos com pedaços de SQL, por exemplo:
#     "Capítulo  9 - Doenças do aparelho circulatório|I00,I01,...,I99,|3"
#   Em vez de copiar esses textos para o script, baixamos a página do
#   formulário e PROCURAMOS a opção pelo começo do rótulo (função
#   valor_opcao()). Assim o script continua funcionando se o site mudar
#   detalhes internos.
#   Outra diferença: a resposta vem como página HTML com um link
#   "Salva como CSV". O script lê esse link e baixa o CSV.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACOTES
# -----------------------------------------------------------------------------
# curl: requisições HTTP (new_handle, handle_setopt, curl_fetch_memory...).
# dplyr: manipulação de tabelas (bind_rows, mutate, left_join, count...).
library(curl)
library(dplyr)


# -----------------------------------------------------------------------------
# 1. PARÂMETROS
# -----------------------------------------------------------------------------
URL_BASE <- "https://sistemas.saude.rj.gov.br/tabnetbd/"
# URL_DEF: a página do formulário (tem todas as opções de cada campo).
URL_DEF  <- paste0(URL_BASE, "dhx.exe?sim/tf_sim_do_geral.def")
# URL_TAB: para onde o botão "Mostra" envia o formulário. Descoberto no
# JavaScript da página: document.Form1.action = "webtabx.exe?sim/..."
URL_TAB  <- paste0(URL_BASE, "webtabx.exe?sim/tf_sim_do_geral.def")

ANOS <- 2006:2026   # o site vai de 1996 até o ano corrente

# Capítulos: o que está à ESQUERDA do "=" é o rótulo que vai para o
# resultado; à DIREITA, o COMEÇO do rótulo da opção no site (repare nos
# DOIS espaços em "Capítulo  9": é assim que está escrito lá).
CAPITULOS <- c(
  "IX - Circulatório (I00-I99)" = "Capítulo  9 -",
  "X - Respiratório (J00-J99)"  = "Capítulo 10 -"
)

# Sexos: rótulo no resultado = começo do rótulo no site.
SEXOS <- c(
  "Feminino"  = "Feminino",
  "Masculino" = "Masculino",
  "Ignorado"  = "~Não informado"
)

# Linha, coluna e o que é contado (começo do rótulo de cada opção).
# Veja todas as opções com listar_opcoes("Linha"), listar_opcoes("Coluna").
LINHA      <- "Bairro de residência"
COLUNA     <- "Faixa etária"
INCREMENTO <- "Óbitos não fetais de residentes RJ"

PASTA_CACHE <- "tabnet_ses_cache"
ARQ_FINAL   <- "sim_rj_bairros_circ_resp_2006_2026.csv"
PAUSA_SEG   <- 2    # cada consulta pesa no servidor (~10 s); seja gentil


# -----------------------------------------------------------------------------
# 2. FUNÇÕES AUXILIARES
# -----------------------------------------------------------------------------

# --- 2.1 codificar_latin1 ----------------------------------------------------
# O site usa latin1. Os nomes e valores dos campos têm acentos ("Óbitos",
# "Capítulo") e precisam ir na requisição como %XX com o byte LATIN1
# ("ó" = byte F3 = "%F3"). URLencode()/curl_escape() convertem para UTF-8
# antes ("%C3%B3") e o site não entende; então escapamos byte a byte:
#   iconv()      -> converte o texto de UTF-8 para latin1
#   charToRaw()  -> texto -> bytes; as.integer() -> bytes -> números 0–255
#   %in%         -> TRUE se o número está no conjunto de caracteres "seguros"
#   sprintf("%%%02X", n) -> "%" + número em hexadecimal com 2 dígitos
#   vapply(x, f, character(1)) -> aplica f a cada elemento, 1 texto por vez
codificar_latin1 <- function(x) {
  vapply(x, function(texto) {
    bytes  <- as.integer(charToRaw(iconv(texto, from = "UTF-8", to = "latin1")))
    seguro <- bytes %in% c(48:57, 65:90, 97:122, 45, 46, 95, 126) # 0-9 A-Z a-z - . _ ~
    partes <- ifelse(seguro, intToUtf8(bytes, multiple = TRUE), sprintf("%%%02X", bytes))
    paste(partes, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# --- 2.2 baixar ---------------------------------------------------------------
# Faz a requisição e devolve o conteúdo como texto UTF-8.
#   corpo = NULL  -> GET (abrir uma página/arquivo)
#   corpo = texto -> POST (enviar o formulário)
baixar <- function(url, corpo = NULL) {
  h <- new_handle(timeout = 600)   # consultas grandes podem demorar
  if (!is.null(corpo)) {
    handle_setopt(h, post = TRUE, postfields = corpo)
    handle_setheaders(h, "Content-Type" = "application/x-www-form-urlencoded")
  }
  resp <- curl_fetch_memory(url, handle = h)
  if (resp$status_code != 200) stop("Falha HTTP ", resp$status_code, " em ", url)
  # O site responde em latin1; convertemos para UTF-8 (padrão do R).
  iconv(rawToChar(resp$content), from = "latin1", to = "UTF-8")
}

# --- 2.3 ler_formulario ------------------------------------------------------
# Baixa a página do formulário (~6 MB) UMA vez e extrai, para cada <SELECT>,
# a lista de opções (valor enviado + rótulo exibido).
# Devolve uma LISTA nomeada: formulario[["SSexo"]] é um data.frame com as
# colunas `valor` e `rotulo`.
# Expressões regulares usadas:
#   <SELECT[^>]*NAME="([^"]*)"   -> captura o nome do campo
#   (?s)                         -> "." também casa quebras de linha
#   .*?                          -> qualquer coisa, o mínimo possível
#   <OPTION VALUE="([^"]*)"[^>]*>([^\n<]*) -> valor e rótulo de cada opção
ler_formulario <- function() {
  html <- baixar(URL_DEF)
  # Cada bloco: do <SELECT ...> até o </SELECT> correspondente.
  blocos <- regmatches(html, gregexpr("(?s)<SELECT[^>]*>.*?</SELECT>", html,
                                      perl = TRUE, ignore.case = TRUE))[[1]]
  # sub(padrão, "\\1", x): devolve só o 1º grupo capturado.
  nomes <- sub('(?s)^<SELECT[^>]*NAME="([^"]*)".*', "\\1", blocos,
               perl = TRUE, ignore.case = TRUE)
  # lapply(): para cada bloco, extrai as opções em um data.frame.
  opcoes <- lapply(blocos, function(b) {
    m <- regmatches(b, gregexpr('<OPTION VALUE="([^"]*)"[^>]*>([^\n<]*)', b,
                                ignore.case = TRUE))[[1]]
    data.frame(
      valor  = sub('.*VALUE="([^"]*)".*', "\\1", m, ignore.case = TRUE),
      rotulo = trimws(sub(".*>", "", m))
    )
  })
  names(opcoes) <- nomes   # dá a cada elemento da lista o nome do campo
  opcoes
}

# Lê o formulário agora (fica guardado no objeto `formulario`).
formulario <- ler_formulario()

# --- 2.4 listar_opcoes / valor_opcao -----------------------------------------
# listar_opcoes("SSexo") -> mostra as opções de um campo (para você explorar).
# names(formulario)      -> mostra TODOS os campos disponíveis.
listar_opcoes <- function(campo) {
  if (!campo %in% names(formulario)) stop("Campo inexistente: ", campo)
  formulario[[campo]]
}

# valor_opcao(): acha o VALOR (o texto longo que o site espera) da opção
# cujo RÓTULO é IGUAL a `comeca_com` ou, se não houver igual, COMEÇA com ele.
# (Primeiro tenta o igual porque "Faixa etária" também é o começo de
#  "Faixa etária menor de 1 ano", "Faixa etária DCNT"...)
#   startsWith(x, prefixo) -> TRUE se o texto x começa com o prefixo
#   which()                -> posições que são TRUE
valor_opcao <- function(campo, comeca_com) {
  op  <- listar_opcoes(campo)
  pos <- which(op$rotulo == comeca_com)                  # 1º: rótulo idêntico
  if (length(pos) == 0) pos <- which(startsWith(op$rotulo, comeca_com))  # 2º: prefixo
  if (length(pos) != 1) {
    stop("Esperava 1 opção em '", campo, "' começando com '", comeca_com,
         "', achei ", length(pos))
  }
  op$valor[pos]
}

# --- 2.5 montar_corpo --------------------------------------------------------
# Monta o texto do formulário ("campo=valor&campo=valor...").
# `campos` é um vetor NOMEADO: nome = campo do formulário, valor = valor.
# Um mesmo campo pode aparecer várias vezes (ex.: vários anos).
montar_corpo <- function(campos) {
  paste(paste0(codificar_latin1(names(campos)), "=", codificar_latin1(campos)),
        collapse = "&")
}

# --- 2.6 consultar_tabnet ----------------------------------------------------
# Envia uma consulta e devolve a tabela em formato LONGO:
#   uma linha por (linha × coluna), com a contagem na coluna `obitos`.
#   anos    -> vetor de anos (ex.: 2023)
#   filtros -> vetor nomeado: campo de seleção ("S...") = valor da opção
consultar_tabnet <- function(anos, filtros = c()) {
  campos <- c(
    "Linha"      = valor_opcao("Linha",      LINHA),
    "Coluna"     = valor_opcao("Coluna",     COLUNA),
    "Incremento" = valor_opcao("Incremento", INCREMENTO),
    # O campo de anos se chama "PAno do óbito" e o valor tem o formato
    # "2023|2023|4". setNames() dá o MESMO nome a cada elemento do vetor,
    # para o campo se repetir quando houver vários anos.
    setNames(sprintf("%d|%d|4", anos, anos), rep("PAno do óbito", length(anos))),
    filtros,
    "nomedef"    = "sim/tf_sim_do_geral.def",   # campo oculto do formulário
    "grafico"    = ""                           # sem gráfico
  )

  html <- baixar(URL_TAB, montar_corpo(campos))

  # Procura o link do CSV na resposta: <A HREF=csv/tf_sim_do_geral123.csv>
  link <- regmatches(html, regexpr("csv/[^ >]*\\.csv", html))
  if (length(link) == 0) {
    # Sem link = sem dados (ou erro). Consulta sem óbitos não é erro:
    # devolve tabela vazia. O texto exato de "sem registros" pode variar,
    # então só tratamos como vazio se não houver a tabela de dados.
    if (!grepl("addRows", html, fixed = TRUE)) {
      return(data.frame(linha = character(), coluna = character(), obitos = integer()))
    }
    stop("Não achei o link do CSV na resposta.")
  }

  # Baixa o CSV. Separador ";" e números entre aspas.
  csv <- baixar(paste0(URL_BASE, link))
  tab <- read.table(text = csv, sep = ";", quote = "\"", header = TRUE,
                    check.names = FALSE, colClasses = "character",
                    comment.char = "")   # "#" não é comentário aqui

  # Remove linha e coluna de "Total" (são somas).
  tab <- tab[tab[[1]] != "Total", names(tab) != "Total", drop = FALSE]

  # Largo -> longo (mesma lógica explicada em tabnet_rio_sim.R):
  #   unlist() empilha coluna por coluna, então o rótulo da linha repete
  #   `times` e o nome da coluna repete `each`.
  valores <- tab[, -1, drop = FALSE]
  data.frame(
    linha  = rep(tab[[1]],       times = ncol(valores)),
    coluna = rep(names(valores), each  = nrow(valores)),
    obitos = as.integer(unlist(valores, use.names = FALSE))
  )
}


# -----------------------------------------------------------------------------
# 3. TABELA DE MUNICÍPIOS (nome -> código IBGE)
# -----------------------------------------------------------------------------
# A linha "Bairro de residência" vem como
#   "RJ, Angra dos Reis                    - ABRAAO"
# ou seja, SEM o código do município. Para poder cruzar depois com população
# (que usa código), montamos um "dicionário" nome -> código a partir das
# opções do filtro "SMunicípio de residência", cujos rótulos são como
#   "RJ,  Angra dos Reis - 330010"
# gsub("\\s+", " ", x): troca qualquer sequência de espaços por UM espaço
#   (\\s = espaço/tab; + = um ou mais). Assim os nomes das duas fontes
#   ficam comparáveis.
municipios <- listar_opcoes("SMunicípio de residência") |>
  filter(startsWith(rotulo, "RJ,")) |>                 # só municípios do RJ
  mutate(
    codmun    = sub(".* - (\\d{6})$", "\\1", rotulo),  # \\d{6} = 6 dígitos
    municipio = gsub("\\s+", " ", sub(" - \\d{6}$", "", rotulo))
  ) |>
  select(municipio, codmun)                            # select(): escolhe colunas


# -----------------------------------------------------------------------------
# 4. TESTE RÁPIDO (rode sozinho antes do loop)
# -----------------------------------------------------------------------------
# Cap. IX, 2023, só residentes do RJ. Leva ~10 s.
teste <- consultar_tabnet(
  anos    = 2023,
  filtros = c(
    "SCausa básica - capítulo" = valor_opcao("SCausa básica - capítulo", "Capítulo  9 -"),
    "SUF de residência"        = valor_opcao("SUF de residência", "RJ,")
  )
)
head(teste)
sum(teste$obitos)   # total de óbitos do Cap. IX em 2023 (residentes RJ)


# -----------------------------------------------------------------------------
# 5. LOOP DE DOWNLOAD: ano × capítulo × sexo
# -----------------------------------------------------------------------------
# 21 anos × 2 capítulos × 3 sexos = 126 consultas de ~10 s + pausa
# => cerca de 25–30 minutos na primeira vez. Graças ao cache, se cair, é só
# rodar de novo que ele continua de onde parou.
combinacoes <- expand.grid(
  ano      = ANOS,
  capitulo = names(CAPITULOS),
  sexo     = names(SEXOS),
  stringsAsFactors = FALSE
)

dir.create(PASTA_CACHE, showWarnings = FALSE)

# Filtro fixo: só residentes do estado do RJ (o indicador já faz isso, mas
# filtrar também deixa a consulta menor e mais rápida).
filtro_uf <- c("SUF de residência" = valor_opcao("SUF de residência", "RJ,"))

for (i in seq_len(nrow(combinacoes))) {
  ano      <- combinacoes$ano[i]
  capitulo <- combinacoes$capitulo[i]
  sexo     <- combinacoes$sexo[i]

  # Nome do arquivo de cache. make.names() troca espaços/acentos/símbolos
  # por caracteres seguros para nome de arquivo.
  arq <- file.path(PASTA_CACHE,
                   paste0(ano, "_", make.names(capitulo), "_", sexo, ".csv"))
  if (file.exists(arq)) next   # já baixado: pula

  message(sprintf("[%d/%d] %d | %s | %s", i, nrow(combinacoes), ano, capitulo, sexo))

  filtros <- c(
    filtro_uf,
    "SCausa básica - capítulo" = valor_opcao("SCausa básica - capítulo", CAPITULOS[[capitulo]]),
    "SSexo"                    = valor_opcao("SSexo", SEXOS[[sexo]])
  )

  # tryCatch(): se a consulta der erro, avisa e segue para a próxima.
  resultado <- tryCatch(
    consultar_tabnet(ano, filtros),
    error = function(e) {
      warning("Falhou: ", arq, " -> ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(resultado)) {
    # Anota as dimensões usadas como filtro (rep(..., nrow) funciona mesmo
    # quando o resultado tem 0 linhas).
    resultado$ano      <- rep(ano,      nrow(resultado))
    resultado$capitulo <- rep(capitulo, nrow(resultado))
    resultado$sexo     <- rep(sexo,     nrow(resultado))
    write.csv(resultado, arq, row.names = FALSE, fileEncoding = "UTF-8")
  }

  Sys.sleep(PAUSA_SEG)
}


# -----------------------------------------------------------------------------
# 6. JUNTAR, SEPARAR MUNICÍPIO/BAIRRO E SALVAR
# -----------------------------------------------------------------------------
arquivos <- list.files(PASTA_CACHE, pattern = "\\.csv$", full.names = TRUE)

sim_rj <- lapply(arquivos, read.csv, colClasses = "character", encoding = "UTF-8") |>
  bind_rows() |>
  mutate(
    obitos = as.integer(obitos),
    ano    = as.integer(ano),
    # A linha "RJ, Angra dos Reis      - ABRAAO" vira duas colunas.
    # Regex: ^(.*?)\\s+- (.*)$
    #   ^ e $   -> início e fim do texto
    #   (.*?)   -> grupo 1: o mínimo possível até achar "espaços + '- '"
    #   (.*)    -> grupo 2: todo o resto (o bairro)
    municipio = gsub("\\s+", " ", sub("^(.*?)\\s+- (.*)$", "\\1", linha)),
    bairro    = trimws(sub("^(.*?)\\s+- (.*)$", "\\2", linha)),
    faixa_etaria = coluna
  ) |>
  # left_join(): acrescenta o código IBGE (codmun) pelo nome do município.
  left_join(municipios, by = "municipio") |>
  select(ano, capitulo, sexo, codmun, municipio, bairro, faixa_etaria, obitos) |>
  filter(obitos > 0)   # descarta combinações com zero (arquivo bem menor)

# Conferências:
# 1) Algum município ficou sem código? (deveria dar 0 linhas)
sim_rj |> filter(is.na(codmun)) |> distinct(municipio)   # distinct(): valores únicos
# 2) Totais por ano e capítulo: compare alguns com o próprio site.
sim_rj |>
  count(ano, capitulo, wt = obitos, name = "obitos") |>
  as_tibble() |>
  print(n = Inf)

write.csv(sim_rj, ARQ_FINAL, row.names = FALSE, fileEncoding = "UTF-8")
message("Arquivo salvo: ", ARQ_FINAL, " (", nrow(sim_rj), " linhas)")


# -----------------------------------------------------------------------------
# 7. COMO ADAPTAR
# -----------------------------------------------------------------------------
# - Ver todos os campos do formulário:          names(formulario)
# - Ver as opções de um campo:                  listar_opcoes("SCor/raça")
# - Trocar o que vai na coluna (ex.: raça/cor): COLUNA <- "Cor/raça"
# - Causa mais detalhada: LINHA <- "Causa básica - categoria" (CID de 3
#   caracteres) e filtre município com "SMunicípio de residência".
# - Mudou LINHA/COLUNA/filtros? APAGUE a pasta do cache antes de rodar.
# - Anos recentes são preliminares: apague do cache os arquivos "2025_*" e
#   "2026_*" de tempos em tempos para rebaixá-los.
