# ------------------------------------------------------------------
# SCRIPT DE ANÁLISE DE COLOCAÇÕES (BIGRAMAS) EM RESUMOS DO CIHEM
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# 0. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------

if (!require(readxl)) install.packages("readxl"); library(readxl)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(quanteda)) install.packages("quanteda"); library(quanteda)
if (!require(quanteda.textstats)) install.packages("quanteda.textstats"); library(quanteda.textstats)
if (!require(writexl)) install.packages("writexl"); library(writexl)

# ------------------------------------------------------------------
# 1. LEITURA E FILTRAGEM DOS DADOS
# ------------------------------------------------------------------

# (1.1) Lê a planilha contendo os resumos dos artigos
CIHEM_dados <- read_excel("CIHEM_revisado_tr.xlsx")

# (1.2) Filtra os dados, removendo entradas com "N/A" nos resumos
CIHEM_dados <- CIHEM_dados %>% 
  filter(`Resumo (tr)` != "N/A")

# ------------------------------------------------------------------
# 2. CRIAÇÃO DO CORPUS E TOKENIZAÇÃO
# ------------------------------------------------------------------

# (2.1) Cria um corpus textual a partir da coluna "Resumo (tr)"
CIHEM_corpus <- corpus(CIHEM_dados, text_field = "Resumo (tr)", docid_field = "ID")

# (2.2) Tokeniza o texto: quebra os resumos em palavras (tokens)
CIHEM_toks <- tokens(CIHEM_corpus)

# ------------------------------------------------------------------
# 3. IDENTIFICAÇÃO DE COLOCAÇÕES (BIGRAMAS)
# ------------------------------------------------------------------

# (3.1) Remove as stopwords (palavras muito comuns, como "de", "e", "a", etc.)
CIHEM_toks_stat <- tokens_remove(CIHEM_toks, pattern = stopwords("pt"), padding = FALSE)

# (3.2) Extrai bigramas (colocações de 2 palavras) com frequência mínima de 40 ocorrências
tstat_col_caps <- textstat_collocations(CIHEM_toks_stat, 
                                        min_count = 40,     # Frequência mínima
                                        size = 2,           # Bigramas (2 palavras)
                                        tolower = TRUE,     # Ignora diferença entre maiúsculas/minúsculas
                                        smoothing = 0.5)    # Suavização para cálculo de força da colocação

# (3.3) Seleciona os top 20 bigramas (ajustável)
head(tstat_col_caps, 20)

# (3.4) salva o df de bigramas
write_xlsx(tstat_col_caps, "tstat_col_caps.xlsx")

# ------------------------------------------------------------------
# 4. LOOP PARA BUSCAR KWIC E SALVAR ARQUIVOS POR BIGRAMA
# ------------------------------------------------------------------

# (4.1) Garante que a pasta "recortes/" exista
if (!dir.exists("recortes")) dir.create("recortes")

# (4.2) Inicia loop para cada bigrama identificado
for (i in seq_len(nrow(tstat_col_caps))) {
  
  # (4.2.1) Extrai o bigrama atual e divide em duas palavras
  bigrama <- c(tstat_col_caps$collocation[i] %>% strsplit(" ") %>% unlist())
  
  # (4.2.2) Compõe os tokens com o bigrama unido por underscore (ex: "história_oral")
  toks_comp <- tokens_compound(CIHEM_toks, pattern = list(bigrama))
  
  # (4.2.3) Cria o padrão a ser buscado (com underline)
  bigrama_pattern <- paste(bigrama, collapse = "_")
  
  # (4.2.4) Busca o bigrama no texto com janela de 10 palavras antes/depois
  kwic_result <- kwic(toks_comp, 
                      pattern = bigrama_pattern,
                      window = 10,
                      case_insensitive = TRUE)
  
  # (4.2.5) Se encontrou ocorrências, salva os resultados
  if (nrow(kwic_result) > 0) {
    
    # (4.2.6) Converte os resultados para data.frame
    kwic_df <- as.data.frame(kwic_result)
    
    # (4.2.7) Define nome do arquivo com o nome do bigrama
    arquivo_nome <- paste0("recortes/", bigrama_pattern, ".xlsx")
    
    # (4.2.8) Exporta para Excel (.xlsx)
    writexl::write_xlsx(kwic_df, arquivo_nome)
    
    # (4.2.9) Mensagem de sucesso no console
    cat("✔️ Arquivo salvo:", arquivo_nome, "\n")
    
  } else {
    # (4.2.10) Mensagem se não encontrou o bigrama no texto
    cat("⚠️ Nenhum resultado para:", bigrama_pattern, "\n")
  }
}

#####

#indicar termos ausentes na lista entre aspas
#isto ocorre devido padding = FALSE, ao remover tokens, que é utilizado para ampliar a busca
termos <- c("formação de professores", "história da educação",
            "ensino de matemática", "ensino da matemática",
            "professores de matemática", "história da matemática")


# Loop para cada termo
for (termo in termos) {
  bigrama <- strsplit(termo, " ")[[1]]
  padrão <- gsub(" ", "_", termo)
  
  kwic_result <- kwic(
    tokens_compound(CIHEM_toks, pattern = list(bigrama)),
    pattern = padrão,
    window = 10,
    case_insensitive = TRUE
  )
  
  if (nrow(kwic_result) > 0) {
    write_xlsx(as.data.frame(kwic_result),
               path = paste0("recortes/", padrão, ".xlsx"))
    cat("✔️ Arquivo salvo para:", termo, "\n")
  } else {
    cat("⚠️ Nada encontrado para:", termo, "\n")
  }
}
