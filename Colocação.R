# ------------------------------------------------------------------
# SCRIPT DE ANÁLISE DE COLOCAÇÕES (BIGRAMAS) EM RESUMOS DO CIHEM
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# 0. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------

require(readxl)                # Leitura de arquivos Excel (.xlsx)
require(dplyr)                 # Manipulação de dados (filter, etc.)
require(quanteda)             # Processamento de texto e criação de corpus
require(quanteda.textstats)   # Extração de estatísticas textuais
library(writexl)              # Exportar resultados para Excel (.xlsx)

# ------------------------------------------------------------------
# 1. LEITURA E FILTRAGEM DOS DADOS
# ------------------------------------------------------------------

# (1.1) Lê a planilha contendo os resumos dos artigos
CIHEM_dados <- read_excel("C:/Users/alexa/OneDrive/Área de Trabalho/CIHEM/CIHEM_revisado_tr.xlsx")

# (1.2) Filtra os dados, removendo entradas com "N/A" nos resumos
CIHEM_dados <- CIHEM_dados %>% 
  filter(`Resumo (tr)` != "N/A")

# ------------------------------------------------------------------
# 2. CRIAÇÃO DO CORPUS E TOKENIZAÇÃO
# ------------------------------------------------------------------

# (2.1) Cria um corpus textual a partir da coluna "Resumo (tr)"
CIHEM_corpus <- corpus(CIHEM_dados, text_field = "Resumo (tr)")

# (2.2) Tokeniza o texto: quebra os resumos em palavras (tokens)
CIHEM_toks <- tokens(CIHEM_corpus)

# ------------------------------------------------------------------
# 3. IDENTIFICAÇÃO DE COLOCAÇÕES (BIGRAMAS)
# ------------------------------------------------------------------

# (3.1) Remove as stopwords (palavras muito comuns, como "de", "e", "a", etc.)
CIHEM_toks_stat <- tokens_remove(CIHEM_toks, pattern = stopwords("pt"), padding = TRUE)

# (3.2) Extrai bigramas (colocações de 2 palavras) com frequência mínima de 40 ocorrências
tstat_col_caps <- textstat_collocations(CIHEM_toks_stat, 
                                        min_count = 40,     # Frequência mínima
                                        size = 2,           # Bigramas (2 palavras)
                                        tolower = TRUE,     # Ignora diferença entre maiúsculas/minúsculas
                                        smoothing = 0.5)    # Suavização para cálculo de força da colocação

# (3.3) Seleciona os top 20 bigramas (ajustável)
top_bigrams <- head(tstat_col_caps, 20)

# ------------------------------------------------------------------
# 4. LOOP PARA BUSCAR KWIC E SALVAR ARQUIVOS POR BIGRAMA
# ------------------------------------------------------------------

# (4.1) Garante que a pasta "recortes/" exista
if (!dir.exists("recortes")) dir.create("recortes")

# (4.2) Inicia loop para cada bigrama identificado
for (i in seq_len(nrow(top_bigrams))) {
  
  # (4.2.1) Extrai o bigrama atual e divide em duas palavras
  bigrama <- c(top_bigrams$collocation[i] %>% strsplit(" ") %>% unlist())
  
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
