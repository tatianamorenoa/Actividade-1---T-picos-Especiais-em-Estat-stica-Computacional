
#install.packages(c("chromote", "rvest", "dplyr", "stringr", "purrr", "janitor", "corrplot", "DataExplorer", "ggplot2"))

library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)
library(corrplot)
library(DataExplorer)
library(ggplot2)

# Configurar sessão com a função Chrome
browser <- ChromoteSession$new()

# Navegar para a página da OECD
url <- "https://data-explorer.oecd.org/vis?fs%5B0%5D=Topic%2C0%7CFinance%20and%20investment%23FIN%23&fs%5B1%5D=Topic%2C1%7CSociety%23SOC%23%7CSocial%20policy%23SOC_PRO%23&pg=0&fc=Topic&snb=6&df%5Bds%5D=dsDisseminateFinalDMZ&df%5Bid%5D=DSD_PAG%40DF_DEC&df%5Bag%5D=OECD.ELS.SPD&df%5Bvs%5D=1.0&dq=.A.....&pd=2022%2C2022&to%5BTIME_PERIOD%5D=true&vw=tb"
browser$Page$navigate(url)

# Esperar que a página carregue (JavaScript e dados)
Sys.sleep(20)  # Mais tempo para garantir o carregamento

# Obter o HTML completo após o carregamento do JavaScript
html_content <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value

# Ler o HTML com rvest
webpage <- read_html(html_content)

# Buscar todas as tabelas na página
tables <- html_nodes(webpage, "table")

cat("Número de tabelas encontradas:", length(tables), "\n")

# Função melhorada para extrair tabelas com nomes duplicados
extract_table_safe <- function(table, index) {
  cat("Processando tabela", index, "...\n")
  
  tryCatch({
    # Extrair a tabela como data frame
    df <- html_table(table, fill = TRUE, header = FALSE)  # No usar header automático
    
    if (length(df) > 0 && nrow(df[[1]]) > 0) {
      table_df <- as.data.frame(df[[1]], stringsAsFactors = FALSE)
      
      # Se a primeira linha parecer ser o cabeçalho, usá-la como nomes
      if (nrow(table_df) > 1) {
        # Verificar se a primeira linha contém dados que parecem cabeçalhos
        first_row <- table_df[1, ]
        looks_like_header <- all(sapply(first_row, function(x) 
          !grepl("^[0-9.,%-]+$", x) | is.na(x)))
        
        if (looks_like_header && length(first_row) > 1) {
          col_names <- as.character(first_row)
          # Tornar os nomes únicos
          col_names <- make.unique(col_names, sep = "_")
          table_df <- table_df[-1, ]
          colnames(table_df) <- col_names
        } else {
          # Usar nomes genéricos
          colnames(table_df) <- paste0("V", 1:ncol(table_df))
        }
      } else {
        # Usar nomes genéricos para tabelas pequenas
        colnames(table_df) <- paste0("Col", 1:ncol(table_df))
      }
      
      # Limpar dados
      table_df <- table_df %>%
        mutate(across(where(is.character), ~str_trim(.x))) %>%
        mutate(across(where(is.character), ~ifelse(.x == "", NA, .x)))
      
      return(table_df)
    } else {
      cat("Tabela", index, "está vazia\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Erro na tabela", index, ":", e$message, "\n")
    return(NULL)
  })
}

# Extrair todas as tabelas
all_tables <- map(seq_along(tables), ~extract_table_safe(tables[.x], .x))

# Filtrar tabelas não nulas
valid_tables <- compact(all_tables)

cat("Tabelas extraídas com sucesso:", length(valid_tables), "\n\n")

# Fechar o navegador
browser$close()


head(tables)

#Carregar apenas a tabela 2
dados<- data.frame(all_tables[2])
dados
str(dados)

#Eliminar as duas primeiras linhas
dados<-data.frame(dados[-c(1,2), -2])

head(dados)

# Identificar colunas que parecem numéricas mas são caracteres
columnas_a_convertir <- sapply(dados, function(x) {
  all(grepl("^[0-9.]+$", x) | is.na(x))
})

# Converter apenas essas colunas
dados[columnas_a_convertir] <- lapply(dados[columnas_a_convertir], as.numeric)
str(dados)

# Dimensões do dados
cat("Dimensões do dados: ", dim(dados), "\n\n")

# Resumo estatístico
summary(dados)

# VALORES Na's
# Verificar valores nulos por coluna
print(colSums(is.na(dados)))

cat("Total de valores nulos:", sum(is.na(dados)), "\n")

#Análise numérica

numeric_cols <- dados %>% select(where(is.numeric))
categorical_cols <- dados %>% select(where(is.factor) | where(is.character))

dados_clean <- dados[complete.cases(numeric_cols), ]
cat("\nLinhas após eliminar NA das colunas numéricas:", nrow(dados_clean), "/", nrow(dados), "\n")

numeric_cols_clean <- dados_clean %>% select(where(is.numeric))
categorical_cols_clean <- dados_clean %>% select(where(is.factor) | where(is.character))

# Histogramas para variáveis numéricas
if(ncol(numeric_cols) > 0) {
  
  # Calcular layout para grid
  n_plots <- ncol(numeric_cols)
  n_cols <- ceiling(sqrt(n_plots))
  n_rows <- ceiling(n_plots / n_cols)
  
  # Configurar layout
  par(mfrow = c(n_rows, n_cols))
  par(mar = c(4, 4, 2, 1))
  
  for(col in names(numeric_cols)) {
    hist(numeric_cols[[col]], 
         main = paste("Histograma de", col),
         xlab = col,
         ylab = "Frequência",
         col = "steelblue",
         border = "white",
         breaks = 20)
  }
  
  # Resetar layout
  par(mfrow = c(1, 1))
}


# Scatter plots entre variáveis numéricas
if(ncol(numeric_cols) > 1) {
  
  # Criar matriz de scatter plots
  pairs(numeric_cols,
        main = "Matriz de Scatter Plots",
        pch = 19,
        col = "steelblue",
        cex = 0.8,
        gap = 0.5)
}

# Boxplots para variáveis numéricas
# Configurar layout
n_total_plots <- length(names(numeric_cols)) * length(names(categorical_cols))
n_cols <- ceiling(sqrt(n_total_plots))
n_rows <- ceiling(n_total_plots / n_cols)

par(mfrow = c(n_rows, n_cols))
par(mar = c(7, 4, 3, 2))
par(cex.main = 0.9)
par(cex.lab = 0.8)

# Gerar boxplots
for(num_col in names(numeric_cols)) {
  for(cat_col in names(categorical_cols)) {
    boxplot(as.formula(paste(num_col, "~", cat_col)),
            data = dados_clean,
            main = paste(num_col, "por", cat_col),
            xlab = "", 
            ylab = num_col,
            col = rainbow(length(unique(dados_clean[[cat_col]]))),
            las = 2)  # Rotacionar rótulos do eixo X
  }
}


