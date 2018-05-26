# import das bibliotecas 
library("corrplot")
library("tidyr")

# função para gerar as correlações das features de cada base
gerarCorrelacao <- function(url_base, tipo, arquivo){
 
  # montando o caminho do local onde encontra-se o dataset
  diretorio <- paste(url_base,tipo,arquivo, sep="/")
  dataset <- read.csv(diretorio);
  
  # removendo linhas que possuem NA como informação
  dataset <- na.omit(dataset)
 
  
  # removendo a coluna Fold. 
  dataset = subset(dataset, select = -c(29))
  
  str(dataset)
  
  table(dataset$Affected)
  # Alterando o tipo da variável Affected: de character para numeric
  # para seguir o padrão das demais e assim possamos gerar a correlação entre as variáveis
  #1 = NEUTRAL and 2 = VULNERABLE
  dataset$Affected <- as.numeric(dataset$Affected)
  
  # gera a correlação de todas as features de cada dataset apresentado
  cor_dataset <- cor(dataset, use="all.obs", method="pearson")
  
  # nível de confiança (probabilidade de estar correto) para ser usado na criação do gráfico
  p_mat <- cor.mtest(cor_dataset, conf.level = .95)
  
  
  library(stringr)
  img_nome <- str_replace(arquivo, "csv", "png")
  img_nome <- paste("RQ1", img_nome, sep = "-")
  img_dir <- paste("/Users/randersondouglas/Pictures", img_nome, sep="/")
  png(img_dir)
  
  # # nível de significância (probabilidade de erro)
  # corrplot(cor_dataset, method = "color", type = "upper",
  #          insig = "label_sig", p.mat = p_mat$p, sig.level = 0.05,
  #          order = "hclust", hclust.method = "single",
  #          tl.col = "black", tl.cex = 0.7,
  #          cl.ratio = 0.15, cl.cex = 0.7,
  #          is.corr = FALSE, diag = F)
  # dev.off()
  
  library(RColorBrewer)
  corrplot(cor_dataset, type = "upper", order = "hclust",
           col = brewer.pal(n = 8, name = "RdYlBu"))
  dev.off()

}

# Seta os diretórios de onde estão cada tipo de dataset de modo separado
caminho_balanceado <- "/Users/randersondouglas/Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/3 - Trabalho/Vulnerability_Dataset"
caminho_desbalanceado <- "/Users/randersondouglas/Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/3 - Trabalho/Vulnerability_Dataset"
balanceado <- "random_undersampling"
desbalanceado <- "unbalanced"

# correlação dos datasets glibc
gerarCorrelacao(caminho_balanceado, balanceado, "glibc_data_balanced.csv")
gerarCorrelacao(caminho_desbalanceado, desbalanceado, "glibc_data.csv")

# correlação dos datasets httpd
gerarCorrelacao(caminho_balanceado, balanceado, "httpd_data_balanced.csv")
gerarCorrelacao(caminho_desbalanceado, desbalanceado, "httpd_data.csv")

# correlação dos datasets kernel
gerarCorrelacao(caminho_balanceado, balanceado, "kernel_data_balanced.csv")
gerarCorrelacao(caminho_desbalanceado, desbalanceado, "kernel_data.csv")

# correlação dos datasets mozilla
gerarCorrelacao(caminho_balanceado, balanceado, "mozilla_data_balanced.csv")
gerarCorrelacao(caminho_desbalanceado, desbalanceado, "mozilla_data.csv")

# correlação dos datasets xen
gerarCorrelacao(caminho_balanceado, balanceado, "xen_data_balanced.csv")
gerarCorrelacao(caminho_desbalanceado, desbalanceado, "xen_data.csv")

