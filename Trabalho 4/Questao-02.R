# import das bibliotecas
library(stringr)
library(tidyr)

# função para buscar vulnerabilidades nos arquivos de dados
buscarVulnerabilidades <- function(arquivo) {
  
  nome_dataset <- str_replace(arquivo, ".csv", "")
  image_dir <- paste("/Users/randersondouglas/Pictures", nome_dataset, sep = "/")
  if(!dir.exists(image_dir)){
    dir.create(image_dir, recursive = TRUE)
  }
  
  # lendo CSV
  dataset <- read.csv(arquivo);
  
  
  # removendo linhas que possuem NA como informação
  dataset <- na.omit(dataset) 
  
  
  # removendo a coluna Fold. 
  dataset = subset(dataset, select = -c(Fold))
  
  # dividindo a coluna Affected em dois grupos (NEUTRAL e VULNERABLE)
  agrupamento <- split(dataset, dataset$Affected)
  neutro <- agrupamento$'NEUTRAL'
  vulneravel <- agrupamento$'VULNERABLE'
  
  # 
  for (colname in colnames(dataset)) {
    if(tolower(colname) == "affected") next
    
    metrica_n <- neutro[,colname]
    metrica_v <- vulneravel[,colname]
    
    # aplicando teste estatistico wilcox para comparar se duas amostras independentes 
    # foram selecionadas a partir de populações que têm a mesma distribuição.
    #test <- wilcox.test(metrica_n, metrica_v )
    test <- t.test(metrica_n, metrica_v )
    
    # verificação do nivel de significância
    if(test$p.value <= 0.05){
      while (!is.null(dev.list()))  dev.off()
      
      # preaparação do arquivo a ser gerado
      img_nome <- str_replace(arquivo, ".csv", paste(colname,"png", sep = "."))
      img_nome <- paste("RQ2",img_nome, sep = "-")
      
      png(filename = paste(image_dir, img_nome, sep="/"))
      
      # geração dos gráficos boxplots comparando sempre com a coluna Affected
      boxplot(dataset[,colname]~Affected, data = dataset,
              main = paste(colname, "Affected", sep = " X "), 
              ylab = colname, xlab="Affected", col= c("blue","pink"))
    }
  }
  dev.off()
}

# caminho onde estão localizados os arquivos (datasets)
diretorio <- "/Users/randersondouglas/Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/3 - Trabalho/Vulnerability_Dataset"

tipos <- c("unbalanced", "random_undersampling")
for (tipo in tipos) {
  setwd(paste(diretorio, tipo, sep = "/"))
  
  files <- list.files(include.dirs = FALSE)
  files <- files[!file.info(files)$isdir]
  
  for (file in files) {
    # executa a função em cima de cada arquivo
    buscarVulnerabilidades(file)
  }
}
