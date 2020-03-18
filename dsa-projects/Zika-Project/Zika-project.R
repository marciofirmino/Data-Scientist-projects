#Definindo pasta do projeto
setwd("~/Documentos/Firma_Things/R/Zika-Project/")
# Verificando pasta do projeto
getwd()


# http://combateaedes.saude.gov.br/pt/situacao-epidemiologica


#Carregando os pacotes

library(dplyr)
library(ggplot2)

#linstando os arquivos e gerando uma lista com os respectivos nomes

temp_files <- list.files(pattern = ".csv")
temp_files

#carregando todos os arquivos em um unico objeto
myfiles <- lapply(temp_files, read.csv, stringsAsFactors = FALSE) 
myfiles
# Resumo dos arquivos
str(myfiles, 1)
lapply(myfiles, names)[1]
lapply(myfiles, head,2)[1:2]

#Organizando o shape dos dados
brazil <- do.call(rbind, myfiles)
View(brazil)
brazil <- brazil %>%
  mutate(report_date = as.Date(report_date))
View(brazil)

#visualizando o dataset
glimpse(brazil)

#Transformando o dataframe  em uma tabela dplyr e removendo as colunas 6 e 7
brazil <- brazil %>% select(-(6:7)) 
View(brazil)

#Visualizando as primeiras 50 linhas 
brazil %>% slice(1:50)

#Para cada reporting_date nós temos 5 regiões
brazil %>% filter(location_type == "region")
brazil %>% filter(location_type == "region") %>%
  ggplot(aes(x = report_date, y = value, group = location, color = location)) +
  geom_line() + 
  geom_point() +
 ggtitle("Casos de Zika por região no Brasil")

#Separando as regiões e Visualizando os dados

region <- brazil %>%
  filter(location_type == "region")
  
  region %>%
    ggplot(aes(x = location, y = value))+ geom_bar(stat = "identity") +
    ylab("Numero de Casos Reportados") + xlab("Region") + ggtitle("Casos de Zika Reportados no Brazil")
  
  # Colocando as regiões por ordem decrescente
  region %>% 
    slice(1:length(unique(region$location))) %>% 
    arrange(desc(value)) %>%
    mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
    ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") +
    ylab("Número de Casos Reportados") + xlab("Region") + 
    ggtitle("Casos de Zika Reportados no Brasil")
  
  #Obtendo localidades unicas
  region %>%
    slice(1:length(unique(region$location)))
  
  #Organizando as localidades únicas por números de casos reportados
  
  region %>%
    slice(1:length(unique(region$location))) %>%
    arrange(desc(value))
  
  #Criando variaveis do tipo fator
    region %>%
      slice(1:length(unique(region$location))) %>%
      arrange(desc(value)) %>%
      mutate(location = factor(location,levels = location, ordered= TRUE))%>%
      glimpse()
  
  
# Agrupando e sumarizando
    brazil_totals <- brazil %>% filter(location == "Brazil")
    region_total <- brazil %>% filter(location_type =="region") %>%
      group_by(report_date,location) %>%
      summarize(tot = sum(value))
    
   
    # Padronizar os dados e remover as sumarizações
    regvec <- vector()  
    length(regvec) <- nrow(brazil)
    for (ii in 1:nrow(brazil)) {
      if (brazil[ii,]$location_type != "region")  {
        regvec[ii] <- newlab
      } else {
        newlab <- brazil[ii,]$location
        regvec[ii] <- newlab
      }
    }
    
    # Agregando o vetor de regiões ao dataframe brasil
    statedf <- cbind(brazil,regvec)
        statedf
    
  # Eliminar o sumário de linhas por região e país
  statedf <- statedf %>% filter(location != "Brazil") 
  statedf <- statedf %>% filter(location_type != "region") 
  
  # Gerar o total por regiões a partir dos dados transformados
  statedf %>% group_by(report_date,regvec) %>% 
    summarize(tot=sum(value)) -> totals
  
  # Gerando os mapas de cada estado do Brasil
    install.packages("ggmap") 
    ## O google maps agora precisa de API KEY!!
    # qualquer duvida consulte a documentação: https://cran.r-project.org/web/packages/ggmap/readme/README.html
    library(ggmap)
  longlat <- geocode(unique(statedf$location)) %>% 
    mutate(loc = unique(statedf$location)) 
  
  
  # Salvando os geocodes do dataframe statedf e salvando em um novo dataframe chamado formapping
  statedf %>% filter(as.character(report_date) == "2016-06-11") %>% 
    group_by(location) %>% summarize(cases = sum(value)) %>% 
    inner_join(longlat, by = c("location" = "loc")) %>% 
    mutate(LatLon = paste(lat, lon, sep = ":")) -> formapping
  
  # Visualizando os dados
  head(formapping) 
  
  # Formatando a saída e gerando um movo dataframe chamado long_formapping
  num_of_times_to_repeat <- formapping$cases
  long_formapping <- formapping[rep(seq_len(nrow(formapping)),
                                    num_of_times_to_repeat),]
  
  # Visualizando os dados
  head(long_formapping)
  
  # Instalando o pacote leaflet
  install.packages("leaflet")
  library(leaflet)
  
  # Gerando o mapa com o dataframe
  # Aplique o zoom
  leaflet(long_formapping) %>% 
    addTiles() %>% 
    addMarkers(clusterOptions = markerClusterOptions())
  
  