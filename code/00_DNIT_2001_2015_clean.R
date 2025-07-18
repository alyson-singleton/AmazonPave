library(readxl)
library(sf)
library(tidyverse)
library(lwgeom)

#######################################
# load and standardize format 2001-2012
#######################################

#Brazilian Amazon boundaries
brazil_amazon <- st_read("~/Desktop/doctorate/ch3 amazon network/data/Limites_Amazonia_Legal_2022_shp/Limites_Amazonia_Legal_2022.shp")
brazil_amazon$geometry <- st_transform(brazil_amazon$geometry, 4326)
numeric_columns <- c("KM_INI", "KM_FIM", "EXTENSAO")

#2001
PNV_2001 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2001.xlsx")
PNV_2001 <- PNV_2001[!is.na(PNV_2001$BR),]
PNV_2001$vl_codigo <- PNV_2001$CODIGO
PNV_2001$name <- substr(PNV_2001$vl_codigo,1,6)
PNV_2001$number <- substr(PNV_2001$vl_codigo,7,10)
PNV_2001$SUP_ESTADUAL <- NA
PNV_2001 <- PNV_2001[,c(1:7,9,8,10:11,16,12:15)]
PNV_2001[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2001[[x]]))
PNV_2001[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2001[[x]],2))

#2002
PNV_2002 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2002.xlsx")
PNV_2002 <- PNV_2002[!is.na(PNV_2002$BR),]
PNV_2002$vl_codigo <- PNV_2002$CODIGO
PNV_2002$name <- substr(PNV_2002$vl_codigo,1,6)
PNV_2002$number <- substr(PNV_2002$vl_codigo,7,10)
PNV_2002$SUP_ESTADUAL <- NA
PNV_2002 <- PNV_2002[,c(1:11,16,12:15)]
PNV_2002[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2002[[x]]))
PNV_2002[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2002[[x]],2))

#2003
PNV_2003 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2003.xlsx")
PNV_2003 <- PNV_2003[!is.na(PNV_2003$BR),]
PNV_2003$vl_codigo <- PNV_2003$CODIGO
PNV_2003$name <- substr(PNV_2003$vl_codigo,1,6)
PNV_2003$number <- substr(PNV_2003$vl_codigo,7,10)
PNV_2003$SUP_ESTADUAL <- NA
PNV_2003 <- PNV_2003[,c(1:11,16,12:15)]
PNV_2003[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2003[[x]]))
PNV_2003[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2003[[x]],2))

#2004
PNV_2004 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2004.xlsx")
PNV_2004 <- PNV_2004[!is.na(PNV_2004$BR),]
PNV_2004$vl_codigo <- PNV_2004$CODIGO
PNV_2004$name <- substr(PNV_2004$vl_codigo,1,6)
PNV_2004$number <- substr(PNV_2004$vl_codigo,7,10)
PNV_2004$SUP_ESTADUAL <- NA
PNV_2004 <- PNV_2004[,c(1:11,16,12:15)]
PNV_2004[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2004[[x]]))
PNV_2004[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2004[[x]],2))

#2005
PNV_2005 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2005.xlsx")
PNV_2005 <- PNV_2005[!is.na(PNV_2005$BR),]
PNV_2005$vl_codigo <- PNV_2005$CODIGO
PNV_2005$name <- substr(PNV_2005$vl_codigo,1,6)
PNV_2005$number <- substr(PNV_2005$vl_codigo,7,10)
PNV_2005$SUP_ESTADUAL <- NA
PNV_2005 <- PNV_2005[,c(1:11,16,12:15)]
PNV_2005[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2005[[x]]))
PNV_2005[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2005[[x]],2))

#2006
PNV_2006 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2006.xlsx")
PNV_2006 <- PNV_2006[!is.na(PNV_2006$BR),]
PNV_2006$vl_codigo <- PNV_2006$CODIGO
PNV_2006$name <- substr(PNV_2006$vl_codigo,1,6)
PNV_2006$number <- substr(PNV_2006$vl_codigo,7,10)
PNV_2006$SUP_ESTADUAL <- NA
PNV_2006 <- PNV_2006[,c(1:11,16,12:15)]
PNV_2006[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2006[[x]]))
PNV_2006[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2006[[x]],2))

#2007
PNV_2007 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2007.xlsx")
PNV_2007 <- PNV_2007[!is.na(PNV_2007$BR),]
PNV_2007$vl_codigo <- PNV_2007$CODIGO
PNV_2007$name <- substr(PNV_2007$vl_codigo,1,6)
PNV_2007$number <- substr(PNV_2007$vl_codigo,7,10)
colnames(PNV_2007)[12] <- "SUP_ESTADUAL"
PNV_2007[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2007[[x]]))
PNV_2007[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2007[[x]],2))

#2008
PNV_2008 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2008.xlsx")
PNV_2008 <- PNV_2008[!is.na(PNV_2008$BR),]
PNV_2008$vl_codigo <- PNV_2008$CODIGO
PNV_2008$name <- substr(PNV_2008$vl_codigo,1,6)
PNV_2008$number <- substr(PNV_2008$vl_codigo,7,10)
colnames(PNV_2008)[12] <- "SUP_ESTADUAL" #check adding state PAV roads here / before
PNV_2008[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2008[[x]]))
PNV_2008[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2008[[x]],2))

#2009
PNV_2009 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2009.xlsx")
PNV_2009 <- PNV_2009[!is.na(PNV_2009$BR),]
PNV_2009$vl_codigo <- PNV_2009$CODIGO
PNV_2009$name <- substr(PNV_2009$vl_codigo,1,6)
PNV_2009$number <- substr(PNV_2009$vl_codigo,7,10)
colnames(PNV_2009)[12] <- "SUP_ESTADUAL"
PNV_2009[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2009[[x]]))
PNV_2009[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2009[[x]],2))

#2010
PNV_2010 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2010.xlsx")
colnames(PNV_2010) <- PNV_2010[2,]
PNV_2010 <- PNV_2010[-c(1,2),]
PNV_2010 <- PNV_2010[!is.na(PNV_2010$BR),]
PNV_2010$VERSAO <- 2010
PNV_2010$vl_codigo <- PNV_2010$Código
PNV_2010$name <- substr(PNV_2010$vl_codigo,1,6)
PNV_2010$number <- substr(PNV_2010$vl_codigo,7,10)
PNV_2010 <- PNV_2010[,c(1:10,13:14,17:20)]
colnames(PNV_2010) <- colnames(PNV_2009)
PNV_2010[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2010[[x]]))
PNV_2010[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2010[[x]],2))

#2011
PNV_2011 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_2011.xlsx")
colnames(PNV_2011) <- PNV_2011[2,]
PNV_2011 <- PNV_2011[-c(1,2),]
PNV_2011 <- PNV_2011[!is.na(PNV_2011$BR),]
PNV_2011$VERSAO <- 2011
PNV_2011$vl_codigo <- PNV_2011$Código
PNV_2011$name <- substr(PNV_2011$vl_codigo,1,6)
PNV_2011$number <- substr(PNV_2011$vl_codigo,7,10)
PNV_2011 <- PNV_2011[,c(1:2,6:13,16:17,20:23)]
colnames(PNV_2011) <- colnames(PNV_2009)
PNV_2011[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2011[[x]]))
PNV_2011[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2011[[x]],2))

#2012
PNV_2012 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_2012.xlsx")
colnames(PNV_2012) <- PNV_2012[2,]
PNV_2012 <- PNV_2012[-c(1,2),]
PNV_2012 <- PNV_2012[!is.na(PNV_2012$BR),]
PNV_2012$VERSAO <- 2012
PNV_2012$vl_codigo <- PNV_2012$Código
PNV_2012$name <- substr(PNV_2012$vl_codigo,1,6)
PNV_2012$number <- substr(PNV_2012$vl_codigo,7,10)
PNV_2012 <- PNV_2012[,c(1:2,6:13,16:17,21:24)]
colnames(PNV_2012) <- colnames(PNV_2009)
PNV_2012[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2012[[x]]))
PNV_2012[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2012[[x]],2))

#2013
PNV_2013 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_2013.xlsx")
colnames(PNV_2013) <- PNV_2013[2,]
PNV_2013 <- PNV_2013[-c(1,2),]
PNV_2013 <- PNV_2013[!is.na(PNV_2013$BR),]
PNV_2013$VERSAO <- 2013
PNV_2013$vl_codigo <- PNV_2013$Código
PNV_2013$name <- substr(PNV_2013$vl_codigo,1,6)
PNV_2013$number <- substr(PNV_2013$vl_codigo,7,10)
PNV_2013 <- PNV_2013[,c(1:2,6:13,16:17,20:23)]
colnames(PNV_2013) <- colnames(PNV_2009)
PNV_2013[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2013[[x]]))
PNV_2013[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2013[[x]],2))

#2014
PNV_2014 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_2014.xlsx")
colnames(PNV_2014) <- PNV_2014[2,]
PNV_2014 <- PNV_2014[-c(1,2),]
PNV_2014 <- PNV_2014[!is.na(PNV_2014$BR),]
PNV_2014$VERSAO <- 2014
PNV_2014$vl_codigo <- PNV_2014$Código
PNV_2014$name <- substr(PNV_2014$vl_codigo,1,6)
PNV_2014$number <- substr(PNV_2014$vl_codigo,7,10)
PNV_2014 <- PNV_2014[,c(1:2,6:13,16:17,22:25)]
colnames(PNV_2014) <- colnames(PNV_2009)
PNV_2014[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2014[[x]]))
PNV_2014[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2014[[x]],2))

#2015
PNV_2015 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_201503A.xls")
colnames(PNV_2015) <- PNV_2015[2,]
PNV_2015 <- PNV_2015[-c(1,2),]
PNV_2015 <- PNV_2015[!is.na(PNV_2015$BR),]
PNV_2015$VERSAO <- 2015
PNV_2015$vl_codigo <- PNV_2015$Código
PNV_2015$name <- substr(PNV_2015$vl_codigo,1,6)
PNV_2015$number <- substr(PNV_2015$vl_codigo,7,10)
PNV_2015 <- PNV_2015[,c(1:2,6:13,16:17,25:28)]
colnames(PNV_2015) <- colnames(PNV_2009)
PNV_2015[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(PNV_2015[[x]]))
PNV_2015[, numeric_columns] <- lapply(numeric_columns, function(x) round(PNV_2015[[x]],2))

#######################################
# add spatial data by linking to 2024
#######################################

#2024
DNIT_2024 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202410A/SNV_202410A.shp")
DNIT_2024$VERSAO <- 2024
colnames(DNIT_2024)[7] <- "codigo"
DNIT_2024$vl_codigo <- DNIT_2024$codigo
DNIT_2024$name <- substr(DNIT_2024$vl_codigo,1,6)
DNIT_2024$number <- substr(DNIT_2024$vl_codigo,7,10)
DNIT_2024 <- DNIT_2024[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2024) <- set_colnames

DNIT_2024$geometry <- st_transform(st_zm(DNIT_2024$geometry), 4326)
#DNIT_2024_bool <- st_covers(brazil_amazon,DNIT_2024$geometry, sparse = FALSE)
#DNIT_2024_amazon <- DNIT_2024[DNIT_2024_bool[1,],]
DNIT_2024_amazon_paved <- DNIT_2024 %>% 
  filter(SUPERFICIE  %in% c('PAV', 'DUP', 'EOD') | SUP_ESTADUAL %in% c('PAV', 'DUP', 'EOD')) %>%
  filter(UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "MT", "MA"))

#loop to add 2024 shapefiles to each year (2001-2012)
linestring_two_splits_function <- function(start_fraction, end_fraction, row_of_interest) {
  
  line <- row_of_interest
  line_proj <- st_transform(line, 3857)
  
  start_point <- st_line_sample(line_proj, sample = start_fraction) %>% st_transform(st_crs(line_proj))
  end_point <- st_line_sample(line_proj, sample = end_fraction) %>% st_transform(st_crs(line_proj))
  
  split_points <- st_union(start_point,end_point)
  
  blades <- split_points %>%
    st_cast("POINT") %>%
    st_coordinates() %>%
    asplit(1) %>%
    lapply(function(x) rbind(x + c(0, -1e-6), x + c(0, 1e-6))) %>%
    st_multilinestring() %>%
    st_sfc(crs = 3857)
  
  segments <- st_split(line_proj, blades) %>% 
    st_collection_extract("LINESTRING")
  
  segments <- segments %>% st_transform(crs=4326)
  
  return(segments)
}
linestring_one_split_function <- function(start_fraction, row_of_interest) {
  
  line <- row_of_interest
  line_proj <- st_transform(line, 3857)
  
  start_point <- st_line_sample(line_proj, sample = start_fraction) %>% st_transform(st_crs(line_proj))

  blades <- start_point %>%
    st_cast("POINT") %>%
    st_coordinates() %>%
    asplit(1) %>%
    lapply(function(x) rbind(x + c(0, -1e-6), x + c(0, 1e-6))) %>%
    st_multilinestring() %>%
    st_sfc(crs = 3857)
  
  segments <- st_split(line_proj, blades) %>% 
    st_collection_extract("LINESTRING")
  
  segments <- segments %>% st_transform(crs=4326)
  
  return(segments)
}

PNV_paved_sf <- list()
years <- 2001:2015
years <- 2015
year <- 2015
for (year in years){
  print(year)
  old_df_name <- paste0("PNV_", year, sep="")
  PNV_paved <- get(old_df_name)
  PNV_paved <- PNV_paved[which(PNV_paved$SUPERFICIE  %in% c('PAV', 'DUP', 'EOD') | PNV_paved$SUP_ESTADUAL %in% c('PAV', 'DUP', 'EOD')),]
  PNV_paved
  
  #big loop to run across years
  for(k in 1:length(unique(PNV_paved$name))){
    #k <- match("222BMA",unique(PNV_paved$name))
    print(k)#40
    name_k <- unique(PNV_paved$name)[k]
    PNV_w_name_k <- PNV_paved[which(PNV_paved$name == name_k),]
    DNIT_2024_w_name_k <- DNIT_2024_amazon_paved[which(DNIT_2024_amazon_paved$name == name_k),] #add coin col here?
    
    geometries <- vector("list",length=nrow(PNV_w_name_k))
    for (i in 1:dim(PNV_w_name_k)[1]){
      #print(i)
      #i <- 3
      row <- PNV_w_name_k[i,]
      row_km_ini <- as.numeric(row$KM_INI)
      row_km_fim <- as.numeric(row$KM_FIM)
      
      #option 1: early segment is fully inside one 2024 segment
      found_row <- DNIT_2024_w_name_k[which(DNIT_2024_w_name_k$KM_INI <= row_km_ini & 
                                              DNIT_2024_w_name_k$KM_FIM >= row_km_fim),]
      if (dim(found_row)[1]==1 &&
          !(year == 2002 && k == 18)) {
        #print("a")
        
        km_ini_2024 <- found_row$KM_INI
        km_fim_2024 <- found_row$KM_FIM
        
        start_fraction <- (row_km_ini-km_ini_2024)/(km_fim_2024-km_ini_2024)
        end_fraction <- (row_km_fim-km_ini_2024)/(km_fim_2024-km_ini_2024)
        
        segments <- linestring_two_splits_function(start_fraction, end_fraction, found_row)
        
        if(dim(segments)[1]==3){
          geom <- segments$geometry[2,]
          geometries[[i]] <- geom[[1]]
        }
        
        if(start_fraction == 0 && dim(segments)[1]==2){
          geom <- segments$geometry[1,]
          geometries[[i]] <- geom[[1]]
        }
        
        if(end_fraction == 1 && dim(segments)[1]==2){
          geom <- segments$geometry[2,]
          geometries[[i]] <- geom[[1]]
        }
        
        # perfect match
        if(found_row$KM_INI == row_km_ini && found_row$KM_FIM == row_km_fim){
          geometries[[i]] <- st_geometry(found_row[,'geometry'])[[1]]
        }
      }
      
      
      #option 2: early segment spans two 2024 segments
      if (!dim(found_row)[1]==1) {
        ini_rows <- DNIT_2024_w_name_k[which(DNIT_2024_w_name_k$KM_INI <= row_km_ini),]
        lowest_row <- last(ini_rows)
        fim_rows <- DNIT_2024_w_name_k[which(DNIT_2024_w_name_k$KM_FIM >= row_km_fim),]
        highest_row <- first(fim_rows)
        highest_row_index <- which(highest_row$CODIGO == DNIT_2024_w_name_k$CODIGO)
        lowest_row_index <- which(lowest_row$CODIGO == DNIT_2024_w_name_k$CODIGO)
        range <- highest_row_index - lowest_row_index
        #print(range)
        
        #option 2.0: check if there is partial coverage
        if(dim(ini_rows)[1]==0 && dim(fim_rows)[1]!=0){
          lowest_row <- first(DNIT_2024_w_name_k)
          lowest_row_index <- which(lowest_row$CODIGO == DNIT_2024_w_name_k$CODIGO)
          range <- highest_row_index - lowest_row_index
        }
        
        if(dim(ini_rows)[1]!=0 && dim(fim_rows)[1]==0){
          highest_row <- last(DNIT_2024_w_name_k)
          highest_row_index <- which(highest_row$CODIGO == DNIT_2024_w_name_k$CODIGO)
          range <- highest_row_index - lowest_row_index
        }
        
        #option 2.1: partial match is inside one row
        if(length(range)!=0 && range==0 && lowest_row$KM_INI >= row_km_ini && dim(fim_rows)[1]==0){
          geometries[[i]] <- st_geometry(DNIT_2024_w_name_k[lowest_row_index,'geometry'])[[1]]
        }
        
        if(length(range)!=0 && range==0 && highest_row$KM_FIM <= row_km_fim && dim(ini_rows)[1]==0){
          geometries[[i]] <- st_geometry(DNIT_2024_w_name_k[highest_row_index,'geometry'])[[1]]
        }
        
        #option 2.2: partial match w one row, but there's a portion outside that row (with no other match)
        if(length(range)!=0 && range==0 && lowest_row$KM_INI <= row_km_ini && dim(fim_rows)[1]==0){
          #cut part outside lower row
          start_fraction <- (row_km_ini-lowest_row$KM_INI)/(lowest_row$KM_FIM-lowest_row$KM_INI)
          segments <- linestring_one_split_function(start_fraction, lowest_row)
          if(dim(segments)[1]==2){
            geom <- segments$geometry[2,]
          }
          if(dim(segments)[1]==1){
            geom <- segments$geometry[1,]
          }
          if(dim(segments)[1]!=2 && dim(segments)[1]!=1){
            geom <- NA
          }
          geometries[[i]] <- geom[[1]]
        }
        
        if(length(range)!=0 && range==0 && highest_row$KM_FIM >= row_km_fim && dim(ini_rows)[1]==0){
          #cut part outside higher row
          start_fraction <- (row_km_fim-highest_row$KM_INI)/(highest_row$KM_FIM-highest_row$KM_INI)
          segments <- linestring_one_split_function(start_fraction, highest_row)
          if(dim(segments)[1]==2){
            geom <- segments$geometry[1,]
          }
          if(dim(segments)[1]==1){
            geom <- segments$geometry[1,]
          }
          if(dim(segments)[1]!=2 && dim(segments)[1]!=1){
            geom <- NA
          }
          geometries[[i]] <- geom[[1]]
        }
        
        #option 2.3: no matches at all
        if(length(range)==0){
          geometries[[i]] <- st_sfc(st_linestring())[[1]]
        }
      
        #option 2.4: if spans two rows, a) grab portion from first segment
        if (!length(range)==0 && range==1 &&
            !(year == 2002 && k == 18)) {
          #print("b")
          
          km_ini_2024 <- lowest_row$KM_INI
          km_fim_2024 <- lowest_row$KM_FIM
          start_fraction <- (row_km_ini-km_ini_2024)/(km_fim_2024-km_ini_2024)
          
          if(start_fraction < 1){
            segments <- linestring_one_split_function(start_fraction, lowest_row)
            
            if(dim(segments)[1]==2){
              geom1 <- segments$geometry[2,]
            }
            if(dim(segments)[1]==1){
              geom1 <- segments$geometry[1,]
            }
            if(dim(segments)[1]!=2 && dim(segments)[1]!=1){
              geom1 <- NA
            } 
          }else{
            geom1 <- NA
          }
          
          #b) grab portion from second segment
          km_ini_2024 <- highest_row$KM_INI
          km_fim_2024 <- highest_row$KM_FIM
          end_fraction <- (row_km_fim-km_ini_2024)/(km_fim_2024-km_ini_2024)
          
          if(end_fraction < 1){
            segments <- linestring_one_split_function(end_fraction, highest_row)
            
            if(dim(segments)[1]==2){
              geom2 <- segments$geometry[1,]
            }
            if(dim(segments)[1]==1){
              geom2 <- segments$geometry[1,]
            }
            if(dim(segments)[1]!=2 && dim(segments)[1]!=1){
              geom2 <- NA
            }
          }else{
            geom2 <- NA
          }
          
          #c) unite and store
          if(!is.na(geom1) && !is.na(geom2)){
            all_geoms <- c(geom1, geom2)
            
            all_geoms <- st_as_sf(all_geoms) %>%
              st_combine() %>%
              st_line_merge()
            
            geometries[[i]] <- all_geoms[[1]]
          }
          if(is.na(geom1) && !is.na(geom2)){
            geometries[[i]] <- geom2[[1]]
          }
          if(!is.na(geom1) && is.na(geom2)){
            geometries[[i]] <- geom1[[1]]
          }
          if(is.na(geom1) && is.na(geom2)){
            geometries[[i]] <- st_sfc(st_linestring())[[1]]
          }
        }
      }
      
      #option 3: early segment spans multiple 2024 segments and matches perfectly
      if (!dim(found_row)[1]==1 && !length(range)==0 && range>1 &&
          !length(which(DNIT_2024_w_name_k$KM_INI == row_km_ini))==0 &&
          !length(which(DNIT_2024_w_name_k$KM_FIM == row_km_fim))==0 &&
          !(year == 2002 && k == 18)) {
        
        multiple_rows <- DNIT_2024_w_name_k[(as.numeric(lowest_row_index)):(as.numeric(highest_row_index)),]
        multiple_rows_trans <- multiple_rows %>% 
          st_transform(crs=4326)
        
        all_geoms <- c(multiple_rows_trans$geometry)
        
        all_geoms <- st_as_sf(all_geoms) %>%
          st_combine() %>%
          st_line_merge()
        
        geometries[[i]] <- all_geoms[[1]]
      }
      
      #option 4: early segment includes full and partial 2024 segments
      if (!dim(found_row)[1]==1 && !length(range)==0 && range>1 &&
          (length(which(DNIT_2024_w_name_k$KM_INI == row_km_ini))==0 ||
           length(which(DNIT_2024_w_name_k$KM_FIM == row_km_fim))==0) &&
          !(year == 2002 && k == 18)) {
        #print("c")
        middle_rows <- DNIT_2024_w_name_k[(as.numeric(lowest_row_index)+1):(as.numeric(highest_row_index)-1),]
        middle_rows_trans <- middle_rows %>% 
          st_transform(crs=4326)
        #print(middle_rows)
        
        #find ends like option 2
        #grab portion from first segment
        km_ini_2024 <- lowest_row$KM_INI
        km_fim_2024 <- lowest_row$KM_FIM
        start_fraction <- (row_km_ini-km_ini_2024)/(km_fim_2024-km_ini_2024)
        
        if(start_fraction < 1){
          segments <- linestring_one_split_function(start_fraction, lowest_row)
          
          if(dim(segments)[1]==2){
            geom1 <- segments$geometry[2,]
          }else{
            geom1 <- NA
          } 
        }else{
          geom1 <- NA
        }
        
        #grab portion from second segment
        km_ini_2024 <- highest_row$KM_INI
        km_fim_2024 <- highest_row$KM_FIM
        end_fraction <- (row_km_fim-km_ini_2024)/(km_fim_2024-km_ini_2024)
        
        if(end_fraction < 1){
          segments <- linestring_one_split_function(end_fraction, highest_row)
          
          if(dim(segments)[1]==2){
            geom2 <- segments$geometry[1,]
          }else{
            geom2 <- NA
          } 
        }else{
          geom2 <- NA
        }
        
        #unite and store
        if(!is.na(geom1) && !is.na(geom2)){
          all_geoms <- c(geom1, geom2, middle_rows_trans$geometry)
          
          all_geoms <- st_as_sf(all_geoms) %>%
            st_combine() %>%
            st_line_merge()
          
          geometries[[i]] <- all_geoms[[1]]
        }
        if(is.na(geom1) && !is.na(geom2)){
          all_geoms <- c(geom2, middle_rows_trans$geometry)
          
          all_geoms <- st_as_sf(all_geoms) %>%
            st_combine() %>%
            st_line_merge()
          
          geometries[[i]] <- all_geoms[[1]]
        }
        if(!is.na(geom1) && is.na(geom2)){
          all_geoms <- c(geom1, middle_rows_trans$geometry)
          
          all_geoms <- st_as_sf(all_geoms) %>%
            st_combine() %>%
            st_line_merge()
          
          geometries[[i]] <- all_geoms[[1]] 
        }
        if(is.na(geom1) && is.na(geom2)){
          all_geoms <- middle_rows_trans$geometry
          
          all_geoms <- st_as_sf(all_geoms) %>%
            st_combine() %>%
            st_line_merge() 
          
          geometries[[i]] <- all_geoms[[1]]
        }
      }
    }
    
    PNV_w_name_k_sf <- st_as_sf(PNV_w_name_k, 
                                geometry=st_sfc(geometries, crs=4326))# %>% st_transform(crs=4326)
    PNV_paved_sf[[k]] <- PNV_w_name_k_sf
  }
  
  PNV_paved_sf_unlisted <- do.call(rbind, PNV_paved_sf)
  #df_bool <- st_covers(brazil_amazon,PNV_paved_sf_unlisted$geometry, sparse = FALSE)
  #df_amazon <- PNV_paved_sf_unlisted[df_bool[1,],]
  new_df_name <- paste0("DNIT_", year, "_amazon_paved_filled", sep="")
  assign(new_df_name, PNV_paved_sf_unlisted) #df_amazon
}

#check
mapview(DNIT_2001_amazon_paved_filled)
mapview(DNIT_2002_amazon_paved_filled)
mapview(DNIT_2003_amazon_paved_filled)
mapview(DNIT_2004_amazon_paved_filled)
mapview(DNIT_2005_amazon_paved_filled)
mapview(DNIT_2006_amazon_paved_filled)
mapview(DNIT_2007_amazon_paved_filled)
mapview(DNIT_2008_amazon_paved_filled)
mapview(DNIT_2009_amazon_paved_filled)
mapview(DNIT_2010_amazon_paved_filled)
mapview(DNIT_2011_amazon_paved_filled)
mapview(DNIT_2012_amazon_paved_filled)
mapview(DNIT_2013_amazon_paved_filled)
mapview(DNIT_2014_amazon_paved_filled)
mapview(DNIT_2015_amazon_paved_filled)

#store
for (year in years){
  df_name <- paste0("DNIT_", year, "_amazon_paved_filled", sep="")
  df <- get(df_name)
  df_bool <- st_covers(brazil_amazon,df$geometry, sparse = FALSE)
  df_amazon <- df[df_bool[1,],]
  st_write(df_amazon, paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_yearly_base_maps/", "DNIT_", year, "_base_map.shp", sep=""), append=FALSE)
}

#check missingness (filter for paved and in amazon)
summary_df <- data.frame(year = numeric(), original = numeric(), edited = numeric(), stringsAsFactors = FALSE)
years <- 2001:2015
years<-2015
for (year in years){
  year_index <- match(year,years)
  df_filtered_name <- paste0("PNV_", year, sep="")
  df_filtered <- get(df_filtered_name)
  df_filtered <- df_filtered %>% 
    filter(SUPERFICIE  %in% c('PAV', 'DUP', 'EOD') | SUP_ESTADUAL %in% c('PAV', 'DUP', 'EOD')) %>%
    filter(UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "MT", "MA"))
  
  df_filled_name <- paste0("DNIT_", year, "_amazon_paved_filled", sep="")
  df_filled <- get(df_filled_name)
  df_filled <- df_filled[!st_is_empty(df_filled),]
  
  summary_df[year_index, ] <- list(year, dim(df_filtered)[1], table(st_is_empty(df_filled$geometry))[[1]])
}

summary_df$difference <- summary_df$original - summary_df$edited #range 57-98
summary_df

#which in original but not edited
df_filtered_codes_no_match <- df_filtered$CODIGO[-which(df_filtered$CODIGO %in% df_filled$CODIGO)]
df_filtered_rows_no_match <- df_filtered[which(df_filtered$CODIGO %in% df_filtered_codes_no_match),]

#010BMA0470 starting w zeroes getting missed even when there should be a match

#######################################
# SCRATCH
#######################################

#join w 2024 by vl_codigo
years <- 2001:2012
year <- 2012
for (year in years){
  print(year)
  
  old_df_name <- paste0("PNV_", year, sep="")
  df <- get(old_df_name)
  
  joined_left_sf <- left_join(df,DNIT_2024_reduced, by=c("vl_codigo")) %>% st_as_sf()
  joined_sf_bool <- st_covers(brazil_amazon,joined_left_sf$geometry, sparse = FALSE)
  joined_sf_amazon <- joined_left_sf[joined_sf_bool[1,],]
  #NOTE: write now ive written it like we don't care about duplicated v non duplicated, just PAVED
  joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$SUPERFICIE  %in% c('PAV', 'DUP', 'EOD') | joined_sf_amazon$SUP_ESTADUAL %in% c('PAV', 'DUP', 'EOD')),]
  
  new_df_name <- paste0("DNIT_", year, "_amazon_paved", sep="")
  assign(new_df_name, joined_sf_amazon_paved)
}

#fill in additional spottiness
join_right_2001 <- right_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo")) #is DNIT_2024_reduced just amazon roads here? can PNV_2001 be DNIT_2001_amazon_paved?
join_inner_2001 <- inner_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo"))

#2024 no match
codes_w_match <- unique(join_right_2001$vl_codigo)[unique(join_right_2001$vl_codigo) %in% unique(join_inner_2001$vl_codigo)]
join_right_2001_match_col <- join_right_2001 %>%
  mutate(match = join_right_2001$vl_codigo %in% codes_w_match)
join_right_2001_match_col$name <- substr(join_right_2001_match_col$vl_codigo,1,6)
join_right_2001_match_col$number <- substr(join_right_2001_match_col$vl_codigo,7,10)
join_right_2001_match_col <- join_right_2001_match_col[,c(1:3,18,4:17)]

#loop to fill in missing pieces that don't reflect actual paving status (i.e., known to paved in 2001)
our_list <- c("364BRO","174BRR","364BMT","163BMT","070BMT","163BMT","153BTO", "010BMA", "230BTO") #all: unique(DNIT_2001_amazon_paved$name)
years <- 2001:2012

for (year in years){
  print(year)
  old_df_name <- paste0("DNIT_", year, "_amazon_paved", sep="")
  df <- get(old_df_name)
  df$match <- TRUE
  df <- df[,c(1:3,18,4:17)]
  for (i in 1:length(our_list)) {
    name_i <- our_list[i]
    all_unmatched_2024_name_i <- join_right_2001_match_col[which(join_right_2001_match_col$name == name_i & join_right_2001_match_col$match == FALSE),] %>% st_as_sf()
    df <- rbind(df,all_unmatched_2024_name_i)
  }
  new_df_name <- paste0("DNIT_", year, "_amazon_paved_filled", sep="")
  assign(new_df_name, df)
}

#store
for (year in years){
  df_name <- paste0("DNIT_", year, "_amazon_paved_filled", sep="")
  df <- get(df_name)
  st_write(df, paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_yearly_base_maps/", "DNIT_", year, "_base_map.shp", sep=""))
}