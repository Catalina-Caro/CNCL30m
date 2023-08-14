CNCl <- function(){
  #--Librería requerida
  library(raster)
  
  #--Definir directorio de trabajo
  setwd("directorio\\personal")
  
  
  
  #--Directorio temporal archivos ráster
  direcTmp <- "directorio\\temporal"
  dir.create(direcTmp, showWarnings = F)
  rasterOptions(tmpdir = direcTmp)
  
  
  #--Cargamos las capas raster, así como la región a trabajar en formato shape
  
  HYSOG_GLOBAL250m <- raster("./HYSOGs250m.tif")
  LC_CHILE30m <- raster("./LCCHILE30m.tif")
  POLIGONO_REGION <-shapefile("./REGIONES/REGION\\A\\EJECUTAR.shp")
  
  #--Cortamos la extensión de HYSOGs250m a la de LCCHILE30m
  #--Además, empleando el polígono de la región a evaluar, 
  #--delimitamos los datos a dicha zona, para ambas agrupaciones de datos
  
  HYSOG_CHILE250m <- crop(HYSOG_GLOBAL250m, extent(LC_CHILE30m))
  HYSOG_ZONA <- crop(HYSOG_CHILE250m, POLIGONO_REGION)
  LC_ZONA <- crop(LC_CHILE30m, POLIGONO_REGION)
  
  #--Cambiamos la resolución espacial de HYSOG a la que posee LCCHILE
  
  HYSOG_ZONA30m <- resample(HYSOG_ZONA, LC_ZONA, method = "ngb")
  
  #--En adición, con el fin de evitar datos no requeridos, 
  #--se ajusta la geometría de los datos ráster al polígono a evaluar
  
  HYSOG_ZONA30m_R <- mask(HYSOG_ZONA30m, POLIGONO_REGION)
  LC_ZONA_R <- mask(LC_ZONA, POLIGONO_REGION)
  
  #--Cargamos la tabla CN
  
  T_CNCL <- read.csv("./CNCHILE_ARC_II_I_III.csv", sep = ";", 
                     stringsAsFactors = F, check.names=FALSE)
  
  #--Creamos la capa de valores CN a partir de la LCCHILE
  
  CN_ARC <- raster(LC_ZONA_R)
  
  for (k in 2:9) {
    
    #--Creamos una capa ráster para la cobertura de suelo k
    LC_k <-  LC_ZONA_R
    LC_k [HYSOG_ZONA30m_R!=as.integer(colnames(T_CNCL)[k])] <- 0
    
    #--Reemplazamos los valores de la cobertura de suelo k 
    #--según el valor correspondiente de la TABLA CN a emplear
    LC_k <- subs(x = LC_k, y = T_CNCL, by = 1, which = colnames(T_CNCL)[k])
    
    #--Almacenamos los valores CN en el raster creado anteriormente,
    #--condicionado a que corresponda los HSG con la tabla CN
    CN_ARCII[HYSOG_ZONA30m_R==as.integer(colnames(T_CNCL)[k])] <- 
      LC_k[HYSOG_ZONA30m_R==as.integer(colnames(T_CNCL)[k])]
    
  }
  #--Escribimos raster CN para la CAH correspondiente
  writeRaster(CN_ARCII,"./PRODUCTO\\CN\\OBTENIDO.tif" , format = "GTiff", 
              overwrite=TRUE, dataType = "INT1U", options="COMPRESS=LZW")
  
  
}
