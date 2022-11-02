#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

#------------------------------------------------------------------------------
#Carga de Datos:
#setwd("~/buckets/b1")
setwd("C:\\Users\\leandro.morinigo\\OneDrive\\!4.DM_EconyFin") 

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv")

#----------------------SELECT COLUMNS--------------------------------------------------------
#Lista de variables a rankear:
cols_to_fix <- c("mcuentas_saldo", #Mucho, casi insalvable --> pero 01 y 05 se parecen
                  "mcuenta_corriente",# 05 es UNIFORME y 01: lineal creciente
                  "mcaja_ahorro", #NO hay tanta diferencia, --> 02 y 05 se parecen
                  "mprestamos_personales", #similares, muy porquito --> 01 y 02: se parecen mas a 05
                  "mrentabilidad_annual",
                  "mcomisiones", #simil c/ desplazamiento: hay una distancia % casi constante de diferencia
                  "ccomisiones_otras", #muy muy poco, dif solo en picos
                  "mpasivos_margen", #simil mcomisiones --> desplazamiento %
                  "mcomisiones_otras", #idem mcomisiones
                  "mpayroll", #solo dezplamiento
                  "mtarjeta_visa_consumo", #desplazamiento % + algun corrimiento en picos
                  "Visa_msaldototal", #desplazamiento minimo
                  "Visa_msaldopesos", #idem Visa_msaldototal
                  "Master_mfinanciacion_limite",
                  "mcuenta_debitos_automaticos",
                  "mtransferencias_recibidas",
                  "mttarjeta_visa_debitos_automaticos",
                  "Visa_mconsumosdolares",
                  "Visa_mfinanciacion_limite",
                  "Master_mpagospesos",
                  "mpagomiscuentas",
                  "Master_mpagominimo",
                  "Visa_fultimo_cierre",
                  "Master_fultimo_cierre",
                  "Visa_mpagado",
                  "Visa_msaldodolares",
                  "cpagomiscuentas",
                  "Visa_mpagosdolares",
                  "Master_mconsumosdolares",
                  "Master_mpagado",
                  "Visa_madelantopesos",
                  "mcuenta_corriente_adicional",
                  "mcaja_ahorro_adicional",
                  "ccajas_otras", #muy distinto 1er mitad --> 04 = 05 y 02 = 03
                  "Visa_mpagominimo", #no mucho: desplazamiento + forma
                  "mtransferencias_emitidas", #solo difiere pendiente
                  "chomebanking_transacciones", #desplazamiento y picos
                  "mcaja_ahorro_dolares", #desplazamiento % --> efecto inflación clarisimo --> sube un poquito mes a mes
                  "Visa_mconsumospesos", #desplazamiento % --> efecto inflación --> sube un poquito mes a mes (menos 04 que disminuye)
                  "Visa_mconsumototal", #idem "Visa_mconsumospesos"--> efecto inflación --> sube un poquito mes a mes (menos 04 que disminuye
                  "ccajas_depositos", #es poco
                  "mcheques_emitidos_rechazados", #dos rectas UNIFORMES, desplazada
                  "Master_msaldototal" #poco
                  )

#----------------------SOLUCION 1: GENERO COLS RANKEADAS c/ FRANK()--------------------------------------------------------
for (column in cols_to_fix) {
  p1 <- frank(abs( dataset[ foto_mes==202103 & get(column) < 0, get(column) ] ))
  p2 <- dataset[ foto_mes==202103 & get(column) == 0, get(column) ]
  p3 <- frank(dataset[ foto_mes==202103 & get(column) > 0, get(column)])
  n1 <- length(p1) 
  n3 <- length(p3)
  dataset[ foto_mes==202103 & get(column) <  0, paste('rank_',column, sep="") :=  -p1/n1]
  dataset[ foto_mes==202103 & get(column) == 0, paste('rank_',column, sep="") :=  p2]
  dataset[ foto_mes==202103 & get(column) >  0, paste('rank_',column, sep="") :=  p3/n3]
  
  p1 <- frank(abs( dataset[ foto_mes==202105 & get(column) < 0, get(column) ] ))
  p2 <- dataset[ foto_mes==202105 & get(column) == 0, get(column) ]
  p3 <- frank(dataset[ foto_mes==202105 & get(column)> 0, get(column) ])
  n1 <- length(p1) 
  n3 <- length(p3)
  dataset[ foto_mes==202105 & get(column) <  0, paste('rank_',column, sep="") :=  -p1/n1]
  dataset[ foto_mes==202105 & get(column) == 0, paste('rank_',column, sep="") :=  p2]
  dataset[ foto_mes==202105 & get(column) >  0, paste('rank_',column, sep="") :=  p3/n3]
}

#-------------BORROlas columnas ORIGINALES-------------------------------------------

#BORRO LAS COLUMNAS QUE NO USO: trabajo SOLO con la parte del dataset que me interesa
#col_select <- setdiff( colnames(dataset), cols_to_fix) 
#dataset <- dataset[, ..col_select]

#verificacion
dim(dataset) #198 = 155 + 43 -> orig
length(cols_to_fix) #43 -> a borrar
length(col_select) #155 -> netas deben quedas
dim(dataset) #155?? -> verifico en el dataset original



#-------------Exporto el dataset generado-------------------------------------------
setwd("C:\\Users\\leandro.morinigo\\OneDrive\\!4.DM_EconyFin\\datasets") 

fwrite( dataset, #solo los campos para Kaggle
        file= "./01.competencia2_2022_cfix_v3.csv",
        sep=  "," )  
#--------------------------------------------------------







