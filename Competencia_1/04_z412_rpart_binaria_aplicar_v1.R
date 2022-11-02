rm( list=ls() )  #remove all objects
gc()

#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]
   
#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#----------------------------------------
#Feature Engineering 1
setwd( "C:\\Users\\leandro.morinigo\\OneDrive\\!4.DM_EconyFin" )
dataset  <- fread("./datasets/competencia1_2022.csv")

#1) con data.table:
dataset[  , g1 :=  as.integer(active_quarter<0.5&cdescubierto_preacordado<0.5) ]
dataset[  , g2 :=  as.integer(active_quarter<0.5&cdescubierto_preacordado>=0.5&ccomisiones_mantenimiento>=0.5&Master_fechaalta>=1006) ]
dataset[  , g3 :=  as.integer(active_quarter>=0.5&mcaja_ahorro<1361&mtarjeta_visa_consumo<2709&mprestamos_personales<15510) ]
dataset[  , g4 :=  as.integer(active_quarter<0.5&cdescubierto_preacordado>=0.5&ccomisiones_mantenimiento>=0.5&Master_fechaalta<1006) ]

dataset[  , g5 :=  as.integer(active_quarter<0.5&cdescubierto_preacordado>=0.5&ccomisiones_mantenimiento<0.5&Master_fechaalta>=128) ]
dataset[  , g6 :=  as.integer(active_quarter>=0.5&mcaja_ahorro<1361&mtarjeta_visa_consumo>=2709&Visa_msaldopesos<9290) ]
dataset[  , g7 :=  as.integer(active_quarter>=0.5&mcaja_ahorro>=1361&mtarjeta_visa_consumo<2001&cpayroll_trx<0.5) ]
dataset[  , g8 :=  as.integer(active_quarter>=0.5&mcaja_ahorro<1361&mtarjeta_visa_consumo<2709&mprestamos_personales>=15510) ]

dataset[  , g9 :=  as.integer(active_quarter>=0.5&mcaja_ahorro<1361&mtarjeta_visa_consumo>=2709&Visa_msaldopesos>=9290) ]
dataset[  , g10 :=  as.integer(active_quarter<0.5&cdescubierto_preacordado>=0.5&ccomisiones_mantenimiento<0.5&Master_fechaalta<128) ]
dataset[  , g11 :=  as.integer(active_quarter>=0.5&mcaja_ahorro>=1361&mtarjeta_visa_consumo>=2001&mpayroll<7043) ]
dataset[  , g12 :=  as.integer(active_quarter>=0.5&mcaja_ahorro>=1361&mtarjeta_visa_consumo<2001&cpayroll_trx>=0.5) ]
dataset[  , g13 :=  as.integer(active_quarter>=0.5&mcaja_ahorro>=1361&mtarjeta_visa_consumo>=2001&mpayroll>=7043) ]

#Exporto el nuevo archivo con FE
setwd( "C:\\Users\\leandro.morinigo\\OneDrive\\!4.DM_EconyFin\\datasets" )
fwrite( dataset, #solo los campos para Kaggle
        file= "./competencia1_2022_fe1.csv",
        sep=  "," )
#----------------------------------------

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\leandro.morinigo\\OneDrive\\!4.DM_EconyFin")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022_fe1.csv" )


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.75,#  -0.89
                 minsplit=  1500,   # 621
                 minbucket=  300,   # 309
                 maxdepth=    22 )  #  12


#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
# dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
# dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
# dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
# dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
# dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
# dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
# dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
# dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
# dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
# dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
# dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
# dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(102191)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


#dir.create( "./exp/" )
#dir.create( "./exp/KA4120" )


for( corte  in  c( 7750, 8000, 8250) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/KA4120/KA4120_005_",  corte, ".csv"),
           sep=  "," )
}

#Modelo final
summary(modelo)

#Feature_importance:
FI <- sort(modelo$variable.importance,decreasing = TRUE)

head(FI)