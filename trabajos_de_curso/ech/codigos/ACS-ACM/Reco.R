
datos=read.table('ech3.txt', header=T)
datos
str(datos)
summary(datos)

> str(datos)
'data.frame':	946 obs. of  9 variables:
 $ id      : int  1 2 3 4 5 6 7 8 9 10 ...
 $ SEXO    : Factor w/ 2 levels "femenino","masculino": 2 2 2 1 2 2 1 1 2 2 ...
 $ EDAD    : int  24 21 29 21 23 29 27 24 22 23 ...
 $ ESTCIVIL: Factor w/ 4 levels "casado","divoviu",..: 3 3 1 3 3 3 1 1 3 3 ...
 $ NIVEL   : Factor w/ 4 levels "NOEducacion",..: 3 3 3 3 2 2 2 2 3 3 ...
 $ ATENCMED: Factor w/ 4 levels "MSP","Mutualista ",..: 2 2 2 2 3 4 4 1 1 2 ...
 $ POBPCOAC: Factor w/ 3 levels "Desocupado ",..: 2 2 2 3 2 2 2 2 1 2 ...
 $ CATEGOR : Factor w/ 4 levels "EmpPrivado","EmpPublico",..: 2 2 2 3 4 1 1 2 2 2 ...
 $ reedad  : Factor w/ 2 levels "25 o mas","menor de 25": 2 2 1 2 2 1 1 2 2 2 ...



> summary(datos)
       id               SEXO          EDAD          ESTCIVIL  
 Min.   :  1.0   femenino :449   Min.   :20.00   casado :238  
 1st Qu.:237.2   masculino:497   1st Qu.:22.00   divoviu: 25  
 Median :473.5                   Median :24.00   soltero:620  
 Mean   :473.5                   Mean   :24.18   union  : 63  
 3rd Qu.:709.8                   3rd Qu.:26.00                
 Max.   :946.0                   Max.   :29.00                
         NIVEL                  ATENCMED          POBPCOAC         CATEGOR   
 NOEducacion:  3   MSP              :104   Desocupado :105   EmpPrivado:105  
 Primaria   :125   Mutualista       :683   Ocupado    :812   EmpPublico:693  
 Secundaria :593   NoCoberturaSalud : 87   seguroBTPV : 29   OtrosCoop : 44  
 Terciaria  :225   OtroSalud        : 72                     PatronTCP :104  
                                                                             
                                                                             
         reedad   
 25 o mas   :424  
 menor de 25:522 



#Aca recodificamos. Para eso se puede exportar para leerlo con la planilla electronica
# el archivo ech3.csv lo leemos y recodificamos
write.csv2(datos,"ech3.csv")

#Luego de hacer los cambios lo volvemos a leer en el R con 

datos.rec=read.csv("ech3.csv",sep=";")
# aca vemos que la estructura es la misma en ambos dataframes

>str(datos.rec)

'data.frame':	946 obs. of  9 variables:
 $ id      : int  1 2 3 4 5 6 7 8 9 10 ...
 $ SEXO    : Factor w/ 2 levels "feme","masc": 2 2 2 1 2 2 1 1 2 2 ...
 $ EDAD    : int  24 21 29 21 23 29 27 24 22 23 ...
 $ ESTCIVIL: Factor w/ 4 levels "casa","divi",..: 3 3 1 3 3 3 1 1 3 3 ...
 $ NIVEL   : Factor w/ 4 levels "NOed","prim",..: 3 3 3 3 2 2 2 2 3 3 ...
 $ ATENCMED: Factor w/ 4 levels "MSP","Mutu ",..: 2 2 2 2 3 4 4 1 1 2 ...
 $ POBPCOAC: Factor w/ 3 levels "Deso ","Ocup ",..: 2 2 2 3 2 2 2 2 1 2 ...
 $ CATEGOR : Factor w/ 4 levels "Coop","EPriv",..: 3 3 3 1 4 2 2 3 3 3 ...
 $ reedad  : Factor w/ 2 levels "25mas","men25": 2 2 1 2 2 1 1 2 2 2 ...


> summary(datos.rec)
       id          SEXO          EDAD       ESTCIVIL    NIVEL      ATENCMED  
 Min.   :  1.0   feme:449   Min.   :20.00   casa:238   NOed:  3   MSP  :104  
 1st Qu.:237.2   masc:497   1st Qu.:22.00   divi: 25   prim:125   Mutu :683  
 Median :473.5              Median :24.00   solt:620   secu:593   Nosa : 87  
 Mean   :473.5              Mean   :24.18   unio: 63   terc:225   Otsa : 72  
 3rd Qu.:709.8              3rd Qu.:26.00                                    
 Max.   :946.0              Max.   :29.00                                    
  POBPCOAC    CATEGOR      reedad   
 Deso :105   Coop : 44   25mas:424  
 Ocup :812   EPriv:105   men25:522  
 Segu : 29   EPub :693              
             PTCP :104  
