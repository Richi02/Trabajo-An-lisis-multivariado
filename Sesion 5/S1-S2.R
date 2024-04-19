
//Analisis Exploratorio de Datos S1//

  //Cargar datos//



Municipalities = read_delim( "C:/Users/USUARIO/Documents/Analisis Multivariado/Municipalities.csv", ",",
escape_double = FALSE, trim_ws=TRUE)


head(Database,8)


  //Filas = 28155 y Columnas= 8 //
    dim(Database) 


  //Estructura/Características de Datos //
    str(Database)


  //Nombres de Variables//
    names(Database)


  //Datos Descriptivos//
    summary(Database)


  //Cambiar nombre//
  trabajadores_nombre = c('Filas', 'Salario', 'Educación', 'Experiencia', 'Etnia', 'Y/N', 'Región', 'TiempoP')

cam_nombre = function(dataframe) {
  for (i in 1:length(trabajadores_nombre)) {
    names(dataframe)[names(dataframe) == names(dataframe)[i]] = trabajadores_nombre[i]
  }
  return(dataframe)
}

Database = cam_nombre(Database)


    head(Database, 8)
    names(Database)
    
    
/////////////////////////////////////////////////////////////////77
    
    //Correlaciones S2//
      
    wage=c(354.94,123.46,370.37,754.94,593.54,377.23,284.90,561.13,264.06,1643.03)
    
    education =c(7,12,9,11,12,16,8,12,12,14)
    
    pairs(education~wage)
    
    
    df = data.frame(wage,education)
    chart.Correlation(df)
    
    //Correlaciones//
      # Ejecutar matriz de correlaciones
      
       
    corrplot(M, method = "ellipse")
    corrplot(M, method = "circle")
    corrplot(M, method = "square")
    corrplot(M, method = "number")
    corrplot(M, method = "shade")
    corrplot(M, method = "color")
    corrplot(M, method = "pie")
    
    