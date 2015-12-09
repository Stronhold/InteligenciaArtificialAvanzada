setwd('C:\\Users\\Sergio\\Documents\\github\\InteligenciaArtificialAvanzada\\RazonamientoProbalistico')

Ejemplo_NaiveBayes=function(){
# The dataset contains cases from a study that was conducted 
# between 1958 and 1970 at the University of Chicago's Billings Hospital 
# on the survival of patients who had undergone surgery for breast cancer.
# 1. Age of patient at time of operation (numerical) 
# 2. Patient's year of operation (year - 1900, numerical) 
# 3. Number of positive axillary nodes detected (numerical) 
# 4. Survival status (class attribute) 
# -- 0 = the patient survived 5 years or longer 
# -- 1 = the patient died within 5 year

  datos = read.table('haberman.txt',header = TRUE, sep = ",");
  
  N = dim(datos)[1];
  datos_entrenamiento = datos[1:round(0.9*N),];
  datos_test          = datos[(round(0.9*N)+1):nrow(datos),];
  aciertos = 0;
  
  Ntest = dim(datos_test)[1];
  cols = dim(datos_test)[2];
  
  for (dato in 1:Ntest){
    dato_prueba = datos_test[dato,];
    print(paste0('Dato de prueba ',dato,': '));
    print(dato_prueba);
    
    Estimaciones = unique(datos[,ncol(datos)]); 
    Probabilidades = matrix(1,length(unique(datos[,ncol(datos)])));

      for (est in 1:length(Estimaciones)){
          tabla = datos_entrenamiento[(datos_entrenamiento[,ncol(datos_entrenamiento)]==Estimaciones[est]),];
          Probabilidades[est] = dim(tabla)[1]/dim(datos_entrenamiento)[1];
          #print("Probabilidades: ")
          #print(Probabilidades[est]);
          #print("------------------------");
          j = dim (datos_entrenamiento)[2] - 1;
          for( col in 1:j){
           # print("Columnas: ");
          #  print(col);
            total = 0;
            for(fila in 1: nrow(tabla)){
              if(tabla[fila, col] == dato_prueba[col]){
                total = total + 1
              }
            }
            prob = total/dim(tabla)[1];
            Probabilidades[est] = Probabilidades[est]*prob;
           # print("***********************");
          }
          #  Calcular Probabilidades(est) <-------****
          #  Probabilidades(est) = P(ultimacolumna=Estimaciones(est))
          #  Para cada columna <i> (menos la última)
          #      Probabilidades(est)*= P(columna_i=dato_prueba(i)|ultimacolumna=Estimaciones(est))
      }
      
    for (est in 1:length(Estimaciones)){
      print(paste0('Estimación de ',Estimaciones[est],': '));
      print(paste0(Probabilidades[est],'    ',Probabilidades[est]/sum(Probabilidades)));          
      if (Probabilidades[est]==max(Probabilidades)){
        estimacion_ganadora = Estimaciones[est];
      }
    }
    print(paste0('Estimación Obtenida/Esperada: ',estimacion_ganadora,'     ',dato_prueba[ncol(dato_prueba)]));
    if (estimacion_ganadora == dato_prueba[ncol(dato_prueba)]){
      aciertos = aciertos+1;
    }
           
  }
  print(paste0('Porcentage de Aciertos: ',aciertos/nrow(datos_test)))
}

