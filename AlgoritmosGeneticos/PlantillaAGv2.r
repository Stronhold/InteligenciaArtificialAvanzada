setwd('C:\\Users\\Sergio\\Documents\\github\\InteligenciaArtificialAvanzada\\AlgoritmosGeneticos')


PlantillaAGv2=function(){
  # LEEMOS LAS POSICIONES DE REPARTO, Y CALCULAMOS LAS DISTANCIAS (No Tocar)
  PROBLEMA = 'berlin52.txt';
  SOLUCION = 'berlin52.opt.txt';
  PUNTOS = read.table(PROBLEMA);
  OPTIMO = read.table(SOLUCION);
  N = dim(PUNTOS)[1];
  DISTANCIAS = matrix(0,N,N);
  for (i in 1:N){
    for (j in 1:N){   
      DISTANCIAS[i,j] = sqrt((PUNTOS[i,1]-PUNTOS[j,1])^2 +(PUNTOS[i,2]-PUNTOS[j,2])^2);
    }
  }
  # PARÁMETROS
  N_INDIVIDUOS  = 50;
  L_INDIVIDUO   = N;
  GENERACIONES  = 200;
  PROB_MUTACION = 0.05;
  PROB_CRUCE    = 0.90;
  # INICIALIZACIÓN
  POBLACION = matrix(0,N_INDIVIDUOS,L_INDIVIDUO);
  for (i in 1:N_INDIVIDUOS){
    # Iniciar cada uno de los individuos de la población 
    # como una permutación aleatoria
    for (j in 1:L_INDIVIDUO){
      POBLACION[i,j] = j;
    }
  }
  #Permutacion
  for(i in 1:N_INDIVIDUOS){
    for (j in 1:L_INDIVIDUO){
      random = sample(1:L_INDIVIDUO, 1);
      numeroPermutacionUno = POBLACION[i, random];
      POBLACION[i,random] = POBLACION[i,j];
      POBLACION[i, j] = numeroPermutacionUno
    }
  }
  print(POBLACION);
  
  for (g in 1:GENERACIONES){
    # EVALUACIÓN (No Tocar)
    FITNESS = Evaluar(POBLACION,DISTANCIAS);      
    print(FITNESS);
    # SELECCIÓN 
    #Seleccionar N_INDIVIDUOS padres por torneo binario
    PADRES = POBLACION;   
    print(FITNESS);
    return;
    for(i in 1:N_INDIVIDUOS){
      numeroCandidatoUno = sample(1:N_INDIVIDUOS, 1);
      numeroCandidatoDos = sample(1:N_INDIVIDUOS, 1);
      fitnessUno = FITNESS[numeroCandidatoUno];
      fitnessDos = FITNESS[numeroCandidatoDos];
      if((fitnessUno < fitnessDos)){
        for (j in 1:L_INDIVIDUO){
        PADRES[i, j]=POBLACION[numeroCandidatoUno, j];
        }
      }
      else{
        for (j in 1:L_INDIVIDUO){
          PADRES[i, j]=POBLACION[numeroCandidatoDos, j];
        }
      }
    }
    print("cruce");
    # CRUCE 
    #Para cada pareja de padres, usar el operador de orden con probabilidad
    # PROB_CRUCE para generar dos hijos
    HIJOS = PADRES;    
    for(i in 1:N_INDIVIDUOS){
      if(runif(1, min=0, max=1) < PROB_CRUCE){
        s = sample(1:N_INDIVIDUOS, 1);
        t = sample(1:N_INDIVIDUOS, 1);
        if(s>t){
          aux=s;
          s=t;
          t=aux;
        }
        t1p1=PADRES[i,1:s-1];
        t2p1=PADRES[i,s:t];
        t3p1=PADRES[i,t+1:length(PADRES[i])];
        t1p2=PADRES[i+1,1:s-1];
        t2p2=PADRES[i+1,s:t];
        t3p2=PADRES[i+1,t+1:length(PADRES[i])];
        HIJO1=t2p1;
        ORDEN= c (t3p2,t1p2,t2p2);
        ORDEN= ORDEN[! ORDEN %in% t2p1];
        HIJO1=c(HIJO1,ORDEN);
        SIZEt1p1= length(t1p1);
        MOVER=HIJO1[length(HIJO1)-(SIZEt1p1-1):length(HIJO1)];
        diferencia = (length(HIJO1) - SIZEt1p1)
        HIJO1=HIJO1[1:diferencia];
        HIJO1=c(MOVER,HIJO1);
        for (j in 1:L_INDIVIDUO){
          HIJOS[i, j]=HIJO1[j];
        }
        
        HIJO2=t2p2;
        ORDEN=c(t3p1,t1p1,t2p1);
        ORDEN= ORDEN[! ORDEN %in% t2p1];
        HIJO2=c(HIJO2,ORDEN);
        SIZEt1p2=length(t1p2);
        MOVER=HIJO2[length(HIJO2)-(SIZEt1p2-1):length(HIJO2)];
        diferencia  = length(HIJO2)-SIZEt1p2;
        HIJO2=HIJO2[1: diferencia];
        HIJO2=c(MOVER,HIJO2);
        for (j in 1:L_INDIVIDUO){
          HIJOS[i, j]=HIJO2[j];
        }
        i = i +1;
      }
    }
    print("Mutacion");
    # MUTACIÓN  
    #Para cada hijo, con probabilidad PROB_MUTACION, intercambiar dos 
    #posiciones elegidas aleatoriamente
    for (i in 1:N_INDIVIDUOS){
      if (runif(1, min=0, max=1)<PROB_MUTACION){
        randomUno = sample(1:L_INDIVIDUO, 1);
        randomDos = 0;
        samePosition = T;
        while(samePosition){
          randomDos = sample(1:L_INDIVIDUO, 1);
          if(randomDos != randomUno){
            samePosition = F;
          }
        }
        numeroPermutacionUno = HIJOS[i, randomUno];
        HIJOS[i,randomUno] = HIJOS[i,randomDos];
        HIJOS[i, randomDos] = numeroPermutacionUno;
      }
    }
    # ACTUALIZAMOS EL MEJOR INDIVIDUO (No Tocar)
    indice = order(FITNESS)[1];
    MEJOR = POBLACION[indice,];
    fitness_mejor = FITNESS[indice];
    
    # MOSTRAMOS EL MEJOR INDIVIDUO HASTA EL MOMENTO (No Tocar)
 #   print(paste0('Mejor Individuo Generación: ',g))
  #  print(MEJOR)
   # print(paste0('Fitness del Mejor Individuo: ',fitness_mejor))
    
    # REEMPLAZAMIENTO (No Tocar)
    POBLACION = HIJOS;
    
    # ELITISMO 
    POBLACION[N_INDIVIDUOS,]=MEJOR; 
  }
  
}

Evaluar=function(POBLACION,DISTANCIAS){
  FITNESS=matrix(0,dim(POBLACION)[1],1);
  maxj = (dim(POBLACION)[2]-1);
  for (i in 1:dim(POBLACION)[1]){
    for (j in 1:maxj){
      FITNESS[i]=FITNESS[i]+DISTANCIAS[POBLACION[i,j],POBLACION[i,j+1]];
    }
    FITNESS[i]=FITNESS[i]+DISTANCIAS[POBLACION[i,dim(POBLACION)[2]],POBLACION[i,1]];
  }
  FITNESS
}