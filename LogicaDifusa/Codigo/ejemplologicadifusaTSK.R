seleccionadorbaloncestoTSK=function(altura,acierto){

  bajo  = c(0, 0, 150, 165);
  medio = c(150, 165, 170, 190);
  alto  = c(170, 190, 300, 300);
  
  malo    = c(0, 0, 40, 55);
  regular = c(40, 55, 65, 90);
  bueno   = c(65, 90, 110, 110);
  
  seleccionar          = 100;
  seleccionarcodudas   = 75;
  dudoso               = 50;
  masbiennoseleccionar = 25;
  noseleccionar        = 0;
  
  mu_bajo  = calcula_Mu(bajo,altura);
  mu_medio = calcula_Mu(medio,altura);
  mu_alto  = calcula_Mu(alto,altura);
  
  mu_malo    = calcula_Mu(malo,acierto);
  mu_regular = calcula_Mu(regular,acierto);
  mu_bueno   = calcula_Mu(bueno,acierto);
  
  basereglas = matrix(c(Y(mu_bajo,mu_malo),noseleccionar,
                        Y(mu_bajo,mu_regular),masbiennoseleccionar,
                        Y(mu_bajo,mu_bueno),dudoso,
                        Y(mu_medio,mu_malo),masbiennoseleccionar,
                        Y(mu_medio,mu_regular),dudoso,
                        Y(mu_medio,mu_bueno),seleccionarcodudas,
                        Y(mu_alto,mu_malo),dudoso,
                        Y(mu_alto,mu_regular),seleccionarcodudas,
                        Y(mu_alto,mu_bueno),seleccionar),nrow=9,ncol=2,byrow=TRUE)
 
  
  antecedentes = basereglas[,1];
  consecuentes = basereglas[,2];
  
  salida = sum(antecedentes*consecuentes)/sum(antecedentes);
  print(paste0('Jugador con Altura=',altura,' y Acierto=',acierto,' -> Seleccionar=',salida))
}

Y = function(valor1,valor2){
  salida = min(valor1,valor2);
}

calcula_Mu = function(difuso,valor){
  a = difuso[1];
  b = difuso[2];
  c = difuso[3];
  d = difuso[4];
  
  if      (valor<a){
    salida=0;
  }
  else{
    if  (valor<b){
      salida = (valor-a)/(b-a);
    }
    else{ 
      if  (valor<c){
        salida = 1;
      }
      else{
        if  (valor<d){
          salida = (d-valor)/(d-c);
        }
        else{
          salida = 0;
        }
      }
    }
  }
}