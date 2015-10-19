seleccionadorTrabajo=function(sueldo,distancia, dietas){

  sueldoPrecario = c(0, 0, 700, 900);
  sueldoBajo  = c(700, 900, 1300, 1500);
  sueldoMedio = c(1300, 1500, 2000, 2200);
  sueldoAlto  = c(2000, 2200, 3000, 3500);
  sueldoEjecutivo = c(3000, 3500, 6000, 6000);

  distanciaCerca    = c(0, 0, 10, 15);
  distanciaMedia = c(10, 15, 25, 30);
  distanciaLejos   = c(25, 30, 50, 50);
  
  dietasInexistentes = c(0, 0, 5, 7);
  dietasCasiInexistentes = c (5, 7, 8, 10);
  dietasMedias = c(8,10, 13, 15);
  dietasElevadas = c(13, 15, 20, 25);
  
  seleccionar          = 100;
  seleccionarCasiSeguro = 90;
  seleccionarcodudas   = 75;
  dudoso               = 50;
  masbiennoseleccionar = 25;
  noseleccionar        = 0;
  
  mu_sueldoPrecario = calcula_Mu(sueldoPrecario, sueldo);
  mu_sueldoBajo  = calcula_Mu(sueldoBajo,sueldo);
  mu_sueldoMedio = calcula_Mu(sueldoMedio,sueldo);
  mu_sueldoAlto  = calcula_Mu(sueldoAlto,sueldo);
  mu_sueldoEjecutivo = calcula_Mu(sueldoEjecutivo, sueldo);
  
  mu_distanciaCerca = calcula_Mu(distanciaCerca,distancia);
  mu_distanciaMedia = calcula_Mu(distanciaMedia,distancia);
  mu_distanciaLejos = calcula_Mu(distanciaLejos,distancia);
  
  mu_dietasInexistentes = calcula_Mu(dietasInexistentes, dietas);
  mu_dietasCasiInexistentes = calcula_Mu(dietasCasiInexistentes, dietas);
  mu_dietasMedias = calcula_Mu(dietasMedias, dietas);
  mu_dietasElevadas = calcula_Mu(dietasElevadas, dietas);
  
  basereglas = matrix(c(Y(mu_sueldoPrecario, 1, 1),noseleccionar,
                        Y(mu_sueldoBajo, mu_distanciaLejos, 1), noseleccionar,
                        Y(mu_sueldoBajo, mu_distanciaMedia, 1), masbiennoseleccionar,
                        Y(mu_sueldoBajo, mu_distanciaCerca, 1), dudoso,
                        Y(mu_sueldoMedio, mu_distanciaLejos, mu_dietasInexistentes), masbiennoseleccionar,
                        Y(mu_sueldoMedio, mu_distanciaLejos, mu_dietasCasiInexistentes), masbiennoseleccionar,
                        Y(mu_sueldoMedio, mu_distanciaLejos, mu_dietasMedias), dudoso,
                        Y(mu_sueldoMedio, mu_distanciaLejos, mu_dietasElevadas), seleccionarcodudas,
                        Y(mu_sueldoMedio, mu_distanciaMedia, 1), dudoso,
                        Y(mu_sueldoMedio, mu_distanciaCerca, 1), seleccionarcodudas,
                        Y(mu_sueldoAlto, mu_distanciaLejos, 1), dudoso,
                        Y(mu_sueldoAlto, mu_distanciaMedia, 1), seleccionarcodudas,
                        Y(mu_sueldoAlto, mu_distanciaCerca, 1), seleccionarCasiSeguro,
                        Y(mu_sueldoEjecutivo, mu_distanciaLejos, 1), seleccionarcodudas,
                        Y(mu_sueldoEjecutivo, mu_distanciaMedia, 1), seleccionarCasiSeguro,
                        Y(mu_sueldoEjecutivo, mu_distanciaCerca, 1), seleccionar

                        ),nrow=16,ncol=2,byrow=TRUE);
 
  
  antecedentes = basereglas[,1];
  consecuentes = basereglas[,2];
  
  salida = sum(antecedentes*consecuentes)/sum(antecedentes);
  print(paste0('Trabajo con sueldo=',sueldo,'???, distancia=',distancia,'km y euros en dietas=', dietas, ' -> Seleccionar=',salida, '%'))
}

Y = function(valor1,valor2, valor3){
  primerValor = min(valor1,valor2);
  salida = min(primerValor, valor3);
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