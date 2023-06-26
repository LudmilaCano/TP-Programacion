Algoritmo RESERVATUCANCHA
	Definir canchas, horariosCancha11, horariosCancha7, horariosCancha5C, horariosCancha5S, reservas Como Caracter
	Definir cantReservas como entero 
	Definir total Como Real
	cantReservas <- 0
	Dimension canchas[4,5]
	Dimension horariosCancha11[6,4]
	Dimension horariosCancha7[6,5]
	Dimension horariosCancha5C[6,8]
	Dimension horariosCancha5S[6,8]
	Dimension reservas[100,6]
	
	//CARGA DE DATOS
	cargaDeCanchas(canchas)
	cargarCancha11(horariosCancha11)
	cargarCancha7(horariosCancha7)
	cargarCancha5Cemento(horariosCancha5C)
	cargarCancha5Sintetico(horariosCancha5S)
	
	Escribir "¡BIENVENIDO!"
	
	//BUCLE PARA EL MENU
	Repetir
		menu()
		Leer optMenu
		
		Segun optMenu Hacer
			1:
				reservarCancha(horariosCancha11, horariosCancha5C, horariosCancha5S, horariosCancha7, reservas)
				cantReservas <- cantReservas + 1
			2:
				//
			3: 
				//
			0:
				Escribir "Hasta luego!"
			De Otro Modo:
				Escribir "Ingrese una opción de menú válida";
		Fin Segun
		
	Hasta Que optMenu == 0 
FinAlgoritmo


//MENU
SubProceso menu()
	Escribir "1. Reservar cancha"
	//Escribir "2. Ver disponibilidad segun cantidad jugadores o tipo de cancha?";
	//Escribir "3. Mostrar canchas ordenas por precio ascendente?";
	//Escribir "4. ";
	Escribir "0. Salir!!";
FinSubProceso


// ////// RESERVAR CANCHAS ///////////////////////////////////////
Subproceso reservarCancha(horariosCancha11, horariosCancha5C, horariosCancha5S, horariosCancha7, reservas)
	Definir cantJugadores, volver, filaAmostrar, horarioEncontrado como entero
	Definir diaElegido, tipoDeCancha, nombreCliente, horarioSeleccionado como caracter
	volver <- 1
	
	Repetir
		Escribir "ingrese su nombre"
		Leer nombreCliente
	Hasta Que Longitud(nombreCliente) > 3
	
	Repetir
		Escribir "Ingrese el día de la semana"
		leer diaElegido
		diaElegido <- Minusculas(diaElegido)
	Hasta Que validarDia(diaElegido) == Verdadero
	
	Repetir
		Escribir "Las cantidades de jugadores permitidas son: 10, 14 y 22"
		Escribir "Ingrese la cantidad de jugadores: "
		Leer cantJugadores 	
	Hasta Que cantJugadores == 10 o cantJugadores == 14 o cantJugadores == 22
	
	Segun cantJugadores
		caso 10:
			Escribir "Los tipos de cancha para 10 jugadores son: cemento y sintetico"
			Escribir "Desea continuar?"
			Escribir "0 - Volver al menu principal"
			Escribir "1 - Continuar con la reserva"
			leer volver

			Si volver == 1 Entonces
				Repetir
					Escribir "Ingrese el tipo de cancha: "
					Leer tipoDeCancha
					tipoDeCancha <- Minusculas(tipoDeCancha)
				Hasta Que tipoDeCancha == "cemento" o tipoDeCancha == "sintetico"
			Fin Si

			Si tipoDeCancha == "cemento" Entonces
				filaAmostrar <- busquedaIndiceDia(horariosCancha5C, 6, diaElegido, 0) // conseguir el indice segun el dia elegido
				mostrarFila(horariosCancha5C, 8, filaAmostrar) // mostrar el horario con el indice encontrado anteriormente
				Escribir "ingrese el horario deseado: "
				Leer horarioSeleccionado
				reservas[cantReservas,4] <- horarioSeleccionado
				horarioEncontrado <- busquedaSecuencial3(horariosCancha5C, 8, horarioSeleccionado, filaAmostrar)
				Escribir horarioEncontrado
				horariosCancha5C[filaAmostrar, horarioEncontrado] <- "Reservado"
			SiNo
				filaAmostrar <- busquedaIndiceDia(horariosCancha5S, 6,diaElegido, 0) // Conseguir indice segun el dia elegido
				mostrarFila(horariosCancha5S, 8, filaAmostrar) // mostrar los horarios con el indice conseguido anteriormente
				Escribir "ingrese el horario deseado: "
				Leer horarioSeleccionado
				reservas[cantReservas,4] <- horarioSeleccionado
				horarioEncontrado <- busquedaSecuencial3(horariosCancha5S, 8, horarioSeleccionado, filaAmostrar)
				Escribir horarioEncontrado
				horariosCancha5S[filaAmostrar, horarioEncontrado] <- "Reservado"
			Fin Si
			
		caso 14:
			Escribir "El tipo de cancha disponibles es: Cesped sintetico"
			Escribir "Desea continuar?"
			Escribir "0 - Volver al menu principal"
			Escribir "1 - Continuar con la reserva"
			leer volver

			Si volver == 1 Entonces
				tipoDeCancha <- "sintetico"
			Fin Si

			filaAmostrar <- busquedaIndiceDia(horariosCancha7, 6,diaElegido, 0) // Conseguir indice segun el dia elegido
			mostrarFila(horariosCancha7, 5, filaAmostrar) // mostrar los horarios con el indice conseguido anteriormente
			Escribir "ingrese el horario deseado: "
			Leer horarioSeleccionado
			reservas[cantReservas,4] <- horarioSeleccionado
			horarioEncontrado <- busquedaSecuencial3(horariosCancha7, 5, horarioSeleccionado, filaAmostrar)
			Escribir horarioEncontrado
			horariosCancha7[filaAmostrar, horarioEncontrado] <- "Reservado"

		caso 22:
			Escribir "El tipo de cancha/s disponible/s son: Cesped Natural"
			Escribir "Desea continuar?"
			Escribir "0 - Volver al menu principal"
			Escribir "1 - Continuar con la reserva"
			leer volver

			Si volver == 1 Entonces
				tipoDeCancha <- "Cesped Natural"
			Fin Si	

			filaAmostrar <- busquedaIndiceDia(horariosCancha11, 6,diaElegido, 0) // Conseguir indice segun el dia elegido
			mostrarFila(horariosCancha11, 4, filaAmostrar) // mostrar los horarios con el indice conseguido anteriormente
			Escribir "ingrese el horario deseado: "
			Leer horarioSeleccionado
			reservas[cantReservas,4] <- horarioSeleccionado
			horarioEncontrado <- busquedaSecuencial3(horariosCancha11, 4, horarioSeleccionado, filaAmostrar)
			Escribir horarioEncontrado
			horariosCancha11[filaAmostrar, horarioEncontrado] <- "Reservado"
	FinSegun
	
	Si volver == 1 Entonces // cargo el arreglo de reservas solo si "volver" es 1
		reservas[cantReservas,0] <- nombreCliente
		reservas[cantReservas,1] <- diaElegido
		reservas[cantReservas,2] <- ConvertirATexto(cantJugadores)
		reservas[cantReservas,3] <- tipoDeCancha
		reservas[cantReservas,5] <- total
		
		Escribir "su reserva ha sido guardada con exito"
		Mostrar diaElegido
		Mostrar cantJugadores
		Mostrar tipoDeCancha
		
		cantReservas <- cantReservas + 1
	SiNo
		Escribir "vuelva pronto"
	Fin Si

FinSubProceso


Funcion return<- busquedaSecuencial3(array, n, elementoABuscar, filaAmostrar)
	Definir i Como Entero;
	Definir elementoEncontrado Como Logico;
	i<-0;
	elementoEncontrado <- Falso;
	Mientras i <= n-1 y no elementoEncontrado
		si array[filaAmostrar,i] == elementoABuscar Entonces
			elementoEncontrado <- Verdadero; //fuerzo la salida del bucle
		SiNo
			i <- i +1
		FinSi
	FinMientras
	
	Si elementoEncontrado Entonces
		return <- i;
	SiNo
		return <- -1;
	FinSi
FinFuncion


//VALIDACION DEL DIA ELEGIDO
Funcion return <- validarDia(diaElegido)
	Definir diasValidos como caracter
	Dimension diasValidos[6]
	
	diasValidos[0] <- "lunes"
	diasValidos[1] <- "martes"
	diasValidos[2] <- "miercoles"
	diasValidos[3] <- "jueves"
	diasValidos[4] <- "viernes"
	diasValidos[5] <- "sabado"
	
	return <- busquedaUnidimensional(diasValidos, 6, diaElegido)
FinFuncion


SubProceso mostrarFila(matrizdondeguardodatos, cantColumnas, indiceEncontrado)
	para i<- 0 Hasta cantColumnas-1 Hacer
		Escribir Sin Saltar matrizdondeguardodatos[indiceEncontrado,i], " "
	FinPara
	Escribir ""
FinSubProceso


//BUSQUEDA SECUENCIAL EN ARRAY BIDIMENSIONAL FIJANDO UNA COLUMNA
// RETORNA EL INDICE SI LO ENCUENTRA, EN CASO CONTRARIO RETORNA -1
Funcion return<- busquedaIndiceDia(array,cantFilas,elementoABuscar,columnaABuscar)
	Definir i Como Entero; //es un contador
	Definir elementoEncontrado Como Logico;
	i<-0;
	elementoEncontrado<- Falso;
	
	Mientras (no elementoEncontrado) y i<cantFilas Hacer
		Si array[i,columnaABuscar] == elementoABuscar Entonces
			elementoEncontrado <- Verdadero;
		SiNo
			i <- i +1
		FinSi
	FinMientras
	
	Si elementoEncontrado Entonces
		return <- i;
	FinSi
FinFuncion


//FUNCION DE BUSQUEDA PARA VALIDACIONES
Funcion return<- busquedaUnidimensional(array, n, elementoABuscar)
	Definir i Como Entero;
	Definir elementoEncontrado Como Logico;
	i<-0;
	elementoEncontrado <- Falso;
	Mientras i <= n-1 y no elementoEncontrado
		si array[i] == elementoABuscar Entonces
			elementoEncontrado <- Verdadero; //fuerzo la salida del bucle
		SiNo
			i <- i +1
		FinSi
	FinMientras
	return	<- elementoEncontrado;
FinFuncion


//CARGA DE DATOS
SubProceso cargaDeCanchas(array)
	array[0,0] <- "01" 
	array[0,1] <- "22"
	array[0,2] <- "Cesped Natural"
	array[0,3] <- "22000"
	array[0,4] <- "120"
	array[1,0] <- "02"
	array[1,1] <- "14"
	array[1,2] <- "Sintetico"
	array[1,3] <- "14000"
	array[1,4] <- "90"	
	array[2,0] <- "03"
	array[2,1] <- "10"
	array[2,2] <- "Sintetico"
	array[2,3] <- "10000"
	array[2,4] <- "60"		
	array[3,0] <- "04" 
	array[3,1] <- "10"
	array[3,2] <- "Cemento"
	array[3,3] <- "5000"
	array[3,4] <- "60"		
FinSubProceso


SubProceso cargarCancha7(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16"
	array[0,2] <- "17:30"
	array[0,3] <- "19"
	array[0,4] <- "20:30"
	array[1,0] <- "martes"
	array[1,1] <- "16"
	array[1,2] <- "17:30"
	array[1,3] <- "19"
	array[1,4] <- "20:30"	
	array[2,0] <- "miercoles"
	array[2,1] <- "16"
	array[2,2] <- "17:30"
	array[2,3] <- "19"
	array[2,4] <- "20:30"
	array[3,0] <- "jueves" 
	array[3,1] <- "16"
	array[3,2] <- "17:30"
	array[3,3] <- "19"
	array[3,4] <- "20:30"
	array[4,0] <- "viernes"		
	array[4,1] <- "16"
	array[4,2] <- "17:30"
	array[4,3] <- "19"
	array[4,4] <- "20:30"
	array[5,0] <- "sabado"
	array[5,1] <- "16"
	array[5,2] <- "17:30"
	array[5,3] <- "19"
	array[5,4] <- "20:30"
FinSubProceso


SubProceso cargarCancha11(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16"
	array[0,2] <- "18"
	array[0,3] <- "20"
	array[1,0] <- "martes"
	array[1,1] <- "16"
	array[1,2] <- "18"
	array[1,3] <- "20"
	array[2,0] <- "miercoles"
	array[2,1] <- "16"
	array[2,2] <- "18"
	array[2,3] <- "20"
	array[3,0] <- "jueves" 
	array[3,1] <- "16"
	array[3,2] <- "18"
	array[3,3] <- "20"
	array[4,0] <- "viernes"		
	array[4,1] <- "16"
	array[4,2] <- "18"
	array[4,3] <- "20"
	array[5,0] <- "sabado"
	array[5,1] <- "16"
	array[5,2] <- "18"
	array[5,3] <- "20"
FinSubProceso


SubProceso cargarCancha5Cemento(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16"
	array[0,2] <- "17"
	array[0,3] <- "18"
	array[0,4] <- "19"
	array[0,5] <- "20"
	array[0,6] <- "21"
	array[0,7] <- "22"
	array[1,0] <- "martes"
	array[1,1] <- "16"
	array[1,2] <- "17"
	array[1,3] <- "18"
	array[1,4] <- "19"	
	array[1,5] <- "20"
	array[1,6] <- "21"
	array[1,7] <- "22"
	array[2,0] <- "miercoles"
	array[2,1] <- "16"
	array[2,2] <- "17"
	array[2,3] <- "18"
	array[2,4] <- "19"	
	array[2,5] <- "20"
	array[2,6] <- "21"
	array[2,7] <- "22"
	array[3,0] <- "jueves" 
	array[3,1] <- "16"
	array[3,2] <- "17"
	array[3,3] <- "18"
	array[3,4] <- "19"	
	array[3,5] <- "20"
	array[3,6] <- "21"
	array[3,7] <- "22"
	array[4,0] <- "viernes"		
	array[4,1] <- "16"
	array[4,2] <- "17"
	array[4,3] <- "18"
	array[4,4] <- "19"	
	array[4,5] <- "20"
	array[4,6] <- "21"
	array[4,7] <- "22"
	array[5,0] <- "sabado"
	array[5,1] <- "16"
	array[5,2] <- "17"
	array[5,3] <- "18"
	array[5,4] <- "19"	
	array[5,5] <- "20"
	array[5,6] <- "21"
	array[5,7] <- "22"
FinSubProceso


SubProceso cargarCancha5Sintetico(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16"
	array[0,2] <- "17"
	array[0,3] <- "18"
	array[0,4] <- "19"
	array[0,5] <- "20"
	array[0,6] <- "21"
	array[0,7] <- "22"
	array[1,0] <- "martes"
	array[1,1] <- "16"
	array[1,2] <- "17"
	array[1,3] <- "18"
	array[1,4] <- "19"	
	array[1,5] <- "20"
	array[1,6] <- "21"
	array[1,7] <- "22"
	array[2,0] <- "miercoles"
	array[2,1] <- "16"
	array[2,2] <- "17"
	array[2,3] <- "18"
	array[2,4] <- "19"	
	array[2,5] <- "20"
	array[2,6] <- "21"
	array[2,7] <- "22"
	array[3,0] <- "jueves" 
	array[3,1] <- "16"
	array[3,2] <- "17"
	array[3,3] <- "18"
	array[3,4] <- "19"	
	array[3,5] <- "20"
	array[3,6] <- "21"
	array[3,7] <- "22"
	array[4,0] <- "viernes"		
	array[4,1] <- "16"
	array[4,2] <- "17"
	array[4,3] <- "18"
	array[4,4] <- "19"	
	array[4,5] <- "20"
	array[4,6] <- "21"
	array[4,7] <- "22"
	array[5,0] <- "sabado"
	array[5,1] <- "16"
	array[5,2] <- "17"
	array[5,3] <- "18"
	array[5,4] <- "19"	
	array[5,5] <- "20"
	array[5,6] <- "21"
	array[5,7] <- "22"
FinSubProceso
