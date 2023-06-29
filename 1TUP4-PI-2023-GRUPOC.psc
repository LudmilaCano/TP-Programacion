Algoritmo ReservaTuCancha
	// DEFINICION VARIABLES
	Definir canchas, horariosCancha11, horariosCancha7, horariosCancha5C, horariosCancha5S, reservas, optMenu Como Caracter
	Definir cantReservas como entero 
	
	// INICIALIZACION VARIABLES
	cantReservas <- 0
	
	// DIMENSIONAMOS ARRAYS	
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
	
	// ------------ Bucle Menu ------------
	Repetir
		menu()
		Leer optMenu
		optMenu <- Minusculas(optMenu) // en caso de que ingrese "salir"
		
		Segun optMenu Hacer
			"1":
				// ver datos de las canchas (tipos y caracteristicas)
				mostrarCanchas(canchas)
			"2":	
				// reservar cancha
				optMenu <- reservarCancha(horariosCancha11, horariosCancha5C, horariosCancha5S, horariosCancha7, reservas, canchas, cantReservas)
			"3":
				// ver lista de reservas
				mostrarReservas(reservas, cantReservas, 6)
			"4": 
				// ordenar por nombre y mostrar lista de reservas
				ordenarPorNombreYMostrar(cantReservas, reservas)
			"5":	
				// buscar reserva por nombre del cliente
				buscarReservaPorNombre(reservas,cantReservas, 6)
			"salir":
				Escribir "Hasta luego!"
			De Otro Modo:
				Escribir "Ingrese una opción de menú válida";
		Fin Segun
		
	Hasta Que optMenu == "salir"
FinAlgoritmo


// ------------ Menu ------------
SubProceso menu()
	Escribir "1. Ver datos de las canchas"
	Escribir "2. Reservar cancha"
	Escribir "3. Ver lista de reservas"
	Escribir "4. Ordenar por nombre y mostrar lista de reservas"
	Escribir "5. Buscar reserva por nombre del cliente"
	Escribir "salir"
FinSubProceso

// ------------ 1- mostrar canchas ------------
SubProceso mostrarCanchas(canchas)
	Definir cantFilas, cantColumnas Como Entero
	cantFilas <- 4
	cantColumnas <- 5
	Escribir "--------------------------------------------------"
	Escribir "                     Canchas                      |"
	Escribir "--------------------------------------------------|"
	Escribir "Id | Jugadores |   Tipo Suelo   | Precio | Tiempo |"
	Escribir "--------------------------------------------------|"
	Para i <- 0 Hasta cantFilas-1 Hacer
		Para j<-0 Hasta cantColumnas-1 Hacer
			segun j Hacer
				1:
					Mostrar Sin Saltar "    " canchas[i,j], "    | "
				2:
					segun canchas[i,j] Hacer
						"Cesped Natural":
							Mostrar Sin Saltar canchas[i,j], " | "
						"Sintetico":
							Mostrar Sin Saltar "   ", canchas[i,j], "   | "
						"Cemento":
							Mostrar Sin Saltar "    ", canchas[i,j], "    | "
					FinSegun
				3:
					si i == 3
						Mostrar Sin Saltar "  ", canchas[i,j], " | "
					SiNo
						Mostrar Sin Saltar " ", canchas[i,j], " | "
					FinSi
				4:
					si i <> 0
						Mostrar Sin Saltar "   ", canchas[i,j], "  | "
					SiNo
						Mostrar Sin Saltar "  ", canchas[i,j], "  | "
					FinSi
				De Otro Modo:
					Mostrar Sin Saltar canchas[i,j], " | "
			FinSegun
		Fin Para
		Escribir " "
	Fin Para
	Escribir "--------------------------------------------------"
	Escribir " "
FinSubProceso

// ------------ 2- Reservar cancha ------------
Funcion return <- reservarCancha(horariosCancha11, horariosCancha5C, horariosCancha5S, horariosCancha7, reservas, canchas, cantReservas Por Referencia)
	Definir filaAmostrar, horarioEncontrado como entero
	Definir diaElegido, tipoDeCancha, nombreCliente, horarioSeleccionado, precio, continuar, cantJugadores como caracter
	Definir nombreEnUso Como Logico
	
	// Pedimos el nombre, mínimo 4 caracteres
	Repetir 
		Escribir "Ingrese su nombre:"
		Leer nombreCliente
		Si busquedaIndice(reservas,cantReservas,nombreCliente,0) <> -1
			Escribir "El nombre ya se encuentra en uso."
			nombreEnUso <- Falso
		SiNo
			nombreEnUso <- Verdadero
		FinSi
		
	Hasta Que Longitud(nombreCliente) > 3 y nombreEnUso == Verdadero
	
	//Solicitamos el dia y lo validamos
	Repetir 
		Escribir "Ingrese el día de la semana"
		leer diaElegido
		diaElegido <- Minusculas(diaElegido) //lo pasamos a minúscula para que la validacion sea correcta
	Hasta Que validarDia(diaElegido) == Verdadero
	
	//Solicitamos cantidad de jugadores y validamos el ingreso
	Repetir 
		Escribir "Las cantidades de jugadores permitidas son: 10, 14 y 22"
		Escribir "Ingrese la cantidad de jugadores: "
		Leer cantJugadores 
	Hasta Que cantJugadores == "10" o cantJugadores == "14" o cantJugadores == "22"  
	
	//Indicamos las caracteristicas y horarios disponibles segun tipo de cancha
	Segun cantJugadores 
		caso "10":
			Escribir "Los tipos de cancha para 10 jugadores son: cemento y sintetico" 
			continuar <- volver()
			Si continuar == "1" Entonces 
				Repetir 
					Escribir "Ingrese el tipo de cancha: "
					Leer tipoDeCancha
					tipoDeCancha <- Minusculas(tipoDeCancha)
				Hasta Que tipoDeCancha == "cemento" o tipoDeCancha == "sintetico"
				
				Si tipoDeCancha == "cemento" Entonces  
					horarioSeleccionado <- registrarHorario(reservas, horariosCancha5C, 8, 6, diaElegido, cantReservas)
					
					// Si no decidio salir ni volver al menu principal
					si horarioSeleccionado <> "salir" y horarioSeleccionado <> "2"
						precio <- canchas[3,3]
					SiNo
						continuar <- horarioSeleccionado
					FinSi
				SiNo								
					horarioSeleccionado <- registrarHorario(reservas, horariosCancha5S, 8, 6, diaElegido, cantReservas)
					
					// Si no decidio salir ni volver al menu principal
					si horarioSeleccionado <> "salir" y horarioSeleccionado <> "2"
						precio <- canchas[2,3]
					SiNo
						continuar <- horarioSeleccionado
					FinSi
				Fin Si
			Fin Si
		caso "14":
			Escribir "El tipo de cancha disponibles es: Cesped sintetico"
			continuar <- volver()
			Si continuar=="1" Entonces
				tipoDeCancha <- "sintetico"
				horarioSeleccionado <- registrarHorario(reservas, horariosCancha7, 5, 6, diaElegido, cantReservas)
				
				// Si no decidio salir ni volver al menu principal
				si horarioSeleccionado <> "salir" y horarioSeleccionado <> "2"
					precio <- canchas[1,3]
				SiNo
					continuar <- horarioSeleccionado
				FinSi
			Fin Si
		caso "22":
			Escribir "El tipo de cancha disponible es: Cesped Natural"
			continuar <- volver()
			Si continuar=="1" Entonces
				tipoDeCancha <- "Cesped Natural"
				horarioSeleccionado <- registrarHorario(reservas, horariosCancha11, 4, 6, diaElegido, cantReservas)

				// Si no decidio salir ni volver al menu principal
				si horarioSeleccionado <> "salir" y horarioSeleccionado <> "2"
					precio <- canchas[0,3]
				SiNo
					continuar <- horarioSeleccionado
				FinSi
			Fin Si	
	FinSegun
	
	Si continuar == "1" Entonces // cargo el arreglo de reservas solo si el usuario desea "continuar" es 1
		total <- calcularTotal(precio)
		guardarReserva(reservas, nombreCliente, diaElegido, cantJugadores, tipoDeCancha, total, cantReservas)
		cantReservas <- cantReservas + 1
	SiNo
		Escribir " "
	Fin Si
	return <- continuar
FinFuncion

//------------ guardar reserva ------------
Subproceso guardarReserva(reservas, nombreCliente, diaElegido, cantJugadores, tipoDeCancha, total, cantReservas)
	reservas[cantReservas,0] <- nombreCliente
	reservas[cantReservas,1] <- diaElegido
	reservas[cantReservas,2] <- cantJugadores
	reservas[cantReservas,3] <- tipoDeCancha
	reservas[cantReservas,4] <- ConvertirATexto(total)
	
	Escribir " "
	Escribir "¡Su reserva ha sido guardada con exito! "
	Escribir Sin Saltar "Dia: " 
	Escribir diaElegido
	Escribir Sin Saltar "Jugadores: " 
	Escribir cantJugadores
	Escribir Sin Saltar "Tipo de cancha: "
	Escribir tipoDeCancha		
	Escribir Sin Saltar "Precio total segun medio de pago: $"
	Escribir total
	Escribir Sin Saltar "Horario: "
	Escribir horarioSeleccionado, "hs"
	Escribir " "
FinSubProceso


//------------ validación de día elegido ------------
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

// ------------ registrar horario ------------
// Funcion para registrar un horario de reserva
Funcion return<-  registrarHorario(reservas, horariosCancha, cantHorarios, cantDias, diaElegido, cantReservas)
	
	Repetir 
		// buscamos la fila del dia elegido para ver los horarios y los mostramos
		filaAmostrar <- busquedaIndice(horariosCancha, cantDias, diaElegido, 0)
		Escribir "Horarios: "
		mostrarFila(horariosCancha, cantHorarios, filaAmostrar)
		continuar <- volver()
		
		// Si el usuario desea continuar
		Si continuar == "1" Entonces
			Escribir "Ingrese el horario deseado: "
			Leer horarioSeleccionado
			
			// Verificamos que el horario ingresado sea correcto y si es asi lo guardamos
			horarioEncontrado <- busquedaSecuencial(horariosCancha, cantHorarios, horarioSeleccionado, filaAmostrar)
			
			Si horarioEncontrado <> -1 Entonces
				reservas[cantReservas,4] <- horarioSeleccionado
				horariosCancha[filaAmostrar, horarioEncontrado] <- "Reservado"
			Sino 
				Escribir "El horario ingresado ya se encuentra reservado o no existe"
			FinSi
		FinSi
	Mientras Que horarioEncontrado == -1 y continuar == "1"
		
	Si  continuar == "1"
		return <- horarioSeleccionado
	SiNo
		return <- continuar
	FinSi
FinFuncion

// ------------ busquedaSecuencial ------------
// Buscar elemento y retornar el indice o -1 si no lo encuentra
// Busca en una fila dada
Funcion return<- busquedaSecuencial(array, n, elementoABuscar, filaAmostrar)
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

// ------------ mostrar fila ------------
SubProceso mostrarFila(array, cantColumnas, numFila)
	para i<- 0 Hasta cantColumnas-1 Hacer
		Escribir Sin Saltar array[numFila,i], " "
	FinPara
	Escribir ""
FinSubProceso

// ------------ mostrar reservas ------------
SubProceso mostrarReservas(array, cantFilas, cantColumnas)
	Si cantFilas > 0 Entonces
		Escribir " "
		Para i <- 0 Hasta cantFilas-1  Hacer
			Para j<-0 Hasta cantColumnas-1  Hacer
				Segun j Hacer
					0:
						Escribir "Nombre: ", array[i,j]
					1:
						Escribir "Dia: ", array[i,j]
					2:
						Escribir "Cancha de ", ConvertirANumero((array[i,j]))/2
						Escribir "Cantidad de jugadores: ", array[i,j]
					3:
						Escribir "Tipo de cancha: ", array[i,j]
					4:
						Escribir "Total: ", array[i,j]
				FinSegun
			Fin Para
			Escribir " "
		Fin Para
	Sino 
		Escribir "Aun no hay reservas para mostrar"
		Escribir " "
	FinSi
FinSubProceso

// ------------ 5- Ordenar por nombre y llamar a mostrar arreglo ------------
SubProceso ordenarPorNombreYMostrar(cantReservas, reservas)
	Si cantReservas > 0 Entonces
		ordernarMatrizPorColumna(reservas,cantReservas,6, 0)
		mostrarReservas(reservas, cantReservas, 6)
	Sino 
		Escribir "Aun no hay reservas para mostrar"
		Escribir " "
	FinSi
FinSubProceso

// ------------5- ordenar matriz por columna ascendente ------------
SubProceso ordernarMatrizPorColumna(array,n,m, columnaABuscar)
	Si n > 1
		Definir aux Como Caracter;
		para i<-0 hasta n-2 Hacer //recorro las filas del array hasta la penultima
			para k<-i+1 hasta n-1 Hacer //recorro las filas del array hasta la última //I ES EL PIVOTE ACTUAL Y K EL RESTO QUE SE COMPARA
				si array[i, columnaABuscar] > array[k, columnaABuscar]  Entonces
					Para j<-0 Hasta m-1 Hacer //recorro las columnas del array
						aux <- array[i,j];
						array[i,j] <- array[k,j]; 
						array[k,j] <- aux; 
					Fin Para
				FinSi
			FinPara
		FinPara
	FinSi 
FinSubProceso

// ------------ busquedaIndice -----------
// Busca el elemento en una columna dada y retornar el indice
Funcion return<- busquedaIndice(array,cantFilas,elementoABuscar,columnaABuscar)
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
	SiNo
		return <- -1
	FinSi
FinFuncion

// ------------ busquedaUnidimensional ------------
// Busqueda secuencial array unidimensional. Funcion de busqueda para validaciones
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

// ------------ busqueda de reserva por nombre ------------
SubProceso buscarReservaPorNombre(array,cantFilas, cantColumnas)
	Definir nombre Como Caracter
	Definir indiceEncontrado como entero
	Escribir "Ingrese el nombre a buscar"
	Leer nombre
	indiceEncontrado <- busquedaIndice(array,cantFilas,nombre,0) //lama a funcion que retorna el indice encontrado ESTA MAS ARRIBA
	
	Si indiceEncontrado <> -1 Entonces
		Escribir " "
		mostrarCliente(array, indiceEncontrado)//SUBPROCESO DEBAJO MENCIONADO
		Escribir " "
	Sino 
		Escribir "El nombre no posee una reserva"
		Escribir " "
	FinSi
FinSubProceso

// ------------ mostrar cliente ------------
SubProceso mostrarCliente(reservas, fila)
	Escribir "Nombre: ", reservas[fila,0]
	Escribir "Dia: ", reservas[fila,1]
	Escribir "Cancha de ", ConvertirANumero((reservas[fila,2]))/2
	Escribir "Cantidad de jugadores: ", reservas[fila,2]
	Escribir "Tipo de cancha: ", reservas[fila,3]
	Escribir "Total: ", reservas[fila,4]
FinSubProceso

// ------------ Volver al menu principal------------
Funcion return <- volver()
	Definir opcionVolver Como caracter
	Repetir 
		Escribir "¿Desea continuar?"
		Escribir "1 - Continuar"
		Escribir "2 - Volver al menu principal"
		Escribir "salir" 
		leer opcionVolver
	Mientras que opcionVolver <> "1" y opcionVolver <> "2" y opcionVolver <> "salir"
	return <- opcionVolver
FinFuncion

// ------------ Calculo de precio Total con descuentos------------
Funcion return <- calcularTotal(precioCancha)
	Definir total, descuento, precio Como Real
	Definir medioDePago Como caracter
	precio <- convertirAnumero(precioCancha)
	
	Escribir "El costo de la cancha es: ", precioCancha
	Escribir "1) EFECTIVO"
	Escribir  "----------------------------"
	Escribir "2) TRANSFERENCIA"
	Escribir  "----------------------------"
	Escribir "3) BILLETERA SANTA FE"
	Escribir  "----------------------------"
	Escribir "3) MODO"
	Escribir  "----------------------------"
	
	Repetir
		Escribir "Ingrese el medio de pago de preferencia:"
		Leer medioDePago
	Hasta Que medioDePago == "1" o medioDePago == "2" o medioDePago == "3" o medioDePago == "4"
	
	Escribir " "
	Escribir "Tu medio de pago fue aceptado"
	Escribir "______________________________"
	Segun medioDePago Hacer
		caso "1": 
			Escribir "Deberas abonar el dia del partido"
			Escribir "Tenes un descuento del 5%"
			descuento <- 0.05
		caso "2": 
			Escribir "Realiza tu transferencia a este CBU 000235590003568571"
			Escribir "Envianos tu comprobante y tu reserva al mail reservatucancha@gmail.com"
		caso "3":
			Escribir "Deberas abonar el dia del partido"
			Escribir "Tenes un descuento del 15%"
			descuento <- 0.15
		caso "4":
			Escribir "Usa tu billetera virtual. Estamos en comercios online como *Reserva tu cancha* ¡facil y rapido!"
	FinSegun
	
	total <- precio - (precio * descuento)
	return <- total
FinFuncion

// ------------ CARGA DE DATOS ------------
// DATOS CANCHAS
SubProceso cargaDeCanchas(array)
	array[0,0] <- "01" // Numero de cancha
	array[0,1] <- "22" // Cantidad de jugadores
	array[0,2] <- "Cesped Natural" // Tipo de suelo
	array[0,3] <- "22000" // precio
	array[0,4] <- "120" // Minutos
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

// DIAS Y HORARIOS CANCHA FUTBOL 7
SubProceso cargarCancha7(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16:00"
	array[0,2] <- "17:30"
	array[0,3] <- "19:00"
	array[0,4] <- "20:30"
	array[1,0] <- "martes"
	array[1,1] <- "16:00"
	array[1,2] <- "17:30"
	array[1,3] <- "19:00"
	array[1,4] <- "20:30"	
	array[2,0] <- "miercoles"
	array[2,1] <- "16:00"
	array[2,2] <- "17:30"
	array[2,3] <- "19:00"
	array[2,4] <- "20:30"
	array[3,0] <- "jueves" 
	array[3,1] <- "16:00"
	array[3,2] <- "17:30"
	array[3,3] <- "19:00"
	array[3,4] <- "20:30"
	array[4,0] <- "viernes"		
	array[4,1] <- "16:00"
	array[4,2] <- "17:30"
	array[4,3] <- "19:00"
	array[4,4] <- "20:30"
	array[5,0] <- "sabado"
	array[5,1] <- "16:00"
	array[5,2] <- "17:30"
	array[5,3] <- "19:00"
	array[5,4] <- "20:30"
FinSubProceso

// DIAS Y HORARIOS CANCHA FUTBOL 11
SubProceso cargarCancha11(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16:00"
	array[0,2] <- "18:00"
	array[0,3] <- "20:00"
	array[1,0] <- "martes"
	array[1,1] <- "16:00"
	array[1,2] <- "18:00"
	array[1,3] <- "20:00"
	array[2,0] <- "miercoles"
	array[2,1] <- "16:00"
	array[2,2] <- "18:00"
	array[2,3] <- "20:00"
	array[3,0] <- "jueves" 
	array[3,1] <- "16:00"
	array[3,2] <- "18:00"
	array[3,3] <- "20:00"
	array[4,0] <- "viernes"		
	array[4,1] <- "16:00"
	array[4,2] <- "18:00"
	array[4,3] <- "20:00"
	array[5,0] <- "sabado"
	array[5,1] <- "16:00"
	array[5,2] <- "18:00"
	array[5,3] <- "20:00"
FinSubProceso

// DIAS Y HORARIOS CANCHA FUTBOL 5 CEMENTO
SubProceso cargarCancha5Cemento(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16:00"
	array[0,2] <- "17:00"
	array[0,3] <- "18:00"
	array[0,4] <- "19:00"
	array[0,5] <- "20:00"
	array[0,6] <- "21:00"
	array[0,7] <- "22:00"
	array[1,0] <- "martes"
	array[1,1] <- "16:00"
	array[1,2] <- "17:00"
	array[1,3] <- "18:00"
	array[1,4] <- "19:00"	
	array[1,5] <- "20:00"
	array[1,6] <- "21:00"
	array[1,7] <- "22:00"
	array[2,0] <- "miercoles"
	array[2,1] <- "16:00"
	array[2,2] <- "17:00"
	array[2,3] <- "18:00"
	array[2,4] <- "19:00"	
	array[2,5] <- "20:00"
	array[2,6] <- "21:00"
	array[2,7] <- "22:00"
	array[3,0] <- "jueves" 
	array[3,1] <- "16:00"
	array[3,2] <- "17:00"
	array[3,3] <- "18:00"
	array[3,4] <- "19:00"	
	array[3,5] <- "20:00"
	array[3,6] <- "21:00"
	array[3,7] <- "22:00"
	array[4,0] <- "viernes"		
	array[4,1] <- "16:00"
	array[4,2] <- "17:00"
	array[4,3] <- "18:00"
	array[4,4] <- "19:00"	
	array[4,5] <- "20:00"
	array[4,6] <- "21:00"
	array[4,7] <- "22:00"
	array[5,0] <- "sabado"
	array[5,1] <- "16:00"
	array[5,2] <- "17:00"
	array[5,3] <- "18:00"
	array[5,4] <- "19:00"	
	array[5,5] <- "20:00"
	array[5,6] <- "21:00"
	array[5,7] <- "22:00"
FinSubProceso

// DIAS Y HORARIOS CANCHA FUTBOL 5 SINTETICO
SubProceso cargarCancha5Sintetico(array)
	array[0,0] <- "lunes" 
	array[0,1] <- "16:00"
	array[0,2] <- "17:00"
	array[0,3] <- "18:00"
	array[0,4] <- "19:00"
	array[0,5] <- "20:00"
	array[0,6] <- "21:00"
	array[0,7] <- "22:00"
	array[1,0] <- "martes"
	array[1,1] <- "16:00"
	array[1,2] <- "17:00"
	array[1,3] <- "18:00"
	array[1,4] <- "19:00"	
	array[1,5] <- "20:00"
	array[1,6] <- "21:00"
	array[1,7] <- "22:00"
	array[2,0] <- "miercoles"
	array[2,1] <- "16:00"
	array[2,2] <- "17:00"
	array[2,3] <- "18:00"
	array[2,4] <- "19:00"	
	array[2,5] <- "20:00"
	array[2,6] <- "21:00"
	array[2,7] <- "22:00"
	array[3,0] <- "jueves" 
	array[3,1] <- "16:00"
	array[3,2] <- "17:00"
	array[3,3] <- "18:00"
	array[3,4] <- "19:00"	
	array[3,5] <- "20:00"
	array[3,6] <- "21:00"
	array[3,7] <- "22:00"
	array[4,0] <- "viernes"		
	array[4,1] <- "16:00"
	array[4,2] <- "17:00"
	array[4,3] <- "18:00"
	array[4,4] <- "19:00"	
	array[4,5] <- "20:00"
	array[4,6] <- "21:00"
	array[4,7] <- "22:00"
	array[5,0] <- "sabado"
	array[5,1] <- "16:00"
	array[5,2] <- "17:00"
	array[5,3] <- "18:00"
	array[5,4] <- "19:00"	
	array[5,5] <- "20:00"
	array[5,6] <- "21:00"
	array[5,7] <- "22:00"
FinSubProceso
