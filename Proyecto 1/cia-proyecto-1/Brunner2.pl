/*****************************************************************************************************************************************
	Proyecto 1 - CIA 2018
	
	Alumno: Brunner, Francisco 
	LU: 102242
	e-mail: brunner.francisco874@gmail.com

*****************************************************************************************************************************************/

/*****************************************************************************************************************************************
	Definicion de Operadores de la Logica Proposicional propuestos por la catedra
*****************************************************************************************************************************************/
:-op(400,fy,'no').
:-op(500,yfx,'and').
:-op(600,yfx,'or').
:-op(700,yfx,'->').
:-op(800,yfx,'equiv').

/*****************************************************************************************************************************************
	solicitarEvaluarFbf.
		Evalua la fbf que ingrese el usuario por teclado.
		Este predicado muestra un cartel inicial para guiar al usuario, 
			luego espera que se ingrese la fbf y por ultimo clasifica la fbf.
*****************************************************************************************************************************************/
solicitarEvaluarFbf:-mostrar_bienvenida,read(Expresion),validar_fbf(Expresion),obtener_todos(Expresion).
solicitarEvaluarFbf:-nl,write('No es una fbf valida').

/*****************************************************************************************************************************************
	obtener_todos(+Expresion).
		Obtiene todas los valores de verdad y luego clasifica Expresion.
		Evalua la Expresion para cada posible Interpretacion de las letras proposicionales (literales).
*****************************************************************************************************************************************/
obtener_todos(Expresion):-literales(Expresion,Literales),
	findall(VVEnLista,obtener_interpretacion(Expresion,Literales,VVEnLista),Respuesta),clasificar(Respuesta).

/*****************************************************************************************************************************************
	obtener_interpretacion(+Expresion,+Literales,-VVEnLista).
		Evalua cada posible interprecion de Literales en Expresion y retorna en una lista el valor de verdad asociado
*****************************************************************************************************************************************/
obtener_interpretacion(Expresion,Literales,VVEnLista):-construir_interpretacion(Literales,Interpretacion),
	findall(VV,evaluar(Expresion,Interpretacion,VV),VVEnLista).

/*****************************************************************************************************************************************
	construir_interpretacion(+Literales,-Interpretacion).
		contruye una posible interpretacion de las letras proposicionales en Literales.
*****************************************************************************************************************************************/
construir_interpretacion([],[]).
construir_interpretacion([Literal|Resto],[[Literal,true]|RestoResultado]):-construir_interpretacion(Resto,RestoResultado).
construir_interpretacion([Literal|Resto],[[Literal,false]|RestoResultado]):-construir_interpretacion(Resto,RestoResultado).

/*****************************************************************************************************************************************
	clasficar(+ListaDeVV).
		ListaDeVV contiene una lista con elementos que son listas cuyo unico elemento es el valor de verdad de una interpretacion.
		ListaDeVV tiene 2^cantidadDeLiterales elementos.
*****************************************************************************************************************************************/
clasificar(ListaDeVV):- \+member([false],ListaDeVV),!,tautologia.
clasificar(ListaDeVV):- \+member([true],ListaDeVV),!,contradiccion.
clasificar(ListaDeVV):- member([true],ListaDeVV),member([false],ListaDeVV),satisfacible.

tautologia:- nl,write('La fbf ingresada es: tautologia').
contradiccion:- nl,write('La fbf ingresada es: contradiccion').
satisfacible:- nl,write('La fbf ingresada es: satisfacible').

/*****************************************************************************************************************************************
	evaluar(+Expresion,+Interpretacion,-VV).
		Dada una Expresion y una posible interpretacion evalua el Valor De Verdad (VV) de Expresion asociado a la Interpretacion.
*****************************************************************************************************************************************/
evaluar(E1 equiv E2, I, true):-  evaluar(E1, I, true),  evaluar(E2, I, true).
evaluar(E1 equiv E2, I, false):- evaluar(E1, I, true),  evaluar(E2, I, false).
evaluar(E1 equiv E2, I, false):- evaluar(E1, I, false), evaluar(E2, I, true).
evaluar(E1 equiv E2, I, true):-  evaluar(E1, I, false), evaluar(E2, I, false).

evaluar(E1 -> E2, I, true):-  evaluar(E1, I, true),  evaluar(E2, I, true).
evaluar(E1 -> E2, I, false):- evaluar(E1, I, true),  evaluar(E2, I, false).
evaluar(E1 -> E2, I, true):-  evaluar(E1, I, false), evaluar(E2, I, true).
evaluar(E1 -> E2, I, true):-  evaluar(E1, I, false), evaluar(E2, I, false).

evaluar(E1 or E2, I, true):-  evaluar(E1, I, true),  evaluar(E2, I, true).
evaluar(E1 or E2, I, true):-  evaluar(E1, I, true),  evaluar(E2, I, false).
evaluar(E1 or E2, I, true):-  evaluar(E1, I, false), evaluar(E2, I, true).
evaluar(E1 or E2, I, false):- evaluar(E1, I, false), evaluar(E2, I, false).

evaluar(E1 and E2, I, true):-  evaluar(E1, I, true),  evaluar(E2, I, true).
evaluar(E1 and E2, I, false):- evaluar(E1, I, true),  evaluar(E2, I, false).
evaluar(E1 and E2, I, false):- evaluar(E1, I, false), evaluar(E2, I, true).
evaluar(E1 and E2, I, false):- evaluar(E1, I, false), evaluar(E2, I, false).

evaluar(no E, I, false):- evaluar(E, I, true).
evaluar(no E, I, true):-  evaluar(E, I, false).

evaluar(E,I,VV):-letra(E),member([E,VV],I).
/*
Esto es de la otra version
Si cambio las variables por las unificaciones no anda, me parece que el problema esta en el caso base con el append y el member

evaluar(E1 equiv E2, I1, I2, true):-  evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, true).
evaluar(E1 equiv E2, I1, I2, false):- evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, false).
evaluar(E1 equiv E2, I1, I2, false):- evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, true).
evaluar(E1 equiv E2, I1, I2, true):-  evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, false).

evaluar(E1 -> E2, I1, I2, true):-  evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, true).
evaluar(E1 -> E2, I1, I2, false):- evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, false).
evaluar(E1 -> E2, I1, I2, true):-  evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, true).
evaluar(E1 -> E2, I1, I2, true):-  evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, false).

evaluar(E1 or E2, I1, I2, true):-  evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, true).
evaluar(E1 or E2, I1, I2, true):-  evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, false).
evaluar(E1 or E2, I1, I2, true):-  evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, true).
evaluar(E1 or E2, I1, I2, false):- evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, false).

evaluar(E1 and E2, I1, I2, true):-  evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, true).
evaluar(E1 and E2, I1, I2, false):- evaluar(E1, I1, I3, true),  evaluar(E2, I3, I2, false).
evaluar(E1 and E2, I1, I2, false):- evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, true).
evaluar(E1 and E2, I1, I2, false):- evaluar(E1, I1, I3, false), evaluar(E2, I3, I2, false).

evaluar(no E, I1, I2, false):- evaluar(E, I1, I2, true).
evaluar(no E, I1, I2, true):-  evaluar(E, I1, I2, false).

evaluar(E, I1, I1, VV):- letra(E), member([E, VV], I1), !.
evaluar(E, I1, I2, true):- letra(E), append(I1, [[E, true]], I2).
evaluar(E, I1, I2, false):- letra(E), append(I1, [[E, false]], I2).
*/

/*****************************************************************************************************************************************
	procesarListaFbfs(+Lista).
		Por cada Expresion en lista verifica si es una fbf valida.
			Si es valida, procesa la expresion 
			Luego procesa el resto de las Expresiones
*****************************************************************************************************************************************/
procesarListaFbfs([]):-!.
procesarListaFbfs([Expresion|Resto]):- validar_fbf(Expresion),!,procesarElemento(Expresion),procesarListaFbfs(Resto).
procesarListaFbfs([_|Resto]):- nl,nl,write('No es una fbf valida'),procesarListaFbfs(Resto).

/*****************************************************************************************************************************************
	procesar_elemento(+Expresion).
		De Expresion obtiene la cantidad de literales: 
			* si es par lo clasifica como solicitarEvaluarFbf;
			* si es impar busca un modelo para esa fbf.
*****************************************************************************************************************************************/
procesarElemento(Expresion):-literales(Expresion,Literales), cantidad_elementos(Literales,Cantidad), 
	0 is Cantidad mod 2, !, nl, nl, obtener_todos2(Expresion).
procesarElemento(Expresion):- literales(Expresion,Literales), cantidad_elementos(Literales,Cantidad), 
	1 is Cantidad mod 2, nl, nl, buscar_modelo(Expresion).

/*****************************************************************************************************************************************
	obtener_todos2(+Expresion).
		Idem obtener_todos() pero Expresion la uso para imprimirla en el cartel.
*****************************************************************************************************************************************/
obtener_todos2(Expresion):-literales(Expresion,Literales),
	findall(VVEnLista,obtener_interpretacion(Expresion,Literales,VVEnLista),Respuesta),clasificar2(Expresion,Respuesta).

/*****************************************************************************************************************************************
	clasficar2(+Expresion,+ListaDeVV).
		Idem clasificar(+Lista) pero Expresion la uso para imprimirla en el cartel.
*****************************************************************************************************************************************/
clasificar2(Expresion,L):- \+member([false],L),!,tautologia2(Expresion).
clasificar2(Expresion,L):- \+member([true],L),!,contradiccion2(Expresion).
clasificar2(Expresion,L):- member([true],L),member([false],L),satisfacible2(Expresion).

tautologia2(Expresion):- write('La fbf "'),write(Expresion),write('" es: tautologia').
contradiccion2(Expresion):- write('La fbf "'),write(Expresion),write('" es: contradiccion').
satisfacible2(Expresion):- write('La fbf "'),write(Expresion),write('" es: satisfacible').

/*****************************************************************************************************************************************
	buscar_modelo(+Expresion).
		Obtiene las interpretaciones y los valores de verdad asociados a Expresion y luego busca algun modelo entre las posibles.
*****************************************************************************************************************************************/
buscar_modelo(Expresion):-literales(Expresion,Literales),
	findall([Interpretacion,VVEnLista],obtener_interpretacion(Expresion,Literales,VVEnLista,Interpretacion),R1),
	modelo(Expresion,R1).

/*****************************************************************************************************************************************
	obtener_interpretacion(+Expresion,+Literales,-VVEnLista,-Interpretacion).
		Evalua cada posible interprecion de Literales en Expresion y 
		retorna en una lista los valores de verdad y sus interpretaciones asociados
*****************************************************************************************************************************************/
obtener_interpretacion(Expresion,Literales,VVEnLista,Interpretacion):-construir_interpretacion(Literales,Interpretacion),
	findall(VV,evaluar(Expresion,Interpretacion,VV),VVEnLista).

/*****************************************************************************************************************************************
	modelo(Expresion,[Interpretacion,VVEnLista]).
		Busca una Interpretacion que tenga un valor de verdad asociado verdadero,
		en caso de no encontrar no existe un modelo para Expresion.
*****************************************************************************************************************************************/
modelo(Expresion,[[Interpretacion,[true]]|_]):-!,write('Un modelo para la fbf "'),write(Expresion),write('" es: '),write(Interpretacion).
modelo(Expresion,[[_,[false]]|Resto]):-!,modelo(Expresion,Resto).
modelo(Expresion,[]):- write('No existe un modelo para la fbf: "'),write(Expresion),write('"').

/*****************************************************************************************************************************************
	literales(+Expresion,-ListaDeLiterales).
		Construye una lista con las letras proposicionales (literales) de Expresion, sin repeticiones.
*****************************************************************************************************************************************/

literales(E1 equiv E2, L):- !, literales(E1,L1),literales(E2,L2),unir(L1,L2,L).
literales(E1 -> E2, L):- !, literales(E1,L1),literales(E2,L2),unir(L1,L2,L).
literales(E1 or E2, L):- !, literales(E1,L1),literales(E2,L2),unir(L1,L2,L).
literales(E1 and E2, L):- !, literales(E1,L1),literales(E2,L2),unir(L1,L2,L).
literales(no E, L):- !, literales(E,L).
literales(E, [E]):- letra(E).

/*****************************************************************************************************************************************
	cantidad_elementos(+Lista,-Cantidad).
		Cuenta la cantidad de elementos en una lista.
*****************************************************************************************************************************************/
cantidad_elementos([],0):-!.
cantidad_elementos([_|L],C):-cantidad_elementos(L,C1), C is C1 + 1.

/*****************************************************************************************************************************************
	unir(+Lista1,+Lista2,-Resultado).
		Une dos listas y elimina elementos repetidos.
*****************************************************************************************************************************************/
unir([],C,C):-!.
unir([X|C1],C2,C3):- unir(C1,C2,C3),member(X,C2),!.
unir([X|C1],C2,C3):- unir(C1,C2,C4),C3=[X|C4].

/*****************************************************************************************************************************************
	mostrar_bienvenida.
		Cartel correspondiente a solictarEvaluarFbf como indicacion al usuario.
*****************************************************************************************************************************************/
mostrar_bienvenida:-nl,write('Ingrese una formula bien formada (fbf) de la Logica Proposicional terminada'),nl,
	write('en "." y presione ENTER al finalizar: '),!.

/*****************************************************************************************************************************************
	validar_fbf(+Expresion).
		Valida que Expresion sea una fbf valida.
*****************************************************************************************************************************************/
validar_fbf(E1 equiv E2):- validar_fbf(E1),!,validar_fbf(E2).
validar_fbf(E1 -> E2):- validar_fbf(E1),!,validar_fbf(E2).
validar_fbf(E1 or E2):- validar_fbf(E1),!,validar_fbf(E2).
validar_fbf(E1 and E2):- validar_fbf(E1),!,validar_fbf(E2).
validar_fbf(no E):- validar_fbf(E),!.
validar_fbf(E):- letra(E),!.
validar_fbf(_):- fail.

/*****************************************************************************************************************************************
	letra(X).
		Hechos correspondientes para verificar las letras proposicionales.
*****************************************************************************************************************************************/
letra(a).
letra(b).
letra(c).
letra(d).
letra(e).
letra(f).
letra(g).
letra(h).
letra(i).
letra(j).
letra(k).
letra(l).
letra(m).
letra(n).
letra(o).
letra(p).
letra(q).
letra(r).
letra(s).
letra(t).
letra(u).
letra(v).
letra(w).
letra(x).
letra(y).
letra(z).

/*
procesarListaFbfs([a -> (b -> a), a or b -> no c, a and no a]).
*/

