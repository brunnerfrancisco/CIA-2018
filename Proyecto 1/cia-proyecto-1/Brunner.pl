/*************************************************************************************************************************************
	Proyecto 1 - CIA 2018
	
	Alumno: Brunner, Francisco 
	LU: 102242
	e-mail: brunner.francisco874@gmail.com

*************************************************************************************************************************************/

/*************************************************************************************************************************************
	Definicion de Operadores de la Logica Proposicional propuestos por la catedra
*************************************************************************************************************************************/
:-op(400, fy, 'no').
:-op(500, yfx, 'and').
:-op(600, yfx, 'or').
:-op(700, yfx, '->').
:-op(800, yfx, 'equiv').

/*************************************************************************************************************************************
	solicitarEvaluarFbf.
		Evalua la fbf que ingrese el usuario por teclado.
		Este predicado muestra un cartel inicial para guiar al usuario, 
			luego espera que se ingrese la fbf y por ultimo clasifica la fbf.
*************************************************************************************************************************************/
solicitarEvaluarFbf:- mostrar_bienvenida, read(Expresion), validar_fbf(Expresion), obtener_todos(Expresion).
solicitarEvaluarFbf:- nl, write('No es una fbf valida').

/*************************************************************************************************************************************
	obtener_todos(+Expresion).
		Obtiene todas los valores de verdad y luego clasifica Expresion.
		Evalua la Expresion para cada posible Interpretacion de las letras proposicionales (literales).
*************************************************************************************************************************************/
obtener_todos(Expresion):- findall([Interpretacion, ValorDeVerdad], evaluar(Expresion, [], Interpretacion, ValorDeVerdad), Resultado),
	clasificar(Resultado).

/*************************************************************************************************************************************
	clasficar(+ListaDeVV).
		ListaDeVV contiene una lista con elementos que son listas cuyos elementos son la interpretacion y el valor de verdad.
		ListaDeVV tiene 2^cantidadDeLiterales elementos.
		Si ListaDeVV no tiene ningun false, entonces la expresion es una tautologia;
		Si ListaDeVV no tiene ningun true, entonces la expresion es una contradiccion;
		Si ListaDeVV tiene al menos un true y al menos un false, entonces es satifacible.
*************************************************************************************************************************************/
clasificar(ListaDeVV):- member([_,true], ListaDeVV),member([_,false], ListaDeVV), !, satisfacible.
clasificar(ListaDeVV):- \+member([_,false], ListaDeVV), !, tautologia.
clasificar(ListaDeVV):- \+member([_,true], ListaDeVV), contradiccion.

tautologia:- nl, write('La fbf ingresada es: tautologia').
contradiccion:- nl, write('La fbf ingresada es: contradiccion').
satisfacible:- nl, write('La fbf ingresada es: satisfacible').

/*************************************************************************************************************************************
	evaluar(+Expresion,?Interpretacion1,?Interpretacion2,-VV).
		Dada una Expresion contruye cada una de las posibles interpretaciones y 
		evalua el valor De Verdad de Expresion asociado a la Interpretacion.
*************************************************************************************************************************************/
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
evaluar(E, I1, I2, true):- letra(E), \+member([E, false], I1),append(I1, [[E, true]], I2).
evaluar(E, I1, I2, false):- letra(E), \+member([E, true], I1),append(I1, [[E, false]], I2).

/*************************************************************************************************************************************
	procesarListaFbfs(+Lista).
		Por cada Expresion en lista verifica si es una fbf valida.
			Si es valida, procesa la expresion 
			Luego procesa el resto de las Expresiones
*************************************************************************************************************************************/
procesarListaFbfs([]):-!.
procesarListaFbfs([Expresion|Resto]):- validar_fbf(Expresion), !, nl, nl, procesar_expresion(Expresion), procesarListaFbfs(Resto).
procesarListaFbfs([_|Resto]):- nl, nl, write('No es una fbf valida'), procesarListaFbfs(Resto).

/*************************************************************************************************************************************
	procesar_expresion(+Expresion).
		De Expresion obtiene la cantidad de literales: 
			* si es par lo clasifica como solicitarEvaluarFbf;
			* si es impar busca un modelo para esa fbf.
*************************************************************************************************************************************/
procesar_expresion(Expresion):-literales(Expresion, Literales), cantidad_elementos(Literales, Cantidad), 
	0 is Cantidad mod 2, !, obtener_todos2(Expresion).
procesar_expresion(Expresion):- literales(Expresion, Literales), cantidad_elementos(Literales, Cantidad), 
	1 is Cantidad mod 2, buscar_modelo(Expresion).

/*************************************************************************************************************************************
	obtener_todos2(+Expresion).
		Idem obtener_todos() pero Expresion la uso para imprimirla en el cartel.
*************************************************************************************************************************************/
obtener_todos2(Expresion):-
	findall( [Interpretacion, ValorDeVerdad], evaluar(Expresion ,[] , Interpretacion, ValorDeVerdad), Resultado),
	clasificar2(Expresion, Resultado).

/*************************************************************************************************************************************
	clasficar2(+Expresion,+ListaDeVV).
		Idem clasificar(+Lista) pero Expresion la uso para imprimirla en el cartel.
*************************************************************************************************************************************/
clasificar2(Expresion, ListaDeVV):- member([_,true], ListaDeVV), member([_,false], ListaDeVV), !, satisfacible2(Expresion).
clasificar2(Expresion, ListaDeVV):- \+member([_,false], ListaDeVV), !, tautologia2(Expresion).
clasificar2(Expresion, ListaDeVV):- \+member([_,true], ListaDeVV), contradiccion2(Expresion).

tautologia2(Expresion):-    write('La fbf "'), write(Expresion), write('" es: tautologia').
contradiccion2(Expresion):- write('La fbf "'), write(Expresion), write('" es: contradiccion').
satisfacible2(Expresion):-  write('La fbf "'), write(Expresion), write('" es: satisfacible').

/*************************************************************************************************************************************
	buscar_modelo(+Expresion).
		Obtiene las interpretaciones y el valor de verdad asociado a Expresion, luego busca algun modelo entre las posibles.
*************************************************************************************************************************************/
buscar_modelo(Expresion):-
	findall([Interpretacion, ValorDeVerdad], evaluar(Expresion, [], Interpretacion, ValorDeVerdad), Resultado), 
	modelo(Expresion,Resultado).

/*************************************************************************************************************************************
	modelo(Expresion,[Interpretacion,VVEnLista]).
		Busca una Interpretacion que tenga un valor de verdad asociado verdadero,
		en caso de no encontrar no existe un modelo para Expresion.
*************************************************************************************************************************************/
modelo(Expresion, [[Interpretacion,true]|_]):-!, existe_modelo(Expresion,Interpretacion).
modelo(Expresion, [[_,false]|Resto]):- !, modelo(Expresion, Resto).
modelo(Expresion, []):-no_existe_modelo(Expresion).

existe_modelo(Expresion,Interpretacion):-nl, write('Un modelo para la fbf "'), write(Expresion), write('" es: '), write(Interpretacion).
no_existe_modelo(Expresion):- nl, write('No existe un modelo para la fbf: "'), write(Expresion), write('"').

/*************************************************************************************************************************************
	literales(+Expresion,-ListaDeLiterales).
		Construye una lista con las letras proposicionales (literales) de Expresion, sin repeticiones.
*************************************************************************************************************************************/
literales(no Expresion, Lista):- !, literales(Expresion, Lista).
literales(E1 and E2, L):- !, literales(E1, L1),literales(E2, L2),unir(L1, L2, L).
literales(E1 or E2, L):- !, literales(E1, L1),literales(E2, L2),unir(L1, L2, L).
literales(E1 -> E2, L):- !, literales(E1, L1),literales(E2, L2),unir(L1, L2, L).
literales(E1 equiv E2, L):- !, literales(E1, L1),literales(E2, L2),unir(L1, L2, L).
literales(E, [E]):- letra(E).

/*************************************************************************************************************************************
	cantidad_elementos(+Lista,-Cantidad).
		Cuenta la cantidad de elementos en una lista.
*************************************************************************************************************************************/
cantidad_elementos([], 0):- !.
cantidad_elementos([_|Lista], Cantidad):-cantidad_elementos(Lista, Cant), Cantidad is Cant + 1.

/*************************************************************************************************************************************
	unir(+Lista1,+Lista2,-Resultado).
		Une dos listas y elimina elementos repetidos.
*************************************************************************************************************************************/
unir([], C, C):-!.
unir([Primer|Resto], Lista, Resultado):- unir(Resto, Lista, Resultado), member(Primer, Lista), !.
unir([Primer|Resto], Lista, Resultado):- unir(Resto, Lista, Resultado_aux), Resultado = [Primer|Resultado_aux].

/*************************************************************************************************************************************
	mostrar_bienvenida.
		Cartel correspondiente a solictarEvaluarFbf como indicacion al usuario.
*************************************************************************************************************************************/
mostrar_bienvenida:-nl, write('Ingrese una formula bien formada (fbf) de la Logica Proposicional terminada'), nl,
	write('en "." y presione ENTER al finalizar: '), !.

/*************************************************************************************************************************************
	validar_fbf(+Expresion).
		Valida que Expresion sea una fbf valida.
*************************************************************************************************************************************/
validar_fbf(E1 equiv E2):- validar_fbf(E1), !, validar_fbf(E2).
validar_fbf(E1 -> E2):- validar_fbf(E1), !, validar_fbf(E2).
validar_fbf(E1 or E2):- validar_fbf(E1), !, validar_fbf(E2).
validar_fbf(E1 and E2):- validar_fbf(E1), !, validar_fbf(E2).
validar_fbf(no E):- validar_fbf(E), !.
validar_fbf(E):- letra(E), !.
validar_fbf(_):- fail.

imprimir_tabla_de_verdad([]).
imprimir_tabla_de_verdad([[X,true] |Resto]):-imprimir_interpretacion(X),write('          '),write('true '),nl,
	imprimir_tabla_de_verdad(Resto).
imprimir_tabla_de_verdad([[X,false]|Resto]):-imprimir_interpretacion(X),write('          '),write('false'),nl,
	imprimir_tabla_de_verdad(Resto).

imprimir_interpretacion([]).
imprimir_interpretacion([[X,true] |Resto]):-write(X),write(': true,  '),imprimir_interpretacion(Resto).
imprimir_interpretacion([[X,false]|Resto]):-write(X),write(': false, '),imprimir_interpretacion(Resto).

/*************************************************************************************************************************************
	letra(X).
		Hechos correspondientes para verificar las letras proposicionales.
*************************************************************************************************************************************/
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

