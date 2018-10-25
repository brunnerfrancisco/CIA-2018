celda([1,1], resbaladizo).
celda([1,2], lava).
celda([1,3], resbaladizo).
celda([1,4], resbaladizo).

celda([2,1], resbaladizo).
celda([2,2], resbaladizo).
celda([2,3], firme).
celda([2,4], lava).

celda([3,1], firme).
celda([3,2], resbaladizo).
celda([3,3], firme).
celda([3,4], resbaladizo).
celda([3,10], lava).
celda([3,11], lava).
celda([3,12], lava).
celda([3,13], lava).

celda([4,1], resbaladizo).
celda([4,2], firme).
celda([4,3], lava).
celda([4,4], firme).
celda([4,10], firme).
celda([4,11], firme).
celda([4,12], firme).
celda([4,13], firme).

celda([5,1], firme).
celda([5,2], firme).
celda([5,3], resbaladizo).
celda([5,4], firme).
celda([5,5], firme).
celda([5,6], resbaladizo).
celda([5,7], lava).
celda([5,8], firme).
celda([5,9], resbaladizo).
celda([5,10], firme).
celda([5,11], firme).
celda([5,12], lava).
celda([5,13], resbaladizo).

celda([6,1], firme).
celda([6,2], firme).
celda([6,3], firme).
celda([6,4], resbaladizo).
celda([6,5], lava).
celda([6,6], firme).
celda([6,7], firme).
celda([6,8], firme).
celda([6,9], resbaladizo).
celda([6,10], lava).
celda([6,11], firme).
celda([6,12], lava).
celda([6,13], firme).

celda([7,1], lava).
celda([7,2], lava).
celda([7,3], lava).
celda([7,4], firme).
celda([7,5], lava).
celda([7,6], resbaladizo).
celda([7,7], lava).
celda([7,8], resbaladizo).
celda([7,9], resbaladizo).
celda([7,10], resbaladizo).
celda([7,11], firme).
celda([7,12], firme).
celda([7,13], resbaladizo).

celda([8,1], resbaladizo).
celda([8,2], firme).
celda([8,3], lava).
celda([8,4], firme).
celda([8,5], resbaladizo).
celda([8,6], resbaladizo).
celda([8,7], firme).
celda([8,8], lava).
celda([8,9], lava).
celda([8,10], lava).
celda([8,11], resbaladizo).
celda([8,12], lava).
celda([8,13], resbaladizo).

celda([9,1], resbaladizo).
celda([9,2], resbaladizo).
celda([9,3], lava).
celda([9,4], resbaladizo).
celda([9,5], firme).
celda([9,6], firme).
celda([9,7], resbaladizo).
celda([9,8], firme).
celda([9,9], resbaladizo).
celda([9,10], firme).
celda([9,11], resbaladizo).
celda([9,12], lava).
celda([9,13], firme).

celda([10,1], firme).
celda([10,2], firme).
celda([10,3], firme).
celda([10,4], lava).
celda([10,5], resbaladizo).
celda([10,6], firme).
celda([10,7], firme).
celda([10,8], resbaladizo).

celda([11,1], resbaladizo).
celda([11,2], lava).
celda([11,3], firme).
celda([11,4], resbaladizo).
celda([11,5], firme).
celda([11,6], resbaladizo).
celda([11,7], firme).
celda([11,8], firme).

celda([12,1], resbaladizo).
celda([12,2], lava).
celda([12,3], resbaladizo).
celda([12,4], firme).
celda([12,5], firme).
celda([12,6], firme).
celda([12,7], firme).
celda([12,8], firme).
celda([12,9], firme).
celda([12,10], lava).
celda([12,11], resbaladizo).

celda([13,1], firme).
celda([13,2], lava).
celda([13,3], resbaladizo).
celda([13,4], firme).
celda([13,5], firme).
celda([13,6], lava).
celda([13,7], lava).
celda([13,8], firme).
celda([13,9], lava).
celda([13,10], lava).
celda([13,11], firme).

celda([14,1], firme).
celda([14,2], lava).
celda([14,3], resbaladizo).
celda([14,4], firme).
celda([14,5], resbaladizo).
celda([14,6], lava).
celda([14,7], lava).
celda([14,8], firme).
celda([14,9], resbaladizo).
celda([14,10], lava).
celda([14,11], firme).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Objetos en la isla:

% Refugios:

estaEn([r, r1, no], [3,2]).
estaEn([r, r2, si], [5,5]).
estaEn([r, r3, si], [6,8]).
estaEn([r, r4, no], [7,10]).
estaEn([r, r5, si], [7,12]).
estaEn([r, r6, si], [13,8]).

% Llaves:

estaEn([l, l1, 2], [5,8]).
estaEn([l, l2, 1], [9,13]).
estaEn([l, l3, 3], [12,9]).

% Obstaculos:

estaEn([o, o1, 1], [6,3]).
estaEn([o, o2, 2], [6,11]).
estaEn([o, o3, 5], [9,2]).
estaEn([o, o4, 3], [11,4]).
estaEn([o, o5, 1], [11,8]).
estaEn([o, o6, 8], [12,4]).

% Palas:

estaEn([p, p1], [8,4]).
estaEn([p, p2], [5,2]).
estaEn([p, p3], [8,13]).
estaEn([p, p4], [6,13]).
estaEn([p, p5], [12,8]).

/*
--------------------------------------------------------------------------------
*/

:-dynamic frontera/1, visitado/1.
%:-retractall(frontera(_)).
%:-retractall(visitado(_)).
%:-assert(frontera(nodo(a,[],0,30))).

buscarPlan(EstadoInicial,Metas,Destino,Plan,Costo):-
    buscarHeuristica(EstadoInicial,Metas,Heuristica),
    assert(frontera(nodo(EstadoInicial,[],0,Heuristica))),
    buscarAE(Metas,Destino,Plan,Costo).

buscarAE(Metas,Destino,Solucion,Costo):-
    seleccionar(nodo(Estado,Camino,Costo,_)),
    esMeta(Estado,Metas,Destino),!,
    reverse(Camino,Solucion).
buscarAE(Metas,Destino,Solucion,Costo):-
    seleccionar(Nodo),
    retract(frontera(Nodo)),
    generarVecinos(Nodo,Vecinos),
    agregar(Vecinos),
    buscarAE(Metas,Destino,Solucion,Costo).

/*
    seleccionar(Nodo):
        selecciona el primer nodo de la frontera, es decir busca la primera unificacion con algun hecho 
        con la estructura frontera(nodo(Estado,Camino,Costo)).
        agrega el nodo a visitados.
*/
seleccionar(nodo(E,L,C,Fn)):-
    frontera(nodo(_,_,_,Primero)),
    buscarMenor(nodo(E,L,C,Fn),Primero),!,
    assertz(visitado(nodo(E,L,C,Fn))).

/*
    agregar(ListaDeVecinos)
        agrega los Nodos de ListaDeVecinos al final de la frontera
        (con assertz(frontera(Nodo)) agrega los hechos como ultimos hechos del programa)
        Control De Ciclos:
            - si ya esta en la frontera: 
                + si tiene un costo peor -> lo reemplazo
                + si notiene un costo peor -> lo descarto
            - si ya esta visitado: 
                + si tiene un costo peor -> lo saco de visitados y pongo el nuevo en la frontera
                + si notiene un costo peor -> lo descarto
            - sino lo agrego a la frontera como un nodo nuevo
*/
agregar([]):-!.
agregar([nodo(E,L,C,F)|RestoVecinos]):-
    frontera(nodo(E,L1,C1,F1)),
    C<C1,!,
    retract(frontera(nodo(E,L1,C1,F1))),
    assertz(frontera(nodo(E,L,C,F))),
    agregar(RestoVecinos).
agregar([nodo(E,L,C,F)|RestoVecinos]):-
    visitado(nodo(E,L1,C1,F1)),
    C<C1,!,
    retract(visitado(nodo(E,L1,C1,F1))),
    assertz(frontera(nodo(E,L,C,F))),
    agregar(RestoVecinos).
agregar([nodo(E,_,C,_)|RestoVecinos]):-
    frontera(nodo(E,_,C1,_)),
    C>=C1,!,
    agregar(RestoVecinos).
agregar([nodo(E,_,C,_)|RestoVecinos]):-
    visitado(nodo(E,_,C1,_)),
    C>=C1,!,
    agregar(RestoVecinos).
agregar([nodo(E,L,C,F)|RestoVecinos]):-
    assertz(frontera(nodo(E,L,C,F))),
    agregar(RestoVecinos).
/*
    generarVecinos(nodo(Estado,Camino,Costo),Vecinos)
        dado un Nodo, genera todos aquellos vecinos con los que se relacione (con el operador opera)
        y calcula el costo del nuevo nodo
*/
generarVecinos(nodo(Estado,Camino,CostoViejo,_),Metas,Vecinos):-
    findall(nodo(EstadoNuevo,[Operador|Camino],CostoNuevo,FNueva,Metas), 
        (sucesor(Estado,EstadoNuevo,Operador,CostoActual),
        CostoNuevo is CostoActual + CostoViejo, /*h(X,HNueva),*/ FNueva is CostoNuevo/** + HNueva*/), 
        Vecinos).

buscarMenor(nodo(E,L,C,F),Menor):-
    frontera(nodo(_,_,_,Fn)),
    Fn < Menor,
    buscarMenor(nodo(E,L,C,F),Fn),!.

buscarMenor(E,L,C,Fn,Fn):-
    frontera(nodo(E,L,C,Fn)).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es firme
        + no hay refugio en la posicion siguiente
*/
sucesor([[FilAct,ColAct],n,Poseciones],[[FilSig,ColSig],n,Poseciones],avanzar,1):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Poseciones],[[FilSig,ColSig],o,Poseciones],avanzar,1):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Poseciones],[[FilSig,ColSig],s,Poseciones],avanzar,1):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Poseciones],[[FilSig,ColSig],e,Poseciones],avanzar,1):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([r,_,_],[FilSig,ColSig]).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es resbaladiza
        + no hay refugio en la posicion siguiente
*/
sucesor([[FilAct,ColAct],n,Poseciones],[[FilSig,ColSig],n,Poseciones],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Poseciones],[[FilSig,ColSig],o,Poseciones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Poseciones],[[FilSig,ColSig],s,Poseciones],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Poseciones],[[FilSig,ColSig],e,Poseciones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([r,_,_],[FilSig,ColSig]).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es firme
        + hay refugio en la posicion siguiente
            + que no requiere llave
*/
sucesor([[FilAct,ColAct],n,Poseciones],[[FilSig,ColSig],n,Poseciones],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Poseciones],[[FilSig,ColSig],o,Poseciones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Poseciones],[[FilSig,ColSig],s,Poseciones],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Poseciones],[[FilSig,ColSig],e,Poseciones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([r,_,no],[FilSig,ColSig]).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es resbaladiza
        + hay refugio en la posicion siguiente
            + que no requiere llave
*/
sucesor([[FilAct,ColAct],n,Poseciones],[[FilSig,ColSig],n,Poseciones],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Poseciones],[[FilSig,ColSig],o,Poseciones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Poseciones],[[FilSig,ColSig],s,Poseciones],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Poseciones],[[FilSig,ColSig],e,Poseciones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([r,_,no],[FilSig,ColSig]).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es firme
        + hay refugio en la posicion siguiente
            + que requiere llave
            + tiene pala
*/
sucesor([[FilAct,ColAct],n,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],n,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].    
sucesor([[FilAct,ColAct],o,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],o,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].
sucesor([[FilAct,ColAct],s,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],s,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].
sucesor([[FilAct,ColAct],e,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],e,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es firme
        + hay refugio en la posicion siguiente
            + que requiere llave
            + no tiene pala
*/
sucesor([[FilAct,ColAct],n,Llaves],[[FilSig,ColSig],n,LlavesNuevo],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],o,Llaves],[[FilSig,ColSig],o,LlavesNuevo],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],s,Llaves],[[FilSig,ColSig],s,LlavesNuevo],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],e,Llaves],[[FilSig,ColSig],e,LlavesNuevo],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].

sucesor([Posicion,n,Poseciones],CostoActual,[Posicion,o,Poseciones],girar(o),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.
sucesor([Posicion,n,Poseciones],CostoActual,[Posicion,s,Poseciones],girar(s),CostoSiguiente):-
    CostoSiguiente is CostoActual + 2.
sucesor([Posicion,n,Poseciones],CostoActual,[Posicion,e,Poseciones],girar(e),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.
sucesor([Posicion,o,Poseciones],CostoActual,[Posicion,s,Poseciones],girar(s),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.
sucesor([Posicion,o,Poseciones],CostoActual,[Posicion,e,Poseciones],girar(e),CostoSiguiente):-
    CostoSiguiente is CostoActual + 2.
sucesor([Posicion,o,Poseciones],CostoActual,[Posicion,n,Poseciones],girar(n),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.
sucesor([Posicion,s,Poseciones],CostoActual,[Posicion,e,Poseciones],girar(e),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.
sucesor([Posicion,s,Poseciones],CostoActual,[Posicion,n,Poseciones],girar(n),CostoSiguiente):-
    CostoSiguiente is CostoActual + 2.
sucesor([Posicion,s,Poseciones],CostoActual,[Posicion,o,Poseciones],girar(o),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.
sucesor([Posicion,e,Poseciones],CostoActual,[Posicion,n,Poseciones],girar(n),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.
sucesor([Posicion,e,Poseciones],CostoActual,[Posicion,o,Poseciones],girar(o),CostoSiguiente):-
    CostoSiguiente is CostoActual + 2.
sucesor([Posicion,e,Poseciones],CostoActual,[Posicion,s,Poseciones],girar(s),CostoSiguiente):-
    CostoSiguiente is CostoActual + 1.



esMeta([[Fila,Columna],_,[[p,_]|_]],Metas,[Fila,Columna]):-member([Fila,Columna],Metas).