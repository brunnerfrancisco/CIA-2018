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

/*
    Defino los predicados dinamicos a ser utilizados
*/
:-dynamic frontera/1, visitado/1, tupla/3, meta/1.

/*
    Activo este flag para que a la hora de hacer las consultas por la consola
        muestre TODOS los elementos de las listas
*/
:-set_prolog_flag(answer_write_options,[max_depth(0)]).

buscar_plan(EstadoInicial,Metas,Destino,Plan,Costo):-
    retractall(frontera(_)),
    retractall(visitado(_)),
    retractall(tupla(_,_,_)),
    retractall(meta(_)),
    agregarTuplasPalaMeta(Metas),
    buscarHeuristica(EstadoInicial,Heuristica),
    assert(frontera(nodo(EstadoInicial,[],0,Heuristica))),
    buscarAE(Destino,Plan,Costo),!.

buscar_plan(_,_,_,_,_):-
    nl,write('No es posible hallar un plan'),nl,
    fail.

agregarTuplasPalaMeta(Metas):-
    agregarMetas(Metas),
    findall(
        tupla([FilaPala,ColumnaPala],[FilaMeta,ColumnaMeta],Distancia),
        (estaEn([p,_],[FilaPala,ColumnaPala]),
        meta([FilaMeta,ColumnaMeta]),
        Distancia is abs(FilaPala - FilaMeta) + abs(ColumnaPala - ColumnaMeta)),
        Tuplas
    ),
    agregarTuplas(Tuplas).

agregarMetas([]):-!.
agregarMetas([Meta|RestoMetas]):-
    \+celda(Meta,firme),
    \+estaEn([o,_,_],Meta),!,
    assertz(meta(Meta)),
    agregarMetas(RestoMetas).
agregarMetas([Meta|RestoMetas]):-
    celda(Meta,resbaladizo),
    \+estaEn([o,_,_],Meta),!,
    assertz(meta(Meta)),
    agregarMetas(RestoMetas).
agregarMetas([_|RestoMetas]):-
    agregarMetas(RestoMetas).

agregarTuplas([]):-!.
agregarTuplas([Tupla|RestoTuplas]):-
    assertz(Tupla),
    agregarTuplas(RestoTuplas).

buscarAE(Destino,Solucion,Costo):-
    seleccionar(nodo(Estado,Camino,Costo,_)),
    esMeta(Estado),!,
    Estado=[Destino,_,_],
    reverse(Camino,Solucion).
buscarAE(Destino,Solucion,Costo):-
    seleccionar(Nodo),
    assertz(visitado(Nodo)),
    retract(frontera(Nodo)),
    generarVecinos(Nodo,Vecinos),
    agregar(Vecinos),
    buscarAE(Destino,Solucion,Costo).

/*
    seleccionar(Nodo):
        selecciona el primer nodo de la frontera, es decir busca la primera unificacion con algun hecho 
        con la estructura frontera(nodo(Estado,Camino,Costo)).
        agrega el nodo a visitados.
*/
seleccionar(nodo(Estado,Camino,Costo,Fn)):-
    buscarMenor(nodo(Estado,Camino,Costo,Fn)),!.

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
generarVecinos(nodo(EstadoActual,Camino,CostoViejo,_Fn),Vecinos):-
    findall(nodo(EstadoNuevo,[Operador|Camino],CostoNuevo,FNueva), 
        (
            sucesor(EstadoActual,EstadoNuevo,Operador,CostoActual),
            CostoNuevo is CostoActual + CostoViejo, 
            buscarHeuristica(EstadoNuevo,HeuristicaNueva), 
            FNueva is CostoNuevo + HeuristicaNueva
        ), 
        Vecinos
    ).

buscarMenor(nodo(E,L,C,MenorF)):-
    frontera(nodo(E,L,C,MenorF)),
    not((
        frontera(nodo(_OtraE,_OtraL,_OtraC,OtraF)),
        MenorF > OtraF
    )),!.

buscarHeuristica([[Fila,Columna],_,[[p,_]|_]],Heuristica):-!,
    buscarMenorMeta([Fila,Columna],Heuristica).

buscarHeuristica([[Fila,Columna],_,_],Heuristica):-
    buscarMenorMetaPala([Fila,Columna],Heuristica).

buscarMenorMeta([Fila,Columna],MenorHeuristica):-
    meta([FilaMeta,ColumnaMeta]),
    MenorHeuristica is abs(Fila - FilaMeta) + abs(Columna - ColumnaMeta),
    not((
        meta([FilMetaOtra,ColMetaOtra]),
        HeuristicaOtra is abs(Fila - FilMetaOtra) + abs(Columna - ColMetaOtra),
        MenorHeuristica > HeuristicaOtra
    )),!.

buscarMenorMetaPala([Fila,Columna],MenorHeuristica):-
    tupla([FilPala,ColPala],[_FilMeta,_ColMeta],DistanciaPalaMeta),
    MenorHeuristica is abs(Fila - FilPala) + abs(Columna - ColPala) + DistanciaPalaMeta,
    not((
        tupla([FilPalaOtra,ColPalaOtra],[_FilMetaOtra,_ColMetaOtra],DistanciaOtra),
        HeuristicaOtra is abs(Fila - FilPalaOtra) + abs(Columna - ColPalaOtra) + DistanciaOtra,
        MenorHeuristica > HeuristicaOtra
    )),!.

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es firme
        + no hay refugio en la posicion siguiente
*/
sucesor([[FilAct,ColAct],n,Posesiones],[[FilSig,ColSig],n,Posesiones],avanzar,1):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Posesiones],[[FilSig,ColSig],o,Posesiones],avanzar,1):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Posesiones],[[FilSig,ColSig],s,Posesiones],avanzar,1):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Posesiones],[[FilSig,ColSig],e,Posesiones],avanzar,1):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es resbaladiza
        + no hay refugio en la posicion siguiente
*/
sucesor([[FilAct,ColAct],n,Posesiones],[[FilSig,ColSig],n,Posesiones],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Posesiones],[[FilSig,ColSig],o,Posesiones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Posesiones],[[FilSig,ColSig],s,Posesiones],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Posesiones],[[FilSig,ColSig],e,Posesiones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es firme
        + hay refugio en la posicion siguiente
            + que no requiere llave
*/
sucesor([[FilAct,ColAct],n,Posesiones],[[FilSig,ColSig],n,Posesiones],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Posesiones],[[FilSig,ColSig],o,Posesiones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Posesiones],[[FilSig,ColSig],s,Posesiones],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Posesiones],[[FilSig,ColSig],e,Posesiones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,no],[FilSig,ColSig]).

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es resbaladiza
        + hay refugio en la posicion siguiente
            + que no requiere llave
*/
sucesor([[FilAct,ColAct],n,Posesiones],[[FilSig,ColSig],n,Posesiones],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Posesiones],[[FilSig,ColSig],o,Posesiones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Posesiones],[[FilSig,ColSig],s,Posesiones],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,no],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Posesiones],[[FilSig,ColSig],e,Posesiones],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
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
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].    
sucesor([[FilAct,ColAct],o,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],o,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].
sucesor([[FilAct,ColAct],s,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],s,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].
sucesor([[FilAct,ColAct],e,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],e,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].
/**/
/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es resbaladiza
        + hay refugio en la posicion siguiente
            + que requiere llave
            + tiene pala
*/
sucesor([[FilAct,ColAct],n,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],n,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].    
sucesor([[FilAct,ColAct],o,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],o,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].
sucesor([[FilAct,ColAct],s,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],s,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],RestoLlaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],RestoLlaves,RestoLlavesAux),
    RestoLlavesNuevo = [[l,NombreL,AccesosLNuevo]|RestoLlavesAux].
sucesor([[FilAct,ColAct],e,[[p,NombreP]|RestoLlaves]],[[FilSig,ColSig],e,[[p,NombreP]|RestoLlavesNuevo]],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
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
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],o,Llaves],[[FilSig,ColSig],o,LlavesNuevo],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],s,Llaves],[[FilSig,ColSig],s,LlavesNuevo],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],e,Llaves],[[FilSig,ColSig],e,LlavesNuevo],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],firme),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].

/*
    avanzar - condiciones
        + existe la posicion siguiente
        + la posicion siguiente es resbaladiza
        + hay refugio en la posicion siguiente
            + que requiere llave
            + no tiene pala
*/
sucesor([[FilAct,ColAct],n,Llaves],[[FilSig,ColSig],n,LlavesNuevo],avanzar,2):-
    FilSig is FilAct - 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],o,Llaves],[[FilSig,ColSig],o,LlavesNuevo],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct - 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],s,Llaves],[[FilSig,ColSig],s,LlavesNuevo],avanzar,2):-
    FilSig is FilAct + 1,
    ColSig is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].
sucesor([[FilAct,ColAct],e,Llaves],[[FilSig,ColSig],e,LlavesNuevo],avanzar,2):-
    FilSig is FilAct,
    ColSig is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    estaEn([r,_,si],[FilSig,ColSig]),
    member([l,NombreL,AccesosL],Llaves),
    AccesosL > 0,
    AccesosLNuevo is AccesosL - 1,
    eliminar([l,NombreL,AccesosL],Llaves,LlavesAux),
    LlavesNuevo = [[l,NombreL,AccesosLNuevo]|LlavesAux].

sucesor([Posicion,n,Posesiones],[Posicion,o,Posesiones],girar(o),1).

sucesor([Posicion,n,Posesiones],[Posicion,s,Posesiones],girar(s),2).

sucesor([Posicion,n,Posesiones],[Posicion,e,Posesiones],girar(e),1).

sucesor([Posicion,o,Posesiones],[Posicion,s,Posesiones],girar(s),1).

sucesor([Posicion,o,Posesiones],[Posicion,e,Posesiones],girar(e),2).

sucesor([Posicion,o,Posesiones],[Posicion,n,Posesiones],girar(n),1).

sucesor([Posicion,s,Posesiones],[Posicion,e,Posesiones],girar(e),1).

sucesor([Posicion,s,Posesiones],[Posicion,n,Posesiones],girar(n),2).

sucesor([Posicion,s,Posesiones],[Posicion,o,Posesiones],girar(o),1).

sucesor([Posicion,e,Posesiones],[Posicion,n,Posesiones],girar(n),1).

sucesor([Posicion,e,Posesiones],[Posicion,o,Posesiones],girar(o),2).

sucesor([Posicion,e,Posesiones],[Posicion,s,Posesiones],girar(s),1).

/*
    saltar_lava
*/
sucesor([[FilAct,ColAct],n,Posesiones],[[FilSig,ColSig],n,Posesiones],saltar_lava,3):-
    FilSig is FilAct - 2,
    ColSig is ColAct,
    FilInt is FilAct - 1,
    ColInt is ColAct,
    celda([FilInt,ColInt],lava),
    celda([FilAct,ColAct],firme),
    celda([FilSig,ColSig],firme),
    \+estaEn([p,_],[FilSig,ColSig]),
    \+estaEn([l,_,_],[FilSig,ColSig]),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Posesiones],[[FilSig,ColSig],o,Posesiones],saltar_lava,3):-
    FilSig is FilAct,
    ColSig is ColAct - 2,
    FilInt is FilAct,
    ColInt is ColAct - 1,
    celda([FilInt,ColInt],lava),
    celda([FilAct,ColAct],firme),
    celda([FilSig,ColSig],firme),
    \+estaEn([p,_],[FilSig,ColSig]),
    \+estaEn([l,_,_],[FilSig,ColSig]),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Posesiones],[[FilSig,ColSig],s,Posesiones],saltar_lava,3):-
    FilSig is FilAct + 2,
    ColSig is ColAct,
    FilInt is FilAct + 1,
    ColInt is ColAct,
    celda([FilInt,ColInt],lava),
    celda([FilAct,ColAct],firme),
    celda([FilSig,ColSig],firme),
    \+estaEn([p,_],[FilSig,ColSig]),
    \+estaEn([l,_,_],[FilSig,ColSig]),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Posesiones],[[FilSig,ColSig],e,Posesiones],saltar_lava,3):-
    FilSig is FilAct,
    ColSig is ColAct + 2,
    FilInt is FilAct,
    ColInt is ColAct + 1,
    celda([FilInt,ColInt],lava),
    celda([FilAct,ColAct],firme),
    celda([FilSig,ColSig],firme),
    \+estaEn([p,_],[FilSig,ColSig]),
    \+estaEn([l,_,_],[FilSig,ColSig]),
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).

/*
    saltar_obstaculo con suelo destino firme
*/
sucesor([[FilAct,ColAct],n,Posesiones],[[FilSig,ColSig],n,Posesiones],saltar_obstaculo,4):-
    FilSig is FilAct - 2,
    ColSig is ColAct,
    FilInt is FilAct - 1,
    ColInt is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Posesiones],[[FilSig,ColSig],o,Posesiones],saltar_obstaculo,4):-
    FilSig is FilAct,
    ColSig is ColAct - 2,
    FilInt is FilAct,
    ColInt is ColAct - 1,
    celda([FilSig,ColSig],firme),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Posesiones],[[FilSig,ColSig],s,Posesiones],saltar_obstaculo,4):-
    FilSig is FilAct + 2,
    ColSig is ColAct,
    FilInt is FilAct + 1,
    ColInt is ColAct,
    celda([FilSig,ColSig],firme),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Posesiones],[[FilSig,ColSig],e,Posesiones],saltar_obstaculo,4):-
    FilSig is FilAct,
    ColSig is ColAct + 2,
    FilInt is FilAct,
    ColInt is ColAct + 1,
    celda([FilSig,ColSig],firme),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).

/*
    saltar_obstaculo con suelo destino resbaladizo
*/
sucesor([[FilAct,ColAct],n,Posesiones],[[FilSig,ColSig],n,Posesiones],saltar_obstaculo,5):-
    FilSig is FilAct - 2,
    ColSig is ColAct,
    FilInt is FilAct - 1,
    ColInt is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],o,Posesiones],[[FilSig,ColSig],o,Posesiones],saltar_obstaculo,5):-
    FilSig is FilAct,
    ColSig is ColAct - 2,
    FilInt is FilAct,
    ColInt is ColAct - 1,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],s,Posesiones],[[FilSig,ColSig],s,Posesiones],saltar_obstaculo,5):-
    FilSig is FilAct + 2,
    ColSig is ColAct,
    FilInt is FilAct + 1,
    ColInt is ColAct,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).
sucesor([[FilAct,ColAct],e,Posesiones],[[FilSig,ColSig],e,Posesiones],saltar_obstaculo,5):-
    FilSig is FilAct,
    ColSig is ColAct + 2,
    FilInt is FilAct,
    ColInt is ColAct + 1,
    celda([FilSig,ColSig],resbaladizo),
    estaEn([o,_,AlturaO],[FilInt,ColInt]),
    AlturaO < 5,
    \+estaEn([o,_,_],[FilSig,ColSig]),
    \+estaEn([r,_,_],[FilSig,ColSig]).

/*
    levantar_llave con pala
*/
sucesor([Posicion,Dir,[[p,NombreP]|Posesiones]],[Posicion,Dir,[[p,NombreP],[l,NombreL,Accesos]|Posesiones]],
    levantar_llave([l,NombreL,Accesos]),0):-
    estaEn([l,NombreL,Accesos],Posicion),
    not(member([l,NombreL,_],Posesiones)).

/*
    levantar_llave sin pala
*/
sucesor([Posicion,Dir,Posesiones],[Posicion,Dir,[[l,NombreL,Accesos]|Posesiones]],
    levantar_llave([l,NombreL,Accesos]),0):-
    estaEn([l,NombreL,Accesos],Posicion),
    not(member([l,NombreL,_],Posesiones)).

/*
    levantar_pala
*/
sucesor([Posicion,Dir,Posesiones],[Posicion,Dir,[[p,NombrePala]|Posesiones]],levantar_pala([p,NombrePala]),1):-
    estaEn([p,NombrePala],Posicion),
    not(member([p,_],Posesiones)).

esMeta([[Fila,Columna],_,[[p,_]|_]]):-meta([Fila,Columna]).

eliminar([l,_,_],[],[]):-!.
eliminar([l,NombreLlave,Accesos],[[l,NombreLlave,Accesos]|Resto],Resto):-!.
eliminar([l,NombreLlave,Accesos],[[l,NombreLlave2,Accesos2]|Resto],RestoAux):-
    NombreLlave\=NombreLlave2,
    Accesos\=Accesos2,
    eliminar([l,NombreLlave,Accesos],Resto,RestoAux).

imprimirVecinos([]):-!,nl.
imprimirVecinos([E|R]):-write(E),nl,imprimirVecinos(R).