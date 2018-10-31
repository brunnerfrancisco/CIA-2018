:-consult('CIA2018-Proyecto2-BusquedaInformada-Prolog/islaExample.pl').
:-consult('sucesores.pl').
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
    celda(Meta,firme),
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

buscarMenor(nodo(E,L,C,MenorF)):-
    frontera(nodo(E,L,C,MenorF)),
    not((
        frontera(nodo(_OtraE,_OtraL,_OtraC,OtraF)),
        MenorF > OtraF
    )),!.

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
        Vecinos).

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

esMeta([[Fila,Columna],_,[[p,_]|_]]):-meta([Fila,Columna]).

eliminar([l,_,_],[],[]):-!.
eliminar([l,NombreLlave,Accesos],[[l,NombreLlave,Accesos]|Resto],Resto):-!.
eliminar([l,NombreLlave,Accesos],[[l,NombreLlave2,Accesos2]|Resto],RestoAux):-
    NombreLlave\=NombreLlave2,
    Accesos\=Accesos2,
    eliminar([l,NombreLlave,Accesos],Resto,RestoAux).