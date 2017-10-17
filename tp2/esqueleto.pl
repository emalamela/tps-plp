%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Predicados a definir:------------------%

%%% Ejercicio 1 %%%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(Tablero, Fila, Columna, Contenido) :- 
        enRango(Tablero,Fila,Columna),
        nth1(Fila, Tablero, FilaTablero), nth1(Columna, FilaTablero, Contenido).

%%% Ejercicio 2 %%%

%disponible(+Tablero, ?Fila, ?Columna)
disponible(Tablero, Fila, Columna) :- 
        posicionLibre(Tablero, Fila, Columna), 
        forall(adyacenteEnRango(Tablero, Fila, Columna, FilaAdy, ColumnaAdy), posicionLibre(Tablero, FilaAdy, ColumnaAdy)).

%posicionLibre(+?Tablero, ?Fila, ?Columna)
posicionLibre(Tablero, Fila, Columna) :- contenido(Tablero, Fila, Columna, Cont), Cont \== o.

%%% Ejercicio 3 %%%

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(1, _, Tablero, Fila, Columna) :- disponible(Tablero, Fila, Columna).
puedoColocar(CantPiezas, Direccion, Tablero, Fila, Columna) :- 
        CantPiezas > 1, disponible(Tablero, Fila, Columna), siguientePosicionEnDireccion(Direccion, Fila, Columna, X, Y),
        PiezasRestantes is CantPiezas - 1, puedoColocar(PiezasRestantes, Direccion, Tablero, X, Y).

%siguientePosicionEnDireccion(?Direccion, +Fila, +Columna, -SigFila, -SigColumna)
siguientePosicionEnDireccion(horizontal, Fila, Columna, SigFila, SigColumna) :- SigFila is Fila, SigColumna is Columna + 1.
siguientePosicionEnDireccion(vertical, Fila, Columna, SigFila, SigColumna) :- SigFila is Fila + 1, SigColumna is Columna.

%%% Ejercicio 4 %%%

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([], _).
ubicarBarcos([Barco | Barcos], Tablero) :- colocarBarco(Barco, Tablero), ubicarBarcos(Barcos, Tablero).

%colocarBarco(+Barco, +?Tablero)
colocarBarco(Barco, Tablero) :-
        setof((X, Y, D), puedoColocar(Barco, D, Tablero, X, Y), ConfiguracionesDisponibles),
        member((X, Y, D), ConfiguracionesDisponibles), colocarBarcoEnDireccion(Barco, D, Tablero, X, Y).

%colocarBarcoEnDirecion(+PiezasBarco, ?Direccion, +?Tablero, +Fila, +Columna)
colocarBarcoEnDireccion(1, _, Tablero, Fila, Columna) :- contenido(Tablero, Fila, Columna, o).
colocarBarcoEnDireccion(PiezasBarco, Direccion, Tablero, Fila, Columna) :-
        PiezasBarco > 1, contenido(Tablero, Fila, Columna, o),
        siguientePosicionEnDireccion(Direccion, Fila, Columna, SigFila, SigColumna), PiezasRestantes is PiezasBarco - 1,
        colocarBarcoEnDireccion(PiezasRestantes, Direccion, Tablero, SigFila, SigColumna).

%%% Ejercicio 5 %%%

%completarConAgua(+?Tablero)
completarConAgua(Tablero) :- maplist(completarFilaConAgua, Tablero).

%completarFilaConAgua(?FilaTablero)
completarFilaConAgua([]).
completarFilaConAgua([ElementoTablero | FilaTablero]) :- ElementoTablero == o, completarFilaConAgua(FilaTablero).
completarFilaConAgua([ElementoTablero | FilaTablero]) :- 
        ElementoTablero \== o, ElementoTablero = '~', completarFilaConAgua(FilaTablero).

%%% Ejercicio 6 %%%
%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(Tablero , NumFila , NumColumna , NuevoTab) :-
        enRango(Tablero,NumFila,NumColumna),
        eliminarElemento(Tablero,NumFila,NumColumna,1,NuevoTab).

%ContadorFila debe venir en 1
%eliminarElemento(+Tablero,+NumFila,+NumColumna,+ContadorFila,?NuevoTab)
eliminarElemento([],_,_,_,[]).
eliminarElemento([Fila|Filas],NumFila,NumColumna,NumFila,[NFila|NFilas]) :- 
        eliminarElementoDeFila(Fila,NumColumna,1,NFila),
        FilaSiguiente is NumFila + 1,
        eliminarElemento(Filas,NumFila,NumColumna,FilaSiguiente,NFilas).
eliminarElemento([Fila|Filas],NumFila,NumColumna,ContadorFila,[Fila|NFilas]) :-
        ContadorFila \= NumFila,
        FilaSiguiente is ContadorFila + 1,
        eliminarElemento(Filas,NumFila,NumColumna,FilaSiguiente,NFilas).

%ContadorColumna debe venir en 1
%eliminarElementoDeFila(+Fila,+NumColumna,+ContadorColumna,?NuevaFila)
eliminarElementoDeFila([],_,_,[]).
eliminarElementoDeFila([Columna|Columnas],NumColumna,NumColumna,['~'|NColumnas]) :- 
        ColumnaSiguiente is NumColumna + 1,
        eliminarElementoDeFila(Columnas,NumColumna,ColumnaSiguiente,NColumnas).
eliminarElementoDeFila([Columna|Columnas],NumColumna,ContadorColumna,[Columna|NColumnas]) :-
        ContadorColumna \= NumColumna,
        ColumnaSiguiente is ContadorColumna + 1,
        eliminarElementoDeFila(Columnas,NumColumna,ColumnaSiguiente,NColumnas).

%%% Ejercicio 7 y 8 %%%
% Completar instanciación soportada y justificar.
%atacar(+Tablero, +Fila, +Columna, -Resultado, -NuevoTab)
%Ni Tablero ni Fila ni Columna son reversibles, pues en enRango no lo son, y golpear utiliza enRango.
%NuevoTab tampoco puede ser reversible porque en golpear no lo es
atacar(Tablero,Fila,Columna,'agua',NuevoTab) :- 
        golpear(Tablero,Fila,Columna,NuevoTab),
        Tablero = NuevoTab.
atacar(Tablero,Fila,Columna,'hundido',NuevoTab) :- 
        golpear(Tablero,Fila,Columna,NuevoTab),
        Tablero \= NuevoTab,
        forall(adyacenteEnRango(Tablero, Fila, Columna, FilaAdy, ColumnaAdy), not(esBarco(NuevoTab,FilaAdy,ColumnaAdy))).
atacar(Tablero,Fila,Columna,'tocado',NuevoTab) :- 
        golpear(Tablero,Fila,Columna,NuevoTab),
        Tablero \= NuevoTab,
        not(forall(adyacenteEnRango(Tablero, Fila, Columna, FilaAdy, ColumnaAdy), not(esBarco(NuevoTab,FilaAdy,ColumnaAdy)))).

%esBarco(+Tablero,?Fila,?Columna)
esBarco(Tablero,Fila,Columna) :-
        contenido(Tablero,Fila,Columna,Contenido), Contenido == o.
%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(3) :- Tablero = [[o,~],[~,~],[o,o]], golpear(Tablero,1,1,NuevoTab), NuevoTab = [[~,~],[~,~],[o,o]].
test(4) :- Tablero = [[o,~],[~,~],[o,o]], atacar(Tablero,1,2,Resultado,NuevoTab), Resultado = 'agua'.
test(5) :- Tablero = [[o,~],[~,~],[~,o]], atacar(Tablero,3,2,Resultado,NuevoTab), Resultado = 'hundido'.
test(6) :- Tablero = [[o,~],[~,~],[o,o]], atacar(Tablero,3,1,Resultado,NuevoTab), Resultado = 'tocado'.
tests :- forall(between(1,6,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.