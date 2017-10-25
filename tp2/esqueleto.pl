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
puedoColocar(1, Direccion, Tablero, Fila, Columna) :- 
        esDireccion(Direccion), disponible(Tablero, Fila, Columna).
puedoColocar(CantPiezas, Direccion, Tablero, Fila, Columna) :- 
        CantPiezas > 1, disponible(Tablero, Fila, Columna), siguientePosicionEnDireccion(Direccion, Fila, Columna, X, Y),
        PiezasRestantes is CantPiezas - 1, puedoColocar(PiezasRestantes, Direccion, Tablero, X, Y).

%esDireccion(?Direccion)
esDireccion(Direccion) :- member(Direccion, [horizontal, vertical]).

%siguientePosicionEnDireccion(?Direccion, +Fila, +Columna, -SigFila, -SigColumna)
siguientePosicionEnDireccion(horizontal, Fila, Columna, SigFila, SigColumna) :- SigFila is Fila, SigColumna is Columna + 1.
siguientePosicionEnDireccion(vertical, Fila, Columna, SigFila, SigColumna) :- SigFila is Fila + 1, SigColumna is Columna.

%%% Ejercicio 4 %%%

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([], _).
ubicarBarcos([Barco | Barcos], Tablero) :- 
        puedoColocar(Barco, Direccion, Tablero, Fila, Columna), 
        colocarBarco(Barco, Direccion, Tablero, Fila, Columna), ubicarBarcos(Barcos, Tablero).

%colocarBarco(+PiezasBarco, ?Direccion, +?Tablero, +Fila, +Columna)
/* Este caso se debe evaluar particularmente para no generar repetidos colocando barcos de tamaño 1 */
/* Este predicado ASUME que se pueden colocar las piezas de los barcos en las posiciones pedidas */
colocarBarco(1, horizontal, Tablero, Fila, Columna) :- contenido(Tablero, Fila, Columna, o).
colocarBarco(PiezasBarco, Direccion, Tablero, Fila, Columna) :-
        PiezasBarco > 1, contenido(Tablero, Fila, Columna, o),
        siguientePosicionEnDireccion(Direccion, Fila, Columna, SigFila, SigColumna), PiezasRestantes is PiezasBarco - 1,
        colocarBarco(PiezasRestantes, Direccion, Tablero, SigFila, SigColumna).

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
golpear([], _, _, NuevoTab) :- NuevoTab = [].
golpear([FilaGolpear | Filas], 1, NumColumna, NuevoTab) :- 
        golpearFila(FilaGolpear, NumColumna, FilaGolpeada), NuevoTab = [FilaGolpeada | Filas].
golpear([Fila | Filas], NumFila, NumColumna, NuevoTab) :- 
        NumFila > 1, ProximaFila is NumFila - 1, golpear(Filas, ProximaFila, NumColumna, TableroGolpeado),
        NuevoTab = [Fila | TableroGolpeado].

%golpearFila(+FilaGolpear, +NumColumna, -NuevaFila).
golpearFila([], _, NuevaFila) :- NuevaFila = [].
golpearFila([_ | Elementos], 1, NuevaFila) :- NuevaFila = ['~' | Elementos].
golpearFila([Elemento | Elementos], NumColumna, NuevaFila) :- 
        NumColumna > 1, ProximaColumna is NumColumna - 1, golpearFila(Elementos, ProximaColumna, FilaGolpeada),
        NuevaFila = [Elemento | FilaGolpeada].

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
esBarco(Tablero, Fila, Columna) :-
        contenido(Tablero, Fila, Columna, Contenido), Contenido == o.

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(3) :- Tablero = [[o,~],[~,~],[o,o]], golpear(Tablero,1,1,NuevoTab), NuevoTab = [[~,~],[~,~],[o,o]].
test(4) :- Tablero = [[o,~],[~,~],[o,o]], atacar(Tablero,1,2,Resultado,NuevoTab), Resultado = 'agua'.
test(5) :- Tablero = [[o,~],[~,~],[~,o]], atacar(Tablero,3,2,Resultado,NuevoTab), Resultado = 'hundido'.
test(6) :- Tablero = [[o,~],[~,~],[o,o]], atacar(Tablero,3,1,Resultado,NuevoTab), Resultado = 'tocado'.
tests :- forall(between(1,6,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.