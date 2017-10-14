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
contenido(Tablero, Fila, Columna, Contenido) :- nth1(Fila, Tablero, F), nth1(Columna, F, Contenido).

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
        setof((X, Y), puedoColocar(Barco, _, Tablero, X, Y), PosicionesDisponibles),
        member((Fila, Columna), PosicionesDisponibles), colocarBarcoEnDireccion(Barco, _, Tablero, Fila, Columna).

%colocarBarcoEnDirecion(+PiezasBarco, ?Direccion, +?Tablero, +Fila, +Columna)
colocarBarcoEnDireccion(1, _, Tablero, Fila, Columna) :- contenido(Tablero, Fila, Columna, o).
colocarBarcoEnDireccion(PiezasBarco, Direccion, Tablero, Fila, Columna) :-
        PiezasBarco > 1, contenido(Tablero, Fila, Columna, o),
        siguientePosicionEnDireccion(Direccion, Fila, Columna, SigFila, SigColumna), PiezasRestantes is PiezasBarco - 1,
        colocarBarcoEnDireccion(PiezasRestantes, Direccion, Tablero, SigFila, SigColumna).

%completarConAgua(+?Tablero)

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
tests :- forall(between(1,2,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.