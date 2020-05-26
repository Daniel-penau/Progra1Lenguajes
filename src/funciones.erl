%%%-------------------------------------------------------------------
%%% @author Dydrey
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. may. 2020 15:54
%%%-------------------------------------------------------------------
-module(funciones).
-author("Dydrey").

%% API
-export([progra/2, generar/3, valorar/3, seleccionar/2, mutar/1, quitar_v/2, cruzar/2]).
%([[a,b],[b,c],[a,d],[c,d]],2,10).
%Funcion que recibe un grafo y cantidad de colores y devuelve una lista con tuplas de nodo y color
%G = Grafo, N = Cantidad de colores
progra([],_N) -> 0;
progra(G, N)-> M = eliminaRep(G), prograAux(M,N).

prograAux([], _N) -> [];
prograAux([H|T],N) -> [{H,rand:uniform(N)}]++prograAux(T,N).

%Elimina los repetidos del grafo y ademas, hace la lista plana
%L = Lista
eliminaRep(L) -> lists:usort(lists:flatten(L)).

%Funcion que recibe un grafo, la cantidad de colores y un tamano de la poblacion
%G=Grafo [[a,b],[a,b],[a,b]], C=Cantidad de colores T= Tamano de la poblacion L=lista de tuplas [{a,1},{b,2},{d,3}]
generar([],_C,_T)->[];
generar(_G,_C,0)->[];
generar(G,C,T)->L=progra(G,C), generaAux(G,C,T,L).

%Funcion auxiliar de generar
generaAux(_G,_C,0,_L)->[];
generaAux(G,C,T,L)when T-length(L)>=0 ->NL=progra(G,C), NT=T-length(L), L++generaAux(G,C,NT,NL);
generaAux(G,C,T,L)when T-length(L)<0 -> NL=lists:droplast(L), generaAux(G,C,T,NL).

%Funcion auxiliar de valorar, Convierte el G en un grafo dirigido
conver([]) -> [];
conver([H|T]) -> [H | [X || X <- conver(T), X /= lists:reverse(H)]].

%Funcion que se encarga de valorar las soluciones
%G = Grafo, S = lista de Posible Soluciones, L = Nueva lista
valorar(_G,[],L) -> L;
valorar(G,[H|T],L)-> valorar(G,T,L ++ [{H,valorar1(G,H)}]).
%Funcion auxiliar de valorar
valorar1([],_S) -> 0;
valorar1([[H1,H2]|T],S)-> valorar2(conver([[H1,H2]|T]),S,buscolor(H1,S),buscolor(H2,S)).
%Funcion auxiliar de valorar1
valorar2([_H|T],S,Color1,Color2)when Color1 == Color2 -> 1+valorar1(T,S);
valorar2([_H|T],S,Color1,Color2)when Color1 /= Color2  -> valorar1(T,S).

%Funcion que se encarga de buscar el color de un nodo
% N = Nodo, [] = Posible solucion
buscolor(_N,[]) -> 0;
buscolor(N,[{N, Color}| _T]) -> Color;
buscolor(N, [_H| T]) -> buscolor(N, T).

%Funcion que se encarga de Seleccionar las T mejores Soluciones
seleccionar(S,T)-> element(1,lists:split(T,lists:keysort(2,S))).

%Funcion que se encarga de mutar una solucion
%S = una posible solucion
mutar(S)-> mutar1(S,lists:nth(rand:uniform(length(S)), S),lists:nth(rand:uniform(length(S)), S)).
%Funcion auxiliar de mutar
mutar1(S,{H1,T1},{H2,T2})when {H1,T1}/= {H2,T2} ->inter_c( inter_c(S,pos_el({H1,T1},S)+1,{H1,T2}),
  pos_el({H2,T2},S)+1,{H2,T1});
mutar1(S,{H1,T1},{H2,T2})when {H1,T1} == {H2,T2} -> mutar(S).

%Funcion que recibe 1 listas y las cruza para crear una lista nueva.
cruzar([],[])->[];
cruzar([[]],[[]])->[];
cruzar([H1|T1],[H2|T2])->[lists:nth(rand:uniform(2), [H1] ++ [H2])] ++ cruzar(T1,T2).

algGenetico([])->[];
algGenetico([[]])->[];
algGenetico(L)-> mutar(cruzar(lists:nth(rand:uniform(length(L)), L), lists:nth(rand:uniform(length(L)), L))).

%Funcion que me devuelve la posicion de un elemento en una lista
% E = elemento, [E|_T] = Lista
pos_el(E,[E|_T]) -> 0;
pos_el(E,[_H|T]) -> 1+ pos_el(E,T).

%Funcion que intercambia valores en una lista
%L =Lista, Index = posicion del elemento a intercambiar, N_Valor = Nuevo elemento
inter_c(L,Index,N_Valor) ->
  {L1,[_|L2]} = lists:split(Index-1,L),
  L1++[N_Valor|L2].

%Funcion que quita los el valor de las mejores soluciones
%[{H1,_H2}|T] = Lista con las mejores soluciones y su valor, L = Nueva lista

quitar_v([],L)-> L;
quitar_v([{H1,_H2}|T],L) -> quitar_v(T,L++[H1]).

%Verifica si la solucion tiene cero colisiones
mejor_s({S,V}) when V == 0 -> true.

%Funcion para generar los hilos

generaHilos(Server,G,P,Cant)->spawn( fun()-> Server ! algo_gene(G,P) end ),
  generaHilos(Server,G,P,Cant-1).

%G = Grafo, P = Poblacion, S = mejor Solucion, T = # de Poblacion_I
%R = 10000 de Repeticiones, B = bandera de Mejor solucion

%prin(G,P,S,T,R,B)when R == 0 -> S;
%prin(G,P,S,T,R,B)when B == true -> S.



