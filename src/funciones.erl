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
-export([progra/2,generar/3]).
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