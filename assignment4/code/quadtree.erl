%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013
%%% Updated : Aug 2013
%%% Usage   : Assignemnt for Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student : [ADD HERE]
%%% KU-ID   : [ADD HERE]
%%%---------------------------------------------------------------------
%%% Student : [ADD HERE]
%%% KU-ID   : [ADD HERE]
%%%---------------------------------------------------------------------

-module(quadtree).

-export([start/2, stop/1, addElement/3]).%, mapFunction/3, mapTreeFunction/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts a quadtree with 
%   Bound: Eucledean size of tree
%   Limit: Maximum elements in each leaf
start(Bound, Limit) ->
	{ok, spawn(fun() -> quadtreeCoordinatorInit(Bound, Limit) end)}.

stop(Qtree) ->
	send_stop(Qtree).

% Adds and element 
addElement(Qtree, Pos, Property) ->
	send_add(Qtree, {element, Pos, Property}).

% mapFunction(Qtree, MapFun, Bound) ->
% 	....

% mapTreeFunction(Qtree, MapTreeFun) ->
% 	....

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Asynchronous communication
info(Pid, Msg) ->
	Pid ! Msg.

send_add(Pid, Element) ->
	info(Pid, {addElement,Element}).

send_mapTree(Pid, MapTreeFun) ->
	info(Pid, {mapTreeFun, MapTreeFun}).

send_mapFun(Pid, MapFun, MapBound) ->
	info(Pid, {mapFun, MapFun, MapBound}).

send_stop(Pid) ->
    info(Pid, stop).

%% synchronous communication
rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
	{Pid, Response} ->
		Response
	end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Quadtree Coordinator
quadtreeCoordinatorInit(Bound, Limit) ->
	Pid = self(),
	Leaf = spawn(fun() -> qtreeLeaf(Bound, Limit, Pid, []) end),
	quadtreeCoordinator(Leaf, Bound).

quadtreeCoordinator (QtreeTop, Bound) ->
	io:format("~p is coordinator for ~p with bound ~p~n", [self(), QtreeTop, Bound]),
 	receive
 		stop ->
 			send_stop(QtreeTop),
 			ok;
 		{addElement, Element} ->
 			case outOfBound(Bound, Element) of
 				false -> send_add(QtreeTop, Element);
 				true -> ok
 			end,
 			quadtreeCoordinator(QtreeTop, Bound)
 	end.


%% Quadtree node
qtreeNode(Bound, Limit, Parent, Children) ->
	io:format("~p is node with parent ~p, bound ~p and children ~p~n", [self(), Parent, Bound, Children]),
	receive
		stop ->
			lists:map(fun send_stop/1, Children),
			ok;
		{addElement, Element} ->
			case outOfBound(Bound, Element) of
				false -> lists:map(fun(C) -> send_add(C, Element) end, Children);
				true -> ok
			end,
			qtreeNode(Bound, Limit, Parent, Children)
	end.


%% Quadtree leaf
qtreeLeaf(Bound, Limit, Parent, Data) ->
	io:format("~p is leaf with parent ~p and bound ~p and data ~p~n", [self(), Parent, Bound, Data]),
	receive
		stop ->
			ok;
		{addElement, Element} ->
			case {outOfBound(Bound, Element), length(Data) < Limit} of
				{false, true} ->
					qtreeLeaf(Bound, Limit, Parent, [Element | Data]);
				{false, false} ->
					NewBounds = lists:map(fun(Quarter) -> createBound(Quarter, Bound) end, qTreeQuarters()),
					NewParent = self(),
					Children = [spawn(fun() -> qtreeLeaf(NewBound, Limit, NewParent, []) end) || NewBound <- NewBounds],
					
					NewData = [Element | Data],
					DataList = [[E || E <- NewData, not outOfBound(B, E)] || B <- NewBounds],
					ZippedList = lists:zip(Children, DataList),
					lists:map(fun({C, Es}) -> lists:map(fun(E) -> send_add(C,E) end, Es) end, ZippedList),
					qtreeNode(Bound, Limit, Parent, Children);
				{true, _} ->
					qtreeLeaf(Bound, Limit, Parent, Data)
			end
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following is suggestions for useful helpler function,
% which can make your loops easier to follow.

% Organization of quadtree:
% +---------+-- -> X
% | NW | NE |
% |----+----|
% | SW | SE |
% +---------+
% |
% v
% Y

% List with quarters in the quadtree.
qTreeQuarters() -> [nw,ne,sw,se].

% Returns the 1/4 of Bound which is defined by Quarter from the list above.
createBound(_, universe) -> universe;
createBound(_, empty) -> empty;
createBound(Quarter,Bound) -> 
	{X1, Y1, X2, Y2} = Bound,
	Xmid = X1 + ((X2 - X1) / 2),
	Ymid = Y1 + ((Y2 - Y1) / 2),
	case Quarter of
		nw -> {X1,   Y1,   Xmid, Ymid};
		ne -> {Xmid, Y1,   X2,   Ymid};
		sw -> {X1,   Ymid, Xmid, Y2  };
		se -> {Xmid, Ymid, X2,   Y2  }
	end.

% Predicate that returns if an Element is outside Bound.
outOfBound(universe, _) -> false;
outOfBound(empty, _) -> true;
outOfBound(Bound, Element) ->
	{X1, Y1, X2, Y2} = Bound,
	{element, Pos, _} = Element,
	{X,Y} = Pos,
	(X < X1) or (X >= X2) or (Y < Y1) or (Y >= Y2).

% Finds the intersection between Bound1 and Bound2.
intersectBounds(universe, Bound)    -> Bound;
intersectBounds(Bound,    universe) -> Bound;
intersectBounds(empty,    _    )    -> empty;
intersectBounds(_,        empty)    -> empty;
intersectBounds(Bound,    Bound)    -> Bound; 
intersectBounds(Bound1,   Bound2)   ->
	{X1_1, Y1_1, X1_2, Y1_2} = Bound1,
	{X2_1, Y2_1, X2_2, Y2_2} = Bound2,
	X1 = lists:max([X1_1, X2_1]),
	Y1 = lists:max([Y1_1, Y2_1]),
	X2 = lists:min([X1_2, X2_2]),
	Y2 = lists:min([Y1_2, Y2_2]),
	case (X1 >= X2) or (Y1 >= Y2) of
		true  -> empty;
		false -> {X1, Y1, X2, Y2}
	end.
