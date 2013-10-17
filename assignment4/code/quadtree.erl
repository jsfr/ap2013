%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013
%%% Updated : Aug 2013
%%% Usage   : Assignemnt for Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student : Jens Fredskov
%%% KU-ID   : chw752
%%%---------------------------------------------------------------------

-module(quadtree).

-export([addElement/3, start/2, stop/1, mapFunction/3, mapTreeFunction/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts a quadtree with
%   Bound: Eucledean size of tree
%   Limit: Maximum elements in each leaf
start(Bound, Limit) ->
    {ok, spawn(fun () -> quadtreeCoordinatorInit(Bound, Limit) end)}.

stop(Qtree) ->
	send_stop(Qtree).

% Adds and element
addElement(Qtree, Pos, Property) ->
    send_add(Qtree, {element, Pos, Property}).

mapFunction(Qtree, MapFun, Bound) ->
	send_mapFun(Qtree, MapFun, Bound).

mapTreeFunction(Qtree, MapTreeFun) ->
	send_mapTree(Qtree, MapTreeFun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Asynchronous communication
info(Pid, Msg) -> Pid ! Msg.

send_add(Pid, Element) ->
    info(Pid, {addElement, Element}).

send_mapTree(Pid, MapTreeFun) ->
    info(Pid, {mapTreeFun, MapTreeFun}).

send_mapFun(Pid, MapFun, MapBound) ->
    info(Pid, {mapFun, MapFun, MapBound}).

send_stop(Pid) -> info(Pid, stop).

%% synchronous communication
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive {Pid, Response} -> Response end.

reply(From, Msg) -> From ! {self(), Msg}.

reply_ok(From) -> reply(From, ok).

reply_ok(From, Msg) -> reply(From, {ok, Msg}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Quadtree Coordinator
quadtreeCoordinatorInit(Bound, Limit) ->
    Pid = self(),
    Leaf = spawn(fun () -> qtreeLeaf(Bound, Limit, Pid, []) end),
    quadtreeCoordinator(Leaf, Bound).

quadtreeCoordinator(QtreeTop, Bound) ->
    receive
    	stop -> send_stop(QtreeTop), ok;
      	{addElement, Element} ->
	  		case outOfBound(Bound, Element) of
	    		false -> send_add(QtreeTop, Element);
	    		true -> ok
	  		end;
	  	{mapFun, MapFun, MapBound} ->
	  		case intersectBounds(MapBound, Bound) of
	  			{X1,Y1,X2,Y2} -> send_mapFun(QtreeTop, MapFun, {X1, Y1, X2, Y2});
	  			empty -> ok
	  		end;
	  	{mapTreeFun, TreeFun} -> send_mapTree(QtreeTop, TreeFun)
    end,
    quadtreeCoordinator(QtreeTop, Bound).

%% Quadtree node
qtreeNode(Bound, Limit, Parent, Children) ->
    receive
    	stop -> [send_stop(C) || {_, C} <- Children];
      	{addElement, Element} ->
	  		case outOfBound(Bound, Element) of
	    		false ->
					Bounds = [{Quarter, createBound(Quarter, Bound)} || Quarter <- qTreeQuarters()],
					[Q | _] = qTreeQuarters() -- [Q || {Q, B} <- Bounds, outOfBound(B, Element)],
					{Q, Child} = lists:keyfind(Q, 1, Children),
					send_add(Child, Element);
	    		true -> send_add(Parent, Element)
	  		end,
	  		qtreeNode(Bound, Limit, Parent, Children);
	  	{mapFun, MapFun, MapBound} -> [mapFunChild(C, MapFun, MapBound, Bound) || C <- Children];
	  	{mapTreeFun, TreeFun} ->
            TreeFun(Bound),
	  		[send_mapTree(C, TreeFun) || {_, C} <- Children],
	  		qtreeNode(Bound, Limit, Parent, Children)
    end.

mapFunChild({Quarter, Child}, MapFun, MapBound, Bound) ->
	case intersectBounds(MapBound, createBound(Quarter, Bound)) of
		{X1, Y1, X2, Y2} -> send_mapFun(Child, MapFun, {X1, Y1, X2, Y2});
		empty -> ok
	end.

%% Quadtree leaf
qtreeLeaf(Bound, Limit, Parent, Data) ->
    receive
     	stop -> ok;
     	{addElement, Element} ->
	  		case {outOfBound(Bound, Element), length(Data) < Limit} of
	    		{false, true} -> qtreeLeaf(Bound, Limit, Parent, [Element | Data]);
	    		{false, false} ->
					NewBounds = [{Quarter, createBound(Quarter, Bound)} || Quarter <- qTreeQuarters()],
					NewParent = self(),
					Children = [{Q, spawn(fun () -> qtreeLeaf(NewBound, Limit, NewParent, []) end)} || {Q, NewBound} <- NewBounds],
					NewData = [Element | Data],
					DataList = [[E || E <- NewData, not outOfBound(B, E)] || {_, B} <- NewBounds],
					ZippedList = lists:zip(Children, DataList),
					[fun({{_, C}, Es}) -> [send_add(C, E) || E <- Es] end(V) || V <- ZippedList],
					qtreeNode(Bound, Limit, Parent, Children);
	    		{true, _} -> qtreeLeaf(Bound, Limit, Parent, Data)
	  		end;
	  	{mapFun, MapFun, MapBound} ->
	  		case intersectBounds(Bound, MapBound) of
	  			{X1, Y1, X2, Y2} ->
	  				NewData = [case outOfBound({X1, Y1, X2, Y2}, E1) of false -> MapFun(E1); true -> E1 end || E1 <- Data],
	  				OutOfBoundData = [E2 || E2 <- NewData, outOfBound(Bound, E2)],
	  				[send_add(Parent, E3) || E3 <- OutOfBoundData],
	  				qtreeLeaf(Bound, Limit, Parent, NewData -- OutOfBoundData);
	  			empty -> ok
	  		end;
	  	{mapTreeFun, TreeFun} ->
	  		TreeFun(Bound),
	  		qtreeLeaf(Bound, Limit, Parent, Data)
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
qTreeQuarters() -> [nw, ne, sw, se].

% Returns the 1/4 of Bound which is defined by Quarter from the list above.
createBound(_, universe) -> universe;
createBound(_, empty) -> empty;
createBound(Quarter, Bound) ->
    {X1, Y1, X2, Y2} = Bound,
    Xmid = X1 + (X2 - X1) / 2,
    Ymid = Y1 + (Y2 - Y1) / 2,
    case Quarter of
      nw -> {X1, Y1, Xmid, Ymid};
      ne -> {Xmid, Y1, X2, Ymid};
      sw -> {X1, Ymid, Xmid, Y2};
      se -> {Xmid, Ymid, X2, Y2}
    end.

% Predicate that returns if an Element is outside Bound.
outOfBound(universe, _) -> false;
outOfBound(empty, _) -> true;
outOfBound(Bound, Element) ->
    {X1, Y1, X2, Y2} = Bound,
    {element, Pos, _} = Element,
    {X, Y} = Pos,
    (X < X1) or (X >= X2) or (Y < Y1) or (Y >= Y2).

% Finds the intersection between Bound1 and Bound2.
intersectBounds(universe, Bound) -> Bound;
intersectBounds(Bound, universe) -> Bound;
intersectBounds(empty, _) -> empty;
intersectBounds(_, empty) -> empty;
intersectBounds(Bound, Bound) -> Bound;
intersectBounds(Bound1, Bound2) ->
    {X1_1, Y1_1, X1_2, Y1_2} = Bound1,
    {X2_1, Y2_1, X2_2, Y2_2} = Bound2,
    X1 = lists:max([X1_1, X2_1]),
    Y1 = lists:max([Y1_1, Y2_1]),
    X2 = lists:min([X1_2, X2_2]),
    Y2 = lists:min([Y1_2, Y2_2]),
    case (X1 >= X2) or (Y1 >= Y2) of
      true -> empty;
      false -> {X1, Y1, X2, Y2}
    end.
