%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013 
%%% Usage   : Assignemnt for Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student : Jens Fredskov
%%% KU-ID   : chw752
%%%---------------------------------------------------------------------

-module(swarm).

-compile(export_all).

run() ->
	fishLoop_init().

% Initiates the swarm simulation with 10 predefined fish.
fishLoop_init() ->
	{ok,Printer} = printer:start(),
	{ok, Qtree} = quadtree:start({0,0,512,512}, 2),
	{Vx, Vy, Dvx, Dvy, Nd} = {0, 0, 0, 0, 1},
	quadtree:addElement(Qtree, {20, 20}, {0, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {25, 25}, {1, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {30, 30}, {2, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {20, 30}, {3, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {100, 100}, {4, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {110, 110}, {5, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {200, 200}, {6, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {210, 210}, {7, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {208, 201}, {8, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	quadtree:addElement(Qtree, {208, 208}, {9, {Vx, Vy}, {Dvx, Dvy}, Nd}),
	{ok, spawn(fun() -> fishLoop(Qtree, Printer, 0) end)}.

fishLoop(Qtree, Printer, N) ->
	% Comment out the if-clause if SVGs are unwanted.
	if N rem 10 =:= 0 -> 
		   timer:sleep(1000),
		   SVGFile = lists:flatten(io_lib:format("qtree~2.10.0B.svg", [N div 10])),
		   printer:reset(Printer, SVGFile),
		   MapFun =
		       fun(Element) ->
		           printer:addCircle(Printer, element(2,Element), 2),
			       Element 
		       end,
			quadtree:mapFunction(Qtree, MapFun, universe),
			timer:sleep(1000);
	   true -> timer:sleep(1000)
	end,

	quadtree:mapFunction(Qtree, fun(Fish) -> swarm:moveFish(Fish) end, universe),
	timer:sleep(50),
	quadtree:mapFunction(Qtree, fun(Fish) -> swarm:updateFish1(Qtree, Fish) end, universe),
	timer:sleep(50),
	fishLoop(Qtree, Printer, N+1).

updateFish1(Qtree, Fish1) ->
	Self = self(),
	GetFish =
		fun(Fish2) ->
			Dist = swarm:distanceFish(Fish1, Fish2),
			{element, _, {ID1, _, _, _}} = Fish1,
			{element, _, {ID2, _, _, _}} = Fish2,
			if 7 =< Dist, Dist =< 10, ID1 =/= ID2 ->
			       Self ! {attraction, Fish2},
			       Fish2;
			   0 < Dist, Dist =< 3, ID1 =/= ID2 ->
			       Self ! {repulsion, Fish2},
			       Fish2;
			   true -> Fish2
			end
		end,
	quadtree:mapFunction(Qtree, GetFish, universe),
	swarm:updateFish2(Fish1).

updateFish2(Fish1) ->
	receive
		{attraction, Fish2} ->
			V = swarm:calculateAttraction(Fish1, Fish2),
			UpdatedFish = swarm:updateChangeVector(Fish1, V),
			swarm:updateFish2(UpdatedFish);
		{repulsion, Fish2} ->
			V = swarm:calculateRepulsion(Fish1, Fish2),
			UpdatedFish = swarm:updateChangeVector(Fish1, V),
			swarm:updateFish2(UpdatedFish)
		after 250 ->
			Fish1
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculates the attraction velocity between the fish at the centre and
% a fish that is expected to be in the attraction zone.
% There is no check for attraction zone in this function.
updateChangeVector(Fish, VelocityCV) ->
	{element, Pos, Prop} = Fish,
	{ID, Velocity, FishVCV, Nd} = Prop,
	{element, Pos, {ID, Velocity, addVectors(FishVCV, VelocityCV), Nd + 1}}.

% Calculates the attraction velocity between the fish at the centre and
% a fish that is expected to be in the attraction zone.
% There is no check for attraction zone in this function.
calculateAttraction(FishCentre, FishInAttractionZone) ->
	{element, PosCF, _} = FishCentre,
	{element, PosAF, _} = FishInAttractionZone,
	toUnitVector(subtractVectors(PosCF, PosAF)).

% Calculates the repulsion velocity between the fish at the centre and
% a fish that is expected to be in the repulsion zone.
% There is no check for repulsion zone in this function.
calculateRepulsion(FishCentre, FishInRepulsionZone) ->
	{element, PosCF, _} = FishCentre,
	{element, PosRF, _} = FishInRepulsionZone,
	toUnitVector(subtractVectors(PosRF, PosCF)).

% Updates the velocity vector of a fish with the velocity difference vector.
% The moves the fish it according to the new velocity vector,
% and finally returns the updated fish with the reset difference vector.
% Maximum speed is 3.
moveFish(Fish) ->
	{element, Pos, Prop} = Fish,
	{ID, {Vx, Vy}, {Dvx, Dvy}, Nd} = Prop,
	NewV = toMaxLenVector({Vx + Dvx/Nd, Vy + Dvy/Nd},3),
	{element, addVectors(Pos, NewV), {ID, NewV, {0,0}, 1}}.

% Returns the distance between two fish
distanceFish(Fish1, Fish2) ->
	{element, Pos1, _} = Fish1,
	{element, Pos2, _} = Fish2,
	distance(Pos1, Pos2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper vector-functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Adds two vectors and returns their sum
addVectors({X1,Y1},{X2,Y2}) ->
	{X1+X2, Y1+Y2}.

% Adds two vectors and returns their sum
subtractVectors({X1,Y1},{X2,Y2}) ->
	{X1-X2, Y1-Y2}.

% Transforms a vector to a unit length (vector of length = 1)
toUnitVector(Vector) ->
	Len = vectorLength(Vector),
	{X,Y} = Vector,
	{X/Len, Y/Len}.

% Returns the length of a vector
vectorLength(Vector) ->
	{X,Y} = Vector,
	math:sqrt(math:pow(X,2)+math:pow(Y,2)).

% Returns a vector of maximum MaxLen length.
toMaxLenVector(Vector,MaxLen) ->
	Len = vectorLength(Vector),
	{X,Y} = Vector,
	case Len > MaxLen of
		true  -> {X*MaxLen/Len, Y*MaxLen/Len};
		false -> Vector
	end.

% Returns the distance between two positions
distance(Pos1, Pos2) ->
	{X1,Y1} = Pos1,
	{X2,Y2} = Pos2,
	math:sqrt(math:pow(X1-X2,2)+math:pow(Y1-Y2,2)).
