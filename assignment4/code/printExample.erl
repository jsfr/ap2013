%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013 
%%%---------------------------------------------------------------------

% Provided the is a very small example of how the printing works.
% It prints 4 SVG-files with increasing number of elements and a small 
% function map.
%
% NOTE: This is no way near to a test.

-module(printExample).

-export([run/0]).

print(Qtree, Printer) ->
	MapFun = 
		fun (Element) ->
			printer:addCircle(Printer, element(2,Element), element(3,Element)),
			Element 
		end,
	quadtree:mapFunction(Qtree, MapFun, universe).

printGrid(Qtree, Printer) ->
	MapFun = 
		fun (Bound) ->
			printer:addBox(Printer, Bound)
		end,
	quadtree:mapTreeFunction(Qtree, MapFun).


run() ->
	{ok,Printer} = printer:start(),
	{ok,Qtree} = quadtree:start({0,0,512,512},2),

	quadtree:addElement(Qtree, {5,10}, 1),
	quadtree:addElement(Qtree, {50,10}, 2),
	quadtree:addElement(Qtree, {15,130}, 3),
	quadtree:addElement(Qtree, {500,10}, 4),
	quadtree:addElement(Qtree, {35,106}, 5),
	quadtree:addElement(Qtree, {34,130}, 6),

	timer:sleep(100),
	printer:reset(Printer,"qtree1.svg"),
	printGrid(Qtree,Printer),
	print(Qtree,Printer),
	timer:sleep(100),

	quadtree:addElement(Qtree, {45,110}, 7),
	quadtree:addElement(Qtree, {125,431}, 8),
	quadtree:addElement(Qtree, {353,105}, 9),
	quadtree:addElement(Qtree, {125,130}, 10),
	quadtree:addElement(Qtree, {125,131}, 11),
		
	timer:sleep(100),
	printer:reset(Printer,"qtree2.svg"),
	printGrid(Qtree,Printer),
	print(Qtree,Printer),
	timer:sleep(100),
	
	quadtree:addElement(Qtree, {15,30}, 12),
	quadtree:addElement(Qtree, {15.3,35}, 13),
	quadtree:addElement(Qtree, {50,40}, 14),
	quadtree:addElement(Qtree, {158,141}, 15),

	timer:sleep(100),
	printer:reset(Printer,"qtree3.svg"),
	printGrid(Qtree,Printer),
	print(Qtree,Printer),
	timer:sleep(100),

	GooooWest = 
		fun(Element) -> 
			{element, Pos, Prop} = Element,
			{X,Y} = Pos,
			{element, {X-30,Y},Prop+2} 
		end,
	quadtree:mapFunction(Qtree, GooooWest, {25,0,126,131}),

	timer:sleep(100),
	printer:reset(Printer,"qtree4.svg"),
	printGrid(Qtree,Printer),
	print(Qtree,Printer),
	timer:sleep(100),

	{Qtree,Printer}.


