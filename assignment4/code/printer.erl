%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013
%%%---------------------------------------------------------------------

% The output of this printer is a SVG-file. 
% You can display SVG file in you favourite browser.

-module(printer).

-export([start/0, reset/2, addBox/2, addCircle/3, addLine/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% asynchronous communication
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info(Pid, Msg) ->
	Pid ! Msg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts the printer process with a deafult filename.
start() -> 
	{ok, spawn(fun() -> printer("FileName.svg",0) end)}.

% Stops the printer process
stop(Printer) ->
	info(Printer, stop).

% Removes all content from the file Filename. 
% Can both be used to set a new filename and to 
% override the content of a current file.
reset(Printer, FileName) -> 
	info(Printer, {reset, FileName}).

% Adds a box to the current file with bounds.
addBox(Printer, Bound) -> 
	info(Printer, {box, Bound}).

% Adds a circle at Pos with Size
addCircle(Printer, Pos, Size) -> 
	info(Printer, {point, Pos, Size}).

% Adds a line from Pos1 to Pos2
addLine(Printer, Pos1, Pos2) -> 
	info(Printer, {point, Pos1, Pos2}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal imlementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

toString(Num) ->
	lists:flatten(io_lib:format("~p", [Num])).

printer(FileName, CurPosition) ->
	EndText = "\n",
	{NextPosition, WritePosition, NewFileName, NewText} =
        receive
	{stop} ->
	    io:format("Printer ~p stopping~n", [self()]);
	{reset, ResetFileName} -> 
		Text = "<svg xmlns=\"http://www.w3.org/2000/svg\">\n\n",
		file:delete(ResetFileName),
		{string:len(Text), 0, ResetFileName, Text};
	{line, Pos1, Pos2} ->
		{X1, Y1} = Pos1, 
		{X2, Y2} = Pos2,
		Text = 
			"<polyline points=\"" ++ 
			toString(X1) ++ "," ++ toString(Y1) ++ " " ++
			toString(X2) ++ "," ++ toString(Y2) ++ "\"" ++
			" style=\"fill:none;stroke:black;stroke-width:1\" />\n",
		{CurPosition + string:len(Text), CurPosition, FileName, Text};
	{box, Bound} ->
		{X1, Y1, X2, Y2} = Bound,
		Text = 
			"<polyline points=\"" ++ 
			toString(X1) ++ "," ++ toString(Y1) ++ " " ++
			toString(X1) ++ "," ++ toString(Y2) ++ " " ++
			toString(X2) ++ "," ++ toString(Y2) ++ " " ++
			toString(X2) ++ "," ++ toString(Y1) ++ " " ++
			toString(X1) ++ "," ++ toString(Y1) ++ "\"" ++
			" style=\"fill:none;stroke:black;stroke-width:1\" />\n",
		{CurPosition + string:len(Text), CurPosition, FileName, Text};
	{point, Pos, Size} ->
		{X,Y} = Pos,
		Text = 
			"<circle cx=\"" ++ toString(X) ++
			"\" cy=\"" ++ toString(Y) ++
			"\" r=\"" ++ toString(Size) ++ "\" stroke=\"black\" stroke-width=\"2\" fill=\"red\"/>\n",
		{CurPosition + string:len(Text), CurPosition, FileName, Text}
	end,
	{ok,File} = file:open(NewFileName, [read,write]),
	file:pwrite(File, WritePosition, NewText ++ EndText),
	file:close(File),
	printer(NewFileName, NextPosition).