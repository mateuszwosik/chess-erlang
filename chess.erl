%chess.erl

-module(chess).
-compile([export_all]).
-import(board,[showBoard/1,initialBoard/0,oppositePlayer/1]).
-import(move,[finished/2,movePiece/4]).
-import(minmax,[evaluate/3]).

changeRow(X) ->
	case X of
		1 -> 8; 
		2 -> 7;
		3 -> 6;
		4 -> 5;
		5 -> 4;
		6 -> 3;
		7 -> 2;
		8 -> 1;
		_ -> 0
	end.

changeColumn(Y) ->
	case string:to_upper(Y) of
		"A" -> 1;
		"B" -> 2;
		"C" -> 3;
		"D" -> 4;
		"E" -> 5;
		"F" -> 6;
		"G" -> 7;
		"H" -> 8;
		_ -> 0
	end.

checkPositions({A,B},{C,D}) when (A==0) or (B==0) or (C==0) or (D==0) -> false;
checkPositions(_,_) -> true.

player() ->
	receive
		{PIDMain,Opponent,C,Board,P} ->
			showBoard(Board),
			io:format("Player ~p - ",[C]),
			{ok,[X,Y,X2,Y2]} = io:fread("Entry your move from to DC DC : ","~d~s~d~s"),
			Pos = {changeRow(X),changeColumn(Y)},
			Pos2 = {changeRow(X2),changeColumn(Y2)},
			case checkPositions(Pos,Pos2) of
				true ->
					io:format("Your move: ~p ~p~n",[{X,Y},{X2,Y2}]),
					try
						Board2 = movePiece(Pos,Pos2,queen,Board),
						case finished(C,Board) of
							true ->
								case finished(C,Board2) of
									true ->
										Opponent!{PIDMain,oppositePlayer(C),Board,win};
									false ->
										Opponent!{PIDMain,self(),oppositePlayer(C),Board2,P},
										player()
								end;
							false ->
								Opponent!{PIDMain,self(),oppositePlayer(C),Board2,P},
								player()
						end
					catch
						throw:error_position -> 
							Opponent!{error_position},
							PIDMain!{end_of_game}	
					end;
					
				false ->
					Opponent!{error_position},
					PIDMain!{end_of_game}
			end;
		{PIDMain,C,Board,win} ->
			showBoard(Board),
			io:format("Player ~p win! Player ~p lose!",[C,oppositePlayer(C)]),
			PIDMain!{end_of_game};
		{error_position} ->
			io:format("Bad position~n")
	end.

computer() ->
	receive
		{PIDMain,Opponent,C,Board,P} ->
			showBoard(Board),
			Board2 = evaluate(C,Board,P),
			case finished(C,Board) of
				true ->
					case finished(C,Board2) of
						true ->
							Opponent!{PIDMain,oppositePlayer(C),Board,win};
						false ->
							Opponent!{PIDMain,self(),oppositePlayer(C),Board2,P},
							computer()
					end;
				false ->
					Opponent!{PIDMain,self(),oppositePlayer(C),Board2,P},
					computer()
			end;
		{PIDMain,C,Board,win} ->
			showBoard(Board),
			io:format("Player ~p win! Player ~p lose!~n",[C,oppositePlayer(C)]),
			PIDMain!{end_of_game};
		{error_position} ->
			io:format("Bad position~n")
	end.

%Main
main() ->
	io:format("============== MAIN ==============~n"),
	io:format("1. Player vs Player~n"),
	io:format("2. Player vs Computer~n"),
	io:format("3. Computer vs Computer~n"),
	io:format("4. Quit~n"),
	{ok,[X]} = io:fread("Chose type of a game: ","~d"),
	case X of
		1 ->
			P1 = spawn(?MODULE,player,[]),
			P2 = spawn(?MODULE,player,[]),
			P1!{self(),P2,white,initialBoard(),queen},
			receive
				{end_of_game} -> {end_main}
			end;
		2 ->
			P1 = spawn(?MODULE,player,[]),
			P2 = spawn(?MODULE,computer,[]),
			P1!{self(),P2,white,initialBoard(),queen},
			receive
				{end_of_game} -> {end_main}
			end;
		3 ->
			P1 = spawn(?MODULE,computer,[]),
			P2 = spawn(?MODULE,computer,[]),
			P1!{self(),P2,white,initialBoard(),queen},
			receive
				{end_of_game} -> {end_main}
			end;
		4 ->
			{end_main};
		_ ->
			io:format("Wrong option~n"),
			main()
	end.