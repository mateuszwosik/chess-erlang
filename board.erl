%board.erl

-module(board).
-compile([export_all]).
-export([showBoard/1,initialBoard/0,getPieceValue/1,getPieceType/1,getPieceColor/1,isFieldEmpty/1,getField/2,oppositePlayer/1]).

showPType(P) ->
	case P of
		pawn -> "P";
		knight -> "N";
		bishop -> "B";
		rook -> "R";
		queen -> "Q";
		king -> "K"
	end.

showPColor(C) ->
	case C of
		white -> "W";
		black -> "B"
	end.

showPiece(empty) ->
	"  ";
showPiece({C,P}) -> 
	string:concat(showPColor(C),showPType(P)).

showBoard(Board) -> 
	io:format("    A    B    C    D    E    F    G    H~n"),
	io:format("  -----------------------------------------~n"),
	showRow(0,Board).
	

showRow(I,[H|T]) when I < 8 ->
	io:format("~p |",[8-I]),
	showColumn(0,H),
	showRow(I+1,T);
showRow(_,_) -> io:format("").

showColumn(I,[H|T]) when I < 8 ->
	io:format("~p|", [showPiece(H)]),
	showColumn(I+1, T);
showColumn(_,_) -> io:format("~n  -----------------------------------------~n").

initialBoard() -> [[{black,rook},{black,knight},{black,bishop},{black,queen},{black,king},{black,bishop},{black,knight},{black,rook}],
		   [{black,pawn},{black,pawn},{black,pawn},{black,pawn},{black,pawn},{black,pawn},{black,pawn},{black,pawn}],
		   [empty,empty,empty,empty,empty,empty,empty,empty],
		   [empty,empty,empty,empty,empty,empty,empty,empty],
		   [empty,empty,empty,empty,empty,empty,empty,empty],
		   [empty,empty,empty,empty,empty,empty,empty,empty],
		   [{white,pawn},{white,pawn},{white,pawn},{white,pawn},{white,pawn},{white,pawn},{white,pawn},{white,pawn}],
		   [{white,rook},{white,knight},{white,bishop},{white,queen},{white,king},{white,bishop},{white,knight},{white,rook}]].

getPieceValue({_,P}) ->
	case P of
		pawn -> 1;
		knight -> 3;
		bishop -> 3;
		rook -> 5;
		queen -> 9;
		king -> 100
	end.

getPieceType({_,P}) -> P.

getPieceColor({C,_}) -> C.

isFieldEmpty(empty) -> true;
isFieldEmpty({_,_}) -> false.

getRow(X,Board) -> lists:nth(X,Board).

getColumn(Y,Row) -> lists:nth(Y,Row).

getField({X,Y},Board) -> lists:nth(Y,lists:nth(X,Board)).

oppositePlayer(C) ->
	case C of
		white -> black;
		black -> white
	end.		