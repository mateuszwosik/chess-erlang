%minmax.erl

-module(minmax).
-compile([export_all]).
-import(board,[getPieceValue/1,getField/2,oppositePlayer/1]).
-import(move,[piecesPositionsOfPlayer/2,getPossibleMoves/3,movePiece/4]).
-export([evaluate/3]).

depth() -> 3.

getPiecesValue(C,Board) -> lists:foldl(fun incValue/2, 0, getAllPlayerPieces(C,Board)).

incValue(empty,Sum) -> Sum;
incValue(P,Sum) -> getPieceValue(P) + Sum.

getAllPlayerPieces(C,Board) -> [P || Pos <- piecesPositionsOfPlayer(C,Board), P <- [getField(Pos,Board)]].

resultOfGame(Board) -> getPiecesValue(white,Board)-getPiecesValue(black,Board).

possibleBoards(Board,Pos,P) -> [Board2 || Pos2 <- getPossibleMoves(getField(Pos,Board),Pos,Board), Board2 <- [movePiece(Pos,Pos2,P,Board)]].

genBoards(C,Board,P) -> lists:concat([Board2 || Pos <- piecesPositionsOfPlayer(C,Board), Board2 <- [possibleBoards(Board,Pos,P)]]).

genTree(0,_,Board,_) -> {Board,resultOfGame(Board),[]};
genTree(N,C,Board,P) ->
	S = self(),
	PIDs = lists:map(fun(B) -> spawn(fun() -> genTreeC(S,N-1,C,B,P) end) end, genBoards(C,Board,P)),
	{Board,resultOfGame(Board),genTreeGather(PIDs)}.

genTreeGather([H|T]) ->
	receive
		{H, Res} -> [Res|genTreeGather(T)]
	end;
genTreeGather([]) -> [].

genTreeC(Parent,0,_,Board,_) -> Parent!{self(),{Board,resultOfGame(Board),[]}};
genTreeC(Parent,N,C,Board,P) ->
	S = self(),
	Parent!{S,{Board,resultOfGame(Board),[genTreeCT(N-1,oppositePlayer(C),Board2,P) || Board2 <- genBoards(oppositePlayer(C),Board,P)]}}.

genTreeCT(0,_,Board,_) -> {Board,resultOfGame(Board),[]};
genTreeCT(N,C,Board,P) -> {Board,resultOfGame(Board),[genTreeCT(N-1,oppositePlayer(C),Board2,P) || Board2 <- genBoards(oppositePlayer(C),Board,P)]}.

gameTree(C,Board,P) -> genTree(depth(),C,Board,P).

bestResult(_,{Board,Result,[]}) -> {Board,Result};
bestResult(C,{_,_,Subtree}) ->
	case C of
		white ->
			maximum([maximize(Z) || Z <- lists:zip(getBoards(Subtree),Subtree)]);
		black ->
			minimum([minimize(Z) || Z <- lists:zip(getBoards(Subtree),Subtree)])
	end.

getBoards([]) -> [];
getBoards([{Board,_,_}|T]) -> [Board] ++ getBoards(T).

maximum([]) -> "Empty score list - maximum";
maximum([{Board,Result}]) -> {Board,Result};
maximum([{Board1,Result1},{_,Result2}|Rest]) when Result1>=Result2 -> maximum([{Board1,Result1}|Rest]);
maximum([_,{Board2,Result2}|Rest]) -> maximum([{Board2,Result2}|Rest]).

minimum([]) -> "Empty score list - minimum";
minimum([{Board,Result}]) -> {Board,Result};
minimum([{Board1,Result1},{_,Result2}|Rest]) when Result1<Result2 -> minimum([{Board1,Result1}|Rest]);
minimum([_,{Board2,Result2}|Rest]) -> minimum([{Board2,Result2}|Rest]).

maximize({Board,{_,Result,[]}}) -> {Board,Result};
maximize({Board,{_,_,Subtree}}) -> {Board,lists:min(getResults([minimize({Board,T}) || T <- Subtree]))}.

minimize({Board,{_,Result,[]}}) -> {Board,Result};
minimize({Board,{_,_,Subtree}}) -> {Board,lists:max(getResults([maximize({Board,T}) || T <- Subtree]))}.

getResults([{_,Result}]) -> [Result];
getResults([{_,Result}|T]) -> [Result] ++ getResults(T).

getBoard({Board,_}) -> Board.

evaluate(C,Board,P) -> getBoard(bestResult(C,gameTree(C,Board,P))).