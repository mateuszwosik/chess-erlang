%move.erl

-module(move).
-compile([export_all]).
-import(board,[showBoard/1,initialBoard/0,getPieceValue/1,getPieceType/1,getPieceColor/1,isFieldEmpty/1,getField/2,oppositePlayer/1]).
-export([piecesPositionsOfPlayer/2,getPossibleMoves/3,movePiece/4,finished/2]).

placePiece(P) -> P.

removePiece(_) -> empty.

isOppositePiece(empty,_) -> false;
isOppositePiece({C,_},C) -> false;
isOppositePiece({_,_},_) -> true.

canMakeMove(empty,_) -> true;
canMakeMove(F,P) -> isOppositePiece(F,getPieceColor(P)).

canChangePawnToOther({black,pawn},{X,_}) when X==8 -> true;
canChangePawnToOther({white,pawn},{X,_}) when X==1 -> true;
canChangePawnToOther(_,_) -> false.

changePawnToOther({C,pawn},P) -> {C,P}.

isValidMove({FX,FY},{TX,TY},Board) when (FX>=1) and (FX=<8) and (FY>=1) and (FY=<8) and (TX>=1) and (TX=<8) and (TY>=1) and (TY=<8) ->
	lists:member({TX,TY},getPossibleMoves(getField({FX,FY},Board),{FX,FY},Board));
isValidMove(_,_,_) -> false.

movePiece({FX,FY},{TX,TY},PType,Board) ->
	case canChangePawnToOther(getField({FX,FY},Board),{TX,TY}) of
		true ->
			P = changePawnToOther(getField({FX,FY},Board),PType);
		false ->
			P = getField({FX,FY},Board)
	end,
	case isValidMove({FX,FY},{TX,TY},Board) of
		true ->
			applyRow(P,{TX,TY},1,applyRow(P,{FX,FY},1,Board,fun removePiece/1),fun placePiece/1);
		false ->
			throw(error_position)
	end.

applyRow(P,{X,Y},I,Board,Foo) when X==I -> [applyColumn(P,Y,1,lists:nth(I,Board),Foo)] ++ applyRow(P,{X,Y},I+1,Board,Foo);
applyRow(P,{X,Y},I,Board,Foo) when I<9 ->
	[lists:nth(I,Board)] ++ applyRow(P,{X,Y},I+1,Board,Foo);
applyRow(_,_,_,_,_) -> [].

applyColumn(P,Y,J,Row,Foo) when Y==J -> [Foo(P)] ++ applyColumn(P,Y,J+1,Row,Foo);
applyColumn(P,Y,J,Row,Foo) when J<9 ->
	[lists:nth(J,Row)] ++ applyColumn(P,Y,J+1,Row,Foo);
applyColumn(_,_,_,_,_) -> [].

pieceMoves(P) ->
	case P of
		{white,pawn} -> [{-1,0}];
		{black,pawn} -> [{1,0}];
		{_,knight} -> [{2,1},{2,-1},{-2,1},{-2,-1},{1,2},{-1,2},{1,-2},{-1,-2}];
		{_,bishop} -> [{1,1},{-1,1},{-1,-1},{1,-1}];
		{_,rook} -> [{1,0},{0,1},{-1,0},{0,-1}];
		{_,queen} -> [{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}];
		{_,king} -> [{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}]
	end.

isPawnInitialPosition({white,pawn},{7,_}) -> true;
isPawnInitialPosition({black,pawn},{2,_}) -> true;
isPawnInitialPosition(_,_) -> false.

addPositions({X1,Y1},{X2,Y2}) -> {X1+X2,Y1+Y2}.

isValidPosition({X,Y}) when (X>0) and (X<9) and (Y>0) and (Y<9) -> true;
isValidPosition(_) -> false.

ifPawnCanCapture({white,pawn},{X,Y},Board) ->
	case isValidPosition(addPositions({X,Y},{-1,1})) of
		true -> 
			case isOppositePiece(getField(addPositions({X,Y},{-1,1}),Board),white) of
				true ->
					P = [addPositions({X,Y},{-1,1})];
				false ->
					P = []
			end;
		false ->
			P = []
	end,
	case isValidPosition(addPositions({X,Y},{-1,-1})) of
		true -> 
			case isOppositePiece(getField(addPositions({X,Y},{-1,-1}),Board),white) of
				true ->
					P ++ [addPositions({X,Y},{-1,-1})];
				false ->
					P ++ []
			end;
		false ->
			P ++ []
	end;
ifPawnCanCapture({black,pawn},{X,Y},Board) ->
	case isValidPosition(addPositions({X,Y},{1,1})) of
		true -> 
			case isOppositePiece(getField(addPositions({X,Y},{1,1}),Board),black) of
				true ->
					P = [addPositions({X,Y},{1,1})];
				false ->
					P = []
			end;
		false ->
			P = []
	end,
	case isValidPosition(addPositions({X,Y},{1,-1})) of
		true -> 
			case isOppositePiece(getField(addPositions({X,Y},{1,-1}),Board),black) of
				true ->
					P ++ [addPositions({X,Y},{1,-1})];
				false ->
					P ++ []
			end;
		false ->
			P ++ []
	end.

allPositions(P,Pos,Board,Pos2) ->
	case isValidPosition(addPositions(Pos,Pos2)) of
		true ->
			case isFieldEmpty(getField(addPositions(Pos,Pos2),Board)) of
				true ->
					[addPositions(Pos,Pos2)] ++ allPositions(P,addPositions(Pos,Pos2),Board,Pos2);
				false ->
					case isOppositePiece(getField(addPositions(Pos,Pos2),Board),getPieceColor(P)) of
						true ->
							[addPositions(Pos,Pos2)];
						false ->
							[]
					end
			end;
		false ->
			[]
	end.

getPossibleMoves(P,Pos,Board) ->
	case P of
		{_,pawn} -> getPawnPossibleMoves(P,Pos,Board);
		{_,knight} -> [Pos2 || M <- pieceMoves(P), Pos2 <- [addPositions(Pos,M)], isValidPosition(Pos2), canMakeMove(getField(Pos2,Board),P)];
		{_,king} -> [Pos2 || M <- pieceMoves(P), Pos2 <- [addPositions(Pos,M)], isValidPosition(Pos2), canMakeMove(getField(Pos2,Board),P)];
		{_,_} -> lists:concat([allPositions(P,Pos,Board,M) || M <- pieceMoves(P)]);
		_ -> [] 
	end.

getPawnPossibleMoves(P,Pos,Board) ->
	case isValidPosition(addPositions(Pos,lists:nth(1,pieceMoves(P)))) of
		true ->
			case isFieldEmpty(getField(addPositions(Pos,lists:nth(1,pieceMoves(P))),Board)) of
				true ->
					case isPawnInitialPosition(P,Pos) of
						true ->
							case isFieldEmpty(getField(addPositions(addPositions(Pos,lists:nth(1,pieceMoves(P))),lists:nth(1,pieceMoves(P))),Board)) of
								true ->
									[addPositions(Pos,lists:nth(1,pieceMoves(P))),addPositions(addPositions(Pos,lists:nth(1,pieceMoves(P))),lists:nth(1,pieceMoves(P)))] ++ ifPawnCanCapture(P,Pos,Board);
								false ->
									[addPositions(Pos,lists:nth(1,pieceMoves(P)))] ++ ifPawnCanCapture(P,Pos,Board)
							end;
						false ->
							[addPositions(Pos,lists:nth(1,pieceMoves(P)))] ++ ifPawnCanCapture(P,Pos,Board)
					end;
				false ->
					[] ++ ifPawnCanCapture(P,Pos,Board)
			end;
		false ->
			[]
	end.

piecesPositionsOfPlayer(C,Board) -> lists:concat(piecesPositionsRow(C,1,Board)).

piecesPositionsRow(C,I,Board) when I<9 -> [piecesPositionsColumn(C,I,1,lists:nth(I,Board))] ++ piecesPositionsRow(C,I+1,Board);
piecesPositionsRow(_,_,_) -> [].

piecesPositionsColumn(C,I,J,Row) when J<9 -> 
	case isFieldEmpty(lists:nth(J,Row)) of
		true ->
			piecesPositionsColumn(C,I,J+1,Row);
		false ->
			case C == getPieceColor(lists:nth(J,Row)) of
				true ->
					[{I,J}] ++ piecesPositionsColumn(C,I,J+1,Row);
				false ->
					piecesPositionsColumn(C,I,J+1,Row)
			end
	end;
piecesPositionsColumn(_,_,_,_) -> [].

finished(C,Board) -> isMate(searchKing(piecesPositionsOfPlayer(C,Board),Board),allMoves(oppositePlayer(C),Board)).

allMoves(C,Board) -> [Pos || Pos2 <- piecesPositionsOfPlayer(C,Board), Pos <- getPossibleMoves(getField(Pos2,Board),Pos2,Board)].

searchKing([H|T],Board) -> 
	case getPieceType(getField(H,Board)) == king of
		true ->
			H;
		false ->
			searchKing(T,Board)
	end.

isMate({KX,KY},[{PX,PY}|T]) ->
	case (KX==PX) and (KY==PY) of
		true ->
			true;
		false ->
			isMate({KX,KY},T)
	end;
isMate(_,[]) -> false.

searchPositionOfAMate(C,Board) -> getPosition(searchKing(piecesPositionsOfPlayer(C,Board),Board),allMoves2(oppositePlayer(C),Board)).

allMoves2(C,Board) -> [{Pos,Pos2} || Pos <- piecesPositionsOfPlayer(C,Board), Pos2 <- getPossibleMoves(getField(Pos,Board),Pos,Board)].

getPosition({KX,KY},[{Pos,{PX2,PY2}}|T]) ->
	case (KX==PX2) and (KY==PY2) of
		true ->
			Pos;
		false ->
			getPosition({KX,KY},T)
	end;
getPosition(_,[]) -> "ERROR - list in searching where is mate".