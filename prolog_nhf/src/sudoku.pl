% :- type col  == int.
% :- type row  == int.
% :- type coords -->row-col.
% :- pred ertekek(sspec::in, coords::in, list(int)::out).
% ertekek(SSpec, R_C, Vals): 
% Egy érték pontosan akkor szerepel a Vals listában, ha:
%    (a) 1..k*k közötti egész, ahol k az SSpec feladvány cellamérete,
%    (b) teljesíti az adott mezőre vonatkozó szám- és paritási infók
%        által előírt megszorításokat, továbbá
%    (c) különbözik az adott mezőt tartalmazó sor, oszlop és cella többi
%        mezőjében szereplő száminfóktól, 
% ahol
%    SSpec az sspec típusspecifikációnak megfelelő Sudoku-feladvány,
%    R_C az adott feladvány egy mezőjének (sor-oszlop formában megadott) koordinátája,
%    Vals list(int) típusú mezőértéklista, az SSpec feladvány R_C koordinátájú
%         mezőjében megengedett értékek listája. 

:- use_module(library(lists)).
:- use_module(library(between)).

sudoku(s(K,F), SSol) :-
    K2 is K*K,
    numlist(1, K2, AllValues), 
        
    feldarabolasa(F, K-K, Cells),   
    Rows = F,
    getCols(F, K2, 1, Cols),
    
    lehetsegesMindenMezore(Rows, K2, 1, K, Cols, Cells, AllValues, LehetsegesMindenMezore),
    
    kezdetiAllapot(LehetsegesMindenMezore, Rows, KezdetiAllapot),
        
    megold(KezdetiAllapot, AllValues, K, SSol).

% SolAcc kezdetben ures tomb
megold(Allapot, AllValues, K, SSol) :-
    K21 = K * K + 1,    
    minKitoltetlen(Allapot, 1, s(0, 0, K21), KitoltetlenHelye),
    (
        KitoltetlenHelye = s(0, 0, _) ->
        SSol = Megoldas
    ;
        (
            KitoltetlenHelye = s(_, _, 0) ->
            false
        ;
            nth1(RowIndex, Allapot, AllapotSor),
            nth1(ColIndex, AllapotSor, KitoltetlenMezo),
           
            osszesErtekkelKitoltEsFrissit(Allapot, KitoltetlenMezo, AllapotSor, RowIndex, ColIndex, AllValues, K, SSol)
        )
        
    ).

% currentMin kezdetben K*K + 1, ha nincs kitoltetlen, akkor R és C 0
minKitoltetlen([], _, Min, Min).
minKitoltetlen([AllapotSor | AllapotSorok], R, CurrentMin, KitoltetlenHelye) :-
    sorMinimum(AllapotSor, R, 1, CurrentMin, RowMin),
    minKitoltetlen(AllapotSorok, R + 1, RowMin, KitoltetlenHelye).
  
sorMinimum([], _, _, CurrentMin, CurrentMin). 
sorMinimum([AllapotMezo | AllapotSor], R, CurrentC, s(MinR, MinC, Min), RowMin) :- 
    s(Lehetosegek, _, Kitoltes, _) = AllapotMezo,
    length(Lehetosegek, LLength),
    CurrentC1 is CurrentC + 1,
    (
        Min > LLength, Kitoltes =:= 0 ->
        sorMinimum(AllapotSor, R, CurrentC1, s(R, CurrentC, LLength), RowMin)
    ;
        sorMinimum(AllapotSor, R, CurrentC1, s(MinR, MinC, Min), RowMin)
    ).
    
kezdetiAllapot([], _, []).
kezdetiAllapot([LehetsegesSor | LehetsegesMindenMezore], [Row | Rows], [AllapotSor | KezdetiAllapot]) :-
    map2(allapotMezo, LehetsegesSor, Row, AllapotSor),
    kezdetiAllapot(LehetsegesMindenMezore, Rows, KezdetiAllapot).
    
    
allapotMezo(Lehetosegek, Field, s(Lehetosegek, Field, 0, Value)) :-
    getValue(Field, Value).
    
lehetsegesMindenMezore([], _K2, _CurrentRowIndex, _K, _Cols, _Cells, _AllValues, []).
lehetsegesMindenMezore([Row | Rows], K2, CurrentRowIndex, K, Cols, Cells, AllValues, [Ertekek | RestResult]) :-
    lehetsegesErtekSorra(Row, K, Row, Cols, Cells, CurrentRowIndex, AllValues, 1, Ertekek),
    NextRowIndex is CurrentRowIndex + 1,
    lehetsegesMindenMezore(Rows, K2, NextRowIndex, K, Cols, Cells, AllValues, RestResult).
     
getCols(_Mx, K2, IndexAcc, []) :-
    IndexAcc > K2.
                                                                                    
getCols(Mx, K2, IndexAcc, [Col | OtherCols]) :-
    IndexAcc =< K2,
    oszlop(Mx, IndexAcc, Col),
    IndexAcc1 is IndexAcc + 1,
    getCols(Mx, K2, IndexAcc1, OtherCols).
    
setnth(1, [_|Rest], New, [New|Rest]).
setnth(I, [E|Rest], New, [E|NewEnd]) :- 
    I > 1,
    I1 is I-1,
    setnth(I1, Rest, New, NewEnd).
                
% map 2 lists paralelly
map2(_Pred, [], _, []).
map2(Pred, [H1 | T1], [H2 | T2], Result) :-
    call(Pred, H1, H2, Mapped),
    Result = [Mapped | Maradek],
    map2(Pred, T1, T2, Maradek).

% sort ad vissza értékekkel, Cacc default 0
lehetsegesErtekSorra([], _, _WholeRow, _Cols, _Cells, _R, _AllValues, _CAcc, []).
lehetsegesErtekSorra([Field | RowTail], K, WholeRow, Cols, Cells, R, AllValues, CAcc, Ertekek) :-
    ertekek(K, R-CAcc, Field, AllValues, Cells, WholeRow, Cols, MezoErtekek),
    Ertekek = [MezoErtekek | MaradekMezoErtekek],
    CAcc1 is CAcc + 1,
    lehetsegesErtekSorra(RowTail, K, WholeRow, Cols, Cells, R, AllValues, CAcc1, MaradekMezoErtekek).

ertekek(K, R-C, Field, AllValues, Cells, Row, Cols, Vals) :-
    nth1(C, Cols, Col),

    filterByConstraints(Field, Row, Col, R, C, AllValues, V1),
    
    removeFromList(C, Row, RowWithoutSelf),
    filterByUnit(RowWithoutSelf, V1, V2),
    
    removeFromList(R, Col, ColWithoutSelf),
    filterByUnit(ColWithoutSelf, V2, V3),

    getCellIndex(R, C, K, CellIndex),
    nth1(CellIndex, Cells, Cell),
    getValue(Field, OwnValue),
    filterByCell(Cell, V3, OwnValue, V4),

    (
        V4 \= [], R > 1 -> 
        R1 is R-1,
        nth1(R1, Col, NorthNeighbor),
        (
            member(s, NorthNeighbor) ->
            getValue(NorthNeighbor, NorthNeighborValue),
            (
                NorthNeighborValue =:= 0 -> SFilter = V4
            ;    
                (
                    NorthNeighborValue mod 2 =:= 1 ->
                    include(even, V4, SFilter)
                ;    
                    include(odd, V4, SFilter)
                )
            )
        ;
            SFilter = V4
        )
    ;
        SFilter = V4
    ),
    K2 is K * K,
    (
        SFilter \= [], C < K2 -> 
        C1 is C+1,
        nth1(C1, Row, EastNeighbor),
        (
            member(w, EastNeighbor) ->
            getValue(EastNeighbor, EastNeighborValue),
            (
                EastNeighborValue =:= 0 -> Vals = SFilter
            ;    
                (
                    EastNeighborValue mod 2 =:= 1 ->
                    include(even, SFilter, Vals)
                ;    
                    include(odd, SFilter, Vals)
                )
            )
        ;
            Vals = SFilter
        )
    ;
        Vals = SFilter
    ).


filterByConstraints([], _Row, _Col, _R, _C, Values, Values).

filterByConstraints([e | FieldTail], Row, Col, R, C, Values, Result) :-
        include(even, Values, FilteredValues),
        filterByConstraints(FieldTail, Row, Col, R, C, FilteredValues, Result).

filterByConstraints([o | FieldTail], Row, Col, R, C, Values, Result) :-
        include(odd, Values, FilteredValues),
        filterByConstraints(FieldTail, Row, Col, R, C, FilteredValues, Result).

filterByConstraints([s | FieldTail], Row, Col, R, C, Values, Result) :-
        R1 is R+1,
        nth1(R1, Col, Field),
        getValue(Field, Value),
        (       % if has value
                Value =\= 0 ->
                (
                    Value mod 2 =:= 1 -> include(even, Values, FilteredValues)
                ;
                    include(odd, Values, FilteredValues)
                ),
                filterByConstraints(FieldTail, Row, Col, R, C, FilteredValues, Result);
                % else 
                filterByConstraints(FieldTail, Row, Col, R, C, Values, Result)               
        ).

filterByConstraints([w | FieldTail], Row, Col, R, C, Values, Result) :-
        C1 is C-1,
        nth1(C1, Row, Field),
        getValue(Field, Value),
        (
                Value =\= 0 ->
                (
                    Value mod 2 =:= 1 -> include(even, Values, FilteredValues);
                    % else
                    include(odd, Values, FilteredValues)
                ),
                filterByConstraints(FieldTail, Row, Col, R, C, FilteredValues, Result);
                % else
                filterByConstraints(FieldTail, Row, Col, R, C, Values, Result)
        ).

filterByConstraints([v(Number)|FieldTail], Row, Col, R, C, _Values, Result) :- 
        filterByConstraints(FieldTail, Row, Col, R, C, [Number], Result).

% A sorban/oszlopban/cellában lévő többi szám alapján szűr
filterByUnit([], Values, Values).
filterByUnit([Field | Fields], Values, Result) :-
        getValue(Field, Value),
        (
                Value =\= 0 ->
                excludeEqual(Value, Values, Excluded),
                filterByUnit(Fields, Excluded, Result);
                % else
                filterByUnit(Fields, Values, Result)
        ).

filterByCell([], Values, _OwnValue, Values).
% field has no value
filterByCell([Field | Fields], Values, 0, Result) :-
        getValue(Field, Value),
        (
                Value =\= 0 ->
                excludeEqual(Value, Values, Excluded),
                filterByUnit(Fields, Excluded, Result)
                % else
        ;       filterByUnit(Fields, Values, Result)
        ).
  
filterByCell(Fields, Values, OwnValue, Result) :-
        OwnValue =\= 0,
        getAllValues(Fields, [], CellValues),
        countValueOccurences(CellValues, OwnValue, 0, Occurences),
        (
            Occurences > 1 ->
            Result = []
        ;   Result = Values
        ).
      
countValueOccurences([], _Value, Acc, Acc).
countValueOccurences([H|T], Value, Acc, Result) :-
        (
                Value =:= H ->
                Acc1 is Acc + 1,
                countValueOccurences(T, Value, Acc1, Result)
        ;       % else
                countValueOccurences(T, Value, Acc, Result)
        ).   
                                                         
excludeEqual(_Value, [], []).

excludeEqual(Value, [H|T], Excluded) :-
        H =:= Value,
        excludeEqual(Value, T, Excluded).

excludeEqual(Value, [H|T], Excluded) :-
        H =\= Value,
        Excluded = [H | Maradek],
        excludeEqual(Value, T, Maradek).                     
                                     
equal(X, Value) :- X =:= Value.
even(X) :- X mod 2 =:= 0.
odd(X) :- X mod 2 =:= 1.

getValue([], 0).
getValue([v(Info)|_Infos], Info).
getValue([Info|Infos], Value) :-
        Info \= v(_Number),
        getValue(Infos, Value).

getAllValues([], Acc, Acc).
getAllValues([Field|Fields], Acc, Result) :-
        getValue(Field, Value),
        (
            Value =\= 0 ->
            getAllValues(Fields, [Value|Acc], Result)
        ;   getAllValues(Fields, Acc, Result)
        ).    
 
% Visszaadja, hogy az adott koordinátájú mező hányadik cellában van a feldarabolásban
getCellIndex(R, C, K, Index) :-
        %sorszam 0-tol
            Sorszam is (R-1) * K * K + C - 1,
            % index, if devided to pieces K x 1
            Chunk is Sorszam div K,
            % every row has K of those pieces and there are K rows in each box
            Row is Chunk div (K*K),
            Col is Chunk rem K,
            Index is Row * K + Col + 1.

removeFromList(Index, List, Result) :- nth1(Index, List, _, Result).                         

% visszaadja az SSpec specifikációval megadott Sudoku-feladvány C-edik oszlopát
oszlop([], _C, []).
oszlop([Row| Rows], C, Col) :- 
        nth1(C, Row, Field),
        Col = [Field | Maradek],
        oszlop(Rows, C, Maradek).

% visszaadja az SSpec specifikációval megadott Sudoku-feladvány R-edik sorát  
sor(SSpec, R, Row) :- nth1(R, SSpec, Row).

take([], _, []).
take(L, 0, []) :- L \= [].
take([H|T], R, Acc) :- 
        R > 0, 
        R1 is R-1, 
        Acc = [H|Vegebol],
        take(T, R1, Vegebol). 

/*take(L, N, P) :- prefix_length(L, P, N).
drop(L, N, P) :- suffix_length(L, P, N).    */
drop([], _, []).
drop(L, 0, L) :- L \= [].
drop([_|T], N, Eredmeny) :- 
        N > 0, 
        N1 is N-1, 
        Eredmeny = Acc,
        drop(T, N1, Acc).
        
             
vizszintesen([], _, []).
vizszintesen(Mx, R, LL) :- 
        Mx \= [],
        take(Mx, R, Eleje),
        drop(Mx, R, Vege),
        LL = [Eleje | Maradek],
        vizszintesen(Vege, R, Maradek).

% létrehozza az I-edik fuggoleges vágást
fuggolegesenIedikOszlopcsoportra([], _C, _I, []).
fuggolegesenIedikOszlopcsoportra([H|T], C, I, Acc) :- 
        [H|T] \= [],
        Eldobando is I * C,
        drop(H, Eldobando, Vege),
        take(Vege, C, E),
        append(E, A, Acc),
        fuggolegesenIedikOszlopcsoportra(T, C, I, A).
                           
% felvág 1 sorcsoportot C oszloponként, I az oszlopcsoport sorszáma, 0-tól indul
fuggolegesenEgySorCsoportot([HLss| TLss], C, I, Acc) :- 
        length(HLss, Hossz),
        ((I+1) * C) < Hossz,
        fuggolegesenIedikOszlopcsoportra([HLss| TLss], C, I, Eleje),
        I1 is I + 1,
        Acc = [Eleje | Vege],
        fuggolegesenEgySorCsoportot([HLss| TLss], C, I1, Vege).

% Maradek a vegen
fuggolegesenEgySorCsoportot([HLss| TLss], C, I, Acc) :- 
        length(HLss, Hossz),
        ((I+1) * C) >= Hossz,
        Acc = [Maradek | []],
        fuggolegesenIedikOszlopcsoportra([HLss| TLss], C, I, Maradek).
        

fuggolegesen([], _C, []).
fuggolegesen([HLss | TLss], C, Acc) :- 
        [HLss | TLss] \= [],
        fuggolegesenEgySorCsoportot(HLss, C, 0, F),
        append(F, Acc0, Acc),
        fuggolegesen(TLss, C, Acc0).

feldarabolasa(Mx, SubRows-SubCols, LL) :- 
        vizszintesen(Mx, SubRows, V),
        fuggolegesen(V, SubCols, LL).
