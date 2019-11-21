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
    K21 is K * K + 1,    
    minKitoltetlen(Allapot, 1, s(0, 0, K21), KitoltetlenHelye),
    (
        KitoltetlenHelye = s(0, 0, _) ->
        megoldasAllapotbol(Allapot, SSol)
    ;
        (
            KitoltetlenHelye = s(_, _, 0) ->
            false
        ;
            s(RowIndex, ColIndex, _) = KitoltetlenHelye,
            nth1(RowIndex, Allapot, AllapotSor),
            nth1(ColIndex, AllapotSor, KitoltetlenMezo),
           
            osszesErtekkelKitoltEsFrissit(Allapot, KitoltetlenMezo, AllapotSor, RowIndex, ColIndex, AllValues, K, SSol)
        )
        
    ).

megoldasAllapotbol([], []).
megoldasAllapotbol([AllapotSor | Allapot], Megoldas) :-
    megoldasSorAllapotSorbol(AllapotSor, MegoldasSor),
    Megoldas = [MegoldasSor | MaradekMegoldasSor],
    megoldasAllapotbol(Allapot, MaradekMegoldasSor).
     
megoldasSorAllapotSorbol([], []).
megoldasSorAllapotSorbol([s(_, _, Kitoltes, _) | AllapotSor], MegoldasSor) :-
    MegoldasSor = [Kitoltes | MaradekMegoldasMezo],
    megoldasSorAllapotSorbol(AllapotSor, MaradekMegoldasMezo).
                            
osszesErtekkelKitoltEsFrissit(_Allapot, s([], _Field, _, _), _Sor, _RowIndex, _ColIndex, _AllValues, _K, _Sol) :- false.
osszesErtekkelKitoltEsFrissit(Allapot, s([Ertek | Lehetosegek], Field, Kitoltes, OwnValue), AllapotSor, RowIndex, ColIndex, AllValues, K, Sol) :-
            excludeEqual(Ertek, Lehetosegek, MaradekLehetoseg),
            (
                var(OwnValue) ->
                KitoltottMezo = s(MaradekLehetoseg, [v(Ertek)|Field], Ertek, Ertek)
            ;    
                KitoltottMezo = s(MaradekLehetoseg, Field, Ertek, Ertek) % mar benne van az ertek
            ),
            
            setnth(ColIndex, AllapotSor, KitoltottMezo, UjSor),
            setnth(RowIndex, Allapot, UjSor, KitoltottAllapot),
              
            oszlop(KitoltottAllapot, ColIndex, AllapotOszlop),

            eliminate(K, RowIndex-ColIndex, KitoltottMezo, UjSor, AllapotOszlop, KitoltottAllapot, FrissitettAllapot), 
        
            
            (
                megold(FrissitettAllapot, AllValues, K, Sol1),
                (
                    nonvar(Sol1) -> Sol = Sol1
                ;
                    false
                )
                
            ;   osszesErtekkelKitoltEsFrissit(Allapot, s(Lehetosegek, Field, Kitoltes, OwnValue), AllapotSor, RowIndex, ColIndex, AllValues, K, Sol)
            ).
                                                                                     
eliminate(K, R-C, KitoltottMezo, AllapotSor, AllapotOszlop, Allapot, UjAllapot) :-
    s(_Lehetseges, Field, Kitoltes, _OwnValue) = KitoltottMezo,
    
    eliminateFromRow(KitoltottMezo, AllapotSor, UjSor),
    setnth(C, UjSor, KitoltottMezo, UjSorKitoltottMezovel),
    setnth(R, Allapot, UjSorKitoltottMezovel, Allapot1),

    eliminateFromCol(KitoltottMezo, AllapotOszlop, UjOszlop),
    setnth(R, UjOszlop, KitoltottMezo, UjOszlopKitoltottMezovel),
    setOszlop(C, Allapot1, UjOszlopKitoltottMezovel, Allapot2),
    
    cellElimination(KitoltottMezo, R, C, K, Allapot2, Allapot3),

    %s
    (
        R < K*K, member(s, Field) ->
        oszlop(Allapot3, C, Allapot3Oszlop),
        R1 is R + 1,
        nth1(R1, Allapot3Oszlop, s(DelLehetosegek, DelField, DelKitoltes, DelErtek)),
        (
            Kitoltes mod 2 =:= 1 ->
            include(even, DelLehetosegek, DelFiltered),
            UjDel = s(DelFiltered, DelField, DelKitoltes, DelErtek)
        ;
            include(odd, DelLehetosegek, DelFiltered),
            UjDel = s(DelFiltered, DelField, DelKitoltes, DelErtek)
        ),
        setnth(R1, Allapot3Oszlop, UjDel, UjOszlopDel),
        setOszlop(C, Allapot3, UjOszlopDel, SFilter)
    ;
        SFilter = Allapot3
    ),   
    (
        C > 1, member(w, Field) ->
        C_1 is C - 1,
        nth1(R, SFilter, SFilterSor),
        nth1(C_1, SFilterSor, s(NyugatLehetosegek, NyugatField, NyugatKitoltes, NyugatErtek)),
        (
            Kitoltes mod 2 =:= 1 ->
            include(even, NyugatLehetosegek, NyugatFiltered),
            UjNyugat = s(NyugatFiltered, NyugatField, NyugatKitoltes, NyugatErtek)
        ;
            include(odd, NyugatLehetosegek, NyugatFiltered),
            UjNyugat = s(NyugatFiltered, NyugatField, NyugatKitoltes, NyugatErtek)
        ),
        setnth(C_1, SFilterSor, UjNyugat, UjSorNyugat),
        setnth(R, SFilter, UjSorNyugat, WFilter)
    ;
        WFilter = SFilter
    ),   
    (
        R > 1 ->
        oszlop(WFilter, C, SNeighborOszlop),
        R_1 is R - 1,
        nth1(R_1, SNeighborOszlop, s(EszakLehetosegek, EszakField, EszakKitoltes, EszakErtek)),
        (
            member(s, EszakField) ->
            (
               Kitoltes mod 2 =:= 1 ->
               include(even, EszakLehetosegek, EszakFiltered),
               UjEszak = s(EszakFiltered, EszakField, EszakKitoltes, EszakErtek)
            ;
               include(odd, EszakLehetosegek, EszakFiltered),
               UjEszak = s(EszakFiltered, EszakField, EszakKitoltes, DelErtek)
            ),
            setnth(R_1, SNeighborOszlop, UjEszak, UjOszlopEszak),
            setOszlop(C, WFilter, UjOszlopEszak, SNeighborFilter)
       ;
            SNeighborFilter = WFilter
       )
    ;
        SNeighborFilter = WFilter
    ),
    (
        C < K * K ->
        nth1(R, SNeighborFilter, WFilterSor),
        C1 is C + 1,
        nth1(C1, WFilterSor, s(KeletLehetosegek, KeletField, KeletKitoltes, KeletErtek)),
        (
            member(w, KeletField) ->
            (
               Kitoltes mod 2 =:= 1 ->
               include(even, KeletLehetosegek, KeletFiltered),
               UjKelet = s(KeletFiltered, KeletField, KeletKitoltes, KeletErtek)
            ;
               include(odd, KeletLehetosegek, KeletFiltered),
               UjKelet = s(KeletFiltered, KeletField, KeletKitoltes, KeletErtek)
            ),
            setnth(C1, WFilterSor, UjKelet, UjSorKelet),
            setnth(R, SNeighborFilter, UjSorKelet, UjAllapot)
       ;
            UjAllapot = SNeighborFilter
       )
    ;
        UjAllapot = SNeighborFilter
    ).
    
setOszlop(_, [], _, []).
setOszlop(C, [ AllapotSor|AllapotSorok ], [OszlopElem | OszlopMaradek], Result) :- 
    setnth(C, AllapotSor, OszlopElem, UjAllapotSor),
    Result = [UjAllapotSor | Acc],
    setOszlop(C, AllapotSorok, OszlopMaradek, Acc).

% FELULIRJA A KITOLTOTT MEZOT IS!
cellElimination(_KitoltottMezo, _R, _C, _K, [], []).
cellElimination(KitoltottMezo, R, C, K, Allapot, UjAllapot) :-
    s(_, _, Kitoltes, _ ) = KitoltottMezo,
    getCellIndex(R, C, K, CellaIndex),
    feldarabolasa(Allapot, K-K, AllapotCellak),
    nth1(CellaIndex, AllapotCellak, AllapotCella),
    eliminateFromCell(Kitoltes, AllapotCella, UjCella),
    setCella(R, C, K, UjCella, Allapot, UjAllapot).

eliminateFromCell(_KitoltottMezo, [], []).
eliminateFromCell(Kitoltes, [s(Lehetosegek, Field, Ki, Value) | AllapotCella], [UjMezo | UjCellaMaradek]):-
    (
        integer(Ki), Ki =:= Kitoltes -> UjLehetosegek = Lehetosegek     %saját maga
    ;
        excludeEqual(Kitoltes, Lehetosegek, UjLehetosegek)
    ),
    UjMezo = s(UjLehetosegek, Field, Ki, Value),
    eliminateFromCell(Kitoltes, AllapotCella, UjCellaMaradek).

setCella(R, C, K, UjCella, Allapot, UjAllapot) :-
    % a cella elso sora és oszlopa
    ElsoSorIndex is ((R-1) div K)*K + 1,
    ElsoOszlopIndex is ((C-1) div K)*K + 1,
    setCellaSoronkent(1, ElsoSorIndex, ElsoOszlopIndex, 0, K, UjCella, Allapot, UjAllapot).    

% cellaSorindex 0-tól indul!
setCellaSoronkent(SorIndex, ElsoSorIndex, _, _, K, _, AllapotMaradek, AllapotMaradek) :- 
    SorIndex >= ElsoSorIndex + K.
setCellaSoronkent(AktualisSorIndex, ElsoSorIndex, ElsoOszlopIndex, CellaSorIndex, K, UjCella, [AllapotSor | AllapotMaradek], UjAllapot) :-
    AktualisSorIndex < ElsoSorIndex + K,
    (
        AktualisSorIndex < ElsoSorIndex -> 
        UjAllapot = [AllapotSor | UjAllapotMaradek],
        AktualisSorIndex_1 = AktualisSorIndex + 1,
        setCellaSoronkent(AktualisSorIndex_1, ElsoSorIndex, ElsoOszlopIndex, CellaSorIndex, K, UjCella, AllapotMaradek, UjAllapotMaradek)
    ;
        From is CellaSorIndex*K,
        sublist(UjCella, UjCellaSor, From, K),
        ElsoOszlopIndex_1 is ElsoOszlopIndex - 1,
        take(AllapotSor, ElsoOszlopIndex_1, Eleje),
        ElsoOszlopIndex_1_K is ElsoOszlopIndex_1 + K,
        drop(AllapotSor, ElsoOszlopIndex_1_K, Vege),
        append([UjCellaSor, Vege], Temp),
        append([Eleje, Temp], UjAllapotSor),
        UjAllapot = [UjAllapotSor | UjAllapotMaradek],
        AktualisSorIndex_1 is AktualisSorIndex + 1,
        CellaSorIndex_1 is CellaSorIndex + 1,
        setCellaSoronkent(AktualisSorIndex_1, ElsoSorIndex, ElsoOszlopIndex, CellaSorIndex_1, K, UjCella, AllapotMaradek, UjAllapotMaradek)
    ).  
     
/*sublist([], _, _, _, _, _Acc).
sublist([_|_], From, _Length, FromAcc, _Acc) :-
    From > FromAcc. 
sublist([H|T], From, Length, FromAcc, Acc) :-
    Length >= 0,
    From =< FromAcc,
    Acc = [H|Rest],
    Length1 is Length - 1,
    FromAcc is FromAcc + 1,
    sublist(T, From, Length1, FromAcc1, Rest).*/
                               
% FELULIRJA A KITOLTOTT MEZOT IS!
eliminateFromCol(_KitoltottMezo, [], []).
eliminateFromCol(KitoltottMezo, [ s(Lehetosegek, Field, Ki, Value) | AllapotOszlop], [UjMezo | UjAllapotOszlopMaradek]) :-
    s( _, _, Kitoltes, _ ) = KitoltottMezo,
    excludeEqual(Kitoltes, Lehetosegek, UjLehetosegek),
    UjMezo = s(UjLehetosegek, Field, Ki, Value),
    eliminateFromCol(KitoltottMezo, AllapotOszlop, UjAllapotOszlopMaradek).

% FELULIRJA A KITOLTOTT MEZOT IS!
eliminateFromRow(_KitoltottMezo, [], []).
eliminateFromRow(KitoltottMezo, [ s(Lehetosegek, Field, Ki, Value) | AllapotSor], [UjMezo | UjAllapotSorMaradek]) :-
    s( _, _, Kitoltes, _ ) = KitoltottMezo,
    excludeEqual(Kitoltes, Lehetosegek, UjLehetosegek),
    UjMezo = s(UjLehetosegek, Field, Ki, Value),
    eliminateFromRow(KitoltottMezo, AllapotSor, UjAllapotSorMaradek).   

% currentMin kezdetben K*K + 1, ha nincs kitoltetlen, akkor R és C 0
minKitoltetlen([], _, Min, Min).
minKitoltetlen([AllapotSor | AllapotSorok], R, CurrentMin, KitoltetlenHelye) :-
    sorMinimum(AllapotSor, R, 1, CurrentMin, RowMin),
    R1 is R + 1,
    minKitoltetlen(AllapotSorok, R1, RowMin, KitoltetlenHelye).
  
sorMinimum([], _, _, CurrentMin, CurrentMin). 
sorMinimum([AllapotMezo | AllapotSor], R, CurrentC, s(MinR, MinC, Min), RowMin) :- 
    s(Lehetosegek, _, Kitoltes, _) = AllapotMezo,
    length(Lehetosegek, LLength),
    CurrentC1 is CurrentC + 1,
    (
        Min > LLength, var(Kitoltes) ->
        sorMinimum(AllapotSor, R, CurrentC1, s(R, CurrentC, LLength), RowMin)
    ;
        sorMinimum(AllapotSor, R, CurrentC1, s(MinR, MinC, Min), RowMin)
    ).
    
kezdetiAllapot([], _, []).
kezdetiAllapot([LehetsegesSor | LehetsegesMindenMezore], [Row | Rows], [AllapotSor | KezdetiAllapot]) :-
    map2(allapotMezo, LehetsegesSor, Row, AllapotSor),
    kezdetiAllapot(LehetsegesMindenMezore, Rows, KezdetiAllapot).
    
% Kitöltés érték változó    
allapotMezo(Lehetosegek, Field, s(Lehetosegek, Field, _Var, Value)) :-
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
                var(NorthNeighborValue) -> SFilter = V4
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
                var(EastNeighborValue) -> Vals = SFilter
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
                integer(Value) ->
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
                integer(Value) ->
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
                integer(Value) ->
                excludeEqual(Value, Values, Excluded),
                filterByUnit(Fields, Excluded, Result);
                % else
                filterByUnit(Fields, Values, Result)
        ).

filterByCell([], Values, _OwnValue, Values).
% field has no value
filterByCell([Field | Fields], Values, OwnValue, Result) :-
        var(OwnValue), % field has no value info
        getValue(Field, Value),
        (
                integer(Value) ->
                excludeEqual(Value, Values, Excluded),
                filterByUnit(Fields, Excluded, Result)
                % else
        ;       filterByUnit(Fields, Values, Result)
        ).
  
filterByCell(Fields, Values, OwnValue, Result) :-
        integer(OwnValue),
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

getValue([], _Var).     % return unassigned if no value
getValue([v(Info)|_Infos], Info).
getValue([Info|Infos], Value) :-
        Info \= v(_Number),
        getValue(Infos, Value).

getAllValues([], Acc, Acc).
getAllValues([Field|Fields], Acc, Result) :-
        getValue(Field, Value),
        (
            integer(Value) ->
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
