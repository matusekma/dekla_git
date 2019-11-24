:- use_module(library(lists)).
:- use_module(library(between)).

% :- type matrix == list(row).
% :- type row == list(any).
% :- type parameter ---> subRows-subCols.
% :- type subRows == int.
% :- type subCols == int.
% :- pred feldarabolasa(+matrix, +parameter, ?list(list(any))).

% :- type sspec ---> s(size, board).
% :- type size  == int.
% :- type field == list(info).
% :- type info ---> e; o; s; w; v(int).
% :- type board == list(list(field)).

% ADATSTRUKTÚRA
% :-type kitoltes == int.
% :-type sajatertek == int.
% :-type lehetosegek == list(int).
% :-type allapotMezo() ---> s(lehetosegek, field, kitoltes, sajatertek).
% :-type allapot == list(list(allapotMezo)).

% :- type ssol == list(list(int)).

% :- pred sudoku(sspec::in, ssol::out).
% SSol az SSpec feladványt kielégítő megoldás.
sudoku(s(K,F), SSol) :-
    K2 is K*K,
    numlist(1, K2, AllValues), 
        
    feldarabolasa(F, K-K, Cells),   
    Rows = F,
    getCols(F, K2, 1, Cols),
    
    lehetsegesMindenMezore(Rows, K2, 1, K, Cols, Cells, AllValues, LehetsegesMindenMezore),
    
    kezdetiAllapot(LehetsegesMindenMezore, Rows, KezdetiAllapot),
     

    megold(KezdetiAllapot, AllValues, K, SSol).

% :-pred megold(allapot::in, list(int)::in, int::in, ssol::out).
% A sudoku eljárásban előállított kezdeti állapot alapján felsorolja
% a megoldásokat. Először alkalmazza a Hidden Single
% szabályt sorokra és oszlopokra, majd keres egy kitöltetlen cellát, és ha van, akkor a következő eljárás segítségével kitölti
% minden lehetséges módon.
megold(KezdetiAllapot, AllValues, K, SSol) :-
    K21 is K * K + 1,    
    [KezdetiAllapotElsoSor | _] = KezdetiAllapot,
    assignHiddenSingleForRows(KezdetiAllapotElsoSor, 1, K, AllValues, KezdetiAllapot, Allapot1),
    oszlop(Allapot1, 1, ElsoOszlop),
    assignHiddenSingleForCols(ElsoOszlop, 1, K, AllValues, Allapot1, Allapot),
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

% :-pred megoldasAllapotbol(allapot::in, ssol::out).
% A bemenő Allapot-ból kinyerve a kitöltéseket kapjuk Megoldas-t.
megoldasAllapotbol([], []).
megoldasAllapotbol([AllapotSor | Allapot], Megoldas) :-
    megoldasSorAllapotSorbol(AllapotSor, MegoldasSor),
    Megoldas = [MegoldasSor | MaradekMegoldasSor],
    megoldasAllapotbol(Allapot, MaradekMegoldasSor).

% :-pred megoldasSorAllapotSorbol(list(allapotMezo)::in, list(int)::out).
% A bemenő AllapotSor-ból kinyerve a kitöltéseket kapjuk a megoldás egy sorát.     
megoldasSorAllapotSorbol([], []).
megoldasSorAllapotSorbol([s(_, _, Kitoltes, _) | AllapotSor], MegoldasSor) :-
    MegoldasSor = [Kitoltes | MaradekMegoldasMezo],
    megoldasSorAllapotSorbol(AllapotSor, MaradekMegoldasMezo).

% :-pred osszesErtekkelKitoltEsFrissit(allapot::in, allapotMezo::in, list(allapotMezo)::in, int::in, int::in, list(int)::in, int::in, Sol::ssol).
% A 2. paraméterben kapott mezőt kitölti az egyik lehetséges értékkel, majd az eliminate függvénnyel frissíti
% a feladvány állapotát, és meghívja a megold eljárást, hogy ehhez az állapothoz
% találjon megoldást. Emellett kipróbálja rekurzívan a következő értéket is.                     
osszesErtekkelKitoltEsFrissit(_Allapot, s([], _Field, _, _), _Sor, _RowIndex, _ColIndex, _AllValues, _K, _Sol) :- false.
osszesErtekkelKitoltEsFrissit(Allapot, s([Ertek | Lehetosegek], Field, Kitoltes, OwnValue), AllapotSor, RowIndex, ColIndex, AllValues, K, Sol) :-
            (
                var(OwnValue) ->
                KitoltottMezo = s([], [v(Ertek)|Field], Ertek, Ertek)
            ;    
                KitoltottMezo = s([], Field, Ertek, Ertek) % mar benne van az ertek
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

% :-pred assignHiddenSingleForRows(list(allapotMezo)::in, int::in, int::in, list(int)::in, allapot::in, allapot::out).
% Az aktuális állapot minden sorára alkalmazza a "Hidden Single" szabályt (lásd. dokumentáció),
% és az új állapotot adja vissza a kimenő paraméterben.
% CurrenRowIndex kezdetben 1, az aktuális sor, K és AllValues konstans szám és lista, az ElozoAllapot
% pedig a frissítendő állapot.
assignHiddenSingleForRows(ElsoSor, CurrentRowIndex, K, AllValues, ElozoAllapot, EredmenyAllapot) :-
    assignHiddenSingleInRow(ElsoSor, AllValues, CurrentRowIndex, K, ElozoAllapot, UjAllapot),
    (
        CurrentRowIndex + 1 =< K*K ->
        CurrentRowIndex1 is CurrentRowIndex + 1,
        nth1(CurrentRowIndex1, UjAllapot, KovetkezoSor),
        assignHiddenSingleForRows(KovetkezoSor, CurrentRowIndex1, K, AllValues, UjAllapot, EredmenyAllapot)
    ;
        EredmenyAllapot = UjAllapot
    ).

% :-pred assignHiddenSingleInRow(list(allapotMezo)::in, list(int)::in, int::in, int::in, allapot::in, allapot::out).
% Az AllapotSor sorban alkalmazza a Hidden Single szabályt, azaz kitölti azokat
% a mezőket, amelyekben a sorban egyedi érték lehetséges. Kimenő paramétere az új állapot.
assignHiddenSingleInRow(_AllapotSor, [], _R, _K, Allapot, Allapot).
assignHiddenSingleInRow(AllapotSor, [Value | Values], R, K, Allapot, EredmenyAllapot) :-
    (
        countIfOccursOnceInLists(AllapotSor, Value, 1, 0, s(s(_Lehetosegek, Field, _, OwnValue ), ColPosition)) ->
        (
            var(OwnValue) ->
            KitoltottMezo = s([], [v(Value)|Field], Value, Value)
        ;
            KitoltottMezo = s([], Field, Value, Value)% mar benne van az ertek
        ),
        setnth(ColPosition, AllapotSor, KitoltottMezo, UjSor),
        setnth(R, Allapot, UjSor, KitoltottAllapot),
        oszlop(KitoltottAllapot, ColPosition, AllapotOszlop),

        eliminate(K, R-ColPosition, KitoltottMezo, 0, AllapotOszlop, KitoltottAllapot, UjAllapot),
        nth1(R, UjAllapot, UjAllapotSor),
            
        assignHiddenSingleInRow(UjAllapotSor, Values, R, K, UjAllapot, EredmenyAllapot)
    ;
        assignHiddenSingleInRow(AllapotSor, Values, R, K, Allapot, EredmenyAllapot)
    ).

% :-pred assignHiddenSingleForCols(list(allapotMezo)::in, int::in, int::in, list(int)::in, allapot::in, allapot::out).
% Az aktuális állapot minden oszlopára alkalmazza a "Hidden Single" szabályt (lásd. dokumentáció),
% és az új állapotot adja vissza a kimenő paraméterben.
% CurrenColIndex kezdetben 1, az aktuális sor, K és AllValues konstans szám és lista, az ElozoAllapot
% pedig a frissítendő állapot.
assignHiddenSingleForCols(ElsoOszlop, CurrentColIndex, K, AllValues, ElozoAllapot, EredmenyAllapot) :-
    assignHiddenSingleInCol(ElsoOszlop, AllValues, CurrentColIndex, K, ElozoAllapot, UjAllapot),
    (
        CurrentColIndex + 1 =< K*K ->
        CurrentColIndex1 is CurrentColIndex + 1,
        oszlop(UjAllapot, CurrentColIndex1, KovetkezoOszlop),
        assignHiddenSingleForCols(KovetkezoOszlop, CurrentColIndex1, K, AllValues, UjAllapot, EredmenyAllapot)
    ;
        EredmenyAllapot = UjAllapot
    ).

    
% :-pred assignHiddenSingleInCol(list(allapotMezo)::in, list(int)::in, int::in, int::in, allapot::in, allapot::out).
% Az AllapotOszlop oszlopban alkalmazza a Hidden Single szabályt, azaz kitölti azokat
% a mezőket, amelyekben az oszlopban egyedi érték lehetséges. Kimenő paramétere az új állapot.
assignHiddenSingleInCol(_AllapotOszlop, [], _C, _K, Allapot, Allapot).
assignHiddenSingleInCol(AllapotOszlop, [Value | Values], C, K, Allapot, EredmenyAllapot) :-
    (
        countIfOccursOnceInLists(AllapotOszlop, Value, 1, 0, s(s(_Lehetosegek, Field, _, OwnValue ), RowPosition)) ->
        (
            var(OwnValue) ->
            KitoltottMezo = s([], [v(Value)|Field], Value, Value)
        ; 
            KitoltottMezo = s([], Field, Value, Value) % mar benne van az ertek
        ),
        setnth(RowPosition, AllapotOszlop, KitoltottMezo, UjOszlop),
        setOszlop(C, Allapot, UjOszlop, KitoltottAllapot),
        nth1(RowPosition, KitoltottAllapot, AllapotSor),
            
            
        eliminate(K, RowPosition-C, KitoltottMezo, AllapotSor, 0, KitoltottAllapot, UjAllapot),
        oszlop(UjAllapot, C, UjAllapotOszlop),
            
        assignHiddenSingleInCol(UjAllapotOszlop, Values, C, K, UjAllapot, EredmenyAllapot)
    ;
        assignHiddenSingleInCol(AllapotOszlop, Values, C, K, Allapot, EredmenyAllapot)
    ).

% :-pred countIfOccursOnceInLists(list(allapotMezo)::in, int::in, int::in, int::in, s(allapotmezo, int)::out).
% Az állapotmezőkön végigmenve ellenőrzi, hogy a mezők lehetőséglistáiból csak 1-ben szerepel-e a Value érték.
% Ha igen, akkor visszaadja a mezőt és a pozícióját a kimenő paraméterben, egyébként meghiúsul.
countIfOccursOnceInLists([], _, _, 1, _FieldAndPosition).
countIfOccursOnceInLists([Field|T], Value, CurrentPosition, Count, FieldAndPosition) :-
    Count =< 1,
    s(Lehetosegek, _, _Kitoltes, _ ) = Field,
    CurrentPosition1 is CurrentPosition + 1,
    (
        member(Value, Lehetosegek) ->
        Count1 is Count + 1,
        FieldAndPosition = s(Field, CurrentPosition),
        countIfOccursOnceInLists(T, Value, CurrentPosition1, Count1, FieldAndPosition)
    ;
        countIfOccursOnceInLists(T, Value, CurrentPosition1, Count, FieldAndPosition)
    ).
  
% :-pred eliminate(int::in, coords::in, allapotMezo::in, list(allapotMezo)::in, list(allapotMezo)::in, allapot::in, allapot::out).
% Az R-C koordinátára behelyettesített KitoltottMezo mező az AllapotSor és AllapotOszlop sor és oszlop
% metszetében van, az eliminate eljárás ezután a behelyettesítés után hoz létre új állapotot az alapszabályok,
% a paritási szabályok és a Naked Pairs illetve Naked Triplets nevű szabályok alkalmazásával, a megmaradó lehetőségek szűrésével,
% és az uj allapotot adja vissza kimenő paraméterében.                                                                                                                                                                                    
eliminate(K, R-C, KitoltottMezo, AllapotSor, AllapotOszlop, Allapot, UjAllapot) :-
    s(_Lehetseges, Field, Kitoltes, _OwnValue) = KitoltottMezo,
   
    (
        AllapotSor \= 0 ->
        eliminateFromRow(KitoltottMezo, AllapotSor, UjSor),
        setnth(C, UjSor, KitoltottMezo, UjSorKitoltottMezovel),
        setnth(R, Allapot, UjSorKitoltottMezovel, Allapot1)
    ;
        Allapot1 = Allapot
    ),

    (
        AllapotOszlop \= 0 ->
        eliminateFromCol(KitoltottMezo, AllapotOszlop, UjOszlop),
        setnth(R, UjOszlop, KitoltottMezo, UjOszlopKitoltottMezovel),
        setOszlop(C, Allapot1, UjOszlopKitoltottMezovel, Allapot2)
    ;
        Allapot2 = Allapot1
    ),
        
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
            setnth(R, SNeighborFilter, UjSorKelet, WNeighborFilter)
       ;
            WNeighborFilter = SNeighborFilter
       )
    ;
        WNeighborFilter = SNeighborFilter
    ),
    (
        K >= 4 ->
        nth1(R, WNeighborFilter, Sor),
        
        getLehetosegek(Sor, SorLehetosegek),
        eliminateByNakedTriplets(SorLehetosegek, Sor, NakedTripletsSor),
            
        getLehetosegek(NakedTripletsSor, SorLehetosegek1),
        eliminateByNakedPairs(SorLehetosegek1, NakedTripletsSor, NakedPairsSor1),
        setnth(R, WNeighborFilter, NakedPairsSor1, NakedPairsSorAllapot1),

        oszlop(NakedPairsSorAllapot1, C, Oszlop),
        getLehetosegek(Oszlop, OszlopLehetosegek),
        eliminateByNakedTriplets(OszlopLehetosegek, Oszlop, NakedTripletsOszlop),

        getLehetosegek(NakedTripletsOszlop, OszlopLehetosegek1),
        eliminateByNakedPairs(OszlopLehetosegek1, NakedTripletsOszlop, NakedPairsOszlop1),
        setOszlop(C, NakedPairsSorAllapot1, NakedPairsOszlop1, NakedPairsOszlopAllapot1),

        getCellIndex(R, C, K, CellaIndex),
        feldarabolasa(NakedPairsOszlopAllapot1, K-K, AllapotCellak), 
        nth1(CellaIndex, AllapotCellak, AllapotCella),
        getLehetosegek(AllapotCella, CellaLehetosegek),
        
        eliminateByNakedPairs(CellaLehetosegek, AllapotCella, NakedPairsCella),
        setCella(R, C, K, NakedPairsCella, NakedPairsOszlopAllapot1, UjAllapot)
    ;
        UjAllapot = WNeighborFilter
    ).
               
% :-pred setOszlop(int::in, list(list(any))::in, any::in, list(list(any))::out).
% Beállítja listák listája által képzett mátrixban a C-edik oszlopot, és visszaadja az új állapotot
% kimenő paraméterében.   
setOszlop(_, [], _, []).
setOszlop(C, [ AllapotSor|AllapotSorok ], [OszlopElem | OszlopMaradek], Result) :- 
    setnth(C, AllapotSor, OszlopElem, UjAllapotSor),
    Result = [UjAllapotSor | Acc],
    setOszlop(C, AllapotSorok, OszlopMaradek, Acc).

% :-pred cellElimination(allapotMezo::in, int::in, int::in, int::in, allapot::in, allapot::out).
% Megkeresi a kapott koordinátájú mezőt tartalmazó cellát, és a
% KitoltottMezo kitöltése alapján kiszűri többi cellamezőből ezt az értéket,
% és kimenő paraméterében visszaadja az új állapotot.
cellElimination(_KitoltottMezo, _R, _C, _K, [], []).
cellElimination(KitoltottMezo, R, C, K, Allapot, UjAllapot) :-
    s(_, _, Kitoltes, _ ) = KitoltottMezo,
    getCellIndex(R, C, K, CellaIndex),
    feldarabolasa(Allapot, K-K, AllapotCellak),
    nth1(CellaIndex, AllapotCellak, AllapotCella),
    eliminateFromCell(Kitoltes, AllapotCella, UjCella),
    setCella(R, C, K, UjCella, Allapot, UjAllapot).

% :-pred eliminateFromCell(int::in, list(allapotMezo)::in, list(allapotMezo)::out).
% Egy cella mezőinek Lehetosegek listájából kiszűri a Kitoltes értékét, 
% és kimenő paraméterében visszaadja az új cellát.
eliminateFromCell(_KitoltottMezo, [], []).
eliminateFromCell(Kitoltes, [s(Lehetosegek, Field, Ki, Value) | AllapotCella], [UjMezo | UjCellaMaradek]):-
    (
        integer(Ki), Ki =:= Kitoltes -> UjLehetosegek = Lehetosegek     %saját maga
    ;
        excludeEqual(Kitoltes, Lehetosegek, UjLehetosegek)
    ),
    UjMezo = s(UjLehetosegek, Field, Ki, Value),
    eliminateFromCell(Kitoltes, AllapotCella, UjCellaMaradek).

% :-pred eliminateByNakedPairs(list(list(int))::in, list(allapotMezo)::in, list(allapotMezo)::out).
% Az EgysegLehetosegek tartalmazza egy sor, oszlop vagy cella (egység) mezőiben lévő lehetősegek listáját,
% az eljárás erre alkalmazza a Naked Pairs (lásd. dokumentáció) szabályt, és visszaadja az EgeszEgyseg egység
% eszerint módosított változatát kimenő paraméterében.
eliminateByNakedPairs([], EgeszEgyseg, EgeszEgyseg).
eliminateByNakedPairs([[A1, A2]| EgysegLehetosegek], EgeszEgyseg, UjEgyseg) :-
    (
        countIfOccursOnce(EgysegLehetosegek, [A1, A2], 0) ->
        filterEgysegByNakedPairsOrTriplets(EgeszEgyseg, [A1, A2], FilteredEgyseg),
        eliminateByNakedPairs(EgysegLehetosegek, FilteredEgyseg, UjEgyseg)
    ;
        eliminateByNakedPairs(EgysegLehetosegek, EgeszEgyseg, UjEgyseg)
    ).
eliminateByNakedPairs([ H | Egyseg], EgeszEgyseg, UjEgyseg) :-
    H \= [_,_],
    eliminateByNakedPairs(Egyseg, EgeszEgyseg, UjEgyseg).

% :-pred eliminateByNakedTriplets(list(list(int))::in, list(allapotMezo)::in, list(allapotMezo)::out).
% Az EgysegLehetosegek tartalmazza egy sor, oszlop vagy cella (egység) mezőiben lévő lehetősegek listáját,
% az eljárás erre alkalmazza a Naked Triplets (lásd. dokumentáció) szabályt, és visszaadja az EgeszEgyseg egység
% eszerint módosított változatát kimenő paraméterében.
eliminateByNakedTriplets([], EgeszEgyseg, EgeszEgyseg).
eliminateByNakedTriplets([[A1, A2, A3]| EgysegLehetosegek], EgeszEgyseg, UjEgyseg) :-
    (    
        countIfOccursTwice(EgysegLehetosegek, [A1, A2, A3], 0) ->
        filterEgysegByNakedPairsOrTriplets(EgeszEgyseg, [A1, A2, A3], FilteredEgyseg),      
        eliminateByNakedTriplets(EgysegLehetosegek, FilteredEgyseg, UjEgyseg)
    ;
        eliminateByNakedTriplets(EgysegLehetosegek, EgeszEgyseg, UjEgyseg)
    ).
eliminateByNakedTriplets([ H | Egyseg], EgeszEgyseg, UjEgyseg) :-
    H \= [_,_,_],
    eliminateByNakedTriplets(Egyseg, EgeszEgyseg, UjEgyseg).

% :-pred filterEgysegByNakedPairsOrTriplets(list(allapotMezo)::in, list(int)::in, list(allapotMezo)::out).
% Elvégzi a bemenő egységre a 2. bemenő paraméterben megadott Pair vagy Triplet
% által meghatározott szűrést, és kimenő paraméterében visszaadja az újat.
filterEgysegByNakedPairsOrTriplets([], _, []).
filterEgysegByNakedPairsOrTriplets([ s(Lehetosegek, Field, Ki, Value) | Egyseg], PairOrTriplet, UjEgyseg) :-
     (
         Lehetosegek = PairOrTriplet -> 
         UjEgyseg = [s(Lehetosegek, Field, Ki, Value) | UjEgysegMaradek],
         filterEgysegByNakedPairsOrTriplets(Egyseg, PairOrTriplet, UjEgysegMaradek)
     ;
         filterNotMember(Lehetosegek,  PairOrTriplet, UjLehetosegek),
         UjEgyseg = [s(UjLehetosegek , Field, Ki, Value) | UjEgysegMaradek],
         filterEgysegByNakedPairsOrTriplets(Egyseg, PairOrTriplet, UjEgysegMaradek)
     ).

% :-pred filterNotMember(list(any)::in, list(any)::in, list(any)::out).
% Az első paraméterben megadott listából kimenő paraméterében visszaadja azon értékeket, melyek nem szerepelnek
% a második lista értékei között.
filterNotMember([], _FilterList, []).
filterNotMember([Lehetoseg | Lehetosegek ], FilterList, UjLehetosegek) :-
    (
        member(Lehetoseg, FilterList) ->
        UjLehetosegek = MaradekLehetosegek,
        filterNotMember(Lehetosegek, FilterList, MaradekLehetosegek)
    ;
        UjLehetosegek = [Lehetoseg | MaradekLehetosegek],
        filterNotMember(Lehetosegek, FilterList, MaradekLehetosegek)
    ).

% :-pred setCella(int::in, int::in, int::in, list(allapotMezo)::in, allapot::in, allapot::out).    
% Beállítja az R-C koordinátájú állapotmezőt tartalmazó cella mezőit 
% a setCellaSoronként eljárással az UjCella értékeire, és visszaadja az új állapotot kimenő paraméterében.                  
setCella(R, C, K, UjCella, Allapot, UjAllapot) :-
    % a cella elso sora és oszlopa
    ElsoSorIndex is ((R-1) div K)*K + 1,
    ElsoOszlopIndex is ((C-1) div K)*K + 1,
    setCellaSoronkent(1, ElsoSorIndex, ElsoOszlopIndex, 0, K, UjCella, Allapot, UjAllapot).    

% :-pred setCellaSoronkent(int::in, int::in, int::in, int::in, int::in, list(allapotMezo)::in, allapot::in, allapot::out).
% K hosszú sorrészletekre bontva soronként állítja be az UjCella cellát az ElsoSorIndex, ElsoOszlopIndex
% bal felső sarok-koordinátájú cellaként az Allapot állapotban, és visszaadja az új állapotot kimenő paraméterében.
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
    
% :-pred eliminateFromCol(allapotMezo::in, list(allapotMezo)::in, list(allapotMezo)::out).                               
% A KitoltottMezo értéke alapján az AllapotOszlop mezőinek Lehetosegek listájából kiszűri a KitoltottMezo
% Kitoltes-ét, és  visszaadja az új állapotoszlopot kimenő paraméterében.
% FELULIRJA A KITOLTOTT MEZOT IS!
eliminateFromCol(_KitoltottMezo, [], []).
eliminateFromCol(KitoltottMezo, [ s(Lehetosegek, Field, Ki, Value) | AllapotOszlop], [UjMezo | UjAllapotOszlopMaradek]) :-
    s( _, _, Kitoltes, _ ) = KitoltottMezo,
    excludeEqual(Kitoltes, Lehetosegek, UjLehetosegek),
    UjMezo = s(UjLehetosegek, Field, Ki, Value),
    eliminateFromCol(KitoltottMezo, AllapotOszlop, UjAllapotOszlopMaradek).

% :-pred eliminateFromRow(allapotMezo::in, list(allapotMezo)::in, list(allapotMezo)::out).
% A KitoltottMezo értéke alapján az AllapotSor mezőinek Lehetosegek listájából kiszűri a KitoltottMezo
% Kitoltes-ét, és  visszaadja az új állapotsort.
% FELULIRJA A KITOLTOTT MEZOT IS!
eliminateFromRow(_KitoltottMezo, [], []).
eliminateFromRow(KitoltottMezo, [ s(Lehetosegek, Field, Ki, Value) | AllapotSor], [UjMezo | UjAllapotSorMaradek]) :-
    s( _, _, Kitoltes, _ ) = KitoltottMezo,
    excludeEqual(Kitoltes, Lehetosegek, UjLehetosegek),
    UjMezo = s(UjLehetosegek, Field, Ki, Value),
    eliminateFromRow(KitoltottMezo, AllapotSor, UjAllapotSorMaradek).   

% :-pred minKitoltetlen(allapot::in, int::in, s(int, int, int)::in, s(int, int, int)::out).
% Kikeresi az Allapot állapotban a minimális kitöltési lehetőséggel rendelkező kitöltetlen mező pozícióját.
% currentMin kezdetben s(0, 0, K*K + 1), R pedig 0, ha nincs kitoltetlen, akkor a visszatérési érték a kezdeti
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
 
% :-pred kezdetiAllapot(list(list(int))::in, board::in, allapot::out).
% A megoldás kezdetén előállított lehetséges értékekből és a feladványból
% létrehozza a kezdeti adatstruktúrát.  
kezdetiAllapot([], _, []).
kezdetiAllapot([LehetsegesSor | LehetsegesMindenMezore], [Row | Rows], [AllapotSor | KezdetiAllapot]) :-
    map2(allapotMezo, LehetsegesSor, Row, AllapotSor),
    kezdetiAllapot(LehetsegesMindenMezore, Rows, KezdetiAllapot).

% :-pred allapotMezo(list(int)::in, field::in, allapotMezo::out)    
% A paraméterként megkapott lehetőségek listájából és egy feladványmezőből előállít
% egy adatstruktúrának megfelelő állapotmezőt kimenő paraméterében.
% Kitöltés érték változó    
allapotMezo(Lehetosegek, Field, s(Lehetosegek, Field, _Var, Value)) :-
    getValue(Field, Value).

% :-pred lehetsegesMindenMezore(board::in, int::in, int::in, int::in, list(field)::in, list(field)::in, list(int)::in, list(list(int))::in).
% Az ertekek/8 eljárás használatával soronként meghatározza a feladvány mezőibe
% behelyettesíthető értékek listáját, és kimenő paraméterében visszaadja az összes sor
% összes mezőjére ezeket az értékeket.    
% A többi paraméter segédparaméter vagy a mátrix oszlopai/cellái.
lehetsegesMindenMezore([], _K2, _CurrentRowIndex, _K, _Cols, _Cells, _AllValues, []).
lehetsegesMindenMezore([Row | Rows], K2, CurrentRowIndex, K, Cols, Cells, AllValues, [Ertekek | RestResult]) :-
    lehetsegesErtekSorra(Row, K, Row, Cols, Cells, CurrentRowIndex, AllValues, 1, Ertekek),
    NextRowIndex is CurrentRowIndex + 1,
    lehetsegesMindenMezore(Rows, K2, NextRowIndex, K, Cols, Cells, AllValues, RestResult).

% :-pred getCols(list(list(any))::in, int::in, int::in, list(list(any))::out).
% Kimenő paraméterében visszaadja egy K2 méretű mátrix oszlopainak listáját.     
getCols(_Mx, K2, IndexAcc, []) :-
    IndexAcc > K2.                                                                                  
getCols(Mx, K2, IndexAcc, [Col | OtherCols]) :-
    IndexAcc =< K2,
    oszlop(Mx, IndexAcc, Col),
    IndexAcc1 is IndexAcc + 1,
    getCols(Mx, K2, IndexAcc1, OtherCols).
 
% :-pred setnth(int::in, list(any)::in, any::in, list(any)::out).
% A második paraméterben kapott lista első paraméterben megadott
% sorszámú elemét a harmadik paraméterben megadottra állítva kapjuk a kimenő paramétert.   
setnth(1, [_|Rest], New, [New|Rest]).
setnth(I, [E|Rest], New, [E|NewEnd]) :- 
    I > 1,
    I1 is I-1,
    setnth(I1, Rest, New, NewEnd).
                
% :-pred map2(pred::in, list(any)::in, list(any)::in, list(any)::out).
% 2 lista adott predikátummal való módosításával kapjuk a kimenő listát.
map2(_Pred, [], _, []).
map2(Pred, [H1 | T1], [H2 | T2], Result) :-
    call(Pred, H1, H2, Mapped),
    Result = [Mapped | Maradek],
    map2(Pred, T1, T2, Maradek).

% :-pred lehetsegesErtekSorra(list(field)::in, int::in, list(field)::in, list(list(field))::in, list(list(field))::in, int::in, list(int)::in, int::in, list(int)::out).
% Adott sorra a segédparaméterek és az értékek/8 predikátum segítségével
% kiszámolja a behelyettesíthető értékeket.
% sort ad vissza értékekkel, Cacc default 0
lehetsegesErtekSorra([], _, _WholeRow, _Cols, _Cells, _R, _AllValues, _CAcc, []).
lehetsegesErtekSorra([Field | RowTail], K, WholeRow, Cols, Cells, R, AllValues, CAcc, Ertekek) :-
    ertekek(K, R-CAcc, Field, AllValues, Cells, WholeRow, Cols, MezoErtekek),
    Ertekek = [MezoErtekek | MaradekMezoErtekek],
    CAcc1 is CAcc + 1,
    lehetsegesErtekSorra(RowTail, K, WholeRow, Cols, Cells, R, AllValues, CAcc1, MaradekMezoErtekek).

% :-pred ertekek(int::in, coords::in, field::in, list(int)::in, list(list(field))::in, list(field)::in, list(list(field))::in, list(int)::out).
% A Vals lista az SSpec specifikáció alapján az R-C koordinátájú mezőben megengedett értékek listája.
% A Cells a specifikáció celláinak listája, a Row és a Col pedig adott sora és oszlopai, AllValues 1-től K*K-ig az egész számok.
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

% :-pred filterByConstraints(field::in, list(field)::in, list(field)::in, int::in, int::in, list(int)::in, list(int)::out).
% A Field mező kényszerei (e,o,s,w) alapján szűri annak lehetséges értékeit, és a Result listában adja vissza azokat.
% Row, Col, R, és C rendre a feladvány adott sora és oszlopa illetve azok sorszámai.
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
                filterByConstraints(FieldTail, Row, Col, R, C, FilteredValues, Result)
        ;
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
                    Value mod 2 =:= 1 -> include(even, Values, FilteredValues)
                ;
                    % else
                    include(odd, Values, FilteredValues)
                ),
                filterByConstraints(FieldTail, Row, Col, R, C, FilteredValues, Result)
        ;
                % else
                filterByConstraints(FieldTail, Row, Col, R, C, Values, Result)
        ).

filterByConstraints([v(Number)|FieldTail], Row, Col, R, C, _Values, Result) :- 
        filterByConstraints(FieldTail, Row, Col, R, C, [Number], Result).

% :-pred filterByUnit(list(field)::in, list(int)::in, list(int)::out).
% A sorban/oszlopban lévő többi szám alapján szűri a Values listában található
% lehetőségeket, és a szűrt Result listát adja vissza.
filterByUnit([], Values, Values).
filterByUnit([Field | Fields], Values, Result) :-
        getValue(Field, Value),
        (
                integer(Value) ->
                excludeEqual(Value, Values, Excluded),
                filterByUnit(Fields, Excluded, Result)
        ;
                % else
                filterByUnit(Fields, Values, Result)
        ).

% :-pred filterByCell(list(field)::in, list(int)::in, int::in, list(int)::out).
% A cellában lévő többi szám alapján szűri a Values listában található
% lehetőségeket, és a szűrt Result listát adja vissza. 
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
        ;       
                filterByUnit(Fields, Values, Result)
        ).
  
filterByCell(Fields, Values, OwnValue, Result) :-
        integer(OwnValue),
        getAllValues(Fields, [], CellValues),
        countValueOccurences(CellValues, OwnValue, 0, Occurences),
        (
            Occurences > 1 ->
            Result = []
        ;   
            Result = Values
        ).

%countIfOccursOnce(any::in, any::in, int::in).
% Egy listában Value érték 1-szer szerepel-e. Ha igen, siker, ha nem, meghiúsul. Count az akkumulátor.    
countIfOccursOnce([], _, 1).
countIfOccursOnce([Value|T], Value, Count) :-
    Count1 is Count + 1,
    Count1 =< 1,
    countIfOccursOnce(T, Value, Count1).
countIfOccursOnce([H|T], Value, Count) :-
    Count =< 1,
    H \= Value,
    countIfOccursOnce(T, Value, Count).

%countIfOccursTwice(any::in, any::in, int::in).
% Egy listában Value érték 2-szer szerepel-e. Ha igen, siker, ha nem, meghiúsul. Count az akkumulátor. 
countIfOccursTwice([], _, 2).
countIfOccursTwice([Value|T], Value, Count) :-
    Count1 is Count + 1,
    Count1 =< 2,
    countIfOccursTwice(T, Value, Count1).
countIfOccursTwice([H|T], Value, Count) :-
    Count =< 2,
    H \= Value,
    countIfOccursTwice(T, Value, Count).

% :-pred countValueOccurences(list(any)::in, any::in, int::in, int::out).
% Adott listában a Value érték Count-szor szerepel.
% Acc kezdetben 0
countValueOccurences([], _Value, Acc, Acc).
countValueOccurences([H|T], Value, Acc, Result) :-
        (
                Value =:= H ->
                Acc1 is Acc + 1,
                countValueOccurences(T, Value, Acc1, Result)
        ;       % else
                countValueOccurences(T, Value, Acc, Result)
        ).   

% :-pred excludeEqual(any::in, list(any)::in, list(any)::out).  
% A Value értéket kiveszi a 2. paraméterben kapott listából,
% és a kapott listát a kimenő paraméterben adja vissza.                                                       
excludeEqual(_Value, [], []).

excludeEqual(Value, [H|T], Excluded) :-
        H =:= Value,
        excludeEqual(Value, T, Excluded).

excludeEqual(Value, [H|T], Excluded) :-
        H =\= Value,
        Excluded = [H | Maradek],
        excludeEqual(Value, T, Maradek).                     

% Egyenlőséget, páros, páratlan tulajdonságot vizsgáló predikátumok.                                     
equal(X, Value) :- X =:= Value.
even(X) :- X mod 2 =:= 0.
odd(X) :- X mod 2 =:= 1.

% :-pred getValue(field::in, int;var::out).
% Visszaadja a Field mezőben található értéket, ha van, egyébként változót. 
getValue([], _Var).     % return unassigned if no value
getValue([v(Info)|_Infos], Info).
getValue([Info|Infos], Value) :-
        Info \= v(_Number),
        getValue(Infos, Value).

% :-pred getAllValues(list(field)::in, list(int)::in, list(int)::out).
% Visszaadja a Field listában található értékek listáját a kimenő paraméterben. 
getAllValues([], Acc, Acc).
getAllValues([Field|Fields], Acc, Result) :-
        getValue(Field, Value),
        (
            integer(Value) ->
            getAllValues(Fields, [Value|Acc], Result)
        ;   
            getAllValues(Fields, Acc, Result)
        ).    

% :-pred getLehetosegek(list(allapotMezo)::in, list(list(int))::out).
% A bemenő mezőlistából előállítja a kimenő paraméterben a lehetséges behelyettesítési
% értékek listáit.
getLehetosegek([], []).
getLehetosegek([AllapotMezo | AllapotMezok], LehetosegListak) :-
    s(Lehetosegek, _, _, _) = AllapotMezo,
    LehetosegListak = [Lehetosegek | Maradek],
    getLehetosegek(AllapotMezok, Maradek).

% :-pred getCellIndex(int::in, int::in, int::in, int::in).
% Visszaadja kimenő paraméterében, hogy az adott koordinátájú mező hányadik cellában van a feldarabolásban.
getCellIndex(R, C, K, Index) :-
        %sorszam 0-tol
            Sorszam is (R-1) * K * K + C - 1,
            
            Chunk is Sorszam div K,
            % minden sorban K darab és minden cellában K sor
            Row is Chunk div (K*K),
            Col is Chunk rem K,
            Index is Row * K + Col + 1.

removeFromList(Index, List, Result) :- nth1(Index, List, _, Result).                         

% :-pred oszlop(matrix::in, int::in, list(any)::out).
% visszaadja az SSpec specifikációval megadott Sudoku-feladvány vagy más mátrix C-edik oszlopát
oszlop([], _C, []).
oszlop([Row| Rows], C, Col) :- 
        nth1(C, Row, Field),
        Col = [Field | Maradek],
        oszlop(Rows, C, Maradek).

take([], _, []).
take(L, 0, []) :- L \= [].
take([H|T], R, Acc) :- 
        R > 0, 
        R1 is R-1, 
        Acc = [H|Vegebol],
        take(T, R1, Vegebol).

drop([], _, []).
drop(L, 0, L) :- L \= [].
drop([_|T], N, Eredmeny) :- 
        N > 0, 
        N1 is N-1, 
        Eredmeny = Acc,
        drop(T, N1, Acc).
     
% :-pred vizszintesen(matrix::in, int::in, list(list(any))::out).   
%% Mx mátrix R soronkénti feldarabolása LL listák listája (R sorú sorcsoportok)             
vizszintesen([], _, []).
vizszintesen(Mx, R, LL) :- 
        Mx \= [],
        take(Mx, R, Eleje),
        drop(Mx, R, Vege),
        LL = [Eleje | Maradek],
        vizszintesen(Vege, R, Maradek).

% :-pred fuggolegesenIedikOszlopcsoportra(list(list(any))::in, int::in, int::in, list(list(any))::out).
% létrehozza az I-edik fuggoleges vágást
fuggolegesenIedikOszlopcsoportra([], _C, _I, []).
fuggolegesenIedikOszlopcsoportra([H|T], C, I, Acc) :- 
        [H|T] \= [],
        Eldobando is I * C,
        drop(H, Eldobando, Vege),
        take(Vege, C, E),
        append(E, A, Acc),
        fuggolegesenIedikOszlopcsoportra(T, C, I, A).

% :-pred fuggolegesenEgySorCsoportot(list(list(any))::in, int::in, int::in, list(list(any))::out).                          
% felvág 1 sorcsoportot, azaz valahány egymás utáni sort C oszloponként, I az oszlopcsoport sorszáma, 0-tól indul
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
        
% :-pred sudoku:fuggolegesen(list(list(any))::in, int::in, list(list(any))::out).
% C oszloponként függőlegesen felvágja a sorcsoportokat.
fuggolegesen([], _C, []).
fuggolegesen([HLss | TLss], C, Acc) :- 
        [HLss | TLss] \= [],
        fuggolegesenEgySorCsoportot(HLss, C, 0, F),
        append(F, Acc0, Acc),
        fuggolegesen(TLss, C, Acc0).

% :-pred feldarabolasa(matrix::in, parameter::in, list(list(any))).
%% Az MX mátrix SubRows-SubCols paraméterű feldarabolása az LL lista.
feldarabolasa(Mx, SubRows-SubCols, LL) :- 
        vizszintesen(Mx, SubRows, V),
        fuggolegesen(V, SubCols, LL).
