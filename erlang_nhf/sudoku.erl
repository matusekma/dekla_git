-module(sudoku).
-author('matusekma@gmail.com').
-vsn('2019-11-24').
-export([sudoku/1]).

-type sspec() :: {size(), board()}.
-type size()  :: integer().
-type field() :: [info()].
-type info()  :: e | o | s | w | integer().
-type board() :: [[field()]].

-type parameter() :: {subRows(), subCols()}.
-type col() :: integer().
-type row() :: integer().
-type coords() :: {row(),col()}.
-type subRows() :: integer().
-type subCols() :: integer().

% ADATSTRUKTÚRA
-type kitoltes() :: integer().
-type sajatertek() :: integer().
-type lehetosegek() :: [integer()].
-type allapotMezo() :: { lehetosegek(), field(), kitoltes(), sajatertek()}.
-type allapot() :: [[allapotMezo()]].

-type ssol() :: [[integer()]].

-spec sudoku:sudoku(SSpec :: sspec()) -> SSols :: [ssol()].
%% SSols az SSpec feladványt kielégítő megoldások listája.
sudoku({K, F}) -> 
    AllValues = lists:seq(1, K*K),
    Cells = feldarabolasa(F, {K, K}),   
    Rows = F,
    Cols = lists:map(fun(C) -> oszlop(F, C) end, AllValues),   

    RowsWithIndex = lists:zip(AllValues, Rows),

    %összezippelve a lehetséges értékek a kezdeti feltételekkel
    LehetsegesMindenMezore = lists:map(fun({R, Row}) -> lehetsegesErtekSorra(Row, K, Row, Cols, Cells, R, AllValues, 1) end, RowsWithIndex),

    KezdetiAllapot = map2(fun(LehetsegesSor, Row) -> map2(fun(Lehetosegek, Field) -> {Lehetosegek, Field, 0, getValue(Field)} end, LehetsegesSor, Row ) end, LehetsegesMindenMezore, Rows),
  
    megold(KezdetiAllapot, AllValues, K, []).

-spec sudoku:megold(KezdetiAllapot :: allapot(), AllValues :: [integer()], K :: integer(), SolAcc :: [ssol()]) -> SSols :: [ssol()].
% A sudoku függvényben előállított kezdeti állapot alapján a SolAcc akkumulárotban
% gyűjti a megoldásokat, és a megoldások listájával tér vissza. Először alkalmazza a Hidden Single
% szabályt sorokra és oszlopokra, majd keres egy kitöltetlen cellát, és ha van, akkor a következő függvény segítségével kitölti
% minden lehetséges módon.
megold(KezdetiAllapot, AllValues, K, SolAcc) ->
    Allapot1 = assignHiddenSingleForRows(hd(KezdetiAllapot), 1, K, AllValues, KezdetiAllapot),
    Allapot = assignHiddenSingleForCols(oszlop(Allapot1, 1), 1, K, AllValues, Allapot1),
    KitoltetlenHelye = minKitoltetlen(Allapot, 1, AllValues, {0, 0, K*K+1}),
    case KitoltetlenHelye of 
        {0, 0, _} -> % mind ki van toltve, kiszedjük az ennesekből a megoldást
            Megoldas = lists:map(fun(MegoldasSor) -> lists:map(fun({_, _, Kitoltes, _}) -> Kitoltes end, MegoldasSor) end, Allapot),
            [Megoldas | SolAcc];
        {_, _, 0} -> %van 0 lehetőséggel rendelkező kitöltetlen mező
            SolAcc; % nincs itt megoldas
        {RowIndex, ColIndex, _} -> 
            AllapotSor = lists:nth(RowIndex, Allapot),
            KitoltetlenMezo = lists:nth(ColIndex, AllapotSor),
           
            osszesErtekkelKitoltEsFrissit(Allapot, KitoltetlenMezo, AllapotSor, RowIndex, ColIndex, AllValues, K, SolAcc)
    end.

-spec sudoku:osszesErtekkelKitoltEsFrissit(Allapot :: allapot(), KitoltendoMezo :: allapotMezo(), AllapotSor :: [allapotMezo()], 
                                            RowIndex :: integer(), ColIndex :: integer(), AllValues :: [integer()], 
                                            K :: integer(), SolAcc :: [ssol()]) -> SSols :: [ssol()].
% A KitoltendoMezo mezőt kitölti az egyik lehetséges értékkel, majd az eliminate függvénnyel frissíti
% a feladvány állapotát, és rekurzívan meghívja a megold függvényt, hogy ehhez az állapothoz
% találjon megoldást. Miután előáll egy megoldás ezen az ágon, egy újabb értéket helyettesít be a mező
% lehetőségei közül.
osszesErtekkelKitoltEsFrissit(_Allapot, {[], _Field, _, _}, _Sor, _RowIndex, _ColIndex, _AllValues, _K, SolAcc) -> SolAcc;
osszesErtekkelKitoltEsFrissit(Allapot, {[Ertek | Lehetosegek], Field, Kitoltes, OwnValue}, AllapotSor, RowIndex, ColIndex, AllValues, K, SolAcc) ->
            case OwnValue of
                0 -> KitoltottMezo = { [], [Ertek|Field], Ertek, Ertek}; 
                _ -> KitoltottMezo = { [], Field, Ertek, Ertek } % mar benne van az ertek
            end,
            
            UjSor = setnth(ColIndex, AllapotSor, KitoltottMezo),
            KitoltottAllapot = setnth(RowIndex, Allapot, UjSor),
              
            AllapotOszlop = oszlop(KitoltottAllapot, ColIndex),

            FrissitettAllapot = eliminate(K, { RowIndex, ColIndex }, KitoltottMezo, UjSor, AllapotOszlop, KitoltottAllapot), 
        
            NewAcc = megold(FrissitettAllapot, AllValues, K, SolAcc),
            % következő érték kipróbálása
            osszesErtekkelKitoltEsFrissit(Allapot, {Lehetosegek, Field, Kitoltes, OwnValue}, AllapotSor, RowIndex, ColIndex, AllValues, K, NewAcc).

-spec sudoku:assignHiddenSingleForRows(ElsoSor :: [allapotMezo()], CurrentRowIndex :: integer(), K :: integer(), 
                                        AllValues :: [integer()], ElozoAllapot ::allapot()) -> UjAllapot :: allapot().
% Az aktuális állapot minden sorára alkalmazza a "Hidden Single" szabályt (lásd. dokumentáció),
% és az új állapotot adja vissza.
% CurrenRowIndex kezdetben 1, az aktuális sor, K és AllValues konstans szám és lista, az ElozoAllapot
% pedig a frissítendő állapot.
assignHiddenSingleForRows(ElsoSor, CurrentRowIndex, K, AllValues, ElozoAllapot) ->
    UjAllapot = assignHiddenSingleInRow(ElsoSor, AllValues, CurrentRowIndex, K, ElozoAllapot),
    case CurrentRowIndex + 1 =< K*K of
        true ->
            KovetkezoSor = lists:nth(CurrentRowIndex + 1, UjAllapot),
            assignHiddenSingleForRows(KovetkezoSor, CurrentRowIndex + 1, K, AllValues, UjAllapot);
        false ->
            UjAllapot
    end.

-spec sudoku:assignHiddenSingleInRow(AllapotSor :: [allapotMezo()], AllValues :: [integer()], R :: integer(), K :: integer(), 
                                        Allapot ::allapot()) -> UjAllapot :: allapot().
% Az AllapotSor sorban alkalmazza a Hidden Single szabályt, azaz kitölti azokat
% a mezőket, amelyekben a sorban egyedi érték lehetséges.
assignHiddenSingleInRow(_AllapotSor, [], _R, _K, Allapot) -> Allapot;
assignHiddenSingleInRow(AllapotSor, [Value | Values], R, K, Allapot) ->
    case countIfOccursOnceInLists(AllapotSor, Value, 1, 0, 0) of 
        false -> assignHiddenSingleInRow(AllapotSor, Values, R, K, Allapot);
        {{_Lehetosegek, Field, _, OwnValue }, ColPosition} -> 
            case OwnValue of
                0 -> KitoltottMezo = { [], [Value|Field], Value, Value}; 
                _ -> KitoltottMezo = { [], Field, Value, Value } % mar benne van az ertek
            end,
            UjSor = setnth(ColPosition, AllapotSor, KitoltottMezo),
            KitoltottAllapot = setnth(R, Allapot, UjSor),
            AllapotOszlop = oszlop(KitoltottAllapot, ColPosition),
            
            UjAllapot = eliminate(K, { R, ColPosition }, KitoltottMezo, 0, AllapotOszlop, KitoltottAllapot),
            UjAllapotSor = lists:nth(R, UjAllapot),
            
            assignHiddenSingleInRow(UjAllapotSor, Values, R, K, UjAllapot)
    end.

-spec sudoku:assignHiddenSingleForCols(ElsoSor :: [allapotMezo()], CurrentColIndex :: integer(), K :: integer(), 
                                        AllValues :: [integer()], ElozoAllapot ::allapot()) -> UjAllapot :: allapot().
% Az aktuális állapot minden oszlopára alkalmazza a "Hidden Single" szabályt (lásd. dokumentáció),
% és az új állapotot adja vissza.
% CurrenColIndex kezdetben 1, az aktuális oszlop, K és AllValues konstans szám és lista, az ElozoAllapot
% pedig a frissítendő állapot.
assignHiddenSingleForCols(ElsoOszlop, CurrentColIndex, K, AllValues, ElozoAllapot) ->
    UjAllapot = assignHiddenSingleInCol(ElsoOszlop, AllValues, CurrentColIndex, K, ElozoAllapot),
    case CurrentColIndex + 1 =< K*K of
        true ->
            KovetkezoOszlop = oszlop(UjAllapot, CurrentColIndex + 1),
            assignHiddenSingleForCols(KovetkezoOszlop, CurrentColIndex + 1, K, AllValues, UjAllapot);
        false -> UjAllapot
    end.

-spec sudoku:assignHiddenSingleInCol(AllapotSor :: [allapotMezo()], AllValues :: [integer()], R :: integer(), K :: integer(), 
                                        Allapot ::allapot()) -> UjAllapot :: allapot().
% Az AllapotOszlop sorban alkalmazza a Hidden Single szabályt, azaz kitölti azokat
% a mezőket, amelyekben az oszlopban egyedi érték lehetséges.
assignHiddenSingleInCol(_AllapotOszlop, [], _C, _K, Allapot) -> Allapot;
assignHiddenSingleInCol(AllapotOszlop, [Value | Values], C, K, Allapot) ->
    case countIfOccursOnceInLists(AllapotOszlop, Value, 1, 0, 0) of 
        false -> assignHiddenSingleInCol(AllapotOszlop, Values, C, K, Allapot);
        {{_Lehetosegek, Field, _, OwnValue }, RowPosition} -> 
            case OwnValue of
                0 -> KitoltottMezo = { [], [Value|Field], Value, Value}; 
                _ -> KitoltottMezo = { [], Field, Value, Value } % mar benne van az ertek
            end,
            UjOszlop = setnth(RowPosition, AllapotOszlop, KitoltottMezo),
            KitoltottAllapot = setOszlop(C, Allapot, UjOszlop, []),
            AllapotSor = lists:nth(RowPosition, KitoltottAllapot),
            
            
            UjAllapot = eliminate(K, { RowPosition, C }, KitoltottMezo, AllapotSor, 0, KitoltottAllapot),
            UjAllapotOszlop = oszlop(UjAllapot, C),
            
            assignHiddenSingleInCol(UjAllapotOszlop, Values, C, K, UjAllapot)
    end.

-spec sudoku:countIfOccursOnceInLists(MezoLista :: [allapotMezo()], Value :: integer(), CurrentPosition :: integer(), 
                                FieldAndPosition :: {{allapotMezo(), integer()}}, Count :: integer()) -> {{allapotMezo(), integer()}} | false.
% A MezoLista-n végigmenve ellenőrzi, hogy a mezők lehetőséglistáiból csak 1-ben szerepel-e a Value érték.
% Ha igen, akkor visszaadja a mezőt és a pozícióját, egyébként false-ot.                              
countIfOccursOnceInLists([], _, _, FieldAndPosition, 1) -> FieldAndPosition;
countIfOccursOnceInLists([], _, _, _, _) -> false;
countIfOccursOnceInLists(_L, _Value, _, _, Count) when Count > 1 -> false;
countIfOccursOnceInLists([Field|T], Value, CurrentPosition, FieldAndPosition, Count) ->
    {Lehetosegek, _, _Kitoltes, _ } = Field,   
    case lists:member(Value, Lehetosegek)  of
        true ->
            Count1 = Count + 1,
            countIfOccursOnceInLists(T, Value, CurrentPosition + 1, {Field, CurrentPosition}, Count1);
        false -> countIfOccursOnceInLists(T, Value, CurrentPosition + 1, FieldAndPosition, Count)
    end.

-spec sudoku:eliminateFromRow(KitoltottMezo :: allapotMezo(), AllapotSor :: [allapotMezo()], 
                                R :: integer(), C :: integer(), RegiAllapot :: allapot()) -> UjAllapot :: allapot().
% A KitoltottMezo értéke alapján az AllapotSor mezőinek Lehetosegek listájából kiszűri a KitoltottMezo
% Kitoltes-ét, és  visszaadja az új állapotot.
eliminateFromRow(KitoltottMezo, AllapotSor, R, C, RegiAllapot) ->
    { _, _, Kitoltes, _ } = KitoltottMezo,
    UjSor = lists:map(fun({ Lehetosegek, Field, Ki, Value }) ->
        { lists:filter(fun(Lehetoseg) -> Lehetoseg =/= Kitoltes end, Lehetosegek), Field, Ki, Value }   
      end, AllapotSor),
    setnth(R, RegiAllapot, setnth(C, UjSor, KitoltottMezo)).

-spec sudoku:eliminateFromCol(KitoltottMezo :: allapotMezo(), AllapotOszlop :: [allapotMezo()], 
                                R :: integer(), C :: integer(), RegiAllapot :: allapot()) -> UjAllapot :: allapot().
% A KitoltottMezo értéke alapján az AllapotOszlop mezőinek Lehetosegek listájából kiszűri a KitoltottMezo
% Kitoltes-ét, és  visszaadja az új állapotot.
eliminateFromCol(KitoltottMezo, AllapotOszlop, R, C, RegiAllapot) ->
    { _, _, Kitoltes, _ } = KitoltottMezo,
    UjOszlop = lists:map(fun({ Lehetosegek, Field, Ki, Value }) ->
        { lists:filter(fun(Lehetoseg) -> Lehetoseg =/= Kitoltes end, Lehetosegek), Field, Ki, Value }   
      end, AllapotOszlop),
       
    UjOszlopKitoltes = setnth(R, UjOszlop, KitoltottMezo),        
    UjAllapot = setOszlop(C, RegiAllapot, UjOszlopKitoltes, []),
    
    UjAllapot.

-spec sudoku:eliminateFromCell(KitoltottMezo :: allapotMezo(), R :: integer(), C :: integer(), K :: integer(), RegiAllapot :: allapot()) -> UjAllapot :: allapot().
% Megkeresi a KitoltottMezo celláját, és mezőinek Lehetosegek listájából kiszűri a KitoltottMezo
% Kitoltes-ét, és  visszaadja az új állapotot.
eliminateFromCell(KitoltottMezo, R, C, K, Allapot) -> 
    { KitoltottLehetosegek, _, Kitoltes, _ } = KitoltottMezo,
    % melyik cellában van
    CellaIndex = getCellIndex(R, C, K),
    AllapotCellak = feldarabolasa(Allapot, {K, K}), 
    AllapotCella = lists:nth(CellaIndex, AllapotCellak),
    UjCella = lists:map(fun({ Lehetosegek, Field, Ki, Value }) ->
        case Ki =/= Kitoltes of
            true -> { lists:filter(fun(Lehetoseg) -> Lehetoseg =/= Kitoltes end, Lehetosegek), Field, Ki, Value };
            false -> { KitoltottLehetosegek, Field, Ki, Value } % saját magát nem filterezzük
        end
      end, AllapotCella),
    setCella(R, C, K, UjCella, Allapot).

-spec sudoku:eliminateByNakedPairs(EgysegLehetosegek :: [lehetosegek()], EgeszEgyseg :: [allapotMezo()]) -> UjEgyseg :: [allapotMezo()].
% Az EgysegLehetosegek tartalmazza egy sor, oszlop vagy cella (egység) mezőiben lévő lehetősegek listáját,
% a függvény erre alkalmazza a Naked Pairs (lásd. dokumentáció) szabályt, és visszaadja az EgeszEgyseg egység
% eszerint módosított változatát.
eliminateByNakedPairs([], EgeszEgyseg) -> EgeszEgyseg;
eliminateByNakedPairs([[A1, A2]| EgysegLehetosegek], EgeszEgyseg) ->
    case countIfOccursOnce(EgysegLehetosegek, [A1, A2], 0) of
        true -> 
            FilteredEgyseg = lists:map(fun({ Lehetosegek, Field, Ki, Value }) ->
                %EGESZ FILTEREZESE, DE SAJAT MAGUKAT NE
                case Lehetosegek  of
                    [A1,A2] -> { Lehetosegek, Field, Ki, Value };
                    _ ->
                    { lists:filter(fun(Lehetoseg) -> Lehetoseg =/= A1 andalso Lehetoseg =/= A2 end, Lehetosegek), Field, Ki, Value }  
                end
            end, EgeszEgyseg),
            
            eliminateByNakedPairs(EgysegLehetosegek, FilteredEgyseg);
        false -> eliminateByNakedPairs(EgysegLehetosegek, EgeszEgyseg)
    end;
eliminateByNakedPairs([ _ | Egyseg], EgeszEgyseg) ->
    eliminateByNakedPairs(Egyseg, EgeszEgyseg).

-spec sudoku:eliminateByNakedTriplets(EgysegLehetosegek :: [lehetosegek()], EgeszEgyseg :: [allapotMezo()]) -> UjEgyseg :: [allapotMezo()].
% Az EgysegLehetosegek tartalmazza egy sor, oszlop vagy cella (egység) mezőiben lévő lehetősegek listáját,
% a függvény erre alkalmazza a Naked Triplets (lásd. dokumentáció) szabályt, és visszaadja az EgeszEgyseg egység
% eszerint módosított változatát.
eliminateByNakedTriplets([], EgeszEgyseg) -> EgeszEgyseg;
eliminateByNakedTriplets([[A1, A2, A3]| EgysegLehetosegek], EgeszEgyseg) ->
    case countIfOccursTwice(EgysegLehetosegek, [A1, A2, A3], 0) of
        true -> 
            FilteredEgyseg = lists:map(fun({ Lehetosegek, Field, Ki, Value }) ->
                %EGESZ FILTEREZESE, DE SAJAT MAGUKAT NE
                case Lehetosegek  of
                    [A1,A2,A3] -> { Lehetosegek, Field, Ki, Value };
                    _ ->
                    { lists:filter(fun(Lehetoseg) -> Lehetoseg =/= A1 andalso Lehetoseg =/= A2 andalso Lehetoseg =/= A3 end, Lehetosegek), Field, Ki, Value }  
                end
            end, EgeszEgyseg),
            
            eliminateByNakedTriplets(EgysegLehetosegek, FilteredEgyseg);
        false -> eliminateByNakedTriplets(EgysegLehetosegek, EgeszEgyseg)
    end;
eliminateByNakedTriplets([ _ | Egyseg], EgeszEgyseg) ->
    eliminateByNakedTriplets(Egyseg, EgeszEgyseg).

-spec sudoku:setCella(R :: integer(), C :: integer(), K  :: integer(), UjCella :: [allapotMezo()], RegiAllapot :: allapot()) -> UjAllapot :: allapot().
% Beállítja az R,C koordinátájú állapotmezőt tartalmazó cella mezőit 
% a setCellaSoronként függvénnyel az UjCella értékeire, és visszaadja az új állapotot.
setCella(R, C, K, UjCella, Allapot) ->
    % a cella elso sora és oszlopa
    ElsoSorIndex = ((R-1) div K)*K + 1,
    ElsoOszlopIndex = ((C-1) div K)*K + 1,

    setCellaSoronkent(1, ElsoSorIndex, ElsoOszlopIndex, 0, K, UjCella, Allapot).

-spec sudoku:setOszlop(C :: integer(), RegiAllapot :: allapot(), Oszlop :: [allapotMezo()], Acc :: allapot()) -> UjAllapot :: allapot().
% Beállítja a RegiAllapotban a C-edik oszlopot Oszlop-ra, és visszaadja az új állapotot.
setOszlop(_, [], _, Acc) -> Acc;
setOszlop(C, [ AllapotSor|AllapotSorok ], [OszlopElem | OszlopMaradek], Acc) -> 
    UjAllapotSor = setnth(C, AllapotSor, OszlopElem),
    [UjAllapotSor | setOszlop(C, AllapotSorok, OszlopMaradek, Acc)].     


% cellaSorindex 0-tól indul!
-spec sudoku:setCellaSoronkent(AktualisSorIndex :: integer(), ElsoSorIndex :: integer(), ElsoOszlopIndex :: integer(), CellaSorIndex :: integer(), 
                                K :: integer(), UjCella :: [allapotMezo()], Allapot :: allapot()) -> UjAllapot :: allapot().
% K hosszú sorrészletekre bontva soronként állítja be az UjCella cellát az ElsoSorIndex, ElsoOszlopIndex
% bal felső sarok-koordinátájú cellaként az Allapot állapotban, és visszaadja az új állapotot.                          
setCellaSoronkent(SorIndex, ElsoSorIndex, _, _, K, _, AllapotMaradek) when SorIndex >= (ElsoSorIndex + K) -> AllapotMaradek;
setCellaSoronkent(AktualisSorIndex, ElsoSorIndex, ElsoOszlopIndex, CellaSorIndex, K, UjCella, [AllapotSor | AllapotMaradek]) ->
    case AktualisSorIndex < ElsoSorIndex of
        true -> [AllapotSor | setCellaSoronkent(AktualisSorIndex + 1, ElsoSorIndex, ElsoOszlopIndex, CellaSorIndex, K, UjCella, AllapotMaradek)];
        false ->
            UjCellaSor = lists:sublist(UjCella, CellaSorIndex*K + 1, K),
            UjAllapotSor = take(AllapotSor, ElsoOszlopIndex - 1) ++ (UjCellaSor ++ lists:nthtail(ElsoOszlopIndex - 1 + K, AllapotSor)),
            
            [UjAllapotSor | setCellaSoronkent(AktualisSorIndex + 1, ElsoSorIndex, ElsoOszlopIndex, CellaSorIndex+1, K, UjCella, AllapotMaradek)]
    end.

-spec sudoku:eliminate(K :: integer(), RC :: { R :: integer(), C :: integer() }, KitoltottMezo :: allapotMezo(), 
                        AllapotSor :: [allapotMezo()], AllapotOszlop :: [allapotMezo()], Allapot :: allapot()) -> UjAllapot :: allapot().
% Az R,C koordinátára behelyettesített KitoltottMezo mező az AllapotSor és AllapotOszlop sor és oszlop
% metszetében van, az eliminate függvény ezután a behelyettesítés után hoz létre új állapotot az alapszabályok,
% a paritási szabályok és a Naked Pairs illetve Naked Triplets nevű szabályok alkalmazásával, a megmaradó lehetőségek szűrésével,
% és az uj allapotot adja vissza.            
eliminate(K, { R, C }, KitoltottMezo, AllapotSor, AllapotOszlop, Allapot) ->
    {Lehetseges, Field, Kitoltes, OwnValue } = KitoltottMezo,
    
    Allapot1 =
    case AllapotSor =/= 0 of
        true -> eliminateFromRow( {Lehetseges, Field, Kitoltes, OwnValue }, AllapotSor, R, C, Allapot);
        false -> Allapot
    end,

    Allapot2 = 
    case AllapotOszlop =/= 0 of
        true -> eliminateFromCol( {Lehetseges, Field, Kitoltes, OwnValue }, AllapotOszlop, R, C, Allapot1);
        false -> Allapot1
    end,
    
    Allapot3 = eliminateFromCell( {Lehetseges, Field, Kitoltes, OwnValue }, R, C, K, Allapot2),

    %s
    SFilter = 
        case R < K*K andalso lists:member(s, Field) of
            true -> 
                Allapot3Oszlop = oszlop(Allapot3, C),
                {DelLehetosegek, DelField, DelKitoltes, DelErtek } = lists:nth(R + 1, Allapot3Oszlop),

                UjDel = 
                case Kitoltes rem 2 of
                    1 -> { lists:filter(fun(V) -> V rem 2 =:= 0 end, DelLehetosegek), DelField, DelKitoltes, DelErtek };
                    0 -> { lists:filter(fun(V) -> V rem 2 =:= 1 end, DelLehetosegek), DelField, DelKitoltes, DelErtek }
                end,
                UjOszlopDel = setnth(R+1, Allapot3Oszlop, UjDel),
                setOszlop(C, Allapot3, UjOszlopDel, []);
            false -> Allapot3
        end,
    %w
    WFilter = case C > 1 andalso lists:member(w, Field) of
            true -> 
                SFilterSor = lists:nth(R, SFilter),
                {NyugatLehetosegek, NyugatField, NyugatKitoltes, NyugatErtek } = lists:nth(C - 1, SFilterSor),

                UjNyugat = 
                case Kitoltes rem 2 of
                    1 -> { lists:filter(fun(V) -> V rem 2 =:= 0 end, NyugatLehetosegek), NyugatField, NyugatKitoltes, NyugatErtek };
                    0 -> { lists:filter(fun(V) -> V rem 2 =:= 1 end, NyugatLehetosegek), NyugatField, NyugatKitoltes, NyugatErtek }
                end,
                UjSorNyugat = setnth(C-1, SFilterSor, UjNyugat),
                setnth(R, SFilter, UjSorNyugat);
            false -> SFilter
    end,
    SNeighborFilter = 
        case R > 1 of
            true -> 
                SNeigborOszlop = oszlop(WFilter, C),
                {EszakLehetosegek, EszakField, EszakKitoltes, EszakErtek } = lists:nth(R-1, SNeigborOszlop),
                    case lists:member(s, EszakField) of
                        true -> 
                            UjEszak = 
                                case Kitoltes rem 2 of
                                    1 -> { lists:filter(fun(V) -> V rem 2 =:= 0 end, EszakLehetosegek), EszakField, EszakKitoltes, EszakErtek };
                                    0 -> { lists:filter(fun(V) -> V rem 2 =:= 1 end, EszakLehetosegek), EszakField, EszakKitoltes, EszakErtek }
                                end,
                            UjOszlopEszak = setnth(R-1, SNeigborOszlop, UjEszak),
                            setOszlop(C, WFilter, UjOszlopEszak, []);
                        false -> WFilter
                    end;
            false -> WFilter
        end,
        %w
        WNeighborFilter = case  C < K * K of
                true -> 
                    WFilterSor = lists:nth(R, SNeighborFilter),
                    { KeletLehetosegek, KeletField, KeletKitoltes, KeletErtek } = lists:nth(C+1, WFilterSor),
                        case lists:member(w, KeletField) of
                            true -> 
                                UjKelet = 
                                case Kitoltes rem 2 of
                                    1 -> { lists:filter(fun(V) -> V rem 2 =:= 0 end, KeletLehetosegek), KeletField, KeletKitoltes, KeletErtek };
                                    0 -> { lists:filter(fun(V) -> V rem 2 =:= 1 end, KeletLehetosegek), KeletField, KeletKitoltes, KeletErtek }
                                end,
                                UjSorKelet = setnth(C+1, WFilterSor, UjKelet),
                                setnth(R, SNeighborFilter, UjSorKelet);
                            false -> SNeighborFilter
                        end;
                false -> SNeighborFilter
        end,
    
    case K >= 4 of 
        true ->

            Sor = lists:nth(R, WNeighborFilter),
            SorLehetosegek = lists:map(fun({ Lehetosegek, _, _, _ }) -> Lehetosegek end, Sor),
            NakedTripletsSor = eliminateByNakedTriplets(SorLehetosegek, Sor),
            
            SorLehetosegek1 = lists:map(fun({ Lehetosegek, _, _, _ }) -> Lehetosegek end, NakedTripletsSor),
            NakedPairsSor1 = eliminateByNakedPairs(SorLehetosegek1, NakedTripletsSor),
            NakedPairsSorAllapot1 = setnth(R, WNeighborFilter, NakedPairsSor1),

            Oszlop = oszlop(NakedPairsSorAllapot1, C),
            OszlopLehetosegek = lists:map(fun({ Lehetosegek, _, _, _ }) -> Lehetosegek end, Oszlop),
            NakedTripletsOszlop = eliminateByNakedTriplets(OszlopLehetosegek, Oszlop),

            OszlopLehetosegek1 = lists:map(fun({ Lehetosegek, _, _, _ }) -> Lehetosegek end, NakedTripletsOszlop),
            NakedPairsOszlop1 = eliminateByNakedPairs(OszlopLehetosegek1, NakedTripletsOszlop),
            NakedPairsOszlopAllapot1 = setOszlop(C, NakedPairsSorAllapot1, NakedPairsOszlop1, []),

            CellaIndex = getCellIndex(R, C, K),
            AllapotCellak = feldarabolasa(NakedPairsOszlopAllapot1, {K, K}), 
            AllapotCella = lists:nth(CellaIndex, AllapotCellak),
            CellaLehetosegek = lists:map(fun({ Lehetosegek, _, _, _ }) -> Lehetosegek end, AllapotCella),
            NakedPairsCella = eliminateByNakedPairs(CellaLehetosegek, AllapotCella),
            setCella(R, C, K, NakedPairsCella, NakedPairsOszlopAllapot1);
        false -> WNeighborFilter
    end.


-spec sudoku:minKitoltetlen(Allapot :: allapot(), R :: integer(), AllValues :: [integer()], CurrentMin :: {integer(), integer(), integer()}) -> MinKitoltetlen :: {integer(), integer(), integer()}.
% Kikeresi az Allapot állapotban a minimális kitöltési lehetőséggel rendelkező kitöltetlen mező pozícióját.
% currentMin kezdetben {0, 0, K*K + 1}, R pedig 0, ha nincs kitoltetlen, akkor a visszatérési érték a kezdeti
minKitoltetlen([], _, _, Min) -> Min;
minKitoltetlen([AllapotSor | AllapotSorok], R, AllValues, CurrentMin) ->
    FieldsWithIndex = lists:zip(AllValues, AllapotSor),
    RowMin = lists:foldl(fun({ ColIndex, {Lehetosegek, _, Kitoltes, _}}, {MinR, MinC, Min}) -> 
        Hossz = length(Lehetosegek),
        case Min > Hossz andalso Kitoltes =:= 0 of
            true -> {R, ColIndex, Hossz};
            false -> {MinR, MinC, Min}
        end
    end, CurrentMin, FieldsWithIndex),
    minKitoltetlen(AllapotSorok, R + 1, AllValues, RowMin).


-spec sudoku:setnth(N :: integer(), L :: [any()], E :: any()) -> NewL :: [any()].
% Az L lista N-edik elemét E-re
% állítva kapjuk NewL-t.
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

-spec sudoku:map2(Pred :: any(), L1 :: [any()], L2 :: [any()]) -> L3 :: [any()].
% L1 és L2 listát párhuzamosan mappeli L3 listává
% a Pred kétargumentumú predikátummal. 
map2(_Pred, [], _) -> [];
map2(Pred, [H1 | T1], [H2 | T2]) ->
    [Pred(H1, H2) | map2(Pred, T1, T2)].

-spec sudoku:lehetsegesErtekSorra(Sor :: [field()], K :: integer(), WholeRow :: [field()],
                                    Cols :: [field()], Cells :: [field()], R :: integer(), AllValues :: [integer()], CAcc :: integer()) -> Row :: [[integer()]].
% A kezdeti feladvány egy adott sorának mezőin végigmegy, és az ertekek
% függvény segítségével meghatározza a kényszerek alapján a mezők lehetséges értékét. 
% Egy sort ad vissza értékekkel, Cacc default 0.
lehetsegesErtekSorra([], _, _WholeRow, _Cols, _Cells, _R, _AllValues, _CAcc) -> [];
lehetsegesErtekSorra([Field | RowTail], K, WholeRow, Cols, Cells, R, AllValues, CAcc) ->
    MezoErtekek = ertekek(K, { R, CAcc }, Field, AllValues, Cells, WholeRow, Cols),
    [MezoErtekek | lehetsegesErtekSorra(RowTail, K, WholeRow, Cols, Cells, R, AllValues, CAcc + 1)].

-spec sudoku:ertekek(SSpec :: sspec(), R_C :: coords(), Field :: field(), AllValues :: [integer()], Cells :: [[field()]], Row :: [field()], Col :: [field()] ) -> Vals :: [integer()].
% A Vals lista az SSpec specifikáció alapján az R,C koordinátájú mezőben megengedett értékek listája.
% A Cells a specifikáció celláinak listája, a Row és a Cols pedig adott sora és oszlopai.
ertekek(K, { R, C }, Field, AllValues, Cells, Row, Cols) ->
    Col = lists:nth(C, Cols),
    
    V1 = filterByConstraints(Field, Row, Col, R, C, AllValues),

    V2 = filterByUnit(removeFromList(C, Row), V1),

    V3 = filterByUnit(removeFromList(R, Col), V2),
    
    V4 = filterByCell(V3, Field, R, C, K, Cells),

    % szomszédos mezők s és w értéke alapján saját értékek szűrése
    %s
    SFilter = 
        case V4 =/= [] andalso R > 1 of
            true -> 
                NorthNeighbor = lists:nth(R-1, Col),
                    case lists:member(s, NorthNeighbor) of
                        true -> 
                            NorthNeighborValue = getValue(NorthNeighbor),
                            case NorthNeighborValue of
                                0 -> V4;
                                NorthValue ->
                                    case NorthValue rem 2 of 
                                        1 -> lists:filter(fun(V) -> V rem 2 =:= 0 end, V4); 
                                        0 -> lists:filter(fun(V) -> V rem 2 =:= 1 end, V4)
                                    end
                            end;
                        false -> V4
                    end;
                false -> V4
            end,
        %w
        case SFilter =/= [] andalso C < K * K of
            true ->
                EastNeighbor = lists:nth(C+1, Row),
                case lists:member(w, EastNeighbor) of
                    true -> 
                        EastNeighborValue = getValue(EastNeighbor),
                            case EastNeighborValue of
                                0 -> SFilter;
                                EastValue ->
                                    case EastValue rem 2 of 
                                        1 -> lists:filter(fun(V) -> V rem 2 =:= 0 end, SFilter); 
                                        0 -> lists:filter(fun(V) -> V rem 2 =:= 1 end, SFilter)
                                    end
                            end;
                    false -> SFilter
                end;
            false -> SFilter
        end. 

-spec filterByConstraints(Field :: field(), Row :: [field()], Col :: [field()], R ::integer(), C ::integer(), ValuesAcc :: [integer()]) -> Values :: [integer()].
% A Field mező kényszerei alapján szűri annak lehetséges értékeit, és a Values listában adja vissza azokat.
% Row, Col, R, és C rendre a feladvány adott sora és oszlopa illetve azok sorszámai.
filterByConstraints(_Field, _Row, _Col, _R, _C, []) -> [];
filterByConstraints([], _Row, _Col, _R, _C, Values) -> Values;
filterByConstraints([Info | FieldTail ], Row, Col, R, C, Values) ->
    FilteredValues = 
        case Info of
            e -> lists:filter(fun(V) -> V rem 2 =:= 0 end, Values); % even
            o -> lists:filter(fun(V) -> V rem 2 =:= 1 end, Values); % odd
            s -> 
                NeighborValue = getValue(lists:nth(R+1, Col)),
                case NeighborValue of
                    0 -> Values;
                    Value ->
                         case Value rem 2 of 
                            1 -> lists:filter(fun(V) -> V rem 2 =:= 0 end, Values); 
                            0 -> lists:filter(fun(V) -> V rem 2 =:= 1 end, Values)
                        end
                end;
            w -> 
                NeighborValue = getValue(lists:nth(C-1, Row)),
                case NeighborValue of
                    0 -> Values;
                    Value ->
                         case Value rem 2 of 
                            1 -> lists:filter(fun(V) -> V rem 2 =:= 0 end, Values); 
                            0 -> lists:filter(fun(V) -> V rem 2 =:= 1 end, Values)
                        end
                end;
            Szam -> [Szam]
        end,      
    filterByConstraints(FieldTail, Row, Col, R, C, FilteredValues).

-spec sudoku:filterByUnit(Unit :: field(), Values :: [integer()]) -> FilteredValues :: [integer()].
% A sorban/oszlopban lévő többi szám alapján szűri a Values listában található
% lehetőségeket, és a szűrt listát adja vissza.
filterByUnit(_Unit, []) -> [];
filterByUnit(Unit, Values) -> 
    UnitValues = lists:filtermap(fun(Field) -> 
        case getValue(Field) of
            0 -> false;
            Value -> {true, Value}
        end end, Unit),
    lists:filter(fun(V) -> not lists:member(V, UnitValues) end, Values).

-spec sudoku:filterByCell(Values :: [integer()], Mezo :: [field()],
                             R :: integer(), C :: integer(), K :: integer(), Cells :: [[field()]]) -> FilteredValues :: [integer()].
% Adott cellában lévő többi szám alapján szűri a Mezo mező lehetséges értékeit tartalmazó 
% Values listát, és a szűrt listát adja vissza.
filterByCell([], _Mezo, _R, _C, _K, _Cells) -> [];
filterByCell(Values, Mezo, R, C, K, Cells) -> 
    % melyik cellában van
    CellIndex = getCellIndex(R, C, K),
    Cell = lists:nth(CellIndex, Cells),
    OwnValue = getValue(Mezo),
    CellValues = lists:filtermap(fun(Field) -> 
        case getValue(Field) of
            0 -> false;
            Value -> {true, Value} 
        end end, Cell),
    case OwnValue of
        0 ->  lists:filter(fun(V) -> not lists:member(V, CellValues) end, Values);
        Value ->
            case countValueOccurences(CellValues, Value) > 1 of  % there is more than 1 same value
                true -> [];
                false -> Values
            end
    end.

-spec sudoku:getValue(Field :: field()) -> integer().
% Visszaadja a Field mezőben található értéket, ha van, egyébként 0-t. 
getValue([]) -> 0;
getValue([Info|_Infos]) when is_integer(Info) -> Info;
getValue([_|Infos]) -> getValue(Infos).

-spec sudoku:countIfOccursOnce(L :: any(), Value :: any(), Count ::integer()) -> boolean().
% L listában Value érték 1-szer szerepel-e.
countIfOccursOnce([], _, 1) -> true;
countIfOccursOnce([], _, _) -> false;
countIfOccursOnce(_L, _Value, Count) when Count > 1 -> false;
countIfOccursOnce([Value|T], Value, Count) ->
    Count1 = Count + 1,
    countIfOccursOnce(T, Value, Count1);
countIfOccursOnce([_|T], Value, Count) ->
    countIfOccursOnce(T, Value, Count).

-spec sudoku:countIfOccursTwice(L :: any(), Value :: any(), Count :: integer()) -> boolean().
% L listában Value érték 2-szer szerepel-e.
countIfOccursTwice([], _, 2) -> true;
countIfOccursTwice([], _, _) -> false;
countIfOccursTwice(_L, _Value, Count) when Count > 2 -> false;
countIfOccursTwice([Value|T], Value, Count) ->
    Count1 = Count + 1,
    countIfOccursTwice(T, Value, Count1);
countIfOccursTwice([_|T], Value, Count) ->
    countIfOccursTwice(T, Value, Count).
   

countValueOccurences([], _Value, Acc) -> Acc;
countValueOccurences([H|T], Value, Acc) ->
    case Value =:= H of
        true -> countValueOccurences(T, Value, Acc + 1);
        false -> countValueOccurences(T, Value, Acc)
    end.

-spec sudoku:countValueOccurences(L :: any(), Value :: any()) -> Count :: integer().
% Az L listában a Value érték Count-szor szerepel.
countValueOccurences(Values, Value) -> countValueOccurences(Values, Value, 0).

-spec sudoku:getCellIndex(R :: integer(), C :: integer(), K :: integer()) -> integer().
% Visszaadja, hogy az adott koordinátájú mező hányadik cellában van a feldarabolásban
getCellIndex(R, C, K) -> 
    %sorszam 0-tol
    Sorszam = (R-1) * K * K + C - 1,
    
    Chunk = Sorszam div K,
    % minden sorban K darab és minden cellában K sor
    Row = Chunk div (K*K),
    Col = Chunk rem K,
    % 1-től indexelünk
    Row * K + Col + 1.

-spec sudoku:removeFromList(Index :: integer(), List :: [any()]) -> NewList :: [any()].
% A List listából kiveszi az Indexedik elemet.
removeFromList(Index, List) -> 
    {Left, [_|Right]} = lists:split(Index-1, List),
    Left ++ Right.

-spec take(L0::[any()], N::integer()) -> L::[any()].
% Az L0 lista N hosszú prefixuma az L lista.
take2([], _N, Acc) -> Acc;
take2(_L0, 0, Acc) -> Acc;
take2(L0, N, Acc) -> [hd(L0)|take2(tl(L0), N-1, Acc)].

take(L0, N) -> take2(L0, N, []).

-spec drop(L0::[any()], N::integer()) -> L::[any()].
% Az L0 lista első N elemét nem tartalmazó szuffixuma az L lista.
drop([], _N) -> [];
drop(L0, 0) -> L0;
drop(L0, N) -> drop(tl(L0), N-1).

-spec oszlop(SSpec :: board(), C :: integer()) -> Col::[field()].
% visszaadja az SSpec specifikációval megadott Sudoku-feladvány C-edik oszlopát.
oszlop(SSpec, C) -> [lists:nth(C, Row) || Row <- SSpec].

-spec vizszintesen(Mss :: board(), R :: integer()) -> Lss :: [[any()]].
%% Mss mátrix R soronkénti feldarabolása Lss listák listája (R sorú sorcsoportok)
vizszintesen([], _R, Acc) -> Acc;
vizszintesen(Mss, R, Acc) -> [take(Mss, R) | vizszintesen(drop(Mss, R), R, Acc)].

vizszintesen(Mss, R) -> vizszintesen(Mss, R, []).

-spec sudoku:fuggolegesenIedikOszlopcsoportra(Lss :: [[any()]], C :: integer(), Acc :: [[any()]], I :: integer()) -> Felvagas :: [[any()]].
% Létrehozza az I-edik függőleges vágást.
fuggolegesenIedikOszlopcsoportra([], _C, Acc, _I) -> Acc;
fuggolegesenIedikOszlopcsoportra(Lss, C, Acc, I) -> take(drop(hd(Lss), I * C), C) ++ fuggolegesenIedikOszlopcsoportra(tl(Lss), C, Acc, I).

-spec sudoku:fuggolegesenEgySorCsoportot(Lss :: [[any()]], C :: integer(), I :: integer(), Acc :: [[any()]]) -> Felvagas :: [[any()]].
% Felvág 1 sorcsoportot, azaz valahány egymás utáni sort C oszloponként, I az oszlopcsoport sorszáma
fuggolegesenEgySorCsoportot(Lss, C, I, Acc) when ((I+1)  * C) < length(hd(Lss)) -> [fuggolegesenIedikOszlopcsoportra(Lss, C, [], I) | fuggolegesenEgySorCsoportot(Lss, C, I+1, Acc)];
fuggolegesenEgySorCsoportot(Lss, C, I, _Acc) -> [fuggolegesenIedikOszlopcsoportra(Lss, C, [], I)].  %% utolsó oszlopcsoport

-spec sudoku:fuggolegesen(Lss :: [[any()]], C :: integer()) -> Felvagas :: [[any()]].
% C oszloponként függőlegesen felvágja a sorcsoportokat.
fuggolegesen(Lss, C) -> [X || L <- Lss, X <- fuggolegesenEgySorCsoportot(L, C, 0, [])].  

-spec feldarabolasa(Mss :: board(), P :: parameter()) -> Lss :: [[any()]].
%% Az Mss mátrix P paraméterű feldarabolása az Lss lista.
feldarabolasa(Mss, { R, C }) -> fuggolegesen(vizszintesen(Mss, R), C).
