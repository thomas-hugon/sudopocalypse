:- use_module(library(clpfd)).
:- use_module(library(solution_sequences)).

random_partial_grid(Size, RegionWidth, RegionHeight, PartialGrid, Solution) :-
    random_grid(Size, Solution, RegionWidth, RegionHeight),
    random_partial(Solution, PartialGrid).

generate_all(Size, Grid, RegionWidth, RegionHeight) :-
    grid(Size, Grid, RegionWidth, RegionHeight),
    flatten(Grid, Vars),
    labeling([ffc], Vars).

random_grid(Size, Grid, RegionWidth, RegionHeight) :-
    grid(Size, Grid, RegionWidth, RegionHeight),
    flatten(Grid, Vars),
    random_permutation(Vars, RandomVars),
    labeling([ffc], RandomVars), !.

grid(Size, Grid, RegionWidth, RegionHeight) :-
    size(Size, Size, RegionWidth, RegionHeight, Grid),
    constraint(Size, RegionWidth, RegionHeight, Grid).

size(Width, Height, RegionWidth, RegionHeight,  Grid) :-
    length(Grid, Height),
    sublength(Grid, Width),
    Width = Height,
    RegionWidth #> 1,
    RegionHeight #> 1,
    Diff #= RegionWidth - RegionHeight,
    Diff #>= 0,
    RegionWidth * RegionHeight #= Width,
    labeling([min(Diff)], [Diff]).

sublength([], _).
sublength([H|T], Size) :- length(H, Size), sublength(T, Size).

constraint(GridSize, RegionWidth, RegionHeight,  Grid) :-
    distincts_rows(Grid),
    distincts_columns(Grid),
    distincts_regions(RegionWidth, RegionHeight, Grid),
    values_in_domain(GridSize, Grid).

distincts_rows(Grid) :-
    maplist(all_distinct, Grid).
distincts_columns(Grid) :-
    transpose(Grid, Transposed),
    distincts_rows(Transposed).
distincts_regions(RegionWidth, RegionHeight, Grid) :-
    regions(RegionWidth, RegionHeight, Grid, Regions),
    distincts_rows(Regions).
values_in_domain(Max, Grid) :-
    flatten(Grid, Vars),
    Vars ins 1..Max.

regions(_, _, [], []) :- !.
regions(RegionWidth, RegionHeight, Grid, X) :-
    length(Rows, RegionHeight),
    append(Rows, Reste, Grid),
    regions_for_rows(RegionWidth, Rows, Regions),
    regions(RegionWidth, RegionHeight, Reste, Y),
    append(Regions, Y, X).

regions_for_rows(_, [], []) :- !.
regions_for_rows(RegionWidth, Rows, [Region|Regions]) :-
    transpose(Rows, Transposed),
    length(Prefix, RegionWidth),
    append(Prefix, ResteTransposed, Transposed),
    flatten(Prefix, Region),
    transpose(ResteTransposed, Reste),
    regions_for_rows(RegionWidth, Reste, Regions).

random_partial(InitialGrid, FinalGrid) :-
    all_coordinates(InitialGrid, Coordinates),
    random_partial(InitialGrid, FinalGrid, Coordinates), !.

all_coordinates(Grid, Coordinates) :-
    length(Grid, Len),
    N is Len - 1,
    findall((X,Y), (between(0,N,X), between(0,N,Y)), Coordinates).

random_partial(FinalGrid, FinalGrid, []) :- !.
random_partial(InitialGrid, FinalGrid, Coordinates) :-
    length(Coordinates, Len),
    random_between(1, Len, K),
    nth1(K, Coordinates, (X, Y), RestCoordinates),
    unset(InitialGrid, X, Y, Grid2),
    copy_term(Grid2, Grid3),
    (
        unique_solution(Grid2),
        random_partial(Grid3, FinalGrid, RestCoordinates)
    ;
        random_partial(InitialGrid, FinalGrid, RestCoordinates)
    ).

unset(Grid, X, Y, Grid2) :-
    nth0(Y, Grid, Row, R1),
    nth0(X, Row, _, R2),
    nth0(X, Row2, _, R2),
    nth0(Y, Grid2, Row2, R1).

unique_solution(Grid) :-
    \+call_nth(generate_all(_, Grid, _, _),2),
    call_nth(generate_all(_, Grid, _, _),1).


