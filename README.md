# sudoku generator

uses SWI PROLOG

use predicate random_partial_grid/5 to generate a random partial grid + solution

ex:
```
?- random_partial_grid(9, RW, RH, PG, Sol), maplist(portray_clause,PG), nl, maplist(portray_clause,Sol).

[6, _, _, _, _, _, _, _, _].
[_, 8, 9, _, _, _, _, _, 4].
[1, _, _, _, _, 7, _, 6, 3].
[_, _, _, _, _, 1, _, _, _].
[3, _, _, _, 8, _, 4, _, 6].
[2, 1, 7, _, 9, _, _, _, _].
[_, _, 2, 9, _, _, _, _, 8].
[_, _, _, _, 3, 8, _, _, 7].
[_, _, _, _, _, _, 3, _, _].

[6, 2, 3, 5, 4, 9, 7, 8, 1].
[7, 8, 9, 1, 6, 3, 5, 2, 4].
[1, 5, 4, 8, 2, 7, 9, 6, 3].
[4, 6, 8, 3, 5, 1, 2, 7, 9].
[3, 9, 5, 7, 8, 2, 4, 1, 6].
[2, 1, 7, 6, 9, 4, 8, 3, 5].
[5, 3, 2, 9, 7, 6, 1, 4, 8].
[9, 4, 1, 2, 3, 8, 6, 5, 7].
[8, 7, 6, 4, 1, 5, 3, 9, 2].
```
