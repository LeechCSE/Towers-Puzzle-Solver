/********************************************************
 * For tower/3
*********************************************************/
% set list length to N
check_length(N, T) :-
    length(T, N).
% set list elements 1..N
my_fd_domain(N, L) :-
    fd_domain(L, 1, N).
% set list elements distinct
my_fd_distinct(N, T) :-
    fd_all_different(T),
    maplist(my_fd_domain(N), T).

% Old SWI Prolog clpfd.pl Library
% transpose/2
% source: http://stackoverflow.com/a/4281159
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).
% transpose ends

% counts-constraints
seen_([], _, []).
seen_([X|Xs], C, [X|T]) :-
    X > C,
    seen_(Xs, X, T).
seen_([X|Xs], C, T) :-
    X =< C,
    seen_(Xs, C, T).

seen([X|Xs], R) :-
    seen_(Xs, X, T),
    length(T, R1),
    R is 1 + R1.
    
counts_constraints(Rows, Forward, Backward) :-
    maplist(seen, Rows, Forward),
    maplist(reverse, Rows, Reversed),
    maplist(seen, Reversed, Backward).
% counts-constraints ends

/*********************************************************
 * tower/3
 *
 * solves NxN towers puzzle
 * N, nonnegative integer, the size of the square grid
 * T, list of N lists of distinct integers from 1 to N
 * C, a structure with function symbol counts and arity 4
**********************************************************/
/*********************************************************
 * Constraints
 * 1. All lists of T have length N
 * 2. Elements in row are distinct
 * 3. Elements in column(transposed row) are distinct
 * 4. Given structure with arity 4 'counts' constraints C
**********************************************************/
tower(N, T, C) :-
    % constrain T is list of N lists
    check_length(N, T),
    % generate rows
    maplist(check_length(N), T),
    % constrain all elements of rows are within 1..N
    maplist(my_fd_distinct(N), T),
    % generate columns
    transpose(T, T_trans),
    % constrain all elements of columns are within 1..N
    maplist(my_fd_distinct(N), T_trans),
    % assign values to elements in rows
    maplist(fd_labeling, T),
    % define a structure counts
    counts(Top, Bottom, Left, Right) = C,
    % constrain counts to both rows and columns
    counts_constraints(T_trans, Top, Bottom),
    counts_constraints(T, Left, Right).

/********************************************************
 * For plain_tower/3
*********************************************************/
% generate_list whose elements are within 1..N
generate_list(N, L) :-
    generate_list_(N, [], L).

generate_list_(0, L, L) :- !.
generate_list_(N, R, L) :-
    N > 0,
    N1 is N - 1,
    generate_list_(N1, [N|R], L).
% generate_list ends

% set domain 1..N
plain_constrain_domain(N, V) :-
    generate_list(N, L),
    member(V, L).
% check if elements in list L are distinct
plain_distinct(L) :-
    sort(L, S),
    length(L, N),
    length(S, N).
% set list elements distinct
plain_fd_distinct(N, T) :-
    plain_distinct(T),
    maplist(plain_constrain_domain(N), T).

/*********************************************************
 * plain_tower/3
 *
 * Same functionality as tower/3 but without GNU Prolog
 * finite domain solver.
**********************************************************/
plain_tower(N, T, C) :-
    check_length(N, T),
    maplist(check_length(N), T),
    maplist(plain_fd_distinct(N), T),
    transpose(T, T_trans),
    maplist(plain_fd_distinct(N), T_trans),
    counts(Top, Bottom, Left, Right) = C,
    % counts_constraints doesn't use any FDS or FDS arithmetic constraints
    counts_constraints(T_trans, Top, Bottom),
    counts_constraints(T, Left, Right),
    maplist(plain_fd_distinct(N), T).

/*********************************************************
 * speedup/1 
 * 
 * Tested on Lnxsrv09. 
 * 
 * Since plain_tower/3 runs too slow with N = 4 to generate 
 * many test samples, I used N = 3 test case.
 * 
 * Test case used:
 * N = 3,
 * C = counts([3,2,1],
 *	      [1,2,2],
 *	      [3,2,1],
 *	      [1,2,2])
 * Output: T
**********************************************************/
speedup(R) :-
    statistics(cpu_time, [S1|L1]),
    tower(3, T, counts([3,2,1],[1,2,2],[3,2,1],[1,2,2])),
    statistics(cpu_time, [S2|L2]),
    plain_tower(3, T, counts([3,2,1],[1,2,2],[3,2,1],[1,2,2])),
    R is (S2-L2)/(S1-L1).

/****************************
 * Results
 * 
 * With 20 runs
 * Max R = 1.2727272727272727
 * Min R = 1.0
 * Mean R = 1.124483333333333
****************************/


/*********************************************************
 * ambiguous/4
 * 
 * finds an ambiguous puzzle that has multiple solutions
 * with the same N and the same counts constraint.
**********************************************************/
ambiguous(N, C, T1, T2) :-
    % run two tower/3 with the same counts and N
    tower(N, T1, C),
    tower(N, T2, C),
    % output/true iff T1 and T2 are different!
    \+sublist(T1, T2).
