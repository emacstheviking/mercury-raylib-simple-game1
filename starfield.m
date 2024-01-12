%-----------------------------------------------------------------------------%
%
% File: starfield.m
% Main author: Sean Charles
% Date: Sat Aug 26 07:21:12 2023
%
% Starfield.
%
% This module manages the creation and ongoing update and display of the
% rolling starfield background.
%
%-----------------------------------------------------------------------------%
:- module starfield.

:- interface.
:- import_module io.
:- import_module list.


:- pred draw_stars(stars::in, io::di, io::uo) is det.
:- pred mk_star(int::in, int::in, star::out, io::di, io::uo) is det.

:- pred stars_init(int::in, int::in, int::in, stars::out,
    io::di, io::uo) is det.

:- pred step_stars(float::in, int::in, int::in, stars::in, stars::out,
    io::di, io::uo) is det.

    % A star.
    %
:- type star ---> star(float, float, float, uint32).
:- type stars == list(star).


:- implementation.
:- import_module float.
:- import_module string.

:- import_module raylib.

:- use_module rnd.

%-----------------------------------------------------------------------------%

    % Initialise a starfield.
    % Initial placement is at a random location on the screen governed by the
    % given MaxX and MaxY values. When a star rolls off, it always starts at
    % the top of the screen.
    %
stars_init(N, MaxX, MaxY, Starfield, !IO) :-
    P = (pred(_::in, !.SF::in, !:SF::out, !.IO::di, !:IO::uo) is det :-
        mk_star(MaxX, MaxY, Star, !IO),
        list.cons(Star, !SF)
    ),
    list.foldl2(P, 0..N, [], Starfield, !IO).

%-----------------------------------------------------------------------------%

    % Make a new star.
    % If MaxY is zero, we MUST start from 0.0 as the star has been rolled off
    % the bottom so we want a full traversal down the screen this time.
mk_star(MaxX, MaxY, Star, !IO) :-
    (  if  MaxY  = 0
     then  StarY = 0.0
     else  rnd.float(0, MaxY, StarY, !IO)
    ),

    % set the 'distance' via a relative velocity and colour.
    rnd.float(0, MaxX, X, !IO),
    rnd.int(0, 2, V, !IO),

    ( if V = 0 then 
        Star = star(X, StarY, 10.0, to_rgba(color(gray)))
    else if V = 1 then
        Star = star(X, StarY, 50.0, to_rgba(color(white)))
    else
        Star = star(X, StarY, 70.0, to_rgba(color(purple)))
    ).

%-----------------------------------------------------------------------------%

    % Render the starfield.
    %
draw_stars([], !IO).
    % TODO: external control over starfield such as star size, global color
    % etc to synchronise in game effects like hyperdive, all that jazz.
draw_stars([ star(X, Y, _, Colour) | Stars ], !IO) :-
    draw_rectangle(
        round_to_int(X), round_to_int(Y), 5, 5,
        Colour, !IO
    ),
    draw_stars(Stars, !IO).    

%-----------------------------------------------------------------------------%

    % Step update all stars.
    % When a star drops of the bottom of the screen we create a new set of
    % variables for it to maintain the scrolling effect.
    % On done we do NOT bother reversing the list, rendering is indifferent.
    %
step_stars(FrameTime, MaxX, MaxY, !Stars, !IO) :-
    step_stars_(FrameTime, MaxX, MaxY, !.Stars, [], !:Stars, !IO).


:- pred step_stars_(float::in, int::in, int::in, stars::in,
    stars::in, stars::out, io::di, io::uo) is det.

step_stars_(_, _, _, [], !Acc, !IO).

step_stars_(FrameTime, MaxX, MaxY,
    [ star(SX, SY, VEL, CLR) | Stars ],
    !Acc, !IO
) :-
    NewY = SY + ( VEL * FrameTime ),
    ( if NewY >= float(MaxY) then
        mk_star(MaxX, 0, Star, !IO)
    else
        Star = star(SX, NewY, VEL, CLR)
    ),
    list.cons(Star, !Acc),
    step_stars_(FrameTime, MaxX, MaxY, Stars, !Acc, !IO).

%----------------------------------------------------------------------------%
:- end_module starfield.
%----------------------------------------------------------------------------%

