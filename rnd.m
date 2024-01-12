%-----------------------------------------------------------------------------%
%
% File: rnd.m
% Main author: Sean Charles
% Date: Wed Sep  6 06:49:16 2023
%
% Random number generation using Raylib.
%
% I somehow never felt comfortable with Mercury way of RNG usage, and as this
% is a game using Raylib, I am comitting to -its- RNG system as well.
%
%-----------------------------------------------------------------------------%
:- module rnd.

:- interface.
:- import_module io.


:- pred seed(uint::in, io::di, io::uo) is det.
:- pred float(int::in, int::in, float::out, io::di, io::uo) is det.
:- pred int(int::in, int::in, int::out, io::di, io::uo) is det.
:- pred u8(uint8::out, io::di, io::uo) is det.


:- implementation.
:- import_module raylib.

:- use_module float.
:- use_module int.
:- use_module uint8.


:- pragma inline(pred(seed/3)).
:- pragma inline(pred(int/5)).
:- pragma inline(pred(u8/3)).
:- pragma inline(pred(float/5)).

seed(Seed, !IO) :-
    set_random_seed(Seed, !IO).


int(From, To, Out, !IO) :-
    get_random_value(From, To, Out, !IO).


u8(Out, !IO) :-
    get_random_value(0, 255, R, !IO),
    Out = uint8.cast_from_int(R).

float(From, To, Out, !IO) :-
    get_random_fvalue(From, To, Out, !IO).

%----------------------------------------------------------------------------%
:- end_module rnd.
%----------------------------------------------------------------------------%

