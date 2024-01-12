%-----------------------------------------------------------------------------%
%
% File: datatypes.m
% Main author: Sean Charles
% Date: Sat Sep  2 10:03:15 2023
%
% Application wide common datatypes.
%
%-----------------------------------------------------------------------------%
:- module datatypes.

:- interface.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.

:- type rectangle
    --->    rectangle(float, float, float, float).

:- type vector4f
    --->    vector4f(float, float, float, float).

:- type vector4i
    --->    vector4i(int, int, int, int).

:- type vector2f
    --->    vector2f(float, float).

:- type vector3f
    --->    vector3f(float, float, float).

:- type chars == list(char).


:- func v2_circle(vector2f::in, float::in) = (vector3f::out) is det.

:- implementation.

v2_circle(vector2f(X, Y), R) = vector3f(X, Y, R).

%----------------------------------------------------------------------------%
:- end_module datatypes.
%----------------------------------------------------------------------------%

