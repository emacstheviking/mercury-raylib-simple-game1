%-----------------------------------------------------------------------------%
%
% File: layout.m
% Main author: Sean Charles
% Date: Wed Sep  6 20:04:38 2023
%
%
% Helpers for simple layout calculations.
%
%-----------------------------------------------------------------------------%
:- module layout.

:- interface.
:- import_module float.
:- import_module io.
:- import_module list.
:- import_module string.

:- import_module datatypes.
:- import_module raylib.

:- pred centre_text(float::in, rfont::in, float::in, string::in, float::out,
    io::di, io::uo) is det.

:- func grid_layout(float::in, float::in, float::in, float::in, float::in,
    float::in, int::in, int::in) = (list(vector2f)::out) is det.


:- func centre(float::in, float::in) = (float::out) is det.

:- implementation.

:- import_module int.

%----------------------------------------------------------------------------%

    % Centre a text string within line length.
    %
centre_text(Line, Font, Size, Text, centre(Line, Width), !IO) :- 
    measure_text_ex(Font, Text, Size, 1.0, Width, _, !IO).


centre(Line, Width) = ( Line / 2.0 ) - ( Width / 2.0 ).


%----------------------------------------------------------------------------%

    % Calculate a simple grid formation for N cells.
    % Width is the enclosing width, typicall screen width.
    % Y0 is top top-edge of the first row of output cells.
    % Sprite, Sprite defines the dimensions of a target cell.
    % GapX, Gapy is the grid spacing across and down.
    % Rows, Cols defines the output grid size.
    % Out will be unified with the final list of vector2f()
    %
grid_layout(Width, Y0, SpriteW, SpriteH, GapX, GapY, Rows, Cols) = Out :-
    Fcols  = float(Cols),
    ChildW = (SpriteW * Fcols ) + (GapX * (Fcols - 1.0)),
    XLeft  = centre(Width, ChildW),

    grid_layout_(
        XLeft,
        XLeft, Y0,
        SpriteW, SpriteH, GapX, GapY,
        Rows, Cols,
        [], Out0
    ),
    list.reverse(Out0, Out).

    % Auxiliary.
    %
:- pred grid_layout_(
    float::in, float::in, float::in, float::in, float::in, float::in,
    float::in, int::in, int::in, list(vector2f)::in, list(vector2f)::out
) is det.

grid_layout_(XLeft, X, Y, W, H, GapX, GapY, Rows, Cols, !Acc) :-
    list.cons(vector2f(X, Y), !Acc),
    Len = list.length(!.Acc),
    ( if list.length(!.Acc) < Rows * Cols then
        % Update X and Y when we reach the end of a row.

        ( if Len mod Cols = 0 then
            % Start a new row.
            grid_layout_(
                XLeft, XLeft, Y + H + GapY,
                W, H, GapX, GapY, Rows, Cols,
                !Acc
            )
        else
            % Fill current row.
            grid_layout_(
                XLeft, X + W + GapX, Y,
                W, H, GapX, GapY, Rows, Cols,
                !Acc
            )
        )
    else
        true
    ).

%----------------------------------------------------------------------------%
:- end_module layout.
%----------------------------------------------------------------------------%
