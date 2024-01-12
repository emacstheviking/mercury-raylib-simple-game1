%-----------------------------------------------------------------------------%
%
% File: falling_object.m
% Main author: Sean Charles
% Date: Fri Oct 13 09:08:38 2023
%
% Each level has a series of what I calle 'falling objects' that rain down from
% the top of the screeb; power-ups, obstacles etc.
%
% This module manages all of those, collisions, score adjustments and all of
% the necessary ODS and spot sound effects too.
%
%-----------------------------------------------------------------------------%
:- module falling_object.

:- interface.
:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module collision.
:- import_module datatypes.
:- import_module easing.
:- import_module raylib.


:- pred step_objects(float::in, fobjects::in, fobjects::out, io::di, io::uo) is det.
:- pred draw_falling_objects(fobjects::in, rtexture::in, bool::in, io::di, io::uo) is det.

:- func fo_bounds_rect(fobject::in) = (rectangle::out) is det.


    % Falling object types.
    % Some are good to hit, most best avoided!
    %
:- type fo_type
    --->    health_boost1
    ;       meteor1.

    % Falling object DU type.
    % This maps each falling object state over the frame.
    %
:- type fobject
    --->    fobject(
                form    :: fo_type,
                xy      :: fp_point,
                angle   :: tween,
                fid     :: int,
                hit     :: bool,
                value   :: int,
                slice   :: rectangle 
            ).

:- type fobjects == list(fobject).
:- instance hittable(fobject).


:- implementation.
:- import_module float.
:- import_module int.
:- import_module string.

    % Collision detection interplay.
    %
:- instance hittable(fobject) where
[
  id(T)         = T ^fid,
  as_point(T)   = fp_float_v2(T ^xy),
  as_circle(T)  = fp_float_v3(T ^xy, 1.0),
  func(as_rect/1) is fo_bounds_rect,
  is_hit(T)     = T ^hit,
  can_be_hit(T) = (if T ^hit = no then yes else no),
  (set_hit(!T)  :- !T ^hit := yes)
].


%----------------------------------------------------------------------------%

    % Return falling object bounds as a rectangle.
    % TODO: Might have to reduce this is I can't find a better way to wrap
    % TODO: a polygonal shape around the irregular rocks and things.
    %
fo_bounds_rect(FO) = rectangle(X, Y, W, H) :-
    FO ^slice = rectangle(_, _, W, H),
    vector2f(X, Y) = fp_float_v2(FO ^xy).

%----------------------------------------------------------------------------%

    % Step a single falling object.
    %
step_objects(FrameTime, Objects, Out, !IO) :-
    Stepper = (pred(
        fobject(Form, XY0, Angle0, Id, Hit, Value, Slice)::in,
        !.Acc::in, !:Acc::out,
        !.IO::di,  !:IO::uo
    ) is det :-
        (   Form = health_boost1
        ;   Form = meteor1
        ),
        XY = fp_step_pt(FrameTime, XY0),
        tween_step(FrameTime, Angle0, Angle),

        list.cons(
            fobject(Form, XY, Angle, Id, Hit, Value, Slice),
            !Acc
        )
    ),
    list.foldl2(Stepper, Objects, [], Out, !IO).

%----------------------------------------------------------------------------%

    % Draw the falling objects.
    %
draw_falling_objects([], _, _, !IO).

draw_falling_objects([ Obj | Objects ], Sheet, Debug, !IO) :-
    rectangle(_, _, UW, UH) = Obj ^slice,
    vector2f(X, Y) = fp_float_v2(Obj ^xy),
    draw_texture_rec_pro(
        Sheet,
        Obj ^slice,
        rectangle(X, Y, UW, UH),
        vector2f(0.0, 0.0),
        0.0, % Obj ^angle^value, DISABLED until I understand how it does what it does
             % and can figure out the relationship betweem centre, rotation and then
             % to calculate the bounding box.
        to_rgba(color(white)),
        !IO
    ),
    ( Debug = yes ->
        ( Obj ^hit = yes
        -> Color = with_a(color(red), 127u8)
        ;  Color = with_a(color(white), 127u8)
        ),
        draw_rectangle(
            truncate_to_int(X), truncate_to_int(Y),
            truncate_to_int(UW), truncate_to_int(UH),
            Color, !IO)
    ; true
    ),
    draw_falling_objects(Objects, Sheet, Debug, !IO).

%----------------------------------------------------------------------------%
:- end_module falling_object.
%----------------------------------------------------------------------------%

