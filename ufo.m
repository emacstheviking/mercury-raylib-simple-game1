%-----------------------------------------------------------------------------%
%
% File: ufo.m
% Main author: Sean Charles
% Date: Thu Oct  5 06:30:44 2023
%
% This manages a single UFO entity from the level_ufo.m module
%
%-----------------------------------------------------------------------------%
:- module ufo.

:- interface.
:- import_module io.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.

:- import_module collision.
:- import_module datatypes.
:- import_module easing.
:- import_module gamestate.
:- import_module raylib.


:- pred draw_ufo(ufo::in, rtexture::in, bool::in, io::di, io::uo) is det.
:- pred step_ufos(float::in, int::in, int::in, ufos::in, ufos::out,
    int::out, io::di, io::uo) is det.

:- func make_ufo(ufo_type::in, float::in, float::in, int::in, game_state::in)
    = (ufo::out) is det.

:- pred random_trait(ufo.ufo_type::out, io::di, io::uo) is det.


:- func ufo_score(ufo_type::in) = (int::out) is det.
:- func ufo_hits(ufo_type::in, int::in) = (int::out) is det.
:- func ufo_xy(ufo::in) = (vector2f::out) is det.
:- func downcount_ufo(ufo::in) = (ufo::out) is det.
:- func explode_color(ufo_type::in) = (color::out) is det.
:- func exploded_ufo(ufo::in) = (ufo::out) is det.
:- func ufo_slice(ufo_type::in) = (rectangle::out) is det.
:- func ufo_bounds(ufo::in) = (vector3f::out) is det.
:- func ufo_bounds_rect(ufo::in) = (rectangle::out) is det.
:- func ufo_rot_bounds(ufo::in) = (vector4f::out) is det.


    % Define the du for a UFO.
    %
:- type ufo
    --->    ufo(
                uid     :: ufo_type,
                state   :: ufo_state,
                trait   :: ufo_type,
                xy      :: fp_point,
                angle   :: tween,
                alpha   :: tween,
                drift   :: tween,
                explode :: tween,
                hit     :: bool,    % one-shot collision detection
                hits    :: int,
                index   :: int
            ).

    % Operational states.
    %
:- type ufo_state
    --->    loading
    ;       drifting
    ;       exploding
    ;       exploded
    .

    % Types of behaviours.
    %
:- type ufo_type
    --->    simple      % (yellow)  takes N hits before dies.
    ;       dodger      % (green)   evasive at last minute.
    ;       mystery     % (blue)    who knows what they will do?
    ;       fighter.    % (red)     aggressively attack.

:- type ufos == list(ufo).

    % TYPECLASS: We can be part of a collision detection process.
    %
:- instance hittable(ufo.ufo).


:- implementation.
:- import_module maybe.
:- import_module set.
:- import_module string.

:- import_module assets.
:- import_module audio.
:- import_module layout.

%----------------------------------------------------------------------------%

    % TYPECLASS: We can be part of a collision detection process.
    %
:- instance hittable(ufo.ufo) where
[
  id(T)         = T ^index,
  as_point(T)   = fp_float_v2(T ^xy),
  is_hit(T)     = T ^hit,
  func(can_be_hit/1) is ufo.can_be_hit,
  func(as_circle/1) is ufo.ufo_bounds,
  func(as_rect/1)   is ufo.ufo_bounds_rect,
  (set_hit(!T) :- !T ^hit := yes)
].

:- func can_be_hit(ufo::in) = (bool::out) is det.
can_be_hit(U) = 
  (if U ^state = drifting then yes else no).

%----------------------------------------------------------------------------%

    % Step-update the UFO tween parts, damage counters etc.
    % DoneCount is the number of UFO-s that have reached the end of their
    % designated flight-path.
    %
step_ufos(FrameTime, MaxX, MaxY, !Ufos, DoneCount, !IO) :-
    step_ufos_(FrameTime, MaxX, MaxY, !.Ufos,
        [], !:Ufos,
        0,  DoneCount,
        !IO).


:- pred step_ufos_(
    float::in, int::in, int::in, ufos::in,
    ufos::in, ufos::out, int::in, int::out, io::di, io::uo
) is det.

step_ufos_(_, _, _, [], !Acc, !_, !IO).

step_ufos_(FrameTime, MaxX, MaxY, [ U | Ufos ],!Acc, !Count, !IO) :-
    U = ufo(
            Id, State0, Slice, XY0, Angle0,
            Alpha0, Drift0, Ex0, Hit, Hits,
            Index
    ),
    tween_step(FrameTime, Angle0, Angle),
    tween_step(FrameTime, Alpha0, Alpha),
    (
        State0 = loading,
        State  = loading,

        Drift  = Drift0,
        Ex     = Ex0,
        XY = fp_step_pt(FrameTime, XY0),
        fp_point(CurX, CurY) = XY,
        (   if  fp_done(CurX), fp_done(CurY)
          then  !:Count = !.Count +1
          else  true
        )
    ;
        State0 = drifting,
        State  = drifting,

        XY     = XY0,
        Ex     = Ex0,
        tween_step(FrameTime, Drift0, Drift)
    ;
        State0 = exploding,

        tween_step(FrameTime, Ex0, Ex),
        ( if tween_done(Ex) then
            State = exploded
        else
            State = State0
        ),
        Drift = Drift0,
        XY    = XY0
    ;
        State0 = exploded,
        State  = exploded,

        Drift  = Drift0,
        XY     = XY0,
        Ex     = Ex0
    ),

    list.cons(
        ufo(Id, State, Slice, XY, Angle, Alpha, Drift, Ex, Hit, Hits, Index),
        !Acc
    ),
    step_ufos_(FrameTime, MaxX, MaxY, Ufos, !Acc, !Count, !IO).

%----------------------------------------------------------------------------%

    % Draw a single UFO.
    %
draw_ufo(UFO, Sheet, Debug, !IO) :-

    Slice = ufo_slice(UFO ^trait),
    vector4f(RotX, RotY, UW, UH) = ufo_rot_bounds(UFO),

    vector2f(X, Y) = ufo_xy(UFO),
    (
        ( UFO ^state = drifting ; UFO ^state = loading ),

        draw_texture_rec_pro(
            Sheet,
            Slice,
            rectangle(X, Y, UW, UH),
            vector2f(RotX, RotY),
            UFO ^angle^value,
            with_af(color(white), UFO ^alpha^value),
            !IO
        ),
        ( Debug = yes -> show_ufo_index(UFO, X, Y, !IO) ; true )
    ;
        UFO ^state = exploding,

        Color = with_af(explode_color(UFO ^trait), UFO ^alpha^value),
        draw_texture_rec_pro(
            Sheet,
            Slice,
            rectangle(X, Y, UW, UH),
            vector2f(RotX, RotY),
            UFO ^angle^value,
            Color,
            !IO
        ),
        draw_circle_gradient(
            X, Y, UFO ^explode^value,
            Color, with_af(color(white), 64.0),
            !IO)
    ;
        UFO ^state = exploded
    ).

:- pred show_ufo_index(ufo::in, float::in, float::in, io::di, io::uo) is det.

show_ufo_index(UFO, X, Y, !IO) :-
    draw_text(
        string.format("%i/%i", [i(UFO ^index), i(UFO ^hits)]),
        truncate_to_int(X), truncate_to_int(Y),
        20,
        to_rgba(color(yellow)), !IO).

%----------------------------------------------------------------------------%

    % Return realised (X,Y) for a UFO.
    % Depending on the state of the UFO, it's x-y position may or may not
    % be as simple as its implicit xy.
    %
ufo_xy(UFO) = XY :-
    State = UFO ^state,
    vector2f(X0, Y0) = fp_float_v2(UFO ^xy),
    (
        (   State = loading
        ;   State = exploding
        ;   State = exploded
        ),
            XY = vector2f(X0, Y0)
    ;
        State = drifting,
        XY = vector2f(X0 + UFO ^drift^value, Y0)
    ).

%----------------------------------------------------------------------------%

    % Calculate score value for a destroyed UFO.
    %
ufo_score(simple)  = 25.
ufo_score(mystery) = 75.
ufo_score(dodger)  = 100.
ufo_score(fighter) = 200.

%----------------------------------------------------------------------------%

    % Calculate hits required to trash a UFO.
    %
ufo_hits(simple, Level)  = Level + 0.
ufo_hits(mystery, Level) = Level + 1.
ufo_hits(dodger, Level)  = Level + 2.
ufo_hits(fighter, Level) = Level + 3.

%----------------------------------------------------------------------------%

    % Map a UFO type to get it's explosion color.
    %
explode_color(simple)  = color(yellow).
explode_color(dodger)  = color(lime).
explode_color(mystery) = color(skyblue).
explode_color(fighter) = color(red).

%----------------------------------------------------------------------------%

    % Map UFO type to the image slice.
    %
ufo_slice(simple)  = slice(ufo_yellow).
ufo_slice(dodger)  = slice(ufo_green).
ufo_slice(mystery) = slice(ufo_blue).
ufo_slice(fighter) = slice(ufo_red).

%----------------------------------------------------------------------------%

    % Returns the rotational center point and bounds.
    %
:- pragma inline(ufo_rot_bounds/1).

ufo_rot_bounds(UFO) = vector4f(UW / 2.0, UH / 2.0, UW, UH) :-
    Slice = ufo_slice(UFO ^trait),
    rectangle(_, _, UW, UH) = Slice.

%----------------------------------------------------------------------------%

    % Returns a UFO bounding circle.
    %
:- pragma inline(ufo_bounds/1).

ufo_bounds(UFO) = vector3f(CX, CY, R) :-
    Slice = ufo_slice(UFO ^trait),
    rectangle(_, _, UW, _) = Slice,
    vector2f(CX, CY) = ufo_xy(UFO),
    R = UW / 2.0.

%----------------------------------------------------------------------------%

    % Return ship bounds as a rectangle.
    %
:- pragma inline(ufo_bounds_rect/1).

ufo_bounds_rect(UFO) = rectangle(X, Y, W, H) :-
    ufo_slice(UFO ^trait) = rectangle(_, _, W, H),
    vector2f(X, Y) = fp_float_v2(UFO ^xy).

%----------------------------------------------------------------------------%

    % Make a UFO instance with given parameters.
    %
make_ufo(Trait, Px, Py, Index, GS) =
    ufo(
        simple,
        loading,
        Trait,
        fp_point(
            dval(tween_init_ex(
                sine_out, GS ^screen_wf / 2.0, Px, 0.5, oneshot, 0.0)
            ),
            dval(tween_init_ex(
                sine_out, GS ^screen_hf / 2.0, Py, 1.0, oneshot, 0.5)
            )
        ),
            % Rotation angle.
        tween_init_ex(linear, 0.0, 360.0, 1.0, repeat, 0.0),

            % Alpha blending.
        tween_init(linear, 0.0, 255.0, 3.0),

            % L-R drift value.
        tween_init_ex(linear, 0.0, 0.0, 0.0, reverse, 0.0),

            % Explosion.
        tween_init(linear, 0.0, 0.0, 0.0),

           % Not hit yet.
        no,

            % Hits before removal.
        ufo_hits(Trait, GS ^level),

            % Index
        Index
    ).

%----------------------------------------------------------------------------%

    % Generate a random UFO trait.
    %
random_trait(Trait, !IO) :-
    get_random_value(0, 3, Rval, !IO),
    (     if Rval = 0 then Trait = simple
     else if Rval = 1 then Trait = fighter
     else if Rval = 2 then Trait = mystery
     else                  Trait = dodger
    ).

%----------------------------------------------------------------------------%

    % Downcount the number of hits remaining, resetting the hit flag.
    %
:- pragma inline(downcount_ufo/1).

downcount_ufo(U0) = (( U0 ^hits := hits(U0) -1 ) ^hit  := no ).

%----------------------------------------------------------------------------%

    % Return a modified UFO that is starting to explode!
    %
exploded_ufo(U0) = Out :-
    Slice = ufo.ufo_slice(U0 ^trait),
    rectangle(_, _, UW, _) = Slice,
    vector2f(CX, CY) = ufo_xy(U0),
    UW2 = UW / 2.0,
    some [ !U ] (
        !:U = U0,
        !U ^state   := exploding,
        !U ^ xy     := fp_point(sval(CX), sval(CY)),
        !U ^explode := tween_init(linear, UW2 / 3.0, UW * 1.5, 0.3),
        !U ^ alpha  := tween_init(linear, 255.0, 0.0, 0.3),
        Out = !.U
    ).


%----------------------------------------------------------------------------%
:- end_module ufo.
%----------------------------------------------------------------------------%
