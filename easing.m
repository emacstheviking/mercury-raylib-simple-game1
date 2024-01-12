%-----------------------------------------------------------------------------%
%
% File: easing.m
% Main author: Sean Charles
% Date: Fri Aug 18 09:20:06 2023
%
% A simple easing library based on Robert Penners now famous equations.
%
%-----------------------------------------------------------------------------%
:- module easing.

:- interface.
:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module datatypes.


    % Easing operations.
    % visuals:   https://easings.net/
    % rust code: https://docs.rs/raylib/latest/src/raylib/ease.rs.htm
    %
:- type ease_op
    --->    back_in
    ;       back_out
    ;       bounce_in
    ;       bounce_out
    ;       bounce_in_out
    ;       elastic_in
    ;       linear % lerp
    ;       sine_in
    ;       sine_out
    ;       sine_in_out
    .

    % A tween for a single value.
    %
:- type tween
    --->    tween(
                op          :: ease_op,
                start       :: float,
                end         :: float,
                current     :: float,
                duration    :: float,
                value       :: float,
                done        :: bool,
                reset_mode  :: tween_mode,
                predelay    :: float,
                events      :: uint8
            ).

:- type tween_handler
    ---> tween_handler(pred(io::di, io::uo) is det).

:- type tweens == list(tween).

:- type tween_mode
    --->    oneshot     % the default
    ;       repeat      % keeps resetting progress
    ;       reverse.    % reset progress, swaps star, end


    % FLIGHTPATH:
    %  x -> a tween or a constant
    %  y -> a tween or a constant
    %
:- type fp_value
    --->    sval(float)
    ;       dval(tween).

:- type fp_point
    --->    fp_point(fp_value, fp_value).

    % A tweenable RGBA color or it can be static.
    %
:- type tween_color
    --->    s_color(uint32)
    ;       d_color(fp_value, fp_value, fp_value, fp_value).



    %--------------------------------------------------%
    % Module API
:- pred tween_step(float::in, tween::in, tween::out) is det.

:- pred tween_done(tween::in) is semidet.
:- func tweens_done(tweens::in) = (bool::out) is det.

:- func tween_init(ease_op::in, float::in, float::in, float::in)
    = (tween::out) is det.

:- func tween_init_ex(ease_op::in, float::in, float::in, float::in,
    tween_mode::in, float::in) = (tween::out) is det.

:- func tween_step_color(float::in, tween_color::in)
    = (tween_color::out) is det.

:- func fp_float(fp_value::in) = (float::out) is det.
:- func fp_float_v2(fp_point::in) = (vector2f::out) is det.
:- func fp_float_v3(fp_point::in, float::in) = (vector3f::out) is det.

:- func fp_step_pt(float::in, fp_point::in) = (fp_point::out) is det.
:- func fp_step(float::in, fp_value::in) = (fp_value::out) is det.
:- pred fp_done(fp_value::in) is semidet.

:- implementation.
:- import_module float.
:- import_module int.
:- import_module maybe.
:- import_module string.

:- use_module math.

%-----------------------------------------------------------------------------%

    % Make an easing instance.
    %
tween_init(Op, Start, End, Duration)
    = tween(Op, Start, End, 0.0, Duration, Start, no, oneshot, 0.0, 0u8).

tween_init_ex(Op, Start, End, Duration, Mode, Predelay)
    = tween(Op, Start, End, 0.0, Duration, Start, no, Mode, Predelay, 0u8).


%-----------------------------------------------------------------------------%

    % Stepping updater.
    % This updates an easing instance according to the amount of time elapsed
    % since the last update. On reaching the desired duration, we explicitly
    % set the end value to avoid any floating point roundoff errors.
    %
tween_step(FrameTime, Tween, Out) :-
    Tween = tween(
        Op, Start, End, Current, Duration, Value, Done, RMode, Delay0, Handler
    ),

    ( if float.is_zero(Tween ^predelay) then
        tween_step_(FrameTime, Tween, Out)
    else
        Delay = Delay0 - FrameTime,
        ( if ((Delay < 0.0 ) ; is_nan_or_infinite(Delay)) then
            Out = Tween ^predelay := 0.0
        else
            Out = tween(Op, Start, End, Current, Duration,
                Value, Done, RMode, Delay, Handler
            )
        )
    ).


:- pred tween_step_(float::in, tween::in, tween::out) is det.    

tween_step_(FrameTime, Tween, Out) :-
    Tween = tween(
        Op, Start, End, Current0, Duration, _, Done, RMode, Delay0, Handler
    ),
    (
        Done = no,
        Current = Current0 + FrameTime,

        ( if ((Current > Duration ) ; is_nan_or_infinite(Current))
        then
            % Tween completed: process it's Reset Mode.
            (
                RMode = oneshot,
                Out = (Tween ^done := yes) ^value := End
            ;
                RMode = repeat,
                Out = Tween ^current := 0.0
            ;
                RMode = reverse,
                Out = ((Tween ^current := 0.0)
                    ^start := End)
                    ^end   := Start
            )
        else
            Val = step_it(Op, Current, End - Start, Duration),
            Out = tween(
                Op, Start, End, Current, Duration,
                Start + Val, Done, RMode, Delay0, Handler
            )
        )
    ;
        Done = yes,
        Out  = Tween
    ).

%-----------------------------------------------------------------------------%

    % Has a tween completed its journey?
    %
tween_done(T) :-
    T ^done = yes.

tweens_done(Tweens) =
    ( list.all_true(tween_done, Tweens) -> yes ; no ).

%-----------------------------------------------------------------------------%
% Tweening operations.

:- func step_it(ease_op::in, float::in, float::in, float::in)
    = (float::out) is det.


    % LINEAR
    %
step_it(linear, Time, Range, Duration) = X :-
    (
        is_zero(Duration)
    ->
        X = Range
    ;
        X = Range * (Time / Duration)
    ).

%-----------------------------------------------------------------------------%
    % t b c d = time start end-start duration
    % BACK
step_it(back_in, Time, _Range, Duration) = V :-
    Percent = Time / Duration,
    P1 = s + 1.0,
    V  = ( P1 * Percent * Percent * Percent ) - ( s * Percent * Percent ).
%    V = Percent * Percent * ( (s + 1.0) * ( Percent - s )).

step_it(back_out, Time, _Range, Duration) = V :-
    Percent = Time / Duration,
    Td = Percent - 1.0,
    V  = (Td * Td * ( (s + 1.0) * Td + s) + 1.0 ).

%-----------------------------------------------------------------------------%

    % ELASTIC
    %
step_it(elastic_in, Time, Range, Duration) = V :-
    Percent = Time / Duration,
    ( if is_zero(Time)
        then V = 0.0
    else if Percent = 1.0
        then V = Range
    else
        C4 = ( 2.0 * math.pi ) / 3.0,
        V  = -(math.pow( 2.0, 10.0 * Percent - 10.0) *  math.sin( Percent * 10.0 - 10.75) * C4)
    ),
    trace [io(!IO)] (
        io.format(io.stderr_stream,"%f,%f\n", [f(Percent), f(V)], !IO)
    ).

%-----------------------------------------------------------------------------%

    % SINE
    %
step_it(sine_in, Time, Range, Duration) = V :-
    Percent = Time / Duration,
    V = (-Range * (math.cos((Percent * math.pi) / 2.0)) + Range).

step_it(sine_out, Time, Range, Duration) = V :-
    Percent = Time / Duration,
    V = Range * math.sin((Percent * math.pi) / 2.0).

step_it(sine_in_out, Time, Range, Duration) = V :-
    Percent = Time / Duration,
    V = (-Range / 2.0) * (math.cos(math.pi * Percent) - 1.0).

%-----------------------------------------------------------------------------%

    % BOUNCE
    %
    % The bounce out does the hard work, the other two call it but
    % make adjustments to the returned value. Get this right first!
    %
step_it(bounce_out, Time, Range, Duration) = V :-
    Percent = Time / Duration,
    ( if Percent < 1.0 / 2.75 then
        V = Range * n1 * Percent * Percent

    else if Percent < 2.0 / 2.75 then
        Td1 = Percent - 1.5 / 2.75,
        V = Range * ( n1 * Td1 * Td1 +0.75 )

    else if Percent < 2.5 / 2.75 then
        Td1 = Percent - 2.25 / 2.75,
        V = Range * ( n1 * Td1 * Td1 +0.9375 )
    else
        Td1 = Percent - 2.625 / 2.75,
        V = Range * ( n1 * Td1 * Td1 + 0.984375 )
    ).

step_it(bounce_in, Time, Range, Duration) =
    Range - step_it(bounce_out, Duration - Time, Range, Duration).

step_it(bounce_in_out, Time, Range, Duration) = V :-
    Time2 = Time * 2.0,
    ( if Time < ( Duration / 2.0 ) then
        V = step_it(bounce_in, Time2, Range, Duration) * 0.5
    else
        V = step_it(bounce_out, Time2 - Duration, Range, Duration) * 0.5 + ( Range * 0.5 )
    ).

%-----------------------------------------------------------------------------%
% Bounce constants.
% Our makefile specifies: --optimize-constant-propagation to flush out static
% constants but this was written before I knew of that flag. Might change!
%

:- pragma inline(func(n1/0)).
:- func n1 = (float::out) is det.
n1 = 7.5625.

:- pragma inline(func(d1/0)).
:- func d1 = (float::out) is det.
d1 = 2.75.

:- pragma inline(func(d1/0)).
:- func s = (float::out) is det.
s = 1.70158.


%----------------------------------------------------------------------------%

    % Step update an animated colour instance.
    %
tween_step_color(_, s_color(X)) = s_color(X).

tween_step_color(FrameTime, d_color(R0, G0, B0, A0))
    = d_color(
        fp_step(FrameTime, R0),
        fp_step(FrameTime, G0),
        fp_step(FrameTime, B0),
        fp_step(FrameTime, A0)).



%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
% Flightpaths
% These build on the basic tween system to provide the concept of a 'point'
% that can be either a static value or a tweenable value.

    % Return the current value of a flighpath float.
    %
fp_float(sval(F)) = F.
fp_float(dval(T)) = T^value.

    % Returns a flighpath vector2f() representation
    %
:- pragma inline(fp_float_v2/1).

fp_float_v2(fp_point(Px, Py))
    = vector2f(fp_float(Px), fp_float(Py)).

    % Returns a flighpath vector3f() representation
    %
:- pragma inline(fp_float_v3/2).

fp_float_v3(fp_point(Px, Py), R)
    = vector3f(fp_float(Px), fp_float(Py), R).

    % Step update a FlighPath -point- by the given time.
    %
fp_step_pt(FrameTime, fp_point(X0, Y0))
    = fp_point(fp_step(FrameTime, X0), fp_step(FrameTime, Y0)).

%-----------------------------------------------------------------------------%

    % Step update a fligh path -value- by the given time.
    %
fp_step(_, sval(F)) = sval(F).

fp_step(FrameTime, dval(Tween0)) = Out :-
    tween_step(FrameTime, Tween0, Tween),
    Out = dval(Tween).

    % Check if a flight path point is stationary.
    %
fp_done(sval(_)).
fp_done(dval(T)) :- T ^done = yes.

%----------------------------------------------------------------------------%
:- end_module easing.
%----------------------------------------------------------------------------%
