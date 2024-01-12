%-----------------------------------------------------------------------------%
%
% File: animtext.m
% Main author: Sean Charles
% Date: Wed Aug 30 20:29:13 2023
%
% Animated text engine.
%
%-----------------------------------------------------------------------------%
:- module animtext.

:- interface.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module datatypes.
:- import_module easing.
:- import_module raylib.

    % A moving text instance.
    %
:- type anim_text
    --->    anim_text(
                at_text     :: string,
                at_index    :: int,
                at_xy       :: fp_point,
                at_color    :: tween_color,
                at_shadow   :: maybe(tween_color),
                at_font     :: rfont,
                at_size     :: fp_value,
                at_timer    :: tween
            ).

:- type text_effect
        % text starts from same origin point.
    --->    from_xy.

:- type anim_texts == list(anim_text).

:- pred draw_anim_text(anim_text::in, io::di, io::uo) is det.
:- pred draw_anim_texts(
    float::in, anim_text::in, anim_texts::in, anim_texts::out, io::di, io::uo
) is det.


:- pred dump_letters(anim_texts::in, io::di, io::uo) is det.
:- pred step_anim_text(float::in, anim_text::in, anim_text::out) is det.

:- func get_color(tween_color::in) = (uint32::out) is det.

    % Create an animated text from a string of text.
    %
:- pred string_to_animtext(
    text_effect::in,            % Desired text effect.
    string::in,                 % Source text string to process.
    rfont::in,                  % The font to be used for all characters.
    fp_value::in,               % Font size.
    tween_color::in,            % Colour as final RGBA value.
    maybe(tween_color)::in,     % Drop-shadow dolour as final RGBA value.
    vector2f::in,               % TextXY, the FINAL left-edge of the string.
    vector2f::in,               % FromXY, the starting X of all tweens.
    ease_op::in,                % Easing operator for X traversal.
    ease_op::in,                % Easing operator for Y traversal.
    vector2f::in,               % Seconds for the XY processing for letters.
    float::in,                  % Initial XY delay before any tweening starts.
    float::in,                  % Incr. value for each letters tween start.
    anim_texts::out,            % The fully instantiated list of tweens.
    io::di, io::uo
) is det.

    % Make a single instance of a text animation object.
    %
:- func mk_animtext(
    string::in, int::in, fp_point::in, tween_color::in, maybe(tween_color)::in,
    rfont::in, fp_value::in, tween::in
) = (anim_text::out) is det.


:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module uint8.
:- import_module uint32.

:- import_module layout.

:- use_module osd.

%----------------------------------------------------------------------------%

    % Create an animated text instance.
    %
mk_animtext(Text, Index, XY, Color, Shadow, Font, Size, Timer)
    = anim_text(Text, Index, XY, Color, Shadow, Font, Size, Timer).

%----------------------------------------------------------------------------%

    % Step a text animation by the elapsed time.
    %
step_anim_text(FrameTime,
    anim_text(
        Text, Index, XY0, Color0, Shadow0, Font, Size0, Timer0
    ),
    anim_text(
        Text, Index, XY, Color, Shadow, Font, Size, Timer
    )
) :-
        % If the Timer is done, we can update the step.
    ( if tween_done(Timer0) then
        Timer = Timer0,
        XY    = fp_step_pt(FrameTime, XY0),
        Size  = fp_step(FrameTime, Size0),
        Color = tween_step_color(FrameTime, Color0),
        (
            Shadow0 = no,
            Shadow  = no
        ;
            Shadow0 = yes(ShCol),
            Shadow  = yes(tween_step_color(FrameTime, ShCol))
        )
    else
        %% Update the timer first, we don't become active yet.
        XY     = XY0,
        Color  = Color0,
        Size   = Size0,
        Shadow = Shadow0,
        tween_step(FrameTime, Timer0, Timer)
    ).

%----------------------------------------------------------------------------%

    % Render an animated text instance.
    % If a shadow colour is present, we draw that text first at a fixed
    % drop shadow offset using the current shadow colour value.
    %
draw_anim_text(Text, !IO) :-
    vector2f(TextX, TextY) = fp_float_v2(Text ^at_xy),
    Color = get_color(Text ^at_color),

    ( if Text ^at_shadow = yes(Shadow) then
        ShColor = get_color(Shadow),
        draw_text_(Text, TextX - 4.0, TextY + 4.0, ShColor, !IO)
    else
        true
    ),
    draw_text_(Text, TextX, TextY, Color, !IO).


:- pred draw_text_(anim_text::in, float::in, float::in, uint32::in,
    io::di, io::uo) is det.

draw_text_(Text, TextX, TextY, Color, !IO) :-
    ( if Text ^at_index < 0 then
        draw_text_ex(
            Text ^at_font,
            Text ^at_text,
            floor_to_int(TextX),
            floor_to_int(TextY),
            floor_to_int(fp_float(Text ^at_size)),
            1.0,
            Color,
            !IO
        )
    else
        draw_text_ex_char(
            Text ^at_font,
            Text ^at_text,
            Text ^at_index,
            floor_to_int(TextX),
            floor_to_int(TextY),
            floor_to_int(fp_float(Text ^at_size)),
            1.0,
            Color,
            !IO
        )
    ).

%----------------------------------------------------------------------------%

    % Draw a list of animated text letters.
    %
draw_anim_texts(FrameTime, Letter, !Acc, !IO) :-
    draw_anim_text(Letter, !IO),
    step_anim_text(FrameTime, Letter, NewLetter),
    list.cons(NewLetter, !Acc).

%----------------------------------------------------------------------------%

    % Get a color as a uint32 value.
    % Colors may be static values, or tweened across a range.
    %
get_color(s_color(Color)) = Color.

get_color(d_color(R, G, B, A))
    % there MIGHT be an issue, eg -255.0 to 255.0 ...abs value?
    = uint32.from_bytes_le(
        uint8.cast_from_int(floor_to_int(fp_float(R))),
        uint8.cast_from_int(floor_to_int(fp_float(G))),
        uint8.cast_from_int(floor_to_int(fp_float(B))),
        uint8.cast_from_int(floor_to_int(fp_float(A)))).

%----------------------------------------------------------------------------%

    % Convert a string to a list of animated text instances.
    %
    % from_xy: all text will start from the same point.
    % note: if fontsize is dynamic we need the final size to correctly.
    %
string_to_animtext(
    Effect, Text, Font, Size, Color, Shadow,
    vector2f(TextX, TextY),
    vector2f(FromX, FromY),
    XEase, YEase,
    vector2f(TimeX, TimeY),
    Delay, Increment, Out, !IO
) :-
    (   Size = sval(N),
        FontSize = N
    ;
        Size = dval(Tween),
        FontSize = Tween ^end
    ),
    Splitter = (pred(
        Chr::in,   !.Index::in, !:Index::out, !.CurX::in, !:CurX::out,
        !.Acc::in, !:Acc::out,  !.Timer::in,  !:Timer::out,
        !.IO::di,  !:IO::uo
    ) is det :-
        (
            Effect = from_xy,
            Entry = mk_animtext(
                Text,
                !.Index,
                fp_point(
                    dval(tween_init(XEase, FromX, !.CurX, TimeX)),
                    dval(tween_init(YEase, FromY, TextY, TimeY))
                ),
                Color, Shadow, Font, Size,
                tween_init(linear, 0.0, 1.0, !.Timer)
            )
        ),
        list.cons(Entry, !Acc),

        measure_text_ex_char(Font, Chr, FontSize, 1.0, Width, _, !IO),
        !:CurX  = !.CurX  + Width,
        !:Index = !.Index + 1,
        !:Timer = !.Timer + Increment
    ),
    list.foldl5(
        Splitter, string.to_char_list(Text),
        0, _, TextX, _, [], Out, Delay, _,
        !IO
    ).

%----------------------------------------------------------------------------%

    % Diagnostic dump.
    %
dump_letters([], !IO).

dump_letters([L|Ls], !IO) :-
    io.print_line(L, !IO),
    dump_letters(Ls, !IO).

%----------------------------------------------------------------------------%
:- end_module animtext.
%----------------------------------------------------------------------------%

