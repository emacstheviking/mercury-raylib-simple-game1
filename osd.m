%-----------------------------------------------------------------------------%
%
% File: osd.m
% Main author: Sean Charles
% Date: Fri Sep  8 06:41:34 2023
%
% OSD - On-Screen Display module.
%
% Any non-game content is handled by the OSD. This is things like level number,
% score, player health status and any transient message text.
%
%-----------------------------------------------------------------------------%
:- module osd.

:- interface.
:- import_module io.

:- import_module assets.
:- import_module datatypes.
:- import_module easing.
:- import_module gamestate.

    % Initialise the OSD module.
    %
:- pred osd_init(game_state::in, osd::out, io::di, io::uo) is det.
:- pred osd_step(float::in, osd::in, osd::out) is det.
:- pred osd_health_update(float::in, osd::in, osd::out) is det.

:- pred draw_lives(osd::in, int::in, io::di, io::uo) is det.
:- pred draw_health(osd::in, float::in, io::di, io::uo) is det.
:- pred draw_score(osd::in, int::in, io::di, io::uo) is det.
:- pred draw_current_track(osd::in, io::di, io::uo) is det.


    % OSD state information.
    %
:- type osd
    --->    osd(
                assets      :: asset_library,
                score_xy    :: vector2f,
                lives_xy    :: vector2f,
                health_xy   :: vector2f,
                track_xy    :: vector2f,
                scale       :: float,
                dig_w       :: float,
                dig_h       :: float,
                health      :: tween
            ) .


:- type digit_mode
    --->    score
    ;       lives.


:- implementation.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module raylib.
:- use_module audio.

%----------------------------------------------------------------------------%

    % Initialise the On-Screen display manager.
    % We precalculate as much as we can to make it more efficient at runtime.
    % Note that the (X,Y) may be subject to tweening adjustments but the core
    % positions can be calculated!
    %
osd_init(GS,
    osd(
        GS ^assets,                 % Full asset library.
        vector2f(ScoreX, ScoreY),   % Player score readout.
        vector2f(LivesX, ScoreY),   % Player lives readout.
        vector2f(HealthX, ScoreY),  % Health bar.
        vector2f(TrackX, TrackY),   % OSD: Current music track name.
        Scale,                      % Initial scale factor of digit tiles.
        DigW, DigH,                 % Cached sizes.

        tween_init(linear, 0.0, 100.0, 1.0)
    ),
    !IO
) :-
    Scale = 4.0,
    rectangle(_, _, DigW, DigH) = slice(dig0),
    ScreenW = GS ^screen_wf,
    ScoreY  = DigH * 0.5,
    ScoreX  = DigW * Scale * 2.0,
    LivesX  = ScreenW - ((Scale * 3.0) * DigW ),
    HealthX = LivesX - 20.0,
    TrackX  = 10.0,
    TrackY  = GS ^screen_hf - 30.0.


osd_step(FrameTime, !OSD) :-
    tween_step(FrameTime, !.OSD ^health, Tween),
    !OSD ^health := Tween.


osd_health_update(NewValue, !OSD) :-
    !OSD ^health := tween_init(
        linear,
        !.OSD ^health^value,
        NewValue,
        1.0).

%----------------------------------------------------------------------------%

    % Draw the current player lives remaining value.
    %
draw_lives(OSD, N, !IO) :-
    digit_at_(
        string.to_char_list(string.format("%02i", [ i(N) ])),
        OSD ^lives_xy,
        to_rgba(color(white)),
        OSD ^scale,
        OSD ^assets ^sheet1,
        !IO).

%----------------------------------------------------------------------------%

    % Draw the current player score value.
    %
draw_score(OSD, N, !IO) :-
    digit_at_(
        string.to_char_list(string.format("%06i", [ i(N) ])),
        OSD ^score_xy,
        to_rgba(color(white)),
        OSD ^scale,
        OSD ^assets ^sheet1,
        !IO).

%----------------------------------------------------------------------------%

    % Draw the current player score value.
    %
draw_health(OSD, N, !IO) :-
    (    if N < 25.0 then Color = with_a(color(red), 250u8)
    else if N < 75.0 then Color = with_a(color(yellow), 250u8)
    else                  Color = with_a(color(lime), 250u8)
    ),
    MaxHeight = OSD ^dig_h * OSD ^scale,
    VisHeight = ( MaxHeight * OSD ^health^value ) / 100.0,
    vector2f(X, Y) = OSD ^health_xy,
    draw_rectanglef(X, Y, 12.0, VisHeight, Color, !IO).

%----------------------------------------------------------------------------%

    % Render a single digit tile.
    % The digit can be specified at a scale factor for animation purposes.
    % The tint colour is also open for manipulation for animation.
    %
:- pred digit_at_(chars::in, vector2f::in, uint32::in, float::in, rtexture::in,
    io::di, io::uo) is det.

digit_at_([], _, _, _, _, !IO).

digit_at_(
    [ C | Cs ], vector2f(X, Y), Color, Scale, Sheet, !IO
) :-
    Digit  = digit_slice(C),
    Width  = DigW * Scale,
    Height = DigH * Scale,

    rectangle(_, _, DigW, DigH) = Digit,

    draw_texture_rec_pro(
        Sheet, Digit,
        rectangle(X, Y, Width, Height),
        vector2f(0.0, 0.0), 0.0, Color,
        !IO
    ),
    digit_at_(Cs, vector2f(X + Width + 4.0, Y), Color, Scale, Sheet, !IO).

%----------------------------------------------------------------------------%

    % Displays currently playing music track.
    %
draw_current_track(OSD, !IO) :-
    Name = audio.current_track_name,
    vector2f(TrackX, TrackY) = OSD ^track_xy,
    draw_text_ex(
        OSD ^assets^main_font,
        Name,
        floor_to_int(TrackX),
        floor_to_int(TrackY),
        20,
        1.0,
        to_rgba(color(white)),
        !IO
    ).

%----------------------------------------------------------------------------%

    % Generic sprite management, similar to animtext...

%----------------------------------------------------------------------------%
:- end_module osd.
%----------------------------------------------------------------------------%

