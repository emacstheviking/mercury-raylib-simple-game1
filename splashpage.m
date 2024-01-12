%-----------------------------------------------------------------------------%
%
% File: splashpage.m
% Main author: Sean Charles
% Date: Tue Sep  5 13:38:09 2023
%
% Splash / Initial animation.
%
%-----------------------------------------------------------------------------%
:- module splashpage.

:- interface.
:- import_module io.

:- import_module animtext.
:- import_module gamestate.
:- import_module starfield.

:- pred splash_page_init(game_state::in, splash::out, io::di, io::uo) is det.

:- pred run_splash(
    game_state::in, game_state::out,
    splash::in, splash::out,
    io::di, io::uo
) is det.


    % All state for the Splash screen.
    %
:- type splash
    --->    splash(
                stars       :: stars,
                title       :: anim_texts,
                subtitle    :: anim_texts,
                play_text   :: anim_texts,
                exit        :: splash_state
            ).

:- type splash_state
    --->    running
    ;       start
    ;       quit.

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module uint8.

:- import_module assets.
:- import_module audio.
:- import_module datatypes.
:- import_module easing.
:- import_module g1.
:- import_module layout.
:- import_module raylib.

:- use_module rnd.

%----------------------------------------------------------------------------%

    % Create the Splash page.
    % This contains the game text image and a space ship as well as the
    % call-to-action push space to play message.
    %
splash_page_init(
    GameState,
     splash(Starfield, TITLE, SUBTITLE, PLAYTEXT, running),
    !IO
) :-
    Assets = GameState ^assets,
    make_title_text(GameState, Assets, TITLE, !IO),
    make_subtitle_text(GameState, Assets, SUBTITLE, !IO),
    make_play_text(GameState, Assets, PLAYTEXT, !IO),

    stars_init(
        150,
        GameState ^screen_w,
        GameState ^screen_h,
        Starfield,
        !IO
    ),
    audio.play_intro(!IO).

%-----------------------------------------------------------------------------%

    % Render and step the splash screen.
    % Most of the component libraries automatically upate the tweens etc.
    % but we are responsible for triggering each animated text instance to
    % render and update, as well as the other content.
    %
run_splash(!GS, !Splash, !IO) :-
    audio.update_intro_stream(!IO),
    window_should_close(Exit, !IO),
    (
        Exit = no,

        get_frame_time(FrameTime, !IO),
        clear_background(color(black), !IO),
        begin_drawing(!IO),

            Stars0 = stars(!.Splash),
            draw_stars(Stars0, !IO),
            draw_splash(FrameTime, !GS, !Splash, !IO),
            draw_fps(0, 0, !IO),

        end_drawing(!IO),
        step_stars(
            FrameTime, !.GS ^screen_w, !.GS ^screen_h,
            Stars0, Stars, !IO
        ),
        !Splash ^stars := Stars,

        dropped_files(Files, !IO),
        ( if list.length(Files) = 1
        then process_dropped_files(Files, !IO)
        else true),

        is_key_down(key_space, State, !IO),
        ( if State = yes then
            !Splash ^exit := start,
            stop_intro(!IO)
        else
            run_splash(!GS, !Splash, !IO)
        )
    ;
        Exit = yes, !Splash ^exit := quit
    ).

%----------------------------------------------------------------------------%

    % Prepare the main game title animated text.
    %
:- pred make_title_text(game_state::in, asset_library::in, anim_texts::out,
    io::di, io::uo) is det.

make_title_text(GameState, MLIB, Out, !IO) :-
    ScreenW   = GameState ^screen_wf,
    ScreenH   = GameState ^screen_hf,
    TitleText = "SUPER-MEGA",
    FontSize  = 180.0,
    centre_text(ScreenW, MLIB^main_font, FontSize, TitleText, TitleX, !IO),
    string_to_animtext(
        from_xy,
        TitleText,
        MLIB ^main_font,
        dval(tween_init(linear, 12.0, FontSize, 2.0)),
        d_color(
            dval(tween_init(bounce_in,     128.0, 255.0, 5.0)),
            dval(tween_init(linear,         64.0, 255.0, 7.0)),
            dval(tween_init(bounce_in_out, 255.0,   0.0, 5.0)),
            dval(tween_init(linear,          0.0, 255.0, 2.0))
        ),
        yes(s_color(with_a(color(skyblue), 0xa0u8))),
        vector2f(TitleX, 60.0),
        vector2f(ScreenW / 2.0, ScreenH),
        sine_out, bounce_out,
        vector2f(2.0, 2.0),     % TimeX, TimeY
        0.2, 0.2,               % Initial delay, delay increment
        Out,
        !IO
    ).

%----------------------------------------------------------------------------%

    % Make the subtitle animated text.
    %
:- pred make_subtitle_text(game_state::in, asset_library::in, anim_texts::out,
    io::di, io::uo) is det.

make_subtitle_text(GameState, MLIB, Out, !IO) :-
    ScreenW = GameState ^screen_wf,
    ScreenH = GameState ^screen_hf,
    SubtitleText = "INVADERS",
    FontSize  = 180.0,
    centre_text(ScreenW, MLIB^main_font, FontSize, SubtitleText, SubX, !IO),
    string_to_animtext(
        from_xy,
        SubtitleText,
        MLIB ^main_font,
        dval(tween_init(linear, 12.0, FontSize, 2.0)),
        d_color(
            dval(tween_init(bounce_in_out,  64.0, 255.0, 3.0)),
            dval(tween_init(bounce_in,     255.0,   0.0, 5.0)),
            dval(tween_init(bounce_out,    255.0,   0.0, 7.0)),
            dval(tween_init(linear,          0.0, 255.0, 2.0))
        ),
        yes(s_color(with_a(color(lime), 0xa0u8))),
        vector2f(SubX, 230.0),  % (X,Y) of first letter in final position
        vector2f(ScreenW / 2.0, ScreenH),
        bounce_in, bounce_out,
        vector2f(1.0, 1.0),     % TimeX, TimeY
        0.2, 0.2,               % Initial delay, delay increment
        Out,
        !IO
    ).

%----------------------------------------------------------------------------%

    % Prepare the game start message.
    %
:- pred make_play_text(game_state::in, asset_library::in, anim_texts::out,
    io::di, io::uo) is det.

make_play_text(GameState, MLIB, Out, !IO) :-
    SWidth   = GameState ^screen_wf,
    SHeight  = GameState ^screen_hf,
    PlayText = "Press SPACE to play!",
    FontSize  = 60.0,
    centre_text(SWidth, MLIB ^main_font, FontSize, PlayText, PlayX, !IO),
    string_to_animtext(
        from_xy,
        PlayText,
        MLIB ^main_font,
        dval(tween_init(linear, 12.0, FontSize, 2.0)),
        d_color(
            dval(tween_init_ex(linear,  0.0, 255.0, 1.5, reverse, 1.0)),
            dval(tween_init_ex(linear,  0.0, 255.0, 1.5, reverse, 2.0)),
            dval(tween_init_ex(linear,  0.0, 255.0, 1.5, reverse, 3.0)),
            dval(tween_init_ex(linear,  0.0, 255.0, 1.0, oneshot, 1.0))
        ),
        no,
        vector2f(PlayX, SHeight - 200.0),
        vector2f(0.0,   SHeight / 2.0),
        sine_in_out, sine_out,
        vector2f(1.0, 1.0),     % TimeX, TimeY
        0.0, 0.1,               % Initial delay, delay increment
        Out,
        !IO
    ).

%----------------------------------------------------------------------------%

    % Render the currently selected game page.
    % (Also updates relevant tween data).
    %
:- pred draw_splash(
    float::in, game_state::in, game_state::out, splash::in, splash::out,
    io::di, io::uo
) is det.

draw_splash(FrameTime, !GS, !Splash, !IO) :-
    Stepper = list.foldl2(draw_anim_texts(FrameTime)),
    Stepper(!.Splash ^title, [], Title, !IO),
    Stepper(!.Splash ^subtitle, [], SubTitle, !IO),
    Stepper(!.Splash ^play_text, [], PlayText, !IO),

    !Splash ^title     := Title,
    !Splash ^subtitle  := SubTitle,
    !Splash ^play_text := PlayText.


%----------------------------------------------------------------------------%
:- end_module splashpage.
%----------------------------------------------------------------------------%
