%-----------------------------------------------------------------------------%
%
% File: g1.m
% Main author: Sean Charles
% Date: Sat Aug 12 09:52:51 2023
%
%-----------------------------------------------------------------------------%
:- module g1.

:- interface.
:- import_module io.
:- import_module list.
:- import_module string.

:- pred main(io::di, io::uo) is det.

:- pred process_dropped_files(list(string)::in, io::di, io::uo) is det.


:- implementation.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module maybe.
:- import_module univ.

:- use_module math.
:- use_module uint8.

:- import_module animtext.
:- import_module assets.
:- import_module datatypes.
:- import_module easing.
:- import_module gamestate.
:- import_module level_ufo.
:- import_module osd.
:- import_module player.
:- import_module raylib.
:- import_module splashpage.
:- import_module starfield.

:- use_module audio.
:- use_module rnd.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = ["ffs"] then
        RequestedWidth = 1920,
        RequestedHeight = 1080,
        WindowPosX = 0,
        WindowPosY = 0,
        ConfigFlags = [vsync_hint, fullscreen_mode]
    else if Args = ["fs"] then
        RequestedWidth = 1920,
        RequestedHeight = 1080,
        WindowPosX = 0,
        WindowPosY = 0,
        ConfigFlags = [vsync_hint]
    else
        RequestedWidth = 1024,
        RequestedHeight = 768,
        WindowPosX = -1050,
        WindowPosY = 0,
        ConfigFlags = []
    ),

    % Graphics initialisation.
    set_config_flags(ConfigFlags, !IO),
    init_window(RequestedWidth, RequestedHeight, "g1", !IO),
    init_audio_device(!IO),
    set_master_volume(1.0, !IO),
    set_target_fps(60, !IO),
    set_window_position(WindowPosX, WindowPosY, !IO),
    get_screen_wh(SWidth, SHeight, !IO),

    % Initialise every for a complete game session.
    assets_load(Assets, !IO),
    audio.init(Assets),

    new_game(
        SWidth, SHeight,    % Screen dimensions, static.
        Assets,
        GameState,          % Globall updated game state.
        !IO
    ),

    % Enter the game loop forever until user quits.
    execute_game(
        GameState,
        !IO
    ),

    % Shutdown and tidy up our environment.
    assets_unload(Assets, !IO),
    close_audio_device(!IO),
    close_window(!IO).

%-----------------------------------------------------------------------------%

    % The complete game control loop.
    %
:- pred execute_game(game_state::in, io::di, io::uo) is det.

execute_game(GameState, !IO) :-
    % Seed the Raylib RNG for this session.
    rnd.seed(42u, !IO),

    % On-Screen Display preparation.
    osd_init(GameState, OSD0, !IO),

    some [ !Splash, !GS, !OSD, !Stars, !Player ]
    (
        !:GS = GameState,
        !:OSD = OSD0,

        player_init(!:Player, !.GS, !IO),
        splash_page_init(!.GS, !:Splash, !IO),

%            !:Stars = !.Splash ^stars,
%            level_ufo(!.GS, _, !.Stars, _, !.OSD, _, !.Player, _, !IO)

        run_splash(!GS, !Splash, !IO),

        ( if !.Splash ^exit = start
        then
            !:Stars = !.Splash ^stars,
            level_ufo(!.GS, _, !.Stars, _, !.OSD, _, !.Player, _, !IO)
        else
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Create a new game state.
    % This state object is passed to every game level. On exit from that level
    % the state will reflect changes in scores etc.
    %
    % As a note: I decided to pass the Player and OSD state separately as they
    % too are subject to state changes and I felt it was more efficient to
    % update them separately. This is probably both an ill-conceived notion of
    % how Mercury operates under the hood as none of the game structures uses
    % in-place updates, and also the sin of premature optimization.
    %
:- pred new_game(

    int::in, int::in,
    asset_library::in,
    game_state::out,
    io::di, io::uo

) is det.

new_game(
    SWidth, SHeight, ALIB,
    game_state(
            % Current screen width in pixels.
        SWidth, float(SWidth),
            % Current screen height in pixels.
        SHeight, float(SHeight),
            % Default font spacing.
        1.0,
            % Asset library.
        ALIB,
            % Starting game level.
        0,
            % Current score.
        0,
            % Resident hi-score this session.
        0,
            % Music paused?
        no,
            % Level paused?
        no,
            % Show bounding boxes.
        no
    ),
    !IO
).

%----------------------------------------------------------------------------%

    % Process any dropped files.
    %
process_dropped_files(Files, !IO) :-
    list.det_index0(Files, 0, F),
    io.print_line(F, !IO),
    load_sound(F, RSound, !IO),
    dump_sound(RSound, !IO),
    is_sound_ready(RSound, OK, !IO),
    (
        OK = yes,
        play_sound(RSound, !IO)
    ;
        OK = no,
        io.format("%s says not ready\n", [s(F)], !IO)
    ).

%----------------------------------------------------------------------------%
:- end_module g1.
%----------------------------------------------------------------------------%
