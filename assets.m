%-----------------------------------------------------------------------------%
%
% File: assets.m
% Main author: Sean Charles
% Date: Mon Aug 28 11:04:31 2023
%
% Asset management.
%
%-----------------------------------------------------------------------------%
:- module assets.

:- interface.
:- import_module char.
:- import_module io.

:- import_module datatypes.
:- import_module raylib.

:- pred assets_load(asset_library::out, io::di, io::uo) is det.
:- pred assets_unload(asset_library::in, io::di, io::uo) is det.

    % The asset library.
    % This contains all required fonts, images, textures and sounds
    % in well known slot names.
    %
:- type asset_library
    --->    assets(
                main_font       :: rfont,
                intro_mp3       :: rmusic,
                main_mp3        :: rmusic,
                sheet1          :: rtexture,
                    % Spot sound effects.
                wave_loading    :: rsound,
                wave_loaded     :: rsound,
                laser1          :: rsound,
                laser_blanked   :: rsound,
                score_up        :: rsound,
                hit1            :: rsound,
                hit1a           :: rsound,
                explode1        :: rsound,
                thrusters       :: rsound,
                powerup1        :: rsound
            ).


:- type s1tile
        % player ship 1 in different colours.
    --->    ship1blue
    ;       ship1green
    ;       ship1orange
    ;       ship1red

        % orange thruster flames.
    ;       fire13
    ;       fire16
    ;       fire17

        % Digits 0-9.
    ;       dig0 ; dig1 ; dig2 ; dig3 ; dig4 ; dig5
    ;       dig6 ; dig7 ; dig8 ; dig9 ; digx

        % Round UFO objects
    ;       ufo_red
    ;       ufo_green
    ;       ufo_blue
    ;       ufo_yellow

        % Player missile shots.
    ;       laser_green
        % Power up coloured pills.
    ;   pill_blue
    ;   pill_green
    ;   pill_yellow
    ;   pill_red
        % Extra / bonus laser weapons
    ;   wing_gun
        % Falling things that hur.
    ;   meteor_brown_big1
    .

    %--------------------------------------------------%
    % Public API

:- func slice(s1tile::in) = (rectangle::out) is det.
:- func digit_slice(char::in) = (rectangle::out) is det.

:- implementation.

:- import_module float.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Load all the sounds, images etc.
    % The asset code assumes the presence of a folder in the current working
    % directory called 'assets':
    %
assets_load(
    assets(
        MainFont, IntroMP3, MainMP3, Sheet1,
        WaveLoading, WaveLoaded,
        Laser1, LaserBlanked, ScoreUp,
        Hit1, Hit1a, Explode1, Thrusters,
        PowerUp1
    ), !IO
) :-
    font("PressStart2P.ttf", MainFont, !IO),
%    font("kenvector_future.ttf", MainFont, !IO),    
    music("mini1111.xm", IntroMP3, !IO),
    music("8bit.mp3", MainMP3, !IO),
    texture("sheet.png", Sheet1, !IO),
        %-
        % Spot Sound Effects
        %-
    sound("wave_loading.wav", WaveLoading, !IO),
    sound("wave_loaded.wav", WaveLoaded, !IO),
    sound("laser1_fire.wav", Laser1, !IO),
    sound("retro_jump_bounce_12.wav", LaserBlanked, !IO),
    sound("retro_computer_code_signal_01.wav", ScoreUp, !IO),
    sound("retro_impact_hit_21.wav", Hit1, !IO),
%    sound("retro_impact_hit_general_05.wav", Hit1a, !IO),
    sound("retro_misc_bass_sound_05.wav", Hit1a, !IO),
    sound("retro_explosion_bass_01.wav", Explode1, !IO),
    sound("515122__matrixxx__rocket-thrust-02.wav", Thrusters, !IO),
    sound("retro_powerup_collect_16.wav", PowerUp1, !IO).

%-----------------------------------------------------------------------------%

    % Unload all the assets.
    %
assets_unload(
    assets(
        MainFont,
        IntroMP3, MainMP3,
        Sheet1,
        WaveLoading, WaveLoaded,
        Laser1, LaserBlanked, ScoreUp,
        Hit1, Hit1a, Explode1, Thrusters,
        PowerUp1
    ), !IO
) :-
    unload_font(MainFont, !IO),
    stop_music_stream(IntroMP3, !IO),
    unload_music_stream(IntroMP3, !IO),
    stop_music_stream(MainMP3, !IO),
    unload_music_stream(MainMP3, !IO),
    unload_texture(Sheet1, !IO),
    unload_sound(WaveLoading, !IO),
    unload_sound(WaveLoaded, !IO),
    unload_sound(Laser1, !IO),
    unload_sound(LaserBlanked, !IO),
    unload_sound(ScoreUp, !IO),
    unload_sound(Hit1, !IO),
    unload_sound(Hit1a, !IO),
    unload_sound(Explode1, !IO),
    unload_sound(Thrusters, !IO),
    unload_sound(PowerUp1, !IO).

%-----------------------------------------------------------------------------%

    % Load a music asset.
    %
:- pred music(string::in, rmusic::out, io::di, io::uo) is det.

music(F, M, !IO) :-
    load_music_stream(string.format("assets/music/%s", [ s(F) ]), M, !IO).

    % Load a sound asset.
    %
:- pred sound(string::in, rsound::out, io::di, io::uo) is det.

sound(F, S, !IO) :-
    load_sound(string.format("assets/sound/%s", [ s(F) ]), S, !IO).

    % Load a font asset.
    %
:- pred font(string::in, rfont::out, io::di, io::uo) is det.

font(F, M, !IO) :-
    load_font(string.format("assets/font/%s", [ s(F) ]), M, !IO).

    % Load a texture asset.
    %
:- pred texture(string::in, rtexture::out, io::di, io::uo) is det.

texture(F, M, !IO) :-
    load_texture(string.format("assets/texture/%s", [ s(F) ]), M, !IO).

%----------------------------------------------------------------------------%

    % Text slices to allow named access to sprite sheets.
    %
:- pragma inline(slice/1).

slice(ship1blue)         = rectangle(211.0, 941.0,  99.0, 75.0).
slice(ship1green)        = rectangle(237.0, 377.0,  99.0, 75.0).
slice(ship1orange)       = rectangle(247.0, 84.0,   99.0, 75.0).
slice(ship1red)          = rectangle(224.0, 832.0,  99.0, 75.0).
slice(fire13)            = rectangle(835.0, 361.0,  14.0, 34.0).
slice(fire16)            = rectangle(828.0, 268.0,  14.0, 31.0).
slice(fire17)            = rectangle(828.0, 237.0,  14.0, 31.0).
slice(dig0)              = rectangle(367.0, 644.0,  19.0, 19.0).
slice(dig1)              = rectangle(205.0, 688.0,  19.0, 19.0).
slice(dig2)              = rectangle(406.0, 290.0,  19.0, 19.0).
slice(dig3)              = rectangle(580.0, 707.0,  19.0, 19.0).
slice(dig4)              = rectangle(386.0, 644.0,  19.0, 19.0).
slice(dig5)              = rectangle(628.0, 646.0,  19.0, 19.0).
slice(dig6)              = rectangle(671.0, 1002.0, 19.0, 19.0).
slice(dig7)              = rectangle(690.0, 1004.0, 19.0, 19.0).
slice(dig8)              = rectangle(709.0, 1004.0, 19.0, 19.0).
slice(dig9)              = rectangle(491.0, 215.0,  19.0, 19.0).
slice(digx)              = rectangle(382.0, 814.0,  17.0, 17.0).
slice(ufo_blue)          = rectangle(444.0, 91.0, 91.0, 91.0).
slice(ufo_green)         = rectangle(434.0, 234.0, 91.0, 91.0).
slice(ufo_red)           = rectangle(444.0, 0.0, 91.0, 91.0).
slice(ufo_yellow)        = rectangle(505.0, 898.0, 91.0, 91.0).
slice(laser_green)       = rectangle(856.0, 94.0, 9.0, 37.0).
slice(pill_blue)         = rectangle(674.0, 262.0, 22.0, 21.0).
slice(pill_red)          = rectangle(222.0, 108.0, 22.0, 21.0).
slice(pill_green)        = rectangle(573.0, 989.0, 22.0, 21.0).
slice(pill_yellow)       = rectangle(222.0, 129.0, 22.0, 21.0).
slice(meteor_brown_big1) = rectangle(224.0, 664.0, 101.0, 84.0).
slice(wing_gun)          = rectangle(829.0, 611.0, 14.0, 36.0).

%----------------------------------------------------------------------------%

    % Map a digit to its corresponding tile.
    % This ensures that any out of range digit is returned as 'digx'.
    %
digit_slice(N0) = Out :-
    ( if digit_char(N0, Slice) then
        Out = Slice
    else
        Out = slice(digx)
    ).


:- pred digit_char(char::in, rectangle::out) is semidet.

digit_char('0', slice(dig0)).
digit_char('1', slice(dig1)).
digit_char('2', slice(dig2)).
digit_char('3', slice(dig3)).
digit_char('4', slice(dig4)).
digit_char('5', slice(dig5)).
digit_char('6', slice(dig6)).
digit_char('7', slice(dig7)).
digit_char('8', slice(dig8)).
digit_char('9', slice(dig9)).

%----------------------------------------------------------------------------%
:- end_module assets.
%----------------------------------------------------------------------------%
