%-----------------------------------------------------------------------------%
%
% File: gamestate.m
% Main author: Sean Charles
% Date: Wed Aug 30 20:23:22 2023
%
%-----------------------------------------------------------------------------%
:- module gamestate.

:- interface.
:- import_module bool.
:- import_module io.

:- import_module assets.

:- pred service_global_keys(
    game_state::in, game_state::out, io::di, io::uo
) is det.

    % Game state.
    %
:- type game_state ---> game_state(
        % Last reported screen width in pixels.
    screen_w        :: int,
    screen_wf       :: float,
        % Last reported screen height in pixels.
    screen_h        :: int,
    screen_hf       :: float,
        % Global font spacing value, typically 1.0
    font_spacing    :: float,
        % Global asset library
    assets          :: asset_library,
        % Current game level, advances difficulty.
    level           :: int,
        % Current player score.
    score           :: int,
        % Current high-score.
    hiscore         :: int,
        % Music playing status
    music_paused    :: bool,
        % Level paused or not?
    level_paused    :: bool,
        %-
        % DIAGNOSTICS
        %-
        % Show bounding boxes.
    show_bounds     :: bool
).

:- implementation.
:- import_module raylib.
:- use_module audio.

%----------------------------------------------------------------------------%

    % Service global keypresses.
    %
service_global_keys(!GS, !IO) :-
    %-
    % 'M' -> Toggle the playback of the main music track.
    %-
    is_key_released(key_m, PauseMusic, !IO),
    ( if PauseMusic = yes then
        !GS ^music_paused := not(!.GS ^music_paused),
        ( if !.GS ^music_paused = yes then
            audio.pause_current_stream(!IO)
        else
            audio.resume_current_stream(!IO)
        )
    else
        true
    ),
    %-
    % 'P' -> Enter a pause loop, pressing 'P' again to return.
    %-
    is_key_released(key_p, PauseGame, !IO),
    ( if PauseGame = yes then
        !GS ^level_paused := not(!.GS ^level_paused)
    else
        true
    ),
    %-
    % Audio management.
    %-
    is_key_released(key_period, TrackNext, !IO),
    is_key_released(key_comma, TrackPrev, !IO),
    ( if TrackPrev = yes then
        audio.skip_prev(!IO)
    else if TrackNext = yes then
        audio.skip_next(!IO)
    else
        true
    ).

%----------------------------------------------------------------------------%
:- end_module gamestate.
%----------------------------------------------------------------------------%

