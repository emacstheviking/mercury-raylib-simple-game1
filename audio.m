%-----------------------------------------------------------------------------%
%
% File: audio.m
% Main author: Sean Charles
% Date: Sun Sep 10 07:02:46 2023
%
% Audio control module. Both background music and spot effects are managed.
%
%-----------------------------------------------------------------------------%
:- module audio.

:- interface.
:- import_module io.

:- import_module assets.
:- import_module raylib.

:- pred init(asset_library::in) is det.
:- func current_track_name = (string::out) is det.

:- pred play(rmusic::in, float::in, io::di, io::uo) is det.
:- pred play_current_track(io::di, io::uo) is det.
:- pred pause_current_stream(io::di, io::uo) is det.
:- pred resume_current_stream(io::di, io::uo) is det.


:- pred play_intro(io::di, io::uo) is det.
:- pred stop_intro(io::di, io::uo) is det.

:- pred sfx(rsound::in, io::di, io::uo) is det.
:- pred sfx_roll(rsound::in, io::di, io::uo) is det.
:- pred skip_next(io::di, io::uo) is det.
:- pred skip_prev(io::di, io::uo) is det.

:- pred update_intro_stream(io::di, io::uo) is det.
:- pred update_current_stream(io::di, io::uo) is det.
:- pred volume(rmusic::in, float::in, io::di, io::uo) is det.

:- type audio_state
    --->    audio_state(
                track_index :: int,
                track_name  :: string,
                intro_track :: rmusic,
                main_mp3    :: rmusic,
                volume      :: float
            ).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.


% Experimenting with a one-time setup of the music catalog using a mutable
% variable to hold the audio manager state.  NOTE: This will need a format
% init() after all as the assets have yet to be loaded so in fact ONLY the
% track list made sense for this!
% Setup the module variables for:
%  Track management / selection
%  Constant track list based on MP3 metadata.
%
:- mutable(audio_state, maybe(audio_state), no, ground, []).
:- mutable(track_list, tracks, track_list, ground, [constant]).

%---------------------------------------------------------------------------%

:- pragma promise_pure(init/1).

init(Assets) :-
    DefaultTrack = 0,
    get_track_list(Tracks),
    (_ - track_entry(_, Name)) = list.det_head(Tracks),
    impure set_audio_state(yes(audio_state(
        DefaultTrack,
        Name,
        Assets ^intro_mp3,
        Assets ^main_mp3,
        0.3
    ))).

%---------------------------------------------------------------------------%

    % Start playing an MP3 track at the given time offset.
    %
play(Music, Position, !IO) :-
    trace_info("play() REQUESTED", [], !IO),
    seek_music_stream(Music, Position, !IO),
    play_music_stream(Music, !IO),
    trace_info("play() RETURNED", [], !IO).


    % Set the volume for the MP3 track.
    %
:- pragma inline(volume/4).

volume(Music, Volume, !IO) :-
    set_music_volume(Music, Volume, !IO).

%---------------------------------------------------------------------------%

    % Play the designated title screen music.
    %
:- pragma promise_pure(play_intro/2).
:- pragma promise_pure(stop_intro/2).
:- pragma promise_pure(update_intro_stream/2).

play_intro(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS),
        volume(AS ^intro_track, AS ^volume, !IO),
        play(AS ^intro_track, 0.0, !IO)
    ).

stop_intro(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS),
        stop_music_stream(AS ^intro_track, !IO)
    ).

update_intro_stream(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS),
        update_music_stream(AS ^intro_track, !IO)
    ).

%---------------------------------------------------------------------------%

    % Play the currently selected music track.
    %
:- pragma promise_pure(play_current_track/2).
:- pragma promise_pure(update_current_stream/2).
:- pragma promise_pure(pause_current_stream/2).
:- pragma promise_pure(resume_current_stream/2).

play_current_track(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(audio_state(Index, _, _, MP3, Volume)),
        get_track_list(Tracks),
        ( if list.index0(Tracks, Index, (_ - track_entry(Position, Name)))
        then
            trace_info("play_current_track: %s => %f, vol: %f",
                [ s(Name), f(Position), f(Volume) ], !IO),
            play(MP3, Position, !IO),
            volume(MP3, Volume, !IO),
            trace_info("play_current_track: DONE", [], !IO)
        else
            trace_error("RANGE! %i of %i\n",
                [ i(Index), i(list.length(Tracks)) ], !IO)
        )
    ).

update_current_stream(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS),
        update_music_stream(AS ^main_mp3, !IO)
    ).

pause_current_stream(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS),
        pause_music_stream(AS ^main_mp3, !IO)
    ).

resume_current_stream(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS),
        resume_music_stream(AS ^main_mp3, !IO)
    ).

%---------------------------------------------------------------------------%

    % Skip to next / previous audio track.
    % We do NOT change the playing status, just the music position as the
    % user MAY have paused playing.
    %
:- pragma promise_pure(skip_next/2).
:- pragma promise_pure(skip_prev/2).

skip_next(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS0),
        adjust_track(1, AS0, AS),
        impure set_audio_state(yes(AS)),
        play_current_track(!IO)
    ).

skip_prev(!IO) :-
    semipure get_audio_state(ASyn),
    ( ASyn = no
    ; ASyn = yes(AS0),
        adjust_track(-1, AS0, AS),
        impure set_audio_state(yes(AS)),
        play_current_track(!IO)
    ).

%----------------------------------------------------------------------------%

    % Adjust the currently playing track forwards or backwards.
    %
:- pred adjust_track(int::in, audio_state::in, audio_state::out) is det.

adjust_track(Dir, !AS) :-
    get_track_list(Tracks),
    Max = list.length(Tracks):int,
    Index = (!.AS ^track_index + Dir) `mod` Max,
    ( if  list.index0(Tracks, Index, Entry)
    then  Entry = (_ - track_entry(_, TrackName))
    else  TrackName = ""
    ),
    !AS ^track_index := Index,
    !AS ^track_name  := TrackName.

%----------------------------------------------------------------------------%

    % Returns the currently selected track name.
    %
:- pragma promise_pure(current_track_name/0).

current_track_name = Name :-
    semipure get_audio_state(ASyn),
    ( ASyn = no,
        Name = "Nothing playing"
    ; ASyn = yes(AS),
        Name = AS ^track_name
    ).

%----------------------------------------------------------------------------%

    % Spot sound effect.
    % This WILL cause the sound effect to start playing from the start of its
    % buffer, this CAN cause unintentional jittering. use the roll variation
    % to avoid this.
    %
:- pragma inline(sfx/3).

sfx(S, !IO) :-
    play_sound(S, !IO).


    % Spot sound effect, roll-on.
    % As before but we ONLY start the sound if it's not already playing to
    % avoid a stuttering sound.
    %
:- pragma inline(sfx_roll/3).

sfx_roll(S, !IO) :-
    is_sound_playing(S, YN, !IO),
    ( if YN = no then play_sound(S, !IO) else true).

%----------------------------------------------------------------------------%

    % Track offsets into the main MP3 file.
    %
:- type track_entry
    --->    track_entry(
                start   :: float,
                name    :: string
            ).

:- type tracks == assoc_list(float, track_entry).
:- func track_list = (tracks::out) is det.

    % Curated! The commented out ones don't have enough BPM and
    % the right mode; the remainder are fast and fit the mood!
    %
track_list = [
    mk_entry(7,  56.0, "Itty Bitty 8 Bit"),
    mk_entry(0,   0.0, "Eight Bit Sahara"),
        %mk_entry(3,  51.0, "Joy Riding on the Superhighway"),
    mk_entry(11, 11.0, "Reformat"),
        %mk_entry(14, 50.0, "Theme for Harold Var.1"),
    mk_entry(16, 41.0, "Rhinoceros"),
        %mk_entry(20,  5.0, "Wagon Wheel - Electronic"),
    mk_entry(25,  9.0, "Hero's Day Off"),
    mk_entry(28, 32.0, "Wwwwub"),
        %mk_entry(32,  0.0, "8-bit Dungeon Level"),
        %mk_entry(35, 38.0, "Video Dungeon Crawl"),
        %mk_entry(39, 11.0, "Eight Bit Stabs"),
        %mk_entry(40, 57.0, "Operation Catch the Bad Guy 8 bit"),
        %mk_entry(44, 33.0, "Purple Town"),
    mk_entry(47, 49.0, "Mighty Eight Bit Ranger"),
    mk_entry(50, 51.0, "Eight Bit Commando"),
    mk_entry(54,  0.0, "Chiptune Does Dubstep")
        %mk_entry(58, 10.0, "Aristocratic Festivities 8-Bit")
].


:- func mk_entry(int::in, float::in, string::in)
    = (pair(float, track_entry)::out) is det.

mk_entry(Mins, Secs, Name) = Out :-
    Time = minsec(Mins, Secs),
    Out = (Time - track_entry(Time, Name)).


:- func minsec(int::in, float::in) = (float::out) is det.

minsec(M, S) = float(M) * 60.0 + S.

%----------------------------------------------------------------------------%
:- end_module audio.
%----------------------------------------------------------------------------%

