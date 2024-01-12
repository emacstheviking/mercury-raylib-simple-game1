%----------------------------------------------------------------------------%
%
% File: level_ufo.m
% Main author: Sean Charles
% Date: Tue Sep  5 15:29:48 2023
%
% All good space pilots start here.
%
%----------------------------------------------------------------------------%
:- module level_ufo.

% TODO: builtin.unsafe_promise_unique(X,Y) => X = Y but can destructive
% TODO: experiment on the tweening engine... this MIGHT be the performance
% TODO: increase for almost no work!


:- interface.
:- import_module io.

:- import_module gamestate.
:- import_module player.

:- use_module osd.
:- use_module starfield.

    % Exported API to run the level.
    %
:- pred level_ufo(
    game_state::in,         game_state::out,
    starfield.stars::in,    starfield.stars::out,
    osd.osd::in,            osd.osd::out,
    player::in,             player::out,
    io::di,                 io::uo
) is det.


:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module string.

:- import_module animsprite.
:- import_module assets.
:- import_module collision.
:- import_module datatypes.
:- import_module easing.
:- import_module falling_object.
:- import_module layout.
:- import_module osd.
:- import_module raylib.
:- import_module ufo.

:- use_module audio.


    % Level state information.
    %
:- type level_state
    --->    level_state(
                    % Current operation state.
                state::wave_state,
                    % Attacking aliens.
                ufos::ufo.ufos,
                    % Falling Objects
                fo_items::fobjects,
                    % Next incremental FO id.
                fo_nextid::int,
                    % Time delay between falling object creation.
                fo_timer::tween,
                    % Debug mode for render issues etc.
                debugging::bool
            ).

    % The states this level can operate in.
    %
:- type wave_state
    --->    loading
    ;       loaded.

%----------------------------------------------------------------------------%

    % UFO Level.
    % This level presents a series of spinning UFOs that must be defeated in
    % order to clear the level. Some die when hit enough times, that value is
    % increased as the level gets higher. Others have different behaviours as
    % well.
    %
level_ufo(!GS, !Stars, !OSD, !Player, !IO) :-
    ScreenW = !.GS ^screen_wf,
    Assets  = !.GS ^assets,

    %-
    % Initialise the attack wave layout.
    %-
    rectangle(_, _, UfoW, UfoH) = slice(ufo_red),

    Grid = grid_layout(
        ScreenW,                        % width of container
        UfoH * 1.2,                     % Initial Y first row
        UfoW, UfoH,                     % Dimensions of sprite
        UfoW / 4.0, UfoH / 4.0,         % Spacing between sprites
        3, 6 + !.GS ^level              % Rows and Cols required
    ),
        %-
        % Create all the UFOs in initial tween position.
        %-
    MakeUfo = (pred(
        vector2f(Px, Py)::in, !.Acc::in, !:Acc::out, !.IO::di, !:IO::uo
    ) is det :-
        ufo.random_trait(Trait, !IO),
        list.cons(
            make_ufo(Trait, Px, Py, list.length(!.Acc), !.GS),
            !Acc
        )
    ),
    list.foldl2(MakeUfo, Grid, [], Ufos, !IO),

    SpriteMgr0 = animsprite_init(Assets ^sheet1),
    init_state(State, Ufos, !IO),

    audio.sfx(Assets ^wave_loading, !IO),
    audio.play_current_track(!IO),

    level_ufo_exec(
        State, _,
        !GS,
        !Stars,
        !OSD,
        !Player,
        SpriteMgr0, SpriteMgr,
        !IO
    ),
    animsprite_free(SpriteMgr).

%----------------------------------------------------------------------------%

    % Initialise the level state.
    %
:- pred init_state(level_state::out, ufo.ufos::in, io::di, io::uo) is det.

init_state(
    level_state(
            % Wave state.
        loading,
            % UFO list.
        Ufos,
            % Falling objects.
        [],
            % Next falling object id.
        0,
            % Timer for falling objects gaps.
        tween_init(linear, 0.0, 1.0, 3.0),
            % Debugging mode?
        no
    ),
    Ufos, !IO
).

%----------------------------------------------------------------------------%

    % UFO Level game loop.
    %
:- pred level_ufo_exec(
    level_state::in,        level_state::out,
    game_state::in,         game_state::out,
    starfield.stars::in,    starfield.stars::out,
    osd.osd::in,            osd.osd::out,
    player::in,             player::out,
    sprite_mgr::in,         sprite_mgr::out,
    io::di,                 io::uo
) is det.

level_ufo_exec(!S, !GS, !Stars, !OSD, !Player, !SPR, !IO) :-
    Assets  = !.GS ^assets,
    Sheet1  = Assets ^sheet1,
    audio.update_current_stream(!IO),
    window_should_close(Exit, !IO),
    (
        Exit = no,
        get_frame_time(FrameTime, !IO),
        clear_background(color(black), !IO),

        UFOS0 = !.S ^ufos,
        DEBUG = !.S ^debugging,
        begin_drawing(!IO),

            starfield.draw_stars(!.Stars, !IO),
            draw_player(!.Player, DEBUG, !IO),
            draw_ufos(UFOS0, Sheet1, DEBUG, !IO),
            draw_falling_objects(!.S ^fo_items, !.GS ^assets^sheet1, DEBUG, !IO),
            osd.draw_lives(!.OSD, !.Player ^lives,!IO),
            osd.draw_health(!.OSD, !.Player ^health, !IO),
            osd.draw_score(!.OSD, !.GS ^score,!IO),
            osd.draw_current_track(!.OSD, !IO),
            draw_fps(0, 0, !IO),

        end_drawing(!IO),

        step_update(FrameTime, !S, !GS, !Stars, !Player, !IO),
        osd_step(FrameTime, !OSD),

        collision_detection(!S, !OSD, !Player, !GS, !SPR, !IO),

        service_local_keys(FrameTime, !S, !.GS, !Player, !IO),
        service_global_keys(!GS, !IO),

        level_ufo_exec(!S, !GS, !Stars, !OSD, !Player, !SPR, !IO)
    ;
        Exit = yes
    ).

%----------------------------------------------------------------------------%

    % Step update all of the actors on the stage.
    %
:- pred step_update(
    float::in,
    level_state::in,        level_state::out,
    game_state::in,         game_state::out,
    starfield.stars::in,    starfield.stars::out,
    player::in,             player::out,
    io::di,                 io::uo
) is det.

step_update(FrameTime, !S, !GS, !Stars, !Player, !IO) :-
    ScreenW = !.GS ^screen_w,
    ScreenH = !.GS ^screen_h,
    UFOS0   = !.S ^ufos,

    starfield.step_stars(FrameTime, ScreenW, ScreenH, !Stars, !IO),
    (
        !.S ^state = loading,

        ufo.step_ufos(FrameTime, ScreenW, ScreenH, UFOS0, UFOS, Count, !IO),
        !S ^ufos := UFOS,

        (   list.length(UFOS, Count)
        ->
            !S ^state := loaded,
            !S ^ufos  := ufos_to_loaded(UFOS, 100.0, 5.0),

            audio.sfx(!.GS ^assets^wave_loaded, !IO)
        ;
            true
        )
    ;
        !.S ^state = loaded,

        ufo.step_ufos(FrameTime, ScreenW, ScreenH, UFOS0, UFOS, _, !IO),
        !S ^ufos  := UFOS,

        step_objects(FrameTime, !.S ^fo_items, NewObjects, !IO),
        !S ^fo_items := NewObjects,

        step_falling_objects(FrameTime, !S, !GS, !IO),
        step_player(FrameTime, !Player, !IO)
    ).

%----------------------------------------------------------------------------%

    % Step move all the falling objects.
    %
:- pred step_falling_objects(float::in, level_state::in, level_state::out,
    game_state::in, game_state::out, io::di, io::uo) is det.

step_falling_objects(FrameTime, !S, !GS, !IO) :-
    % If falling object timer has expired, launch something
    % and reset for another delay period.
    tween_step(FrameTime, !.S ^fo_timer, FoTimer),

    ( if tween_done(FoTimer) then

        new_falling_object(
            float(!.GS ^screen_w),
            float(!.GS ^screen_h),
            Object,
            !S,
            !IO),

        !S ^fo_items := list.cons(Object, !.S ^fo_items),
        !S ^fo_timer := tween_init(linear, 0.0, 1.0, 5.0)
    else
        !S ^fo_timer := FoTimer
    ).

%----------------------------------------------------------------------------%

    % Generate a new randomised falling object.
    %
:- pred new_falling_object(
    float::in, float::in, fobject::out,
    level_state::in, level_state::out,
    io::di, io::uo
) is det.

new_falling_object(ScreenW, ScreenH,
    fobject(
        Type,
        fp_point(sval(RndX), dval(tween_init(linear, -H, ScreenH, 20.0))),
        Tween, FID, no, Value, Slice
    ),
    !S,
    !IO
) :-
    FID = !.S ^fo_nextid,
    !S ^fo_nextid := FID + 1,

    get_random_value(0, 9, RndT, !IO),

    ( if RndT < 3 then
        Type  = health_boost1,
        Value = 50,
        Slice = slice(pill_green),
        Tween = tween_init_ex(linear, 0.0, 360.0, 1.0, repeat, 0.0)
    else
        Type  = meteor1,
        Value = -50,
        Slice = slice(meteor_brown_big1),
        Tween = tween_init_ex(linear, 0.0, 360.0, 4.0, repeat, 0.0)
    ),
    % Get its dimensions, ensure a visible X.
    rectangle(_, _, W, H) = Slice,
    get_random_fvaluef(W, ScreenW - W, RndX, !IO).

%----------------------------------------------------------------------------%

    % All collision detection for the complete level is processed here.
    %
:- pred collision_detection(
    level_state::in,    level_state::out,
    osd::in,            osd::out,
    player::in,         player::out,
    game_state::in,     game_state::out,
    sprite_mgr::in,     sprite_mgr::out,
    io::di,             io::uo
) is det.

collision_detection(!S, !OSD, !Player, !GS, !SPR, !IO) :-
    % Player shots vs. the world.
    some [ !Shots, !UFOS, !FObjects ] (
        !:Shots    = !.Player ^laser_shots,
        !:FObjects = !.S ^fo_items,
        !:UFOS     = !.S ^ufos,

        %-
        % Falling Objects.
        %-
        collisions_over(pt_rect, !Shots, !FObjects),
        remove_hit_fobjects(!FObjects, !OSD, !Player, !GS, !SPR, !IO),
        !Player ^laser_shots := list.filter(unspent, !.Shots),
        !S ^fo_items := !.FObjects,

        %-
        % UFOS.
        %-
        collisions_over(pt_circle, !Shots, !UFOS),
        remove_hit_ufos(!UFOS, !GS, !IO),
        !Player ^laser_shots := list.filter(unspent, !.Shots),
        !S ^ufos := !.UFOS
    ),

    % Falling objects hitting the player.
    some [ !FOs ] (
        !:FOs = !.S ^fo_items,
        !Player ^hit := no,
        collisions_over(rect_rect, [ !.Player ], Plist, !FOs),
        ( if NewPlayer = list.head(Plist):player then
            ( if NewPlayer ^hit = yes then
                process_falling_objects(!.FOs, [], _, !S, !OSD,
                    !Player, !GS, !IO
                )
            else true)
        else true)
    ).


:- pred unspent(missile::in) is semidet.
:- pragma inline(unspent/1).

unspent(S) :- hit(S) = no.

%----------------------------------------------------------------------------%

    % Falling objects have hit the player.
    % This might be good, it might be bad, it might be different if there
    % are shields currently in effect. All that stuff.
    %
:- pred process_falling_objects(
    fobjects::in,
    fobjects::in,       fobjects::out,
    level_state::in,    level_state::out,
    osd::in,            osd::out,
    player::in,         player::out,
    game_state::in,     game_state::out,
    io::di,             io::uo
) is det.

process_falling_objects([], !Acc, !S, !OSD, !Player, !GS, !IO) :-
    !S ^fo_items := !.Acc.

process_falling_objects([ FI | FIs ], !Acc, !S, !OSD, !Player, !GS, !IO) :-
    ( if FI ^hit = yes then
        (
            FI ^form = meteor1,

            player_health_adjust(!Player, -5.0),
            move_down(!.GS ^screen_hf, -20.0, !Player),
            osd_health_update(!.Player ^health, !OSD),

            % ANIMATION: EXPLODE THE METEOR

            audio.sfx_roll(!.GS ^assets^explode1, !IO),
            !GS ^score := !.GS ^score + 1
        ;
            FI ^form = health_boost1,

            % ANIMATION: EXPLODE THE POWER UP

            player_health_adjust(!Player, 5.0),
            osd_health_update(!.Player ^health, !OSD),

            !GS ^score := !.GS ^score + 100,
            audio.sfx_roll(!.GS ^assets^powerup1, !IO)
        )
    else
        list.cons(FI, !Acc)
    ),
    process_falling_objects(FIs, !Acc, !S, !OSD, !Player, !GS, !IO).

%----------------------------------------------------------------------------%

    % Remove a UFO indicated as having been hit.
    %
:- pred remove_hit_ufos(ufos::in, ufos::out, game_state::in, game_state::out,
    io::di, io::uo) is det.

remove_hit_ufos(!UFOS, !GS, !IO) :-
    remove_hit_ufos_(!.UFOS, [], !:UFOS, !GS, !IO).

:- pred remove_hit_ufos_(ufos::in, ufos::in, ufos::out, game_state::in,
    game_state::out, io::di, io::uo) is det.

remove_hit_ufos_([], !UFOS, !GS, !IO).

remove_hit_ufos_([ U | Us ], !UFOS, !GS, !IO) :-
    ( if U ^hit = yes then
        ( if U ^hits > 0 then
            %-
            % Note the hit, reset the hit flag.
            %-
            list.cons(downcount_ufo(U), !UFOS),
            audio.sfx(!.GS ^assets^hit1a, !IO)
            %-
            % Dying ships passed through for animation.
            %-
        else if ( U ^state = exploding ; U ^state = exploded ) then
            list.cons(U, !UFOS)
        else
            %-
            % Final hit now, animate, update stats.
            %-
            !GS ^score := !.GS ^score + ufo_score(U ^trait),
            audio.sfx(!.GS ^assets^explode1, !IO),
            list.cons(exploded_ufo(U), !UFOS)
        )
    else
        list.cons(U, !UFOS)
    ),
    remove_hit_ufos_(Us, !UFOS, !GS, !IO).

%----------------------------------------------------------------------------%

    % Remove any falling objects struck by missiles.
    %
:- pred remove_hit_fobjects(
    fobjects::in,   fobjects::out,
    osd::in,        osd::out,
    player::in,     player::out,
    game_state::in, game_state::out,
    sprite_mgr::in, sprite_mgr::out,
    io::di,         io::uo
) is det.

remove_hit_fobjects(!FOS, !OSD, !P, !GS, !SPR, !IO) :-
    remove_hit_fobjects_(!.FOS, [], !:FOS, !OSD, !P, !GS, !SPR, !IO).


:- pred remove_hit_fobjects_(
    fobjects::in,
    fobjects::in,   fobjects::out,
    osd::in,        osd::out,
    player::in,     player::out,
    game_state::in, game_state::out,
    sprite_mgr::in, sprite_mgr::out,
    io::di, io::uo
) is det.

remove_hit_fobjects_([], !FOS, !OSD, !Player, !GS, !SPR, !IO).

remove_hit_fobjects_([ FO | FObjs ], !FOS, !OSD, !Player, !GS, !SPR, !IO) :-
    ( if FO ^hit = yes then
        (
            FO ^form = meteor1,

            player_health_adjust(!Player, 50.0),
            osd_health_update(!.Player ^health, !OSD),

        % These CREDIT the player score and health values.
            % ANIMATION: EXPLODE THE METEOR

            audio.sfx_roll(!.GS ^assets^explode1, !IO),
            !GS ^score := !.GS ^score + 100
        ;
            FO ^form = health_boost1,

            %%% Text "+NN" that goes up but slides too like a ufo

            player_health_adjust(!Player, 5.0),
            osd_health_update(!.Player ^health, !OSD),

        % These CREDIT the player score and health values.
            % ANIMATION: EXPLODE THE POWER UP

            audio.sfx_roll(!.GS ^assets^powerup1, !IO),
            !GS ^score := !.GS ^score + 100
        )
    else
        list.cons(FO, !FOS)
    ),
    remove_hit_fobjects_(FObjs, !FOS, !OSD, !Player, !GS, !SPR, !IO).

%----------------------------------------------------------------------------%

    % Move UFOS to loaded state.
    % Once the initial tweening into position has completed, we replace the
    % tweens for all UFO-s
    %
:- func ufos_to_loaded(ufo.ufos::in, float::in, float::in)
    = (ufo.ufos::out) is det.

ufos_to_loaded(In, Amount, Duration)
    = list.map(side_to_side(Amount, Duration), In).

%----------------------------------------------------------------------------%

    % Install 'DRIFTING' mode tween.
    % This mode makes the UFO remain motionless apart from drifting side to
    % side by the given amount.
    %
:- func side_to_side(float::in, float::in, ufo.ufo::in)
    = (ufo.ufo::out) is det.

side_to_side(Amount, Duration, Ufo) = Out :-
    Drift = tween_init_ex(
        linear, % sine_in_out higher levels
        0.0,
        Amount,
        Duration,
        reverse,
        0.0
    ),
    Out = ( Ufo ^drift := Drift ) ^state := drifting.

%----------------------------------------------------------------------------%

    % Render the current wave of UFO baddies.
    %
:- pred draw_ufos(ufo.ufos::in, rtexture::in, bool::in, io::di, io::uo) is det.

draw_ufos([], _, _, !IO).

draw_ufos([ UFO | UFOS ], Sheet, Debug, !IO) :-
    draw_ufo(UFO, Sheet, Debug, !IO),
    draw_ufos(UFOS, Sheet, Debug, !IO).

%----------------------------------------------------------------------------%

    % Service key presses affecting this level.
    %
:- pred service_local_keys(

    float::in,
    level_state::in, level_state::out,
    game_state::in,
    player::in,      player::out,
    io::di,          io::uo

) is det.

service_local_keys(FrameTime, !S, GS, !Player, !IO) :-
        %-
        % Player motion.
        %-
        is_key_down(key_a, L, !IO),
        is_key_down(key_d, R, !IO),
        is_key_down(key_w, W, !IO),
        is_key_down(key_s, S, !IO),
        is_key_down(key_enter, Fire, !IO),
        is_key_down(key_left_bracket, PrevWeapon, !IO),
        is_key_down(key_right_bracket, NextWeapon, !IO),

        % Debug control.
        is_key_down(key_kp_subtract, DebugOff, !IO),
        is_key_down(key_kp_add, DebugOn, !IO),
        ( if DebugOn = yes then
            !S ^debugging := yes
        else if DebugOff = yes then
            !S ^debugging := no
        else
            true
        ),
        %-
        % Left-Right motion.
        %-
        ( if L = yes then
            move_left(GS ^screen_wf, !Player)
         else if R = yes then
            move_right(GS ^screen_wf, !Player)
         else
            true
        ),
        %-
        % Thrusters on, move up / down?
        %-
        ( if W = yes then
            audio.sfx_roll(GS ^assets^thrusters, !IO),
            move_up(GS ^screen_hf, !Player)
        else
            %-
            % Sink back if NOT thrusting upwards.
            %-
            move_down(GS ^screen_hf, !Player),
            stop_sound(GS ^assets^thrusters, !IO)
        ),
        ( if S = yes then
            %-
            % Active return will double the rate.
            %-
            move_down(GS ^screen_hf, !Player)
        else
            true
        ),
        %-
        % Weapons selection.
        %-
        ( if PrevWeapon = yes then
            weapon_previous(!Player, !IO)
        else if NextWeapon = yes then
            weapon_next(!Player, !IO)
        else
            true
        ),
        %-
        % Player firing.
        %-
        ( if Fire = yes then
            fire_laser(FrameTime, GS ^screen_w, !Player, !IO)
         else true
        ).

% TODO: going to need generic animtext type tween now for ANY texture slice!!!

%----------------------------------------------------------------------------%
:- end_module level_ufo.
%----------------------------------------------------------------------------%
