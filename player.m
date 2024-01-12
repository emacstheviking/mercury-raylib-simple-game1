%-----------------------------------------------------------------------------%
%
% File: player.m
% Main author: Sean Charles
% Date: Sun Sep 10 06:33:30 2023
%
% The user controlled player ship.
%
%-----------------------------------------------------------------------------%
:- module player.

:- interface.
:- import_module bool.
:- import_module float.
:- import_module io.
:- import_module list.


:- import_module assets.
:- import_module collision.
:- import_module datatypes.
:- import_module easing.
:- import_module gamestate.


:- pred player_init(player::out, game_state::in, io::di, io::uo) is det.
:- pred player_expired(player::in) is semidet.
:- pred player_health_adjust(player::in, player::out, float::in) is det.

:- pred draw_player(player::in, bool::in, io::di, io::uo) is det.
:- pred step_player(float::in, player::in, player::out, io::di, io::uo) is det.

:- pred move_left(float::in, player::in, player::out) is det.
:- pred move_right(float::in, player::in, player::out) is det.
:- pred move_up(float::in, player::in, player::out) is det.
:- pred move_down(float::in, player::in, player::out) is det.
:- pred move_down(float::in, float::in, player::in, player::out) is det.

:- pred fire_laser(float::in, int::in, player::in, player::out, io::di, io::uo) is det.
:- pred weapon_previous(player::in, player::out, io::di, io::uo) is det.
:- pred weapon_next(player::in, player::out, io::di, io::uo) is det.

:- func player_bounds_rect(player::in) = (rectangle::out) is det.
:- func missile_bounds_rect(missile::in) = (rectangle::out) is det.


    % The player (user) entity state.
    %
:- type player
    --->    player(
                xy          :: vector2f,
                lives       :: int,
                health      :: float,
                hit         :: bool,
                thrusters   :: bool,
                alpha       :: float,
                laser_rate  :: float,
                laser_last  :: float,
                laser_max   :: int,
                laser_shots :: missiles,
                assets      :: asset_library,
                ship_type   :: ship_type,
                wing_guns   :: bool,
                shot_id     :: int
            ).

:- type ship_type
    --->    normal.

:- type missile_type
    --->    normal.

:- type missile
    --->    missile(
                mtype       :: missile_type,
                mxy         :: fp_point,
                hit         :: bool,
                missile_id  :: int
            ).

:- type missiles == list(missile).

:- instance hittable(player).
:- instance hittable(missile).


:- implementation.
:- import_module int.
:- import_module string.
:- import_module uint.

:- import_module audio.
:- import_module layout.
:- import_module raylib.

:- use_module rnd.

%----------------------------------------------------------------------------%

    % TYPECLASS: hittable(T) for Player.
    %
:- instance hittable(player) where
[
  id(_)         = -1,
  as_circle(T)  = v2_circle(T ^xy, 1.0),
  as_point(T)   = T ^xy,
  is_hit(_)     = no, %% SAFE?
  can_be_hit(_) = yes,
  func(as_rect/1) is player_bounds_rect,
  (set_hit(!T) :- !T ^hit := yes)
].

    % TYPECLASS: hittable(T) for Player MISSILE.
    %
:- instance hittable(missile) where
[
  id(T)         = T ^missile_id,
  as_point(T)   = fp_float_v2(T ^mxy),
  as_circle(T)  = fp_float_v3(T ^mxy, 1.0),
  func(as_rect/1) is missile_bounds_rect,
  is_hit(T)     = T ^hit,
  can_be_hit(_) = yes,
  (set_hit(!T) :- !T ^hit := yes)
].


%----------------------------------------------------------------------------%

    % Initialise a player state.
    %
player_init(
    player(
        vector2f(centre(ScreenW, SW),  initial_y(ScreenH, SH)),
        5,
        100.0,
            % hit flag
        no,
            % thrusters on?
        yes,
            % aplha.
        255.0,
            % 4 times/second intial firing rate
        0.125,
            % last time a laser shot was fired.
        0.0,
            % maximum allows missiles.
        3,
            % active missiles / bullets.
        [],
            % Global assets.
        GameState ^assets,
            % Ship type
        normal,
            % Wing guns initially off.
        no,
            % Shot id. Increment each time player fires something
        0
    ),
    GameState,
    !IO
) :-
    rectangle(_, _, SW, SH) = slice(ship1green),
    ScreenW = float(GameState ^screen_w),
    ScreenH = float(GameState ^screen_h).


:- func initial_y(float::in, float::in) = (float::out) is det.

initial_y(ScreenH, SH) = ScreenH - SH * 3.0.

%----------------------------------------------------------------------------%

    % Render the Player ship.
    %
draw_player(Player, Debug, !IO) :-
    Sheet      = Player ^assets^sheet1,
    Ship       = slice(ship1green),

    rectangle(_, _, SW, SH) = Ship,
    vector2f(Px, Py) = Player ^xy,


    ( Debug = yes
    ->
        draw_rectangle(
            truncate_to_int(Px), truncate_to_int(Py),
            truncate_to_int(SW), truncate_to_int(SH),
            with_a(color(white), 127u8),
            !IO
        ),
        draw_text(
            string.format(
                "(%i, %i)",
                [ i(truncate_to_int(Px)), i(truncate_to_int(Py)) ]
            ),
            floor_to_int(Px - 40.0),
            floor_to_int(Py + 100.0),
            20,
            to_rgba(color(yellow)),
            !IO
        )
    ;
        true
    ),
        %-
        % Render missiles.
        %-
    DrawMissile = (pred(
        missile(_MType, Mxy, _Hit, Id)::in, !.IO::di, !:IO::uo
    ) is det :-
        draw_texture_rec(
            Sheet,
            slice(laser_green),
            fp_float_v2(Mxy),
            to_rgba(color(white)),
            !IO
        ),
        ( Debug = yes
        ->
            vector2f(Mx, My) = fp_float_v2(Mxy),
            draw_text(
                string.format("%i", [ i(Id) ]),
                floor_to_int(Mx - 10.0),
                floor_to_int(My + 10.0),
                20,
                to_rgba(color(yellow)),
                !IO
            )
        ;
            true
        )
    ),
    list.foldl(DrawMissile, Player ^laser_shots, !IO),

    draw_texture_rec(
        Sheet,
        Ship,
        Player ^xy,
        with_af(color(white), Player ^alpha),
        !IO
    ),
        %-
        % Ship engine thruster flames.
        %-
    rnd.int(0, 2, Rval, !IO),
    (    if Rval = 0 then Flames = slice(fire13)
    else if Rval = 1 then Flames = slice(fire16)
    else                  Flames = slice(fire17)
    ),
        %-
        % render ship flames.
        %-
    rectangle(_, _, FW, _)  = Flames,

    draw_texture_rec(
        Sheet,
        Flames,
        vector2f(
            centre(SW, FW) + Px,
            Py + SH
        ),
        to_rgba(color(white)),
        !IO
    ).

%----------------------------------------------------------------------------%

    % Step update the player ship.
    % We filter out any that are 'done' so new shots can be fired.
    %
step_player(FrameTime, !Player, !IO) :-
    StepMissile = (pred(
        missile(MType, Mxy0, Hit, Id)::in,
        missile(MType, Mxy, Hit, Id)::out
    ) is semidet :-
        Hit = no,
        Mxy = fp_step_pt(FrameTime, Mxy0),
        fp_point(_, MY) = Mxy,
        not fp_done(MY)
    ),
    list.filter_map(StepMissile, !.Player ^laser_shots, Shots),
    !Player ^laser_shots := Shots.

%----------------------------------------------------------------------------%

    % Move left: decrease x-coordinate until edge of screen.
    % According to screen logic, the player MAY be able to wrap to the
    % opposite edge but for now, simple constraints apply.
    %
move_left(_, !Player) :-
    vector2f(X0, Y) = !.Player ^xy,
    !Player ^xy := vector2f(max( X0 - 4.0, 0.0), Y).


    % Moving right.
    % Same as for moving for left wrt. bounds.
    %
move_right(ScreenW, !Player) :-
    vector2f(X0, Y) = !.Player ^xy,
    rectangle(_, _, W, _) = player_slice(!.Player ^ship_type),
    !Player ^xy := vector2f(min( ScreenW - W, X0 + 4.0), Y).


    % Moving forwards (up the screen).
    %
move_up(_ScreenH, !Player) :-
    vector2f(X, Y0) = !.Player ^xy,
    %rectangle(_, _, _, _H) = player_slice(!.Player ^ship_type),
    !Player ^xy := vector2f(X, max(600.0, Y0 - 1.0)).

    % Moving backwards (down the screen).
    %
move_down(ScreenH, !Player) :-
    move_down(ScreenH, 1.0, !Player).

    % Forcibly Moving backwards (down the screen).
    % Value specifies a jump amount, e.g. collision knock-back.
move_down(ScreenH, Value, !Player) :-
    vector2f(X, Y0) = !.Player ^xy,
    rectangle(_, _, _, H) = player_slice(!.Player ^ship_type),
    Limit = initial_y(ScreenH, H),
    !Player ^xy := vector2f(X, min(Limit, Y0 + Value)).


weapon_previous(!Player, !IO) :-
    true.

weapon_next(!Player, !IO) :-
    true.

%----------------------------------------------------------------------------%

    % See if a new laser shot can be raised.
    % The ship has to throttle the firing-rate, apply auto-fire, overheating
    % checks etc.
    % TODO: Vary according to selected weapon, weapons mode etc.
    %
fire_laser(FrameTime, ScreenH, !P, !IO) :-
    IdleTime = !.P ^laser_last + FrameTime,

    ( if IdleTime > !.P ^laser_rate then

        Shots0 = !.P ^laser_shots,

        ( if list.length(Shots0) < !.P ^laser_max then

            vector2f(Px, Py) = !.P ^xy,
            rectangle(_, _, MW, _MH) = slice(laser_green),
            rectangle(_, _, SW, _SH) = player_slice(!.P ^ship_type),

            Shot = missile(
                normal,
                fp_point(
                        % missile centered in ship + ship x
                    sval(centre(SW, MW) + Px),
                    dval(tween_init(linear, Py, 0.0,
                        % Duration is 1 second full screen height
                        ( Py / float(ScreenH) )
                    ))
                ),
                no,
                !.P ^shot_id
            ),
            list.cons(Shot, Shots0, Shots),
            !P ^laser_shots := Shots,
            !P ^laser_last  := 0.0,
            !P ^shot_id     := !.P ^shot_id + 1,
            sfx(!.P ^assets^laser1, !IO)
        else
            % Laser queue is full until one dies off so we
            % make a silly noise just because we can.
            sfx_roll(!.P ^assets^laser_blanked, !IO)
        )
    else
        !P ^laser_last := IdleTime
    ).

%----------------------------------------------------------------------------%

    % Return image for current player ship type.
    %
:- pragma inline(player_slice/1).

:- func player_slice(ship_type::in) = (rectangle::out) is det.

player_slice(normal) = slice(ship1green).


    % Return ship bounds as a rectangle.
    %
player_bounds_rect(Player) = rectangle(X, Y, W, H) :-
    rectangle(_, _, W, H) = slice(ship1green),
    vector2f(X, Y) = Player ^xy.

    % Return missile bounds as a rectangle.
    %
missile_bounds_rect(Missile) = rectangle(X, Y, W, H) :-
    rectangle(_, _, W, H) = slice(laser_green),
    vector2f(X, Y) = fp_float_v2(Missile ^mxy).


player_expired(Player) :-
    Player ^health < 0.0.


player_health_adjust(!Player, Value) :-
    V = !.Player ^health + Value,
    (    if V < 0.0   then New = 0.0
    else if V > 100.0 then New = 100.0
    else                   New = V
    ),
    !Player ^health := New.

%----------------------------------------------------------------------------%
:- end_module player.
%----------------------------------------------------------------------------%
