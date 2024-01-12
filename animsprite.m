%-----------------------------------------------------------------------------%
%
% File: animsprite.m
% Main author: Sean Charles
% Date: Wed Oct 08 10:21:42 2023
%
% Animated sprite engine.
%
% This is a simple sprite based engine for performing the explosions, power
% up animations and other incidental animations required throughout.
%
%-----------------------------------------------------------------------------%
:- module animsprite.

:- interface.
:- import_module io.
:- import_module list.

:- import_module datatypes.
:- import_module easing.
:- import_module raylib.



:- type sprite_mgr
    --->    sprite_mgr(
                sheet1  :: rtexture,
                sprites :: sprites
            ).


:- type sprite
    --->    sprite(
                sheet   :: rtexture,
                slice   :: rectangle,
                xy      :: fp_point,
                color   :: tween_color,
                timer   :: tween,
                angle   :: tween
            ).

:- type sprites == list(sprite).


:- func animsprite_init(rtexture::in) = (sprite_mgr::out) is det.
:- pred animsprite_free(sprite_mgr::in) is det.

:- pred sprite_add(sprite::in, sprite_mgr::in, sprite_mgr::out) is det.

:- pred draw_sprites(sprite_mgr::in, io::di, io::uo) is det.
:- pred step_sprites(float::in, sprite_mgr::in, sprite_mgr::out) is det.

% User API
%
:- pred pop_fade(
    sprite::in, sprite_mgr::in, sprite_mgr::out, io::di, io::uo
) is det.


:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module uint8.
:- import_module uint32.

:- import_module animtext.
:- import_module layout.

:- use_module osd.


    % Make a sprite expand from it's centre, whilst fading out.
    % It completes when faded from view at alpha 0.
    %
pop_fade(Sprite, !Mgr, !IO) :-
    sprite_add(Sprite, !Mgr).


%----------------------------------------------------------------------------%

animsprite_init(Texture) =
    sprite_mgr(Texture, []).


animsprite_free(_Mgr) :- true.


sprite_add(Sprite, !Mgr) :-
    trace [io(!Dbg), runtime(env("FELT_AST"))] (
        io.format("SpriteMgr: ADD: %s\n", [s(string(Sprite))], !Dbg)
    ),
    !Mgr ^sprites := list.cons(Sprite, !.Mgr ^sprites).

%----------------------------------------------------------------------------%

    % Step a sprite animation by the elapsed time.
    %
step_sprites(_FrameTime, !Mgr) :-
    true.
%        % If the Timer is done, we can update the step.
%    ( if tween_done(Timer0) then
%        Timer = Timer0,
%        XY    = fp_step_pt(FrameTime, XY0),
%        Color = tween_step_color(FrameTime, Color0)
%    else
%        %% Update the timer first, we don't become active yet.
%        XY     = XY0,
%        Color  = Color0,
%        Shadow = Shadow0,
%        tween_step(FrameTime, Timer0, Timer)
%    ).

%----------------------------------------------------------------------------%

draw_sprites(_Mgr, !IO) :-
    true.
%    vector2f(X, Y) = fp_float_v2(Sprite ^xy),
%    Color = to_rgba(color(yellow)),

%    rectangle(_, _, W, H) = Sprite ^slice,

%    draw_texture_rec_pro(
%        Sprite ^sheet,
%        Sprite ^rectangle,
%        rectangle(X, Y, W, H),
%        vector2f(0.0, 0.0),
%        0.0,
%        Color,
%        !IO
%    ).

%----------------------------------------------------------------------------%
:- end_module animsprite.
%----------------------------------------------------------------------------%

