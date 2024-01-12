%-----------------------------------------------------------------------------%
%
% File: collision.m
% Main author: Sean Charles
% Date: Thu Oct 12 10:38:04 2023
%
% The typeclass powered collision detection system for all objects.
%
% Each thing that wants pass this way MUST have implemented the typeclass
% called 'hittable'. Then it can join in the collision parties at will.
%
% The input is a list of hittable entities, and any that are detected as being
% in collision with something else have their 'hit' flag, and on exit we are
% returning a new list of 'shots' (L1: list one) and 'targets' (L2: list two).
%
%-----------------------------------------------------------------------------%
:- module collision.

:- interface.
:- import_module bool.
:- import_module list.

:- import_module datatypes.

    % Single API: collisions_over/5.
    %
:- pred collisions_over(
    collision_type::in,            % Check
    list(S)::in,    list(S)::out,   % !Shots
    list(T)::in,    list(T)::out    % !Targets
) is det <= ( hittable(S), hittable(T) ).

    % Collision detection category.
    % This determines the nature of the type of collision and
    % is specified by the caller for the entire operation.
    %
:- type collision_type
    --->    pt_circle   % Missiles vs. UFOs.
    ;       pt_rect     % Missiles vs. Falling objects.
    ;       rect_rect.  % Typically falling objects against objects.

    % Anything that can be hit.
    % If an game object wants to be used within this detection
    % system is MUST fully implement this to be accepted.
    %
:- typeclass hittable(T) where
[
   func id(T)         = int,
   func as_point(T)   = vector2f,
   func as_circle(T)  = vector3f,
   func as_rect(T)    = rectangle,
   func is_hit(T)     = bool,
   func can_be_hit(T) = bool,
   pred set_hit(T::in, T::out) is det
].


:- implementation.
:- import_module set.
:- import_module string.

:- import_module raylib.

:- type collision_set == set(int).


%----------------------------------------------------------------------------%

    % Collisions Over a 2D Cartesian Product of shots and targets.
    %
    % Currently it does a crude Cartesian product sweep of both sets of inputs
    % as the size is never large. On a LARGE dataset then K-D might be worthy?
    %
collisions_over(Check, Shots0, Shots, Targets0, Targets) :-
    list.foldl2(
        outer_cartesian_loop(Check, Targets0),
        Shots0, 
        set.init:collision_set, Shots1,
        set.init:collision_set, Targets1
    ),
    list.map(
        hitmapper(set.to_sorted_list(Shots1)),
        Shots0, Shots
    ),
    list.map(
        hitmapper(set.to_sorted_list(Targets1)),
        Targets0, Targets
    ).


:- pred hitmapper(list(int)::in, S::in, S::out)
    is det <= ( hittable(S), hittable(S) ).

hitmapper(IDS, S0, S) :-
    ( list.member(S0 ^id, IDS) -> set_hit(S0, S) ; S = S0 ).

%----------------------------------------------------------------------------%

    % Outer loop: this iterates all shots against all targets as a cartesion
    % operation. Not particularly efficient in the large but given the small
    % number of moving objects in our game, not really an issue. k-d trees
    % are available in the rtree module but the API is not a good fit for the
    % code I needed so i am doing it my way.
    %
:- pred outer_cartesian_loop(
    collision_type::in,                     % Check
    list(T)::in,                            % Targets (Targets0 from caller)
    S::in,                                  % Shot
    collision_set::in, collision_set::out,  % !L1acc
    collision_set::in, collision_set::out   % !L2acc
) is det <= ( hittable(S), hittable(T) ).

outer_cartesian_loop(Check, Targets, Shot, !L1acc, !L2acc) :-
    list.foldl2(
        inner_cartesian_loop(Check, Shot),
        Targets,
        !L1acc,
        !L2acc
    ).

%----------------------------------------------------------------------------%

    % Inner loop: this will check each individual shot (L1) against each
    % individual target (lL2), if a collision is detected then both parties
    % are called to update their state, and then added to the set.
    % Objects not colliding are added to the set unchanged.
    %
:- pred inner_cartesian_loop(
    collision_type::in,                     % Check
    S::in,                                  % Shot
    T::in,                                  % Target
    collision_set::in, collision_set::out,  % L1acc
    collision_set::in, collision_set::out   % !L2acc
) is det <= ( hittable(S), hittable(T) ).

inner_cartesian_loop(Check, Shot, Target, !L1acc, !L2acc) :-
%    trace [io(!Dbg)] (
%        io.format("HIT?\n%s\n%s\n\n",
%            [ s(string(Shot)), s(string(Target))], !Dbg)
%    ),
    ( if did_collide(Check, Shot, Target) then
%        trace [io(!Dbg)] (
%            io.format("HIT! %s\n", [ s(string(Shot))], !Dbg)
%        ),
        set.insert(Shot ^id, !L1acc),
        set.insert(Target ^id, !L2acc)
    else
        true
    ).

%----------------------------------------------------------------------------%

    % Perform a type-specific collision check.
    %
:- pred did_collide(collision_type::in, S::in, T::in)
    is semidet <= ( hittable(S), hittable(T) ).

did_collide(pt_circle, O1, O2) :-
    can_be_hit(O1) = yes,
    can_be_hit(O2) = yes,
    check_collision_point_circle(as_point(O1), as_circle(O2)).

did_collide(pt_rect, O1, O2) :-
    can_be_hit(O1) = yes,
    can_be_hit(O2) = yes,
    check_collision_point_rec(as_point(O1), as_rect(O2)).

did_collide(rect_rect, O1, O2) :-
    can_be_hit(O1) = yes,
    can_be_hit(O2) = yes,
    check_collision_recs(as_rect(O1), as_rect(O2)).

%----------------------------------------------------------------------------%
:- end_module collision.
%----------------------------------------------------------------------------%

