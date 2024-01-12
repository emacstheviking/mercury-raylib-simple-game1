%-----------------------------------------------------------------------------%
%
% File: raylib.m
% Main author: Sean Charles
% Date: Fri Jul  7 07:34:18 2023
%
%        [        ]
%        [        ]
%        [ raylib ]
%
% By and large I have attempted to make this file be in the same order as the
% content in the underlying raylib.h file for side by side comparison of all
% implemented features.
%
% I have, and continue to implement only those features I need / needed for my
% application. If you tried and failed to extend or add something, feel free
% to raise an issue.
%
%-----------------------------------------------------------------------------%
:- module raylib.

:- interface.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- use_module float.
:- use_module uint8.
:- use_module uint32.

:- import_module datatypes.

    %--------------------------------------------------%
    % MODULE: rcore
    %
    % Window-related functions.
    %
:- pred set_config_flags(list(config_flags)::in, io::di, io::uo) is det.
:- pred init_window(int::in, int::in, string::in, io::di, io::uo) is det.
:- pred window_should_close(bool::out, io::di, io::uo) is det.
:- pred close_window(io::di, io::uo) is det.
:- pred get_screen_wh(int::out, int::out, io::di, io::uo) is det. % because
:- pred set_window_position(int::in, int::in, io::di, io::uo) is det.

    % Drawing-related functions.
    %
:- pred clear_background(color::in, io::di, io::uo) is det.
:- pred begin_drawing(io::di, io::uo) is det.
:- pred end_drawing(io::di, io::uo) is det.

    % Timing-related functions.
    %
:- pred set_target_fps(int::in, io::di, io::uo) is det.
:- pred get_frame_time(float::out, io::di, io::uo) is det.
:- pred get_time(float::out, io::di, io::uo) is det.

    % Misc. functions
    %
:- pred set_trace_log_level(log_level::in, io::di, io::uo) is det.
:- pred trace_info(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pred trace_warn(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pred trace_error(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pred trace_debug(string::in, list(poly_type)::in, io::di, io::uo) is det.

    % Input-related functions: keyboard
    %
:- pred is_key_pressed(key_code::in, bool::out, io::di, io::uo) is det.
:- pred is_key_down(key_code::in, bool::out, io::di, io::uo) is det.
:- pred is_key_released(key_code::in, bool::out, io::di, io::uo) is det.
:- pred is_key_up(key_code::in, bool::out, io::di, io::uo) is det.
:- pred set_exit_key(key_code::in, io::di, io::uo) is det.
:- pred get_char_pressed(int::out, io::di, io::uo) is det.
:- pred get_key_pressed(key_code::out, int::out, io::di, io::uo) is det.
:- pred is_mouse_button_pressed(mouse_button::in, bool::out, io::di, io::uo) is det.
:- pred is_mouse_button_down(mouse_button::in, bool::out, io::di, io::uo) is det.
:- pred is_mouse_button_released(mouse_button::in, bool::out, io::di, io::uo) is det.
:- pred is_mouse_button_up(mouse_button::in, bool::out, io::di, io::uo) is det.
:- pred get_mouse_xy(int::out, int::out, io::di, io::uo) is det. % because
:- pred poll_input_events(io::di, io::uo) is det.

:- pred load_shader(maybe(string)::in, maybe(string)::in, rshader::out, io::di, io::uo) is det.

    %--------------------------------------------------%
    % MODULE: rshapes
    %
:- pred draw_rectangle(int::in, int::in, int::in, int::in, uint32::in, io::di, io::uo) is det.
:- pred draw_rectanglef(float::in, float::in, float::in, float::in, uint32::in, io::di, io::uo) is det.
:- pred draw_rectangle_rounded(float::in, float::in, float::in, float::in, float::in, int::in, uint32::in, io::di, io::uo) is det.
:- pred draw_circle(int::in, int::in, float::in, uint32::in, io::di, io::uo) is det.
:- pred draw_circleV(vector2f::in, float::in, uint32::in, io::di, io::uo) is det.
:- pred draw_circleV_(float::in, float::in, float::in, uint32::in, io::di, io::uo) is det.
:- pred draw_circle_gradient(float::in, float::in, float::in, uint32::in, uint32::in, io::di, io::uo) is det.
    %--------------------------------------------------%
    % Collision detection.
    %--------------------------------------------------%
:- pred check_collision_point_circle(vector2f::in, vector2f::in, float::in) is semidet.
:- pred check_collision_point_circle(vector2f::in, vector3f::in) is semidet.
:- pred check_collision_point_circle_(float::in, float::in, float::in, float::in, float::in) is semidet.

:- pred check_collision_point_rec(vector2f::in, rectangle::in) is semidet.
:- pred check_collision_point_rec_(float::in, float::in, float::in, float::in, float::in, float::in) is semidet.

:- pred check_collision_recs(rectangle::in, rectangle::in) is semidet.
:- pred check_collision_recs_(float::in, float::in, float::in, float::in, float::in, float::in, float::in, float::in) is semidet.


    %--------------------------------------------------%
    % MODULE: rtextures
    %
:- pred load_texture(string::in, rtexture::out, io::di, io::uo) is det.
:- pred unload_texture(rtexture::in, io::di, io::uo) is det.

:- pred draw_texture_rec(rtexture::in, rectangle::in, vector2f::in, uint32::in, _IO0::di, _IO::uo) is det.
:- pred draw_texture_rec_(rtexture::in, float::in, float::in, float::in, float::in, float::in, float::in, uint32::in, _IO0::di, _IO::uo) is det.

:- pred draw_texture_rec_pro(rtexture::in, rectangle::in, rectangle::in, vector2f::in, float::in, uint32::in, _IO0::di, _IO::uo) is det.

:- pred draw_texture_rec_pro_(rtexture::in,
    float::in, float::in, float::in, float::in,
    float::in, float::in, float::in, float::in,
    float::in, float::in,
    float::in, uint32::in,
    _IO0::di, _IO::uo) is det.


    %--------------------------------------------------%
    % MODULE: rtext
    %
:- pred load_font(string::in, rfont::out, io::di, io::uo) is det.
:- pred unload_font(rfont::in, io::di, io::uo) is det.
:- pred draw_text(string::in, int::in, int::in, int::in, uint32::in, io::di, io::uo) is det.
:- pred draw_text_ex(rfont::in, string::in, int::in, int::in, int::in, float::in, uint32::in, io::di, io::uo) is det.
:- pred draw_text_ex_char(rfont::in, string::in, int::in, int::in, int::in, int::in, float::in, uint32::in, io::di, io::uo) is det.

:- pred draw_fps(int::in, int::in, io::di, io::uo) is det.
:- pred measure_text(string::in, int::in, int::out, io::di, io::uo) is det.
:- pred measure_text_ex(rfont::in, string::in, float::in, float::in, float::out, float::out, io::di, io::uo) is det.
:- pred measure_text_char(char::in, int::in, int::out, io::di, io::uo) is det.
:- pred measure_text_ex_char(rfont::in, char::in, float::in, float::in, float::out, float::out, io::di, io::uo) is det.

    %--------------------------------------------------%
    % MODULE: raudio
:- pred init_audio_device(io::di, io::uo) is det.
:- pred close_audio_device(io::di, io::uo) is det.
:- pred is_audio_device_ready(bool::out, io::di, io::uo) is det.
:- pred set_master_volume(float::in, io::di, io::uo) is det.
:- pred load_sound(string::in, rsound::out, io::di, io::uo) is det.
:- pred unload_sound(rsound::in, io::di, io::uo) is det.
:- pred is_sound_ready(rsound::in, bool::out, io::di, io::uo) is det.
:- pred is_sound_playing(rsound::in, bool::out, io::di, io::uo) is det.
:- pred play_sound(rsound::in, io::di, io::uo) is det.
:- pred stop_sound(rsound::in, io::di, io::uo) is det.
:- pred load_music_stream(string::in, rmusic::out, io::di, io::uo) is det.
:- pred is_music_ready(rmusic::in, bool::out, io::di, io::uo) is det.
:- pred unload_music_stream(rmusic::in, io::di, io::uo) is det.
:- pred play_music_stream(rmusic::in, io::di, io::uo) is det.
:- pred update_music_stream(rmusic::in, io::di, io::uo) is det.
:- pred is_music_stream_playing(rmusic::in, bool::out, io::di, io::uo) is det.
:- pred stop_music_stream(rmusic::in, io::di, io::uo) is det.
:- pred pause_music_stream(rmusic::in, io::di, io::uo) is det.
:- pred resume_music_stream(rmusic::in, io::di, io::uo) is det.
:- pred seek_music_stream(rmusic::in, float::in, io::di, io::uo) is det.
:- pred set_music_volume(rmusic::in, float::in, io::di, io::uo) is det.
:- pred set_music_pan(rmusic::in, float::in, io::di, io::uo) is det.
:- pred get_music_time_length(rmusic::in, float::out, io::di, io::uo) is det.
:- pred get_music_time_played(rmusic::in, float::out, io::di, io::uo) is det.

:- pred dump_sound(rsound::in, io::di, io::uo) is det.

    %--------------------------------------------------%
    % Misc. functions.
:- pred get_random_value(int::in, int::in, int::out, io::di, io::uo) is det.
:- pred get_random_fvalue(int::in, int::in, float::out, io::di, io::uo) is det.
:- pred get_random_fvaluef(float::in, float::in, float::out, io::di, io::uo) is det.
:- pred set_random_seed(uint::in, io::di, io::uo) is det.

    %--------------------------------------------------%
    % MERCURY: higher-level abstractions
    %
:- pred dropped_files(list(string)::out, io::di, io::uo) is det.
:- pred color_parts(color::in, uint8::out, uint8::out, uint8::out, uint8::out) is det.
:- func to_rgba(color::in) = (uint32::out) is det.
:- func with_a(color::in, uint8::in) = (uint32::out) is det.
:- func with_af(color::in, float::in) = (uint32::out) is det.

    % Colour selection and reverse lookup by RGBA.
    %
:- pred color_rgba(p_color, uint8, uint8, uint8, uint8).
:- mode color_rgba(in, out, out, out, out) is det.
:- mode color_rgba(out, in, in, in, in) is semidet.

%----------------------------------------------------------------------------%

    % Window and environment configuration flags.
    % Note that some raylib features must be set after the window is created
    % but before the main loop is rendered, such as custom fonts etc as some
    % functions need an internal OpenGL context to exist.
    %
:- type config_flags
    --->    vsync_hint
    ;       fullscreen_mode
    ;       window_resizable
    ;       window_undecorated
    ;       window_hidden
    ;       window_minimized
    ;       window_maximised
    ;       window_unfocused
    ;       window_topmost
    ;       window_always_run
    ;       window_transparent
    ;       window_highdpi
    ;       window_mouse_passthrough
    ;       window_msaa_4x_hint
    ;       window_interlaced_hint.


    % Trace functions allow different logging levels to be
    % filtered out.
    %
:- type log_level
    --->    log_all
    ;       log_trace
    ;       log_debug
    ;       log_info
    ;       log_warning
    ;       log_error
    ;       log_fatal
    ;       log_none.


    % Mouse button codes.
    % Most people have three but raylib supports gaming mice too.
    %
:- type mouse_button
    --->    btn_left
    ;       btn_right
    ;       btn_middle
    ;       btn_side
    ;       btn_extra
    ;       btn_forward
    ;       btn_back.


    % Mouse cursor shapes.
    %
:- type mouse_cursor
    --->    cursor_default
    ;       cursor_arrow
    ;       cursor_ibeam
    ;       cursor_crosshair
    ;       cursor_pointing_hand
    ;       cursor_resize_ew
    ;       cursor_resize_ns
    ;       cursor_resize_nwse
    ;       cursor_resize_nesw
    ;       cursor_resize_all
    ;       cursor_not_allowed.


    % Key codes.
    % This is the complete set of scan codes raylib returns.
    % They do NOT reflect the status of SHIFT etc.
    %
:- type key_code
    --->    key_null
        % Alphanumeric keys
    ;       key_apostrophe
    ;       key_comma
    ;       key_minus
    ;       key_period
    ;       key_slash
    ;       key_zero
    ;       key_one
    ;       key_two
    ;       key_three
    ;       key_four
    ;       key_five
    ;       key_six
    ;       key_seven
    ;       key_eight
    ;       key_nine
    ;       key_semicolon
    ;       key_equal
    ;       key_a
    ;       key_b
    ;       key_c
    ;       key_d
    ;       key_e
    ;       key_f
    ;       key_g
    ;       key_h
    ;       key_i
    ;       key_j
    ;       key_k
    ;       key_l
    ;       key_m
    ;       key_n
    ;       key_o
    ;       key_p
    ;       key_q
    ;       key_r
    ;       key_s
    ;       key_t
    ;       key_u
    ;       key_v
    ;       key_w
    ;       key_x
    ;       key_y
    ;       key_z
    ;       key_left_bracket
    ;       key_backslash
    ;       key_right_bracket
    ;       key_grave
        % Function keys
    ;       key_space
    ;       key_escape
    ;       key_enter
    ;       key_tab
    ;       key_backspace
    ;       key_insert
    ;       key_delete
    ;       key_right
    ;       key_left
    ;       key_down
    ;       key_up
    ;       key_page_up
    ;       key_page_down
    ;       key_home
    ;       key_end
    ;       key_caps_lock
    ;       key_scroll_lock
    ;       key_num_lock
    ;       key_print_screen
    ;       key_pause
    ;       key_f1
    ;       key_f2
    ;       key_f3
    ;       key_f4
    ;       key_f5
    ;       key_f6
    ;       key_f7
    ;       key_f8
    ;       key_f9
    ;       key_f10
    ;       key_f11
    ;       key_f12
    ;       key_left_shift
    ;       key_left_control
    ;       key_left_alt
    ;       key_left_super
    ;       key_right_shift
    ;       key_right_control
    ;       key_right_alt
    ;       key_right_super
    ;       key_kb_menu
        % Keypad keys
    ;       key_kp_0
    ;       key_kp_1
    ;       key_kp_2
    ;       key_kp_3
    ;       key_kp_4
    ;       key_kp_5
    ;       key_kp_6
    ;       key_kp_7
    ;       key_kp_8
    ;       key_kp_9
    ;       key_kp_decimal
    ;       key_kp_divide
    ;       key_kp_multiply
    ;       key_kp_subtract
    ;       key_kp_add
    ;       key_kp_enter
    ;       key_kp_equal
        % Android key buttons
    ;       key_back
    ;       key_menu
    ;       key_volume_up
    ;       key_volume_down.


    % A 'P'allete color. Raylib conveniently predefines a range of
    % colours allowing simple colourful displays to be created.
    %
:- type p_color
    --->    lightgray
    ;       gray
    ;       darkgray
    ;       yellow
    ;       gold
    ;       orange
    ;       pink
    ;       red
    ;       maroon
    ;       green
    ;       lime
    ;       darkgreen
    ;       skyblue
    ;       blue
    ;       darkblue
    ;       purple
    ;       violet
    ;       darkpurple
    ;       beige
    ;       brown
    ;       darkbrown
    ;       white
    ;       black
    ;       blank
    ;       magenta
    ;       raywhite.


    % Three ways to express a color in your application code.
    % As an RGB, RGBA or a named colour from the stock palette.
    %
:- type color
    --->    rgb(uint8, uint8, uint8)
    ;       rgba(uint8, uint8, uint8, uint8)
    ;       color(p_color).


:- type rfont.
:- type rmusic.
:- type rsound.
:- type rtexture.
:- type rshader.

:- pragma foreign_type("C", rsound,   "MR_Integer", [can_pass_as_mercury_type]).
:- pragma foreign_type("C", rmusic,   "MR_Integer", [can_pass_as_mercury_type]).
:- pragma foreign_type("C", rfont,    "MR_Integer", [can_pass_as_mercury_type]).
:- pragma foreign_type("C", rtexture, "MR_Integer", [can_pass_as_mercury_type]).
:- pragma foreign_type("C", rshader,  "MR_Integer", [can_pass_as_mercury_type]).

%----------------------------------------------------------------------------%

:- implementation.

:- use_module easing.


    % Header files.
    %
:- pragma foreign_decl("C", "#include <stdio.h>").
:- pragma foreign_decl("C", "#include <string.h>").
:- pragma foreign_decl("C", "#include <raylib.h>").

    % Local supporting C functions.
    %
:- pragma foreign_decl("C",
"
static unsigned int flags_to_uint(MR_Word);
static void dump_sound_(Sound*, const char*);
static void dump_music_(Music*, const char*);

static unsigned int flags_to_uint(MR_Word list) {
    unsigned int flags = 0;
    while (!MR_list_is_empty(list))
    {
        flags |= (unsigned int) MR_list_head(list);
        list = MR_list_tail(list);
    }
    return flags;
}
static void dump_sound_(Sound* s, const char* title) {
    if (s) {
        TraceLog(LOG_INFO, ""### C Sound @ %p [%s]"", s, title);
        TraceLog(LOG_INFO, ""### rate: %i, bits: %i, channels:%i"",
           s->stream.sampleRate,
           s->stream.sampleSize,
           s->stream.channels
        );
    }
}
static void dump_music_(Music* m, const char* title) {
typedef struct Music {
    AudioStream stream;         // Audio stream
    unsigned int frameCount;    // Total number of frames (considering channels)
    bool looping;               // Music looping enable

    int ctxType;                // Type of music context (audio filetype)
    void *ctxData;              // Audio context data, depends on type
} Music;

    if (m) {
        TraceLog(LOG_INFO, ""### C Music @ %p [%s]"", m, title);
        TraceLog(LOG_INFO, ""### framecount: %u, looping: %i, ctxType: %i, ctxData: %p"",
            m->frameCount, m->looping, m->ctxType, m->ctxData
        );
    }
}

").

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
%                           module: rcore
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pragma foreign_proc(
    "C", init_window(Width::in, Height::in, Title::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        InitWindow(Width, Height, Title);
    ").


:- pragma foreign_proc(
    "C", window_should_close(Close::out, _IO0::di, _IO::uo),

    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Close = WindowShouldClose() == 0 ? MR_NO : MR_YES;
    ").


:- pragma foreign_proc(
    "C", set_window_position(X::in, Y::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        SetWindowPosition(X, Y);
    ").


:- pragma foreign_proc(
    "C", close_window(_IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        CloseWindow();
    ").


:- pragma foreign_proc(
    "C", begin_drawing(_IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        BeginDrawing();
    ").


clear_background(C, !IO) :-
    clear_background_(to_rgba(C), !IO).

:- pred clear_background_(uint32::in, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", clear_background_(C::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        ClearBackground(*((Color*)&C));
    ").


dropped_files(Files, !IO) :-
    dropped_files_(X, !IO),
    Files = list.reverse(X).

:- pred dropped_files_(list(string)::out, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", dropped_files_(Files::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Files = MR_list_empty();

        if (IsFileDropped())
        {
            FilePathList dfl = LoadDroppedFiles();
            for(int i=0; i < dfl.count; i++)
                Files = MR_list_cons(MR_copy_string(dfl.paths[i]), Files);
            UnloadDroppedFiles(dfl);
        }
    ").


:- pragma foreign_proc(
    "C", end_drawing(_IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        EndDrawing();
    ").


:- pragma foreign_proc(
    "C", set_target_fps(Rate::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        SetTargetFPS(Rate);
    ").


:- pragma foreign_proc(
    "C", get_frame_time(Val::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Val = GetFrameTime();
    ").

:- pragma foreign_proc(
    "C", get_time(Val::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Val = GetTime();
    ").


:- pragma foreign_proc(
    "C", set_config_flags(Flags::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        SetConfigFlags(flags_to_uint(Flags));
    ").


:- pragma foreign_proc(
    "C", set_trace_log_level(Level::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        SetTraceLogLevel(Level);
    ").

trace_info(Format, Args, !IO) :-
    trace_log_(log_info, string.format(Format, Args):string, !IO).

trace_warn(Format, Args, !IO) :-
    trace_log_(log_warning, string.format(Format, Args):string, !IO).

trace_error(Format, Args, !IO) :-
    trace_log_(log_error, string.format(Format, Args):string, !IO).

trace_debug(Format, Args, !IO) :-
    trace_log_(log_debug, string.format(Format, Args):string, !IO).


:- pred trace_log_(log_level::in, string::in, io::di, io::uo) is det.    

:- pragma foreign_proc(
    "C", trace_log_(Level::in, Text::in, _IO0::di, _IO::uo),

    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        TraceLog(Level, Text);
    ").


:- pragma foreign_proc(
    "C", is_key_pressed(Key::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsKeyPressed(Key) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", is_key_down(Key::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsKeyDown(Key) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", is_key_released(Key::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsKeyReleased(Key) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", is_key_up(Key::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsKeyUp(Key) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", set_exit_key(Key::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        SetExitKey(Key)
    ").

:- pragma foreign_proc(
    "C", get_char_pressed(KeyInt::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        KeyInt = GetCharPressed();
    ").

:- pragma foreign_proc(
    "C", get_key_pressed(Key::out, KeyInt::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        KeyInt = Key = GetKeyPressed();
    ").

:- pragma foreign_proc(
    "C", is_mouse_button_pressed(Btn::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsMouseButtonPressed(Btn) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", is_mouse_button_down(Btn::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsMouseButtonDown(Btn) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", is_mouse_button_released(Btn::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsMouseButtonReleased(Btn) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", is_mouse_button_up(Btn::in, YN::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        YN = IsMouseButtonUp(Btn) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", get_mouse_xy(X::out, Y::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        X = GetMouseX();
        Y = GetMouseY();
    ").

:- pragma foreign_proc(
    "C", get_screen_wh(W::out, H::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io
    ],
    "
        W = GetScreenWidth();
        H = GetScreenHeight();
    ").

:- pragma foreign_proc(
    "C", poll_input_events(_IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        PollInputEvents();
    ").


load_shader(VertexYN, FragmentYN, Shader, !IO) :-
    ( if VertexYN   = yes(V) then VS = V else VS = ""),
    ( if FragmentYN = yes(F) then FS = F else FS = ""),
    load_shader_(VS, FS, Shader, !IO).



:- pragma foreign_proc(
    "C", load_shader_(VS::in, FS::in, Shader::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        char *vsFilename = strlen(VS) ? VS : NULL ;
        char *fsFilename = strlen(FS) ? FS : NULL ;
        Sh shader = LoadShader(vsFilename, fsFilename);
    ").




%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
%                           module: rshapes
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pragma foreign_proc(
    "C", draw_rectangle(X::in, Y::in, W::in, H::in, C::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        DrawRectangle(X, Y, W, H, *((Color*)&C));
    ").

    % Concession: floats are ubiquitious in my apps.
    %
:- pragma foreign_proc(
    "C", draw_rectanglef(X::in, Y::in, W::in, H::in, C::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        DrawRectangle((float)X, (float)Y, (float)W, (float)H, *((Color*)&C));
    ").


:- pragma foreign_proc(
    "C", draw_rectangle_rounded(
        X::in, Y::in, W::in, H::in, Roundness::in, Segments::in,
        C::in, _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Rectangle rect = { X, Y, W, H };
        DrawRectangleRounded(rect, Roundness, Segments, *((Color*)&C));
    ").

:- pragma foreign_proc(
    "C", draw_circle(X::in, Y::in, Radius::in, Clr::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        DrawCircle(X, Y, Radius, *((Color*)&Clr));
    ").

draw_circleV(vector2f(X, Y), Radius, Color, !IO) :-
    draw_circleV_(X, Y, Radius, Color, !IO).

:- pragma inline(draw_circleV_/6).
:- pragma foreign_proc(
    "C", draw_circleV_(X::in, Y::in, Radius::in, Clr::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Vector2 center = { X, Y };
        DrawCircleV(center, Radius, *((Color*)&Clr));
    ").

:- pragma foreign_proc(
    "C", draw_circle_gradient(
        Cx::in, Cy::in, Radius::in, Color1::in, Color2::in, _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        DrawCircleGradient((int)Cx, (int)Cy, Radius,
            *((Color*)&Color1), *((Color*)&Color2)
        );
    ").

    % Mercury friendly using DUTs.
    %
check_collision_point_circle(vector2f(Px, Py), vector2f(Cx, Cy), Radius) :-
    check_collision_point_circle_(Px, Py, Cx, Cy, Radius).

check_collision_point_circle(vector2f(Px, Py), vector3f(Cx, Cy, Radius)) :-
    check_collision_point_circle_(Px, Py, Cx, Cy, Radius).

:- pragma foreign_proc(
    "C", check_collision_point_circle_(Px::in, Py::in, Cx::in, Cy::in, Radius::in),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    ],
    "
        Vector2 point  = { Px, Py };
        Vector2 center = { Cx, Cy };
        SUCCESS_INDICATOR = CheckCollisionPointCircle(point, center, Radius);
    ").

    % Mercury friendly using DUTs.
    %
check_collision_recs(rectangle(RX1, RY1, RW1, RH1), rectangle(RX2, RY2, RW2, RH2)) :-
    check_collision_recs_(RX1, RY1, RW1, RH1, RX2, RY2, RW2, RH2).

:- pragma foreign_proc(
    "C", check_collision_recs_(
        RX1::in, RY1::in, RW1::in, RH1::in,
        RX2::in, RY2::in, RW2::in, RH2::in
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    ],
    "
        Rectangle rec1 = { RX1, RY1, RW1, RH1 };
        Rectangle rec2 = { RX2, RY2, RW2, RH2 };
        SUCCESS_INDICATOR = CheckCollisionRecs(rec1, rec2);
    ").

    % Mercury friendly using DUTs.
    %
check_collision_point_rec(vector2f(Px, Py), rectangle(RX1, RY1, RW1, RH1)):-
    check_collision_point_rec_(Px, Py, RX1, RY1, RW1, RH1).

:- pragma foreign_proc(
    "C", check_collision_point_rec_(
        PX::in, PY::in, RX2::in, RY2::in, RW2::in, RH2::in
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    ],
    "
        Vector2 pt = { PX, PY };
        Rectangle rec = { RX2, RY2, RW2, RH2 };
        SUCCESS_INDICATOR = CheckCollisionPointRec(pt, rec);
    ").


%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
%                           module: rtextures
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pragma foreign_proc(
    "C", load_texture(Filename::in, Out::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        void* p = MR_GC_malloc_uncollectable(sizeof(Texture2D));
        Out = p;
        Texture2D tex = LoadTexture(Filename);
        memcpy(p, &tex, sizeof(Texture2D));
    ").

:- pragma foreign_proc(
    "C", unload_texture(RTexture::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Texture2D* p = (Texture2D*)RTexture;
        UnloadTexture(*p);
        MR_GC_free((void*)RTexture);
    ").


draw_texture_rec(Tex, rectangle(SX, SY, SW, SH), vector2f(X, Y), Color, !IO) :-
    draw_texture_rec_(Tex, SX, SY, SW, SH, X, Y, Color, !IO).

:- pragma foreign_proc(
    "C", draw_texture_rec_(
        RTexture::in, SrcX::in, SrcY::in, SrcW::in, SrcH::in,
        X::in, Y::in, Colour::in, _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Texture2D* p = (Texture2D*)RTexture;
        Rectangle source = {SrcX, SrcY, SrcW, SrcH};
        Vector2 position = {X, Y};
        Color tint;
        memcpy((void*)&tint, &Colour, sizeof(Color));
        DrawTextureRec(*p, source, position, tint);
    ").


draw_texture_rec_pro(
    Tex,
    rectangle(SX, SY, SW, SH),
    rectangle(DX, DY, DW, DH),
    vector2f(X, Y),
    Rotation,
    Color,
    !IO
) :-
    draw_texture_rec_pro_(
        Tex,
        SX, SY, SW, SH,
        DX, DY, DW, DH,
        X, Y, Rotation, Color,
        !IO
    ).

:- pragma foreign_proc(
    "C", draw_texture_rec_pro_(
        RTexture::in,
        SrcX::in, SrcY::in, SrcW::in, SrcH::in,
        DstX::in, DstY::in, DstW::in, DstH::in,
        X::in, Y::in,
        Rotation::in,
        Colour::in,
        _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Texture2D* p = (Texture2D*)RTexture;
        Rectangle source = { SrcX, SrcY, SrcW, SrcH };
        Rectangle dest   = { DstX, DstY, DstW, DstH };
        Vector2   origin = { X, Y };
        Color tint;
        memcpy((void*)&tint, &Colour, sizeof(Color));
        DrawTexturePro(*p, source, dest, origin, Rotation, tint);
    ").

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
%                           module: rtext
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pragma foreign_proc(
    "C", draw_fps(X::in, Y::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        DrawFPS(X, Y);
    ").


:- pragma foreign_proc(
    "C", load_font(FontName::in, Out::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        void* p = MR_GC_malloc_uncollectable(sizeof(Font));
        Out = p;
        Font font = LoadFont(FontName);
        memcpy(p, &font, sizeof(Font));
    ").


:- pragma foreign_proc(
    "C", unload_font(RFont::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Font* pFont = (Font*)RFont;
        UnloadFont(*pFont);
        MR_GC_free((void*)RFont);
    ").


:- pragma foreign_proc(
    "C", draw_text(
        Text::in, X::in, Y::in, Size::in, C::in,
        _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        DrawText(Text, X, Y, Size, *((Color*)&C));
    ").


:- pragma foreign_proc(
    "C", draw_text_ex(
        RFont::in, Text::in, X::in, Y::in, Size::in, Spacing::in, C::in,
        _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Vector2 position = { X, Y };
        Font* pFont = (Font*)RFont;

        DrawTextEx(*pFont, Text, position, (float)Size, Spacing, *((Color*)&C));
    ").

:- pragma foreign_proc(
    "C", draw_text_ex_char(
        RFont::in, Text::in, Index::in, X::in, Y::in, Size::in,
        Spacing::in, C::in, _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Vector2 position = { X, Y };
        Font* pFont = (Font*)RFont;
        char buffer[2] = {0};

        if (Index < 0)
            DrawTextEx(*pFont, Text, position, (float)Size, Spacing, *((Color*)&C));
        else {
            // NOTE: NO BOUNDS CHECKING FOR SPEED!
            buffer[0] = *(Text+Index);
            DrawTextEx(*pFont, &buffer[0], position, (float)Size, Spacing, *((Color*)&C));
        }
    ").


:- pragma foreign_proc(
    "C", measure_text(Text::in, Size::in, Width::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Width = MeasureText(Text, Size);
    ").


:- pragma foreign_proc(
    "C", measure_text_ex(
        RFont::in, Text::in, Size::in, Spacing::in, Width::out, Height::out,
        _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Font* pFont = (Font*)RFont;
        Vector2 out = MeasureTextEx(*pFont, Text, Size, Spacing);
        Width  = out.x;
        Height = out.y;
    ").

:- pragma foreign_proc(
    "C", measure_text_char(Char::in, Size::in, Width::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        char buffer[2] = {Char, 0};
        Width = MeasureText(&buffer[0], Size);
    ").


:- pragma foreign_proc(
    "C", measure_text_ex_char(
        RFont::in, Char::in, Size::in, Spacing::in, Width::out, Height::out,
        _IO0::di, _IO::uo
    ),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Font* pFont = (Font*)RFont;
        char buffer[2] = {Char, 0};
        Vector2 out = MeasureTextEx(*pFont, &buffer[0], Size, Spacing);
        Width  = out.x;
        Height = out.y;
    ").
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
%                           module: raudio
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pragma foreign_proc(
    "C", init_audio_device(_IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        InitAudioDevice();
    ").

:- pragma foreign_proc(
    "C", close_audio_device(_IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        CloseAudioDevice();
    ").

:- pragma foreign_proc(
    "C", is_audio_device_ready(Ready::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Ready = IsAudioDeviceReady() ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", set_master_volume(Volume::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        SetMasterVolume(Volume);
    ").

    %--------------------------------------------------%
    % SOUND
    %--------------------------------------------------%

:- pragma foreign_proc(
    "C", load_sound(Filename::in, RSound::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        void* p = MR_GC_malloc_uncollectable(sizeof(Sound));
        Sound sound = LoadSound(Filename);
        RSound = p;
        memcpy(p, &sound, sizeof(Sound));
    ").

:- pragma foreign_proc(
    "C", unload_sound(RSound::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Sound* pSound = (Sound*)RSound;
        UnloadSound(*pSound);
        MR_GC_free((void*)RSound);
    ").

:- pragma foreign_proc(
    "C", dump_sound(RSound::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Sound* s = (Sound*)RSound;
    ").

:- pragma foreign_proc(
    "C", is_sound_ready(RSound::in, Ready::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Sound* pSound = (Sound*)RSound;
        Ready = IsSoundReady(*pSound) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", play_sound(RSound::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Sound* pSound = (Sound*)RSound;
        PlaySound(*pSound);
    ").

:- pragma foreign_proc(
    "C", stop_sound(RSound::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Sound* pSound = (Sound*)RSound;
        StopSound(*pSound);
    ").

:- pragma foreign_proc(
    "C", is_sound_playing(RSound::in, Flag::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Sound* pSound = (Sound*)RSound;
        Flag = IsSoundPlaying(*pSound) ? MR_YES : MR_NO ;
    ").

    %--------------------------------------------------%
    % MUSIC
    %--------------------------------------------------%

:- pragma foreign_proc(
    "C", load_music_stream(Filename::in, RMusic::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        void* p = MR_GC_malloc_uncollectable(sizeof(Music));
        RMusic = p;
        Music music = LoadMusicStream(Filename);
        memcpy(p, &music, sizeof(Music));
    ").

:- pragma foreign_proc(
    "C", play_music_stream(RMusic::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        PlayMusicStream(*pMusic);
    ").

:- pragma foreign_proc(
    "C", is_music_ready(RMusic::in, Ready::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        Ready = IsMusicReady(*pMusic) ? MR_YES : MR_NO ;
    ").

:- pragma foreign_proc(
    "C", unload_music_stream(RMusic::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        UnloadMusicStream(*pMusic);
        MR_GC_free((void*)RMusic);
    ").

:- pragma foreign_proc(
    "C", is_music_stream_playing(RMusic::in, Playing::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        Playing = IsMusicStreamPlaying(*pMusic);
    ").

:- pragma foreign_proc(
    "C", stop_music_stream(RMusic::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        StopMusicStream(*pMusic);
    ").

:- pragma foreign_proc(
    "C", update_music_stream(RMusic::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        UpdateMusicStream(*pMusic);
    ").

:- pragma foreign_proc(
    "C", pause_music_stream(RMusic::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        PauseMusicStream(*pMusic);
    ").

:- pragma foreign_proc(
    "C", resume_music_stream(RMusic::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        ResumeMusicStream(*pMusic);
    ").

:- pragma foreign_proc(
    "C", seek_music_stream(RMusic::in, Pos::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        SeekMusicStream(*pMusic, Pos);
    ").

:- pragma foreign_proc(
    "C", set_music_volume(RMusic::in, Vol::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        SetMusicVolume(*pMusic, Vol);
    ").

:- pragma foreign_proc(
    "C", set_music_pan(RMusic::in, Pan::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        SetMusicPan(*pMusic, Pan);
    ").

:- pragma foreign_proc(
    "C", get_music_time_length(RMusic::in, Val::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        Val = GetMusicTimeLength(*pMusic);
    ").

:- pragma foreign_proc(
    "C", get_music_time_played(RMusic::in, Val::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Music* pMusic = (Music*)RMusic;
        Val = GetMusicTimePlayed(*pMusic);
    ").

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
% Misc. functions.
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pragma foreign_proc(
    "C", get_random_value(Min::in, Max::in, Rnd::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Rnd = GetRandomValue(Min, Max);
    ").

:- pragma foreign_proc(
    "C", get_random_fvalue(Min::in, Max::in, Rnd::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Rnd = (float)GetRandomValue(Min, Max);
    ").

:- pragma foreign_proc(
    "C", get_random_fvaluef(Min::in, Max::in, Rnd::out, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        Rnd = (float)GetRandomValue((int)Min, (int)Max);
    ").

:- pragma foreign_proc(
    "C", set_random_seed(Seed::in, _IO0::di, _IO::uo),
    [ promise_pure, will_not_call_mercury, will_not_throw_exception
    , will_not_modify_trail, thread_safe, does_not_affect_liveness
    , tabled_for_io],
    "
        SetRandomSeed(Seed);
    ").

%----------------------------------------------------------------------------%

    % Color management.
    %
color_rgba(lightgray , 200u8, 200u8, 200u8, 255u8).
color_rgba(gray      , 130u8, 130u8, 130u8, 255u8).
color_rgba(darkgray  , 80u8, 80u8, 80u8, 255u8).
color_rgba(yellow    , 253u8, 249u8, 0u8, 255u8).
color_rgba(gold      , 255u8, 203u8, 0u8, 255u8).
color_rgba(orange    , 255u8, 161u8, 0u8, 255u8).
color_rgba(pink      , 255u8, 109u8, 194u8, 255u8).
color_rgba(red       , 230u8, 41u8, 55u8, 255u8).
color_rgba(maroon    , 190u8, 33u8, 55u8, 255u8).
color_rgba(green     , 0u8, 228u8, 48u8, 255u8).
color_rgba(lime      , 0u8, 158u8, 47u8, 255u8).
color_rgba(darkgreen , 0u8, 117u8, 44u8, 255u8).
color_rgba(skyblue   , 102u8, 191u8, 255u8, 255u8).
color_rgba(blue      , 0u8, 121u8, 241u8, 255u8).
color_rgba(darkblue  , 0u8, 82u8, 172u8, 255u8).
color_rgba(purple    , 200u8, 122u8, 255u8, 255u8).
color_rgba(violet    , 135u8, 60u8, 190u8, 255u8).
color_rgba(darkpurple, 112u8, 31u8, 126u8, 255u8).
color_rgba(beige     , 211u8, 176u8, 131u8, 255u8).
color_rgba(brown     , 127u8, 106u8, 79u8, 255u8).
color_rgba(darkbrown , 76u8, 63u8, 47u8, 255u8).
color_rgba(white     , 255u8, 255u8, 255u8, 255u8).
color_rgba(black     , 0u8, 0u8, 0u8, 255u8).
color_rgba(blank     , 0u8, 0u8, 0u8, 0u8).
color_rgba(magenta   , 255u8, 0u8, 255u8, 255u8).
color_rgba(raywhite  , 245u8, 245u8, 245u8, 255u8).


    % Map a 'color' into RGBA values.
    %
color_parts(rgb(R, G, B), R, G, B, 255u8).
color_parts(rgba(R, G, B, A), R, G, B, A).
color_parts(color(C), R, G, B, A) :-
    color_rgba(C, R, G, B, A).

    % Return a uint32 from an RGBA instance.
    %
:- pragma inline(to_rgba/1).

to_rgba(C) = RGBA :-
    color_parts(C, R, G, B, A),
    RGBA = uint32.from_bytes_le(R, G, B, A).

:- pragma inline(with_a/2).

with_a(C, Alpha) = RGBA :-
    color_parts(C, R, G, B, _),
    RGBA = uint32.from_bytes_le(R, G, B, Alpha).

:- pragma inline(with_af/2).

with_af(C, Alpha) = RGBA :-
    color_parts(C, R, G, B, _),
    RGBA = uint32.from_bytes_le(
        R, G, B,
        uint8.cast_from_int(
            float.floor_to_int(Alpha)
        )
    ).


:- pragma foreign_enum("C", config_flags/0, [
    vsync_hint               - "FLAG_VSYNC_HINT",
    fullscreen_mode          - "FLAG_FULLSCREEN_MODE",
    window_resizable         - "FLAG_WINDOW_RESIZABLE",
    window_undecorated       - "FLAG_WINDOW_UNDECORATED",
    window_hidden            - "FLAG_WINDOW_HIDDEN",
    window_minimized         - "FLAG_WINDOW_MINIMIZED",
    window_maximised         - "FLAG_WINDOW_MAXIMIZED",
    window_unfocused         - "FLAG_WINDOW_UNFOCUSED",
    window_topmost           - "FLAG_WINDOW_TOPMOST",
    window_always_run        - "FLAG_WINDOW_ALWAYS_RUN",
    window_transparent       - "FLAG_WINDOW_TRANSPARENT",
    window_highdpi           - "FLAG_WINDOW_HIGHDPI",
    window_mouse_passthrough - "FLAG_WINDOW_MOUSE_PASSTHROUGH",
    window_msaa_4x_hint      - "FLAG_MSAA_4X_HINT",
    window_interlaced_hint   - "FLAG_INTERLACED_HINT"
]).

:- pragma foreign_enum("C", log_level/0, [
    log_all     - "LOG_ALL",
    log_trace   - "LOG_TRACE",
    log_debug   - "LOG_DEBUG",
    log_info    - "LOG_INFO",
    log_warning - "LOG_WARNING",
    log_error   - "LOG_ERROR",
    log_fatal   - "LOG_FATAL",
    log_none    - "LOG_NONE"
]).


:- pragma foreign_enum("C", mouse_button/0, [
    btn_left    - "MOUSE_BUTTON_LEFT",
    btn_right   - "MOUSE_BUTTON_RIGHT",
    btn_middle  - "MOUSE_BUTTON_MIDDLE",
    btn_side    - "MOUSE_BUTTON_SIDE",
    btn_extra   - "MOUSE_BUTTON_EXTRA",
    btn_forward - "MOUSE_BUTTON_FORWARD",
    btn_back    - "MOUSE_BUTTON_BACK"
]).

:- pragma foreign_enum("C", mouse_cursor/0, [
    cursor_default - "MOUSE_CURSOR_DEFAULT",
    cursor_arrow - "MOUSE_CURSOR_ARROW",
    cursor_ibeam - "MOUSE_CURSOR_IBEAM",
    cursor_crosshair - "MOUSE_CURSOR_CROSSHAIR",
    cursor_pointing_hand - "MOUSE_CURSOR_POINTING_HAND",
    cursor_resize_ew - "MOUSE_CURSOR_RESIZE_EW",
    cursor_resize_ns - "MOUSE_CURSOR_RESIZE_NS",
    cursor_resize_nwse - "MOUSE_CURSOR_RESIZE_NWSE",
    cursor_resize_nesw - "MOUSE_CURSOR_RESIZE_NESW",
    cursor_resize_all - "MOUSE_CURSOR_RESIZE_ALL",
    cursor_not_allowed - "MOUSE_CURSOR_NOT_ALLOWED"
]).

:- pragma foreign_enum("C", key_code/0, [
    key_null - "KEY_NULL",
    % Alphanumeric keys
    key_apostrophe - "KEY_APOSTROPHE",
    key_comma - "KEY_COMMA",
    key_minus - "KEY_MINUS",
    key_period - "KEY_PERIOD",
    key_slash - "KEY_SLASH",
    key_zero - "KEY_ZERO",
    key_one - "KEY_ONE",
    key_two - "KEY_TWO",
    key_three - "KEY_THREE",
    key_four - "KEY_FOUR",
    key_five - "KEY_FIVE",
    key_six - "KEY_SIX",
    key_seven - "KEY_SEVEN",
    key_eight - "KEY_EIGHT",
    key_nine - "KEY_NINE",
    key_semicolon - "KEY_SEMICOLON",
    key_equal - "KEY_EQUAL",
    key_a - "KEY_A",
    key_b - "KEY_B",
    key_c - "KEY_C",
    key_d - "KEY_D",
    key_e - "KEY_E",
    key_f - "KEY_F",
    key_g - "KEY_G",
    key_h - "KEY_H",
    key_i - "KEY_I",
    key_j - "KEY_J",
    key_k - "KEY_K",
    key_l - "KEY_L",
    key_m - "KEY_M",
    key_n - "KEY_N",
    key_o - "KEY_O",
    key_p - "KEY_P",
    key_q - "KEY_Q",
    key_r - "KEY_R",
    key_s - "KEY_S",
    key_t - "KEY_T",
    key_u - "KEY_U",
    key_v - "KEY_V",
    key_w - "KEY_W",
    key_x - "KEY_X",
    key_y - "KEY_Y",
    key_z - "KEY_Z",
    key_left_bracket     - "KEY_LEFT_BRACKET",
    key_backslash        - "KEY_BACKSLASH",
    key_right_bracket    - "KEY_RIGHT_BRACKET",
    key_grave            - "KEY_GRAVE",
    % Function keys
    key_space            - "KEY_SPACE",
    key_escape           - "KEY_ESCAPE",
    key_enter            - "KEY_ENTER",
    key_tab              - "KEY_TAB",
    key_backspace        - "KEY_BACKSPACE",
    key_insert           - "KEY_INSERT",
    key_delete           - "KEY_DELETE",
    key_right            - "KEY_RIGHT",
    key_left             - "KEY_LEFT",
    key_down             - "KEY_DOWN",
    key_up               - "KEY_UP",
    key_page_up          - "KEY_PAGE_UP",
    key_page_down        - "KEY_PAGE_DOWN",
    key_home             - "KEY_HOME",
    key_end              - "KEY_END",
    key_caps_lock        - "KEY_CAPS_LOCK",
    key_scroll_lock      - "KEY_SCROLL_LOCK",
    key_num_lock         - "KEY_NUM_LOCK",
    key_print_screen     - "KEY_PRINT_SCREEN",
    key_pause            - "KEY_PAUSE",
    key_f1 - "KEY_F1",
    key_f2 - "KEY_F2",
    key_f3 - "KEY_F3",
    key_f4 - "KEY_F4",
    key_f5 - "KEY_F5",
    key_f6 - "KEY_F6",
    key_f7 - "KEY_F7",
    key_f8 - "KEY_F8",
    key_f9 - "KEY_F9",
    key_f10 - "KEY_F10",
    key_f11 - "KEY_F11",
    key_f12 - "KEY_F12",
    key_left_shift - "KEY_LEFT_SHIFT",
    key_left_control - "KEY_LEFT_CONTROL",
    key_left_alt - "KEY_LEFT_ALT",
    key_left_super - "KEY_LEFT_SUPER",
    key_right_shift - "KEY_RIGHT_SHIFT",
    key_right_control - "KEY_RIGHT_CONTROL",
    key_right_alt - "KEY_RIGHT_ALT",
    key_right_super - "KEY_RIGHT_SUPER",
    key_kb_menu - "KEY_KB_MENU",
    % Keypad keys
    key_kp_0 - "KEY_KP_0",
    key_kp_1 - "KEY_KP_1",
    key_kp_2 - "KEY_KP_2",
    key_kp_3 - "KEY_KP_3",
    key_kp_4 - "KEY_KP_4",
    key_kp_5 - "KEY_KP_5",
    key_kp_6 - "KEY_KP_6",
    key_kp_7 - "KEY_KP_7",
    key_kp_8 - "KEY_KP_8",
    key_kp_9 - "KEY_KP_9",
    key_kp_decimal - "KEY_KP_DECIMAL",
    key_kp_divide - "KEY_KP_DIVIDE",
    key_kp_multiply - "KEY_KP_MULTIPLY",
    key_kp_subtract - "KEY_KP_SUBTRACT",
    key_kp_add - "KEY_KP_ADD",
    key_kp_enter - "KEY_KP_ENTER",
    key_kp_equal - "KEY_KP_EQUAL",
    % Android key buttons
    key_back - "KEY_BACK",
    key_menu - "KEY_MENU",
    key_volume_up - "KEY_VOLUME_UP",
    key_volume_down - "KEY_VOLUME_DOWN"
]).

%----------------------------------------------------------------------------%
:- end_module raylib.
%----------------------------------------------------------------------------%
