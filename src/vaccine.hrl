%%%-------------------------------------------------------------------
%%% @author Kristof Hetzl <>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
%% Types

%% Records
-record(response, {type :: atom(),
                   msg :: list()}).
-type response() :: #response{}.

%% Macros
-define(DEFAULT_RPC_TIMEOUT, 5000).

-define(ERROR_HALT(Format, Args), vaccine_lib:error_halt(Format, Args)).
-define(ERROR_HALT(Msg), vaccine_lib:error_halt(Msg, [])).

%% TODO: test how these macros would fail when there is a syntax error in
%%       the format or the argument.
-define(T_OK_RESPONSE, response_type_ok).
-define(T_ERROR_RESPONSE, response_type_error).

-define(OK_RESPONSE(Format, Args), vaccine_lib:ok_response(Format, Args)).
-define(ERROR_RESPONSE(Format, Args), vaccine_lib:error_response(Format, Args)).

%% List of modules neccesary for vaccine commands to operate properly
-define(PLUGIN_MODULES, [vaccine_lib]). 
