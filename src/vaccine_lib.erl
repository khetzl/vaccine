%%%-------------------------------------------------------------------
%%% @author Kristof Hetzl <kristof.hetzl@erlang-solutions.com>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(vaccine_lib).

-include("vaccine.hrl").

%% API
-export([ok_response/2,
         error_response/2]).

-export([error_halt/2]).

%%% API
%% @doc

ok_response(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    #ok_response{msg = Msg}.

error_response(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    #error_response{msg = Msg}.

%% TODO: check if args==[] should we use lists:flatten
error_halt(Format, Args) ->
    io:format(Format, Args),
    halt(1).

%%% Internal functions

