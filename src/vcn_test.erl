%%%-------------------------------------------------------------------
%%% @author Kristof Hetzl <>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2016 by Kristof Hetzl <>
%%%-------------------------------------------------------------------
-module(vcn_test).

-behaviour(vaccine_command).

-include("vaccine.hrl").

%% API
-export([cmd/1]).

%%% API

%% @doc
cmd(["error"]) ->
    ?ERROR_RESPONSE("Node ~p was able to return this error message", [node()]);
cmd(_) ->
    ?OK_RESPONSE("Node ~p ready to receive vaccine commands", [node()]).

%%% Internal functions

