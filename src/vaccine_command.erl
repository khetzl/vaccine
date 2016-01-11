%%%-------------------------------------------------------------------
%%% @author Kristof Hetzl <kristof.hetzl@erlang-solutions.com>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc Vaccine command behaviour definition.
%%% 
%%% Created :  7 Jan 2016 by Kristof Hetzl <>
%%%-------------------------------------------------------------------
-module(vaccine_command).

-include("vaccine.hrl").

-callback cmd(Arguments :: [list(atom())]) -> ok_response() | error_response().
