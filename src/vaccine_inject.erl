%%%-------------------------------------------------------------------
%%% @author Kristof Hetzl <>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2016 by Kristof Hetzl <>
%%%-------------------------------------------------------------------
-module(vaccine_inject).

-include("vaccine.hrl").

%% API
-export([inject_all/3,
         purge_all/3]).

inject_all(Node, Mod, Timeout) ->
    lists:foreach(fun(M) ->
                          inject_module(Node, M, Timeout)
                  end, [Mod | ?PLUGIN_MODULES]).

purge_all(Node, Mod, Timeout) ->
    lists:foreach(fun(M) ->
                          purge_module(Node, M, Timeout)
                  end, [Mod | ?PLUGIN_MODULES]).
    
    

%%% API
%% @doc

%%% Internal functions
inject_module(Node, Mod, Timeout) ->
    case rpc:call(Node, code, which, [Mod], Timeout) of
        non_existing ->
            compile_n_load(Node, Mod, Timeout);
        _  ->
            ok
    end.

compile_n_load(Node, Mod, Timeout) ->
    CompileInfo = Mod:module_info(compile),
    Path = proplists:get_value(source, CompileInfo),
    {ok, Mod, BEAM} = compile:file(Path, [binary, debug_info]),
    case rpc:call(Node, code, load_binary, [Mod, Path, BEAM], Timeout) of
        {module, Mod} ->
            ok;
        _ ->
            throw(remote_load_error)
    end.

purge_module(Node, Mod, Timeout) ->
    case rpc:call(Node, code, delete, [Mod], Timeout) of
        true ->
            ok;
        false ->
            %% Some kind of re-try solutions should be done here
            throw({failed_to_delete, Mod})
    end,
    rpc:call(Node, code, purge, [Mod], Timeout).
            