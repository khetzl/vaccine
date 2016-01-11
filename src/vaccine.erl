%%%-------------------------------------------------------------------
%%% @author Kristof Hetzl <>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(vaccine).

-include("vaccine.hrl").

%% API
-export([run/1]).

%%% API
%% @doc
run(Args) when length(Args) < 2 ->
    usage();
run([NodeNameStr | CmdList]) ->
    NodeName = list_to_atom(NodeNameStr),
    case net_adm:ping(NodeName) of
        pong ->
            try_cmd(NodeName, CmdList);
        Else ->
            ?ERROR_HALT("Unable to connect to node:~p (~p)~n", [NodeName, Else])
    end,
    halt(0).

%%% Internal functions
try_cmd(Node, [Cmd | Args]) ->
    Mod = list_to_atom("vcn_" ++ Cmd),
    try Mod:module_info(exports) of
        ExportList when is_list(ExportList) ->
            proplists:is_defined(cmd, ExportList) orelse error_not_cmd(Mod),
            inject_n_run_cmd(Node, Mod, Args)
    catch
        error:undef ->
            ?ERROR_HALT("No command module found with name ~p~n", [Mod])
    end.

inject_n_run_cmd(Node, Mod, Args) ->
    %%FIXME: configurable timeout
    Timeout = 20000,
    vaccine_inject:inject_all(Node, Mod, Timeout),
    case rpc:call(Node, Mod, cmd, [Args], Timeout) of
        #ok_response{msg = undefined} ->
            io:format("OK~n", []);
        #ok_response{msg = Msg} ->
            io:format("~s~n", [Msg]);
        #error_response{msg = ErrorMsg} ->
            io:format("ERROR: ~s~n", [ErrorMsg]);
        {badrpc, Reason} ->
            ?ERROR_HALT("RPC call returned with error: ~p", [Reason])
            
    end,
    vaccine_inject:purge_all(Node, Mod, Timeout).

error_not_cmd(Cmd) ->
    ?ERROR_HALT("Module (~p) is not a vaccine command module~n", [Cmd]).

usage() ->
    ?ERROR_HALT("Not enough arguments, use vaccine --help for detailed description.").
