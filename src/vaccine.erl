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
run([RunNodeStr | CmdList]) ->
    RunNode = list_to_atom(RunNodeStr),
    ConnectNode = get_connected_node(RunNode),
    check_connection(ConnectNode, RunNode),
    try_cmd(RunNode, CmdList),
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
    Timeout = rpc_timeout(),
    try vaccine_inject:inject_all(Node, Mod, Timeout) of
        ok ->
            case rpc:call(Node, Mod, cmd, [Args], Timeout) of
                #ok_response{msg = undefined} ->
                    io:format("OK~n", []);
                #ok_response{msg = Msg} ->
                    io:format("~s~n", [Msg]);
                #error_response{msg = ErrorMsg} ->
                    io:format("ERROR: ~s~n", [ErrorMsg]);
                {badrpc, Reason} ->
                    ?ERROR_HALT("RPC call returned with error: ~p", [Reason])
            end
    catch
        E:R ->
            ?ERROR_HALT("Inject error: ~p:~p~n", [E,R])
    end,
    vaccine_inject:purge_all(Node, Mod, Timeout).

error_not_cmd(Cmd) ->
    ?ERROR_HALT("Module (~p) is not a vaccine command module~n", [Cmd]).

usage() ->
    ?ERROR_HALT("Not enough arguments, use vaccine --help for detailed description.").

get_connected_node(RunNodeName) ->
    %% Connected node is not necessarily the same as the node where we run the
    %% command. If not defined, connect directly to the node where the command
    %% will be invoked.
    application:get_env(vaccine, connect_node, RunNodeName).

check_connection(ConnectNode, RunNode) ->
    pong == net_adm:ping(ConnectNode) orelse
        ?ERROR_HALT("Unable to connect to node:~p (~p)~n", [ConnectNode]),
    ConnectNode == RunNode orelse pong == net_adm:ping(RunNode) orelse
        ?ERROR_HALT("Unable to connect to node:~p (~p)~n", [RunNode]).

rpc_timeout() ->
    application:get_env(vaccine, rpc_timeout, ?DEFAULT_RPC_TIMEOUT).
