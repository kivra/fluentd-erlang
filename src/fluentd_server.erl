%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2012-2014 Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Fluentd Erlang client
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(fluentd_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
%% fluentd_server Api
-export([start_link/1]).
-export([stop/0]).

%% fluentd Api
-export([send/2]).
-export([send_async/2]).

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%%%_* Includes =========================================================
-include("fluentd.hrl").

%%%_* Macros ===========================================================
%% Make sure we time out internally before our clients time out.
-define(TIMEOUT,  120000).         % gen_server:call/3
-define(TCP_OPTS, [ binary
                  , {active, true}
                  , {sndbuf, 0}
                  , {buffer, 0}
                  , {packet, raw}
                  ]).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s,
        { ip                     :: inet:ip_address() %\ Fluentd
        , port                   :: inet:port_number()%/ Agent
        , sock=undefined         :: gen_tcp:socket()  % Socket
        , enabled=true           :: boolean()         % Fluentd Enabled
        , auto_reconnect=false   :: boolean()         % true, auto reconnect
                                                      % false, exit on conn fail
        , reconnect_interval=100 :: non_neg_integer() % Reconnect Interval
        }).

%%%_ * API -------------------------------------------------------------
send(Label, Data)       -> call({send, {Label, Data}}).
send_async(Label, Data) -> cast({send, {Label, Data}}).

start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
stop()           -> gen_server:call(?MODULE, stop).

%%%_ * gen_server callbacks --------------------------------------------
init(Args)                     ->
    process_flag(trap_exit, true),
    IP         = s2_env:get_arg(Args, ?APP, fluentd_ip,         "127.0.0.1"),
    Port       = s2_env:get_arg(Args, ?APP, fluentd_port,       24224),
    Reconnect  = s2_env:get_arg(Args, ?APP, auto_reconnect,     true),
    ReInterval = s2_env:get_arg(Args, ?APP, reconnect_interval, 100),
    Enabled    = s2_env:get_arg(Args, ?APP, enabled,            true),
    State      = #s{ ip=IP
                   , port=Port
                   , enabled=Enabled
                   , auto_reconnect=Reconnect
                   , reconnect_interval=ReInterval
                   },
    case Enabled of
        false -> {ok, State};
        true  ->
            case Reconnect of
                true  -> self() ! reconnect, {ok, State};
                false -> connect(State)
            end
    end.

handle_call(stop, _, S) ->
    {stop, stopped, gen_tcp:close(S#s.sock), S}; %kills linked
handle_call({send, {_, _}}, _, #s{enabled=false} = S) ->
    {reply, ok, S};
handle_call({send, {Label, Data}}, _, #s{sock=Socket} = S) ->
    {reply, do_send(Socket, pack_data(Label, Data)), S}.

handle_cast({send, {_, _}}, #s{enabled=false} = S) ->
    {noreply, S};
handle_cast({send, {Label, Data}}, #s{sock=Socket} = S) ->
    do_send(Socket, pack_data(Label, Data)),
    {noreply, S};
handle_cast(_Req, S) ->
    {stop, bad_cast, S}.

handle_info(reconnect, State0) ->
    case connect(State0) of
        {ok, State1} -> {noreply, State1};
        {error, _}   -> disconnect(State0)
    end;
handle_info({'EXIT', Pid, disconnected}, State) ->
    ?critical("~p EXIT disconnected: ~p", [?MODULE, Pid]),
    ?increment([exits, disconnected]),
    disconnect(State);
handle_info({'EXIT', Pid, Rsn}, State) ->
    ?error("~p EXIT ~p: ~p", [?MODULE, Pid, Rsn]),
    ?increment([exits, other]),
    disconnect(State);
handle_info({tcp_closed, Sock}, State) ->
    ?critical("~p EXIT disconnected: ~p", [?MODULE, Sock]),
    ?increment([exits, disconnected]),
    disconnect(State);
handle_info(Info, State) ->
    ?warning("~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, S, _Extra) -> {ok, S}.
terminate(_Rsn, #s{})           -> ok.

%%%_* Private functions ================================================
call(Req)    -> call(Req, ?TIMEOUT).
call(Req, T) -> gen_server:call(?MODULE, Req, T).

cast(Req)    -> gen_server:cast(?MODULE, Req).

do_send(undefined, _) -> {error, disconnected};
do_send(Sock, Req)    -> ok = gen_tcp:send(Sock, Req).

connect(#s{ ip=IP, port=Port }=S) ->
    case gen_tcp:connect(IP, Port, ?TCP_OPTS) of
        {ok, Socket} -> {ok, S#s{ sock=Socket }};
        {error, _}=E -> E
    end.

disconnect(#s{sock=Socket} = State0) ->
    ok     = maybe_close_socket(Socket),
    State1 = State0#s{ sock=undefined },
    case State1#s.auto_reconnect of
        false -> {stop, disconnected, State1};
        true  ->
            erlang:send_after(State1#s.reconnect_interval, self(), reconnect),
            {noreply, State1}
    end.

maybe_close_socket(undefined) -> ok;
maybe_close_socket(Socket)    -> gen_tcp:close(Socket).

%% @doc Pack Payload according to Fluentd-spec using MessagePack
pack_data(Label, Data) ->
    {M, S, _} = os:timestamp(),
    msgpack:pack([Label, (M*1000000)+S, Data]).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
