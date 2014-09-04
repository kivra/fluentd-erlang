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
-module(fluentd).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([send/2]).
-export([send_async/2]).

%%%_* Includes =========================================================
-include("fluentd.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-type label()  :: binary().
-type data()   :: binary().
-type reason() :: atom().

%%%_ * API -------------------------------------------------------------
-spec send(label(), data())       -> ok | {error, reason()}.
send(Label, Data) when is_binary(Label) andalso is_binary(Data) ->
    fluentd_server:send(Label, Data).

-spec send_async(label(), data()) -> ok | {error, reason()}.
send_async(Label, Data) when is_binary(Label) andalso is_binary(Data) ->
    fluentd_server:send_async(Label, Data).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
