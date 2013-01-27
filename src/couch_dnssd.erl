%% @doc discover nodes using dnnsd

-module(couch_dnssd).
-behaviour(gen_server).

-export([start_link/0]).
-export([list_nodes/0, list_nodes/2,
         monitor/0, unmonitor/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DNSSD_BYNAME, couch_dnssd_byname).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc list discovered nodes
list_nodes() ->
    DefaultFun = fun(Node, Acc) ->
            [Node |Acc]
    end,
    list_nodes(DefaultFun, []).

%% @doc return Acc.
list_nodes(Fun, Acc0) ->
    WrapperFun = fun({Name, Node}, Acc) ->
            Fun({Name, Node}, Acc)
    end,
    ets:foldl(WrapperFun, Acc0, ?DNSSD_BYNAME).

monitor() ->
    gproc:reg({p, l, {?MODULE, dnssd_browse}}).

unmonitor() ->
    gproc:unreg({p,l, {?MODULE, dnssd_browse}}).

%% --------------------
%% gen_server callbacks
%% --------------------

init([]) ->
    ?DNSSD_BYNAME = ets:new(?DNSSD_BYNAME, [ordered_set, named_table,
                                            public]),
    {ok, BrowseRef} = dnssd:browse("_couch._tcp"),
    {ok, BrowseRef}.

handle_call(_Request, _From, Ref) ->
    {noreply, Ref}.

handle_cast(_Msg, Ref) ->
    {noreply, Ref}.

handle_info({dnssd, Ref, {browse, BrowseType, BrowseInfo}}, Ref) ->
    spawn_link(fun() -> resolve(BrowseType, BrowseInfo) end),
    {noreply, Ref};
handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, Ref) ->
    ok = dnssd:stop(Ref),
    ok.

code_change(_OldVsn, Ref, _Extra) ->
    {ok, Ref}.


%% -----------------------
%% private functions
%% -----------------------

resolve(BrowseType, {Name, Type, Domain}=BrowseInfo) ->
    case dnssd:resolve_sync(Name, Type, Domain) of
        {ok, NodeInfo} ->
            Time = couch_dnssd_util:get_unix_timestamp(erlang:now()),
            case BrowseType of
                add ->
                    ets:insert(?DNSSD_BYNAME, {Name, {Time, NodeInfo}}),
                    notify({dnssd_add, Name});
                remove ->
                    case ets:lookup(?DNSSD_BYNAME, Name) of
                    [{Name, _}] ->
                        ets:delete(?DNSSD_BYNAME, Name),
                        notify({dnssd_remove, Name});
                    _ ->
                        ok
                    end
            end;
        Error ->
            lager:error("error resolving ~p : ~p", [BrowseInfo, Error])
    end.

notify(Msg) ->
    Key = {?MODULE, dnssd_browse},
    gproc:send({p, l, Key}, Msg).
