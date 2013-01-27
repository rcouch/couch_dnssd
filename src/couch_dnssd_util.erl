-module(couch_dnssd_util).

-export([get_unix_timestamp/1]).

%% @doc get_unix_timestamp
%% @spec
%% @output
get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).
