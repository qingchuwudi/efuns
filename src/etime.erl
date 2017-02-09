%%% ------------------------------------------------------------------ 
%%% Licensed under the Apache License, Version 2.0 (the 'License');
%%%  you may not use this file except in compliance with the License.
%%%  You may obtain a copy of the License at
%%%
%%%      http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Copyright (c) 2016-2017 qingchuwudi <bypf2009@vip.qq.com>
%%%
%%%  Unless required by applicable law or agreed to in writing, software
%%%  distributed under the License is distributed on an 'AS IS' BASIS,
%%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%  See the License for the specific language governing permissions and
%%%  limitations under the License.
%%%
%%% @doc  Erlang日期与时间处理
%%% @author  qingchuwudi <'bypf2009@vip.qq.com'> 
%%% @copyright 2016-2017 qingchuwudi <bypf2009@vip.qq.com>
%%% @reference http://www.cnblogs.com/me-sa/archive/2012/05/17/erlang-calendar-date-time.html
%%% @end
%%% created|changed : 2016-03-21 15:15
%%% coding : utf-8 
%%% ------------------------------------------------------------------
-module(etime).

-author("qingchuwudi").

-export ([
    timestamp2iso/1,    
    local2utc/0,
    local2utc/1,     
    now2utc/1,          
    now2local/0,        
    now2local/1,        
    datetime2stamp/1,   
    return_2columns/1,
    get_current_time/0,
    day/1,
    month_to_list/1,
    list_to_month/1
]).

-export ([
    unixtime/0,
    list2fi/1,
    rand/2,
    difftime/1
]).

%%@doc 取得当前的unix时间戳（秒）
-spec unixtime() -> TimeStamp when
        TimeStamp :: non_neg_integer() .
unixtime() ->
    os:system_time(1).

%%@doc 列表转换成整型、浮点型
-spec list2fi(TimeString) -> TimeNumber when
        TimeString :: list(),
        TimeNumber :: integer() | float() .
list2fi(AnyList) when is_list(AnyList) ->
    case catch list_to_integer(AnyList) of
        {'EXIT', _} ->
            case catch list_to_float(AnyList) of
                {'EXIT', _} -> throw(undefined);
                Float -> Float
            end;
        Int -> Int
    end.
    
%%@doc 产生一个介于Min到Max之间的随机整数
-spec rand(Min, Max) -> Number when
        Min :: integer(),
        Max :: integer(),
        Number :: integer() .
rand(Same, Same) -> Same;
rand(Min, Max) when Max < Min -> 0;
rand(Min, Max) ->
    %% 以保证不同进程都可取得不同的种子
    case get("rand_seed") of
        undefined ->
            rand:seed(exsplus, os:timestamp()),
            put("rand_seed", 1);
        _ -> skip
    end,
    M = Min - 1,
    rand:uniform(Max - M) + M.
    
%%@doc 计算到当前时刻的时间差
%% Startime 时间戳
-spec difftime(StartTime) -> DiffTime when
        StartTime :: non_neg_integer(),
        DiffTime :: non_neg_integer().
difftime(StartTime) ->
    os:system_time(1) - StartTime.

%%%
%%% 时间格式化
%%%

%%@doc 时间戳转iso字符串
timestamp2iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second])).

%%@doc 本地时间转UTC格式，精确到秒
local2utc() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
            [Year, Month, Day, Hour, Minute, Second])).

%%@doc 本地时间转UTC格式，精确到毫秒
local2utc(1) ->
    {_, _, MicroSecs} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
            [Year, Month, Day, Hour, Minute, Second, MicroSecs])).


%%doc 输入时间转UTC格式
now2utc({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
    calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
            [Year, Month, Day, Hour, Minute, Second, MicroSecs])).
    
%%@doc 时间戳转本地时
now2local() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    LocalTime = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    UTCTime = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    Seconds = calendar:datetime_to_gregorian_seconds(LocalTime) -
            calendar:datetime_to_gregorian_seconds(UTCTime),
    {{H, M, _}, Sign} = if
                Seconds < 0 ->
                {calendar:seconds_to_time(-Seconds), "-"};
                true ->
                {calendar:seconds_to_time(Seconds), "+"}
    end,
    {{Year, Month, Day}, {Hour, Minute, Second}} = LocalTime,
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0w~s~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second, MicroSecs, Sign, H, M])).

%%@doc 外部输入时间戳转本地时间
now2local({MegaSecs, Secs, MicroSecs}) ->
    LocalTime = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    UTCTime = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    Seconds = calendar:datetime_to_gregorian_seconds(LocalTime) -
            calendar:datetime_to_gregorian_seconds(UTCTime),
    {{H, M, _}, Sign} = if
                Seconds < 0 ->
                {calendar:seconds_to_time(-Seconds), "-"};
                true ->
                {calendar:seconds_to_time(Seconds), "+"}
    end,
    {{Year, Month, Day}, {Hour, Minute, Second}} = LocalTime,
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0w~s~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second, MicroSecs, Sign, H, M])).


%%@doc 日期转时间戳，日期格式：yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm}
%%@end
%% yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs}
datetime2stamp(TimeStr) ->
    case catch parse_datetime(TimeStr) of
    {'EXIT', _Err} ->
        undefined;
    TimeStamp ->
        TimeStamp
    end.

parse_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

%%@doc yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
    true ->
        Date1;
    _ ->
        false
    end.

%%@doc hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case string:str(Time, "Z") of
    0 ->
        parse_time_with_timezone(Time);
    _ ->
        [T | _] = string:tokens(Time, "Z"),
        {TT, MS} = parse_time1(T),
        {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
    case string:str(Time, "+") of
    0 ->
        case string:str(Time, "-") of
        0 ->
            false;
        _ ->
            parse_time_with_timezone(Time, "-")
        end;
    _ ->
        parse_time_with_timezone(Time, "+")
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = string:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
    "-" ->
        {TT, MS, -TZH, -TZM};
    "+" ->
        {TT, MS, TZH, TZM}
    end.

parse_timezone(TZ) ->
    [H, M] = string:tokens(TZ, ":"),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
         [] ->
         0;
         [Val] ->
         list_to_integer(string:left(Val, 6, $0))
     end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
      V = list_to_integer(L),
      if
          (V >= 0) and (V =< N) ->
          {V, B};
          true ->
          {false, false}
      end
      end, true, List).

%%%
%%%
%%%
%%@doc a function to format date/time properly (e.g. 09 instead of 9)
return_2columns(X) ->
    case length(X) of
        1 ->
            "0" ++ X;
        _ ->
            X
    end.

%%% 显然这里可以直接使用 io_lib:format("~2..0B", [X])

%%@doc returns date/time as a properly formatted string (e.g. "01-01-2000 12:12:12")
get_current_time() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    L = lists:map(fun(X) -> 
                          X2=integer_to_list(X), 
                          return_2columns(X2) 
                  end, 
                  [Y, M, D, H, Mi, S]
                 ),
    [Y2, M2, D2, H2, Mi2, S2] = L,
    Y2 ++ "-" ++ M2 ++ "-" ++ D2 ++ " " ++ H2 ++ ":" ++ Mi2 ++ ":" ++ S2.

%%%
%%% 
%%%
day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

month_to_list(1)  -> "Jan";
month_to_list(2)  -> "Feb";
month_to_list(3)  -> "Mar";
month_to_list(4)  -> "Apr";
month_to_list(5)  -> "May";
month_to_list(6)  -> "Jun";
month_to_list(7)  -> "Jul";
month_to_list(8)  -> "Aug";
month_to_list(9)  -> "Sep";
month_to_list(10) -> "Oct";
month_to_list(11) -> "Nov";
month_to_list(12) -> "Dec".

list_to_month("Jan") -> 1;
list_to_month("Feb") -> 2;
list_to_month("Mar") -> 3;
list_to_month("Apr") -> 4;
list_to_month("May") -> 5;
list_to_month("Jun") -> 6;
list_to_month("Jul") -> 7;
list_to_month("Aug") -> 8;
list_to_month("Sep") -> 9;
list_to_month("Oct") -> 10;
list_to_month("Nov") -> 11;
list_to_month("Dec") -> 12.
