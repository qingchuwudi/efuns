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
%%% @doc  ip的元组格式和一般格式相互转化
%%% @author  qingchuwudi <'bypf2009@vip.qq.com'> 
%%% @copyright 2014-2016 qingchuwudi <bypf2009@vip.qq.com>
%%% @reference http://erlycoder.com/63/erlang-ipv6-string-to-erlang-ip-tuple-and-erlang-ip-tuple-to-ipv6-string
%%% @end
%%% created|changed : 2016-01-12 15:35
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 
-module(ip46).

-export([
    get_ip/1,
    ip_tup2bin/1,
    ip6_str2tup/1,
    ip6_bin2tup/1,
    ip6_tup2str/1,
    ip6_tup2bin/1,
    ip4_tup2str/1,
    ip4_tup2bin/1,
    ip4_bin2tup/1
]).
-author("qingchuwudi").

%%@doc 从socket中提取ip
-spec get_ip(port()) -> binary() .
get_ip(Socket) -> 
	{ok,{IPTuple, _}} = inet:peername(Socket),
	ip_tup2bin(IPTuple).

%%@doc 把erl格式的ip转成一般格式
%%@end
%%param IPTuple: ipv4|ipv6
%%return <<"A.B.C.D">> | "A.B.C.D"
-spec ip_tup2bin(tuple()) -> binary() .
ip_tup2bin({_, _, _, _} = Tuple) ->
	ip4_tup2bin(Tuple);
ip_tup2bin({_,_,_,_,_,_,_,_} = Tuple) ->
	ip6_tup2bin(Tuple);
ip_tup2bin(_) ->
	<<"unkown host">>.

%%@doc 字符串的ipv6转成tuple
-spec ip6_str2tup([any()]) -> tuple() .
ip6_str2tup(Str) when is_list(Str)->
    case io_lib:fread("~16u:~16u:~16u:~16u:~16u:~16u:~16u:~16u", Str) of
    {ok, Lst, _} ->
        list_to_tuple(Lst);
    _ ->
        error
    end.

%%@doc 二进制的ipv6转成tuple
-spec ip6_bin2tup(binary()) -> tuple() .
ip6_bin2tup(Str) when is_binary(Str)->
    case io_lib:fread("~16u:~16u:~16u:~16u:~16u:~16u:~16u:~16u", binary_to_list(Str)) of
    {ok, Lst, _} ->
        list_to_tuple(Lst);
    _ ->
        error
    end.

%%@doc tuple的ipv6转成字符串
-spec ip6_tup2str(tuple()) -> [any()] .
ip6_tup2str(Tuple) when is_tuple(Tuple) ->
    lists:flatten(io_lib:format("~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B", tuple_to_list(Tuple))).
    
%%@doc tuple的ipv6转成二进制
-spec ip6_tup2bin(tuple()) -> binary() .
ip6_tup2bin(Tuple) when is_tuple(Tuple)->
    list_to_binary(io_lib:format("~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B", tuple_to_list(Tuple))).

%%@doc tuple的ipv4转成字符串
-spec ip4_tup2str(tuple()) -> list().
ip4_tup2str(Tuple) when is_tuple(Tuple) ->
	inet_parse:ntoa(Tuple).
%%@doc tuple的ipv4转成二进制
-spec ip4_tup2bin(tuple()) -> binary() .
ip4_tup2bin(Tuple) when is_tuple(Tuple) ->
	list_to_binary(inet_parse:ntoa(Tuple)).

%%@doc 二进制的ipv4转成tuple
-spec ip4_bin2tup(binary()) -> tuple() .
ip4_bin2tup(Str) when is_binary(Str)->
    case io_lib:fread("~u.~u.~u.~u", binary_to_list(Str)) of
    {ok, Lst, _} ->
        list_to_tuple(Lst);
    _ ->
        error
    end.