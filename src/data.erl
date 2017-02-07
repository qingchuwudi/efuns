%%% ------------------------------------------------------------------ 
%%% Licensed under the Apache License, Version 2.0 (the 'License');
%%%  you may not use this file except in compliance with the License.
%%%  You may obtain a copy of the License at
%%%
%%%      http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Copyright (c) 2014-2016 dwg <bypf2009@vip.qq.com>
%%%
%%%  Unless required by applicable law or agreed to in writing, software
%%%  distributed under the License is distributed on an 'AS IS' BASIS,
%%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%  See the License for the specific language governing permissions and
%%%  limitations under the License.
%%%
%%% @doc  数据处理，如：大小端转换 
%%% @author  dwg <'bypf2009@vip.qq.com'> 
%%% @copyright 2014-2016 dwg <bypf2009@vip.qq.com>
%%% @end
%%% created|changed : 2016-03-23 10:09
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 
-module(data).
-export([
	reverse/1,	% 大小端反转，类型不变
	b2l_uint/1,	% 二进制转成小端无符号整型
	base64en/1, % Json的键值列表打包
	base64de/1, % Json键值列表解包
	int2bin/1,  % 整型按二进制存储
	b2hex/1,	% 二进制转成16进制，按字符串输出
	h2bin/1,
	hex2int/1	% 
	% map_get/2	% 在map中获取满足条件的某些值
]).
-author("dwg").

-type flag()  :: binary | list.
-type mdata() :: binary() | list() | integer().

%% 16进制的串转成10进制
% binary_to_integer(<<"af">>,16).
% list_to_integer("af",16).

%%@doc LoRa发送的几个数据是大端，要转成小端
-spec b2l_uint(binary()) -> integer().
b2l_uint(IoData) when is_binary(IoData) ->
	binary:decode_unsigned(IoData, little).

%%@doc 数据转成16进制，lora中用来处理ID
-spec b2hex(binary() | list()) -> list().
b2hex(IoData) when is_binary(IoData) ->
	<< <<Y>> || <<X:4>> <= IoData, Y <- integer_to_list(X,16)>>;
b2hex(IoData) when is_list(IoData) ->
	b2hex(list_to_binary(IoData)).

%%@doc 16进制转成10进制，lora中用来处理ID
-spec h2bin(binary() | list()) -> binary() .
h2bin(IoData) when is_binary(IoData) ->
	<<<<Z>> || <<X:8,Y:8>> <= IoData,Z <- [binary_to_integer(<<X,Y>>,16)]>>;
h2bin(IoData) ->
	h2bin(list_to_binary(IoData)).

%%@doc 数据大小端反转,整型可用"ineger-unsigned-little"获取
%%@end
%%param IoData:待反转的数据 
%%return 反转后的 IoData
-spec reverse(mdata()) -> mdata().
reverse(IoData) when is_binary(IoData) ->
	reverse(binary, IoData);
reverse(IoData) when is_list(IoData) ->
	reverse(list, IoData);
reverse(IoData) when is_integer(IoData) ->
	reverse(integer, IoData).

%%@private 反转二进制数据
%% 大小端转换
-spec reverse(flag(), iodata()) -> iodata() .
reverse(binary, Binary) ->
	reverse(binary, Binary, <<>>);
reverse(list, List) ->
	reverse(list, List, []);
reverse(integer, Integer) ->
	case int_len(Integer) of
		8 -> Integer; % 单字节数据没有大小端之分
		Bits ->
			<<Result:Bits>> = 
			reverse(binary, <<Integer:Bits>>, <<>>),
			Result
	end.

%%@private 反转大小端
%% 二进制反转
reverse(binary, <<H, Rest/binary>>, Bin) ->
	reverse(binary, Rest, <<H, Bin/binary>>);
reverse(binary, <<>>, Bin) -> Bin;

%%@private 列表反转
reverse(list, [H | Rest], List) ->
	reverse(list, Rest, [H | List]);
reverse(list, [], List) -> List.


%%@doc 整型存入二进制
int2bin(Integer) ->
	Bytes = int_len(Integer),
	<<Integer:(Bytes * 8)>>.


%%@doc 计算整型字节数
%%@end
%%param Integer:带计算数据 
%%return 字节数
-spec int_len(integer()) -> integer() .
int_len(0) -> 8;
int_len(Integer) ->
	int_len(Integer, 0).

int_len(0, Len) -> Len * 8;
int_len(Integer, Len) ->
	int_len(Integer bsr 8, Len + 1).

%%@doc Json键值列表打包
base64en(JsonList) when is_list(JsonList) ->
	base64en(JsonList, []).

base64en([{K, V} | T], NewList) when is_binary(V) ->
	base64en(T, [{K, base64:encode(V)} | NewList]);
base64en([H | T], NewList) ->
	base64en(T, [H| NewList]);
base64en([], NewList) ->
	lists:reverse(NewList).


%%@doc Json键值列表解包
base64de(JsonList) when is_list(JsonList) ->
	base64de(JsonList, []).

base64de([{K, V} | T], NewList) when is_binary(V) ->
	base64de(T, [{K, base64:decode(V)} | NewList]);
base64de([H | T], NewList) ->
	base64de(T, [H | NewList]);
base64de([], NewList) ->
	lists:reverse(NewList).

hex2int(Any) when is_list(Any) ->
	list_to_integer(Any, 16);
hex2int(Any) when is_binary(Any) ->
	list_to_integer(binary_to_list(Any), 16).
% %% 在Map中取出Key在List中的数据
% -spec map_get(list(), map()) -> map() .
% map_get(List, Map) when is_list(List) andalso is_map(Map) ->
% 	maps:filter(fun(K, _) ->
% 		lists:member(K, List)
% 	end, Map)