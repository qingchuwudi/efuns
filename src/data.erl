%%% ------------------------------------------------------------------ 
%%% Licensed under the Apache License, Version 2.0 (the 'License');
%%%  you may not use this file except in compliance with the License.
%%%  You may obtain a copy of the License at
%%%
%%%      http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Copyright (c) 2014-2016 qingchuduwi <bypf2009@vip.qq.com>
%%%
%%%  Unless required by applicable law or agreed to in writing, software
%%%  distributed under the License is distributed on an 'AS IS' BASIS,
%%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%  See the License for the specific language governing permissions and
%%%  limitations under the License.
%%%
%%% @doc  数据处理，如：大小端转换 
%%% @author  qingchuduwi <'bypf2009@vip.qq.com'> 
%%% @copyright 2014-2016 qingchuduwi <bypf2009@vip.qq.com>
%%% @end
%%% created|changed : 2016-03-23 10:09
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 
-module(data).
-export([
	reverse/1,	% 大小端反转，类型不变
	b2l_uint/1,	% 二进制转成小端无符号整型
	int2bin/1,  % 整型按二进制存储
	b2hex/1,	% 二进制转成16进制，按字符串输出
	h2bin/1,
	hex2int/1
]).
-author("qingchuduwi").

-type flag()  :: binary | list.
-type mdata() :: binary() | list() | integer().


%%@doc 二进制转成小端无符号整型
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

-spec hex2int(Any) -> Number when
		Any :: list() | binary(),
		Number :: integer() .
hex2int(Any) when is_list(Any) ->
	list_to_integer(Any, 16);
hex2int(Any) when is_binary(Any) ->
	list_to_integer(binary_to_list(Any), 16).