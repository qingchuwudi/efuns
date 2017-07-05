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
%%% @doc  数据处理，如：大小端转换 
%%% @author  qingchuwudi <'bypf2009@vip.qq.com'> 
%%% @copyright 2016-2017 qingchuwudi <bypf2009@vip.qq.com>
%%% @end
%%% created|changed : 2016-03-23 10:09
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 
-module(data).
-export([
	reverse/1,
	int2bin/1,
	b2hex/1,
	h2bin/1,
	hex2int/1
]).

-export ([list2fi/1, rand/2]).

-author("qingchuwudi").

-type flag()  :: binary | list.
-type mdata() :: binary() | list() | integer().

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

%%@private 二进制转成小端无符号整型
%% 原函数可用基础语法实现:
%% <<Uint64:64/ineger-unsigned-little>> = <<1,2,3,4,5,6,7,8>>.
%%@end

%%@doc 数据转成16进制，lora中用来处理ID
-spec b2hex(binary() | list()) -> list().
b2hex(IoData) when is_binary(IoData) ->
	<< <<Y>> || <<X:4>> <= IoData, Y <- integer_to_list(X,16)>>;
b2hex(IoData) when is_list(IoData) ->
	b2hex(list_to_binary(IoData)).

%%@doc 16进制转成10进制，lora中用来处理ID
-spec h2bin(binary() | list()) -> binary() .
h2bin(IoData) when is_binary(IoData) ->
	<< <<(binary_to_integer(<<X,Y>>,16))>> || <<X:8,Y:8>> <= IoData>>;
h2bin(IoData) ->
	h2bin(list_to_binary(IoData)).

%%@doc 数据大小端反转,整型可用"ineger-unsigned-little"获取
%%@end
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