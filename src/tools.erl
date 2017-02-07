%%% ------------------------------------------------------------------ 
%%% Licensed under the Apache License, Version 2.0 (the 'License');
%%%  you may not use this file except in compliance with the License.
%%%  You may obtain a copy of the License at
%%%
%%%      http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Copyright (c) 2015-2017 qingchuwudi <bypf2009@vip.qq.com>
%%%
%%%  Unless required by applicable law or agreed to in writing, software
%%%  distributed under the License is distributed on an 'AS IS' BASIS,
%%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%  See the License for the specific language governing permissions and
%%%  limitations under the License.
%%%
%%% @doc  常用工具
%%% @author  qingchuwudi <'bypf2009@vip.qq.com'> 
%%% @copyright 2015-2017 qingchuwudi <bypf2009@vip.qq.com>
%%% @end
%%% created|changed : 2015-09-23 14:35
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 

-module (tools).
-author("qingchuwudi").

-export ([
	is_mail/1,
	is_phone/1,
	phone_or_email/1,
	string_to_term/1,
	list_to_string/1,
	bitstring_to_term/1,
	term_to_string/1,
	term_to_string2/1,
	term_to_bitstring/1
	]).

-type account() :: binary() | list().

-define (FILTERS,"@").
%% 最宽松的邮箱验证
-define (MAIL, "^\\w+([-+.]\\w+)*@\\w+([-.]\\w+)*\\.\\w+([-.]\\w+)*$").
%% 略严格的邮箱验证
-define (MAIL_CRITCAL, "^[a-zA-Z0-9_-\\.]+@\\w+([-.]\\w+)*\\.[a-zA-Z0-9_-]+$").
%% 严格的邮箱验证
-define (MAIL_CRITCAL_, "^[a-zA-Z0-9-_]+@[a-zA-Z0-9_-]+\\.[a-z]{2,4}$").
% 中国手机号|国际手机号(建议无空格，可以有+86头)
-define (PHONE, "^\\s*(?:\\+?(\\d{1,3}))?[-. (]*(\\d{3})[-. )]*(\\d{3})[-. ]*(\\d{4})(?: *x(\\d+))?\\s*$").


%%@doc 检查是手机号还是邮箱
-spec phone_or_email(Accout) -> PhOrMa when
		Accout :: account(),
		PhOrMa :: binary() | error.
phone_or_email(Accout) ->
    case is_phone(Accout) of
        true -> <<"phone">>;
        false ->
            case tools:is_mail(Accout) of
                true -> <<"email">>;
                false -> error
            end
    end.

%%@doc 检查是不是手机号
-spec is_mail(Mail) -> Bool when
		Mail :: account(),
		Bool :: boolean().
is_mail(Mail) ->
	check(Mail, ?MAIL_CRITCAL_).

%%@doc 检查是不是邮箱
-spec is_phone(Phone) -> Bool when
		Phone :: account(),
		Bool :: boolean().
is_phone(Phone) ->
	check(Phone, ?PHONE).

%%@doc 正则表达式匹配
-spec check(String, Express) -> Bool when
		String :: account(),
		Express :: list(),
		Bool :: boolean() .
check(String, Express) when is_list(String) ->
	case re:run(String, Express) of
		{match, _} -> true;
		nomatch -> false
	end;
check(String, Express) when is_binary(String) -> 
	check(binary_to_list(String), Express).

%%@doc term转换为string格式
%%@end
%%e.g., [{a},1] => "[{a},1]"
-spec term_to_string(Term) -> String when
		Term :: term(),
		String :: string().
term_to_string(Term) ->
	binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%%@doc term 保留字符串显示
-spec term_to_string2(Term) -> String when
		Term :: term(),
		String :: string().
term_to_string2(Term) ->
	binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%%@doc term转换为bitstring格式
%%@end
%%e.g., [{a},1] => < < "[{a},1]" > >
-spec term_to_bitstring(Term) -> BitString when
		Term :: term(),
		BitString :: bitstring() .
term_to_bitstring(Term) ->
	erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%%@doc 任意串转为Erlang对应数据
-spec string_to_term(String) -> Term when
		String :: string(),
		Term :: term() .
string_to_term(String) ->
	case erl_scan:string(String++".") of
		{ok, Tokens, _} ->
			case erl_parse:parse_term(Tokens) of
				{ok, Term} -> Term;
				_Err -> undefined
			end;
		_Error ->
			undefined
	end.

%%@doc 任意bitstring转为Erlang对应数据
-spec bitstring_to_term(BitString) -> Term when
		BitString :: bitstring(),
		Term :: term().
bitstring_to_term(BitString) when is_binary(BitString) ->
	string_to_term(binary_to_list(BitString)).

-spec list_to_string(list()) -> list() .
list_to_string(List) ->
    IOList = io_lib:format("~p", [List]),
    lists:flatten(IOList).