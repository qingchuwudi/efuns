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
%%% @doc  进程相关
%%% @author  qingchuwudi <'bypf2009@vip.qq.com'> 
%%% @copyright 2014-2016 qingchuwudi <bypf2009@vip.qq.com>
%%% @end
%%% created|changed : 2015-10-29 14:52
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 
-module(misc).

-author("qingchuwudi").

-export ([
	unixtime/0,
	list2fi/1,
	rand/2,
	difftime/1,
	whereis_name/2,
	register/3,
	unregister/2,
	is_process_alive/1
]).

%%@doc 取得当前的unix时间戳（秒）
unixtime() ->
	os:system_time(1).

%%@doc 列表转换成整型、浮点型
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
difftime(Startime) ->
	unixtime() - Startime.

whereis_name(local, Atom) -> 
	erlang:whereis(Atom);

whereis_name(global, Atom) ->
	global:whereis_name(Atom).

register(local, Name, Pid) ->
	erlang:register(Name, Pid);

register(global, Name, Pid) ->
	global:re_register_name(Name, Pid).

unregister(local, Name) ->
	erlang:unregister(Name);

unregister(global, Name) ->
	global:unregister_name(Name).

%%@doc 进程是否存活
is_process_alive(Pid) ->    
	try 
		if
			is_pid(Pid) ->
				Node = node(Pid),
				case Node =:= node() of
					true ->
						erlang:is_process_alive(Pid);
					false ->
						case rpc:call(Node, erlang, is_process_alive, [Pid]) of
							{badrpc, _Reason}  -> false;
							Res -> Res
						end
				end;
			true -> false
		end
	catch 
		_:_ -> false
	end.
