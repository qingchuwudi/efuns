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
%%% @doc  
%%% @author  qingchuwudi <'bypf2009@vip.qq.com'> 
%%% @copyright 2016-2017 qingchuwudi <bypf2009@vip.qq.com>
%%% @reference https://github.com/processone/ejabberd.git
%%% @end
%%% created|changed : 2016-06-29 08:39
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 
-module(mnesia_config).

-author("qingchuwudi").

-export([
  convert_table_to_binary/5
]).


-spec convert_table_to_binary(atom(), [atom()], atom(),
                              fun(), fun()) -> ok.

convert_table_to_binary(Tab, Fields, Type, DetectFun, ConvertFun) ->
    case is_table_still_list(Tab, DetectFun) of
        true ->
            error_logger:info_msg("Converting '~s' table from strings to binaries.", [Tab]),
            TmpTab = list_to_atom(atom_to_list(Tab) ++ "_tmp_table"),
            catch mnesia:delete_table(TmpTab),
            case mnesia:create_table(TmpTab,
                                     [{disc_only_copies, [node()]},
                                      {type, Type},
                                      {local_content, true},
                                      {record_name, Tab},
                                      {attributes, Fields}]) of
                {atomic, ok} ->
                    mnesia:transform_table(Tab, ignore, Fields),
                    case mnesia:transaction(
                           fun() ->
                                   mnesia:write_lock_table(TmpTab),
                                   mnesia:foldl(
                                     fun(R, _) ->
                                             NewR = ConvertFun(R),
                                             mnesia:dirty_write(TmpTab, NewR)
                                     end, ok, Tab)
                           end) of
                        {atomic, ok} ->
                            mnesia:clear_table(Tab),
                            case mnesia:transaction(
                                   fun() ->
                                           mnesia:write_lock_table(Tab),
                                           mnesia:foldl(
                                             fun(R, _) ->
                                                     mnesia:dirty_write(R)
                                             end, ok, TmpTab)
                                   end) of
                                {atomic, ok} ->
                                    mnesia:delete_table(TmpTab);
                                Err ->
                                    report_and_stop(Tab, Err)
                            end;
                        Err ->
                            report_and_stop(Tab, Err)
                    end;
                Err ->
                    report_and_stop(Tab, Err)
            end;
        false ->
            ok
    end.

is_table_still_list(Tab, DetectFun) ->
    is_table_still_list(Tab, DetectFun, mnesia:dirty_first(Tab)).

is_table_still_list(_Tab, _DetectFun, '$end_of_table') ->
    false;
is_table_still_list(Tab, DetectFun, Key) ->
    Rs = mnesia:dirty_read(Tab, Key),
    Res = lists:foldl(fun(_, true) ->
                              true;
                         (_, false) ->
                              false;
                         (R, _) ->
                              case DetectFun(R) of
                                  '$next' ->
                                      '$next';
                                  El ->
                                      is_list(El)
                              end
                      end, '$next', Rs),
    case Res of
        true ->
            true;
        false ->
            false;
        '$next' ->
            is_table_still_list(Tab, DetectFun, mnesia:dirty_next(Tab, Key))
    end.


report_and_stop(Tab, Err) ->
    ErrTxt = lists:flatten(
               io_lib:format(
                 "Failed to convert '~s' table to binary: ~p",
                 [Tab, Err])),
    error_logger:error_msg(ErrTxt, []),
    timer:sleep(1000),
    halt(string:substr(ErrTxt, 1, 199)).