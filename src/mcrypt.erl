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
%%% @doc  加密处理
%%% @author  qingchuwudi <'bypf2009@vip.qq.com'> 
%%% @copyright 2015-2017 qingchuwudi <bypf2009@vip.qq.com>
%%% @end
%%% created|changed : 2015-10-22 16:31
%%% coding : utf-8 
%%% ------------------------------------------------------------------ 
-module(mcrypt).
-export([ 
    rand_bytes/1,
    read_rsa_key/1, 
    decode_rsa_key/1,
    rsa_enc/2, rsa_dec/2,
    rsa_enc/3, rsa_dec/3,
    rsa_sign/2, rsa_sign/3,
    rsa_verify/3, rsa_verify/4,

    aes_gcm_enc/2, aes_gcm_dec/2,
    aes_cbc_enc/3, aes_cbc_dec/3,
    pad_128/1      % 补位
    ]).

-define (L128, 16).
-define (L256, 32).

% -define (OPTS, [{rsa_pad, 'rsa_no_padding'}]).
-define (OPTS, [{rsa_pad, rsa_pkcs1_padding}]).
% -define (OPTS, [{rsa_pad, 'rsa_pkcs1_oaep_padding'}]).

%%@doc 生成随机均匀的二进制数据
%%@end
%% Len 长度
rand_bytes(Len) ->
    crypto:strong_rand_bytes(Len).

%%@doc rsa 加密
rsa_enc(Plain, PubKey) ->
    public_key:encrypt_public(Plain, PubKey).

rsa_enc(Plain, PubKey, Opts) ->
    public_key:encrypt_public(Plain, PubKey, Opts).

%% 解密
rsa_dec(Cipher, PriKey)->
    public_key:decrypt_private(Cipher, PriKey).

rsa_dec(Cipher, PriKey, Opts)->
    public_key:decrypt_private(Cipher, PriKey, Opts).


%%@doc rsa签名验证
%%@end
%%, Plain: 明文
%%, PriKey: 私钥
%%, Signature: 用明文和私钥得到的签名
%%, PubKey: 公钥
%% 签名，私钥签名
rsa_sign(Plain, PriKey) ->
    rsa_sign(Plain, sha, PriKey).

rsa_sign(Plain, Type, PriKey) ->
    public_key:sign(Plain, Type, PriKey).

%% 签名验证，公钥验证
rsa_verify(Plain, Signature, PubKey) ->
    rsa_verify(Plain, sha, Signature, PubKey).

rsa_verify(Plain, Type, Signature, PubKey) ->
    public_key:verify(Plain, Type, Signature, PubKey).

%% read file
read_rsa_key(FileName) ->
    {ok, PemBin} = file:read_file(FileName),
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).

decode_rsa_key(PemBin) ->
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).

%% aes gcm 模式加密
%% 参考 http://qiita.com/voluntas/items/22e5787197cab3eafecb
%%@return {Cipher, Tag}
aes_gcm_enc(Plain, Key256) ->
    crypto:block_encrypt(aes_gcm, Key256, <<Key256:24/binary>>, {<<Key256:8/binary>> ,Plain}).

%%@return Plain
aes_gcm_dec({Cipher, Tag}, Key256) ->
    crypto:block_decrypt(aes_gcm, Key256, <<Key256:24/binary>>, {<<Key256:8/binary>> , Cipher, Tag}).

%%@doc AES CBC 256 加密算法
%%@end
%% ,Key 秘钥，必须是256位，32字节
%% ,Ivec 初始化向量，扰码——必须是128位
%% ,Plain 明文，必须是128的整数倍字节
%% ;aes_cbc_enc(<<0:256>>, <<0:128>>, <<0:128>>).
aes_cbc_enc(Plain, Key, Ivec) ->
    crypto:block_encrypt(aes_cbc256, Key, Ivec, pad_128(Plain)). % aes_cbc256

aes_cbc_dec(Cipher, Key, Ivec) ->
    crypto:block_decrypt(aes_cbc256, Key, Ivec, Cipher).


%%@doc 数据补位，补齐128的整数倍
pad_128(Data) when is_binary(Data) ->
    pad_binary_128(Data);
pad_128(Data) when is_bitstring(Data) ->
    pad_binary_128(Data);
pad_128(Data) when is_list(Data) ->
    pad_binary_128(list_to_binary(Data)).

pad_binary_128(Bitstr) ->
    %% cbc 明文是128的整数倍
    case bit_size(Bitstr) rem 128 of
        0   -> Bitstr;
        Rem ->
            <<Bitstr/bitstring, 0:(128 - Rem)>>
    end.