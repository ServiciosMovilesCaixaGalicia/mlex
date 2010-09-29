%%%----------------------------------------------------------------------
%%% File    : mlex_test.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : 
%%% Created :  2 Sep 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------
%%%
%%% $Id: mlex_test.erl,v 1.4 2002/10/04 00:03:52 svg Exp $
%%%
%%% $Log: mlex_test.erl,v $
%%% Revision 1.4  2002/10/04 00:03:52  svg
%%% Fixed DOS new line
%%% Added new line tests
%%%
%%% Revision 1.3  2002/10/02 20:43:56  svg
%%% Fixed start and end line bugs.
%%% Fixed empty string bug.
%%% Fixed DOS and Mac line endings.
%%% Most buffer functions curried to mlex.erl
%%% Added new tests.
%%% Written EDoc documentation.
%%%
%%% Revision 1.2  2002/10/01 22:32:05  svg
%%% *** empty log message ***
%%%
%%% Revision 1.1.1.1  2002/09/04 20:19:07  svg
%%% Imported sources
%%%
%%%

-module(mlex_test).
-author('svg@surnet.ru').

-export([test_all/0, test_all/1]).
-export([test_file_scan/1, test_predictive/1, test_short/1, test_scan_cc/1]).
-export(['test_*'/1, test_sol/1, test_end_line/1]).

-import(mlex,[c/1, nc/1, ci/1, ci/2, cni/1, cni/2, str/1, btw/3]).
-import(mlex,['@'/1, '|'/1, '*'/1, '+'/1, '?'/1, '.'/0, sol/1, eol/1]).

-define(match(_Pat), fun (_Res) ->
                         case _Res of
                           _Pat -> ok;
                           _ -> false
                         end
                     end).

-define(ASN1_RESULT,
	[{upper,12,"CISCO-AAA-SESSION-MIB"},
	 {definitions_kw,12,"DEFINITIONS"},
	 {assign_op,12,"::="},
	 {begin_kw,12,"BEGIN"},
	 {imports_kw,14,"IMPORTS"},
	 {upper,15,"MODULE-IDENTITY"},
	 {comma,15,","},
	 {upper,16,"OBJECT-TYPE"},
	 {comma,16,","},
	 {upper,17,"Counter32"},
	 {comma,17,","},
	 {upper,18,"Gauge32"},
	 {comma,18,","},
	 {upper,19,"IpAddress"}|_]).

-define(TEST_STRING,
"CISCO-AAA-SESSION-MIB DEFINITIONS ::= BEGIN
IMPORTS
        MODULE-IDENTITY,
        OBJECT-TYPE,
        Counter32,
        Gauge32,
        IpAddress
                FROM SNMPv2-SMI
        MODULE-COMPLIANCE,
        OBJECT-GROUP
                FROM SNMPv2-CONF
        DisplayString,
        TruthValue,
        TEXTUAL-CONVENTION
                FROM SNMPv2-TC
        Unsigned32
               FROM CISCO-TC
        ciscoMgmt
                FROM CISCO-SMI;").
 


test_all([quit]) ->
  (catch test_all()),
  halt().

test_all() ->
  lists:foreach(fun(T) ->
                    io:format("~n~s~n",[apply({?MODULE, T}, [info])]),
                    apply({?MODULE, T}, [do])
                end,
                [test_predictive, test_file_scan, test_scan_cc, test_short,
		 'test_*', test_sol, test_end_line
		]
               ).

test_file_scan(info) ->
  "*** Scan file ***";
test_file_scan(do) ->
  FileAsn1 = "./priv/test/CISCO-AAA-SESSION-MIB.txt",
  Tests =
    [
     {"ASN.1", [FileAsn1, mlex_asn1:grammar()],
      ?match([{upper,12,"CISCO-AAA-SESSION-MIB"}|_])}
    ],
  process_tests(fun scan_file/2, Tests).

test_end_line(info) ->
  "*** Test Unix, DOS and Mac end lines ***";
test_end_line(do) ->
  Grammar = mlex_asn1:grammar(),
  Str = ?TEST_STRING,
  Unix = scan_string(Str, Grammar),
  Dos = scan_string(string_replace(Str, "\n", "\r\n"), Grammar),
  Mac = scan_string(string_replace(Str, "\n", "\r"), Grammar),
  Tests =
    [
     {"Unix", [Unix, Unix], true},
     {"DOS", [Dos, Unix], true},
     {"Mac", [Mac, Unix], true}
    ],
  process_tests(fun (List1, List2) -> List1 == List2 end, Tests).

test_scan_cc(info) ->
  "*** Scan file with continuation ***";
test_scan_cc(do) ->
  BufConf = file_to_buf("./priv/test/CISCO-AAA-SESSION-MIB.txt"),
  Tests =
    [{"ASN.1", [BufConf, mlex_asn1:grammar()],
      ?match(?ASN1_RESULT)}
    ],
  process_tests(fun scan_cc/2, Tests).

'test_*'(info) ->
  "*** * expression ***";
'test_*'(do) ->
  G = mlex:grammar([{"a*", eol('*'(c($a))), fun yeec_token/3}]),
  Str = "aaaaa",
  Tests =
    [{"empty", ["", G],
      ?match([{"a*",1,[]},{'$end',1}])},
     {"aaaaa", [Str, G],
      ?match([{"a*",1,"aaaaa"},{'$end',1}])}
    ],
  process_tests(fun scan_string/2, Tests).

test_sol(info) ->
  "*** Sol expression ***";
test_sol(do) ->
  G1 = mlex:grammar([{sol, sol(str("abba")), fun yeec_token/3},
		      {abba, str("abba"), fun yeec_token/3}]),
  Str1 = "abbaabbaabbaabba",

  G2 = mlex:grammar([{sol, sol(str("abba")), fun yeec_token/3},
		     {abb1, str("abb1"), fun yeec_token/3}]),
  Str2 = "abb1abb1abb1abb1",

  Tests =
    [{"sol(abba)", [Str1, G1],
      ?match([{sol,1,"abba"},
	      {abba,1,"abba"},
	      {abba,1,"abba"},
	      {abba,1,"abba"},
	      {'$end',1}])},
     {"second(abb1)", [Str2, G2],
      ?match([{abb1,1,"abb1"},
	      {abb1,1,"abb1"},
	      {abb1,1,"abb1"},
	      {abb1,1,"abb1"},
	      {'$end',1}])}
    ],
  process_tests(fun scan_string/2, Tests).

test_short(info) ->
  "*** Short expression ***";
test_short(do) ->
  Short = mlex:grammar([{short, '+'(str("abba")), fun yeec_token/3, short},
			{long, '+'(str("abba")), fun yeec_token/3}]),
  Long = mlex:grammar([{short, '+'(str("abba")), fun yeec_token/3, short},
		       {long, '+'(str("abba")), fun yeec_token/3}]),
  Str = "abbaabbaabbaabba",
  Tests =
    [{"short first", [Str, Short],
      ?match([{short,1,"abba"},
	      {short,1,"abba"},
	      {short,1,"abba"},
	      {short,1,"abba"},
	      {'$end',1}])},
     {"long first", [Str, Long],
      ?match([{short,1,"abba"},
	      {short,1,"abba"},
	      {short,1,"abba"},
	      {short,1,"abba"},
	      {'$end',1}])}
    ],
  process_tests(fun scan_string/2, Tests).

test_predictive(info) ->
  "*** Predictive operator ***";
test_predictive(do) ->
  StrTrue = "abcdeabcdede",
  StrFalse = "abcdeabcdedeabcabc",
  Tests =
    [{"must be true", [StrTrue, predictive_grammar()],
      ?match([{abc,1,"abc"}, {de,1,"de"}, {abc,1,"abc"},
	      {de,1,"de"}, {de,1,"de"}, {'$end',1}])},
     {"must be false", [StrFalse, predictive_grammar()],
      ?match("   Scan error at line: 1;\n"
	     "                 Char: a;\n"
	     " Scanned chars before: abc;\n"
	     "Expected char classes: d;\n")}
    ],
  process_tests(fun scan_string/2, Tests).

predictive_grammar() ->
  mlex:grammar([{abc, str("abc"), '/', '+'(str("de")), fun yeec_token/3},
		{de, str("de"), fun yeec_token/3}]).

scan(Buf, Grammar) ->
  case mlex:scan(mlex_str_buf, Buf, Grammar) of
    {error, {_, Module, Error}} ->
      Module:format_error(Error);
    Res ->
      Res
  end.

scan_cc(Buf, Grammar) ->
  scan_cc(first_token(Buf, Grammar), Grammar, []).

scan_cc({error, {_, Module, Error}},_Grammar, _Tokens) ->
  Module:format_error(Error);
scan_cc({eof, _}, _, Res) ->
  lists:reverse(Res);
scan_cc({ok, {Token, Buf}}, Grammar, Tokens) ->
  NxtTokens =
    case Token of
      AddTokens when is_list(AddTokens) ->
	lists:reverse(AddTokens) ++ Tokens;
      _ ->
	[Token|Tokens]
    end,
  scan_cc(next_token(Buf, Grammar), Grammar, NxtTokens).

first_token(Buf, Grammar) ->
  mlex:scan_token(mlex_str_buf, Buf, Grammar).

next_token(Buf, Grammar) ->
  mlex:scan_token(Buf, Grammar).

scan_file(FileName, Grammar) ->
  _Buf = file_to_buf(FileName),
  scan(file_to_buf(FileName), Grammar).

scan_string(Str, Grammar) ->
  scan(string_to_buf(Str), Grammar).

file_to_buf(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  string_to_buf(Bin).

string_to_buf(Bin) when is_binary(Bin) ->
  string_to_buf(binary_to_list(Bin));
string_to_buf(Str) ->
  mlex_str_buf:str_to_buf(Str).

yeec_token(Class, Line, Data) ->
  {Class, Line, Data}.

string_replace(Str, From, To) ->
  string_replace(Str, From, lists:reverse(To), length(From), []).

string_replace([], _From, _, _, Acc) ->
  lists:reverse(Acc);
string_replace(Str, From, To, FromLen, Acc) ->
  case lists:prefix(From, Str) of
    true ->
      string_replace(lists:nthtail(FromLen, Str), From, To, FromLen, To++Acc);
    false ->
      string_replace(tl(Str), From, To, FromLen, [hd(Str)|Acc])
  end.

%% Test help funs

%process_tests(Tests) ->
%  lists:foreach(fun ({Fun, FunTests}) ->
%                    process_tests(Fun, FunTests)
%                end, Tests).

process_tests(Fun, Tests) ->
  process_tests(Fun, Tests, 1).

%% Test argument is in the form {Msg, Args, TrueResult}
%% Msg        - test purpose
%% Args       - [Arg]
%% TrueResult - true result pattern
process_tests(_Fun, [], _) ->
  ok;
process_tests(Fun, [Test|Tail], Num) ->
  {Msg, Args, ResPat} = Test,
  Status = case catch apply(Fun,Args) of
             Res when is_function(ResPat) ->
               case catch ResPat(Res) of
                 ok ->
                   [passed, ok];
                 _ ->
                   [Res, failed]
               end;
             ResPat ->
               [passed, ok];
             Other ->
               [Other, failed]
           end,
  io:format("Test ~2.2w: "++Msg++" -> ~p, ~p~n", [Num|Status]),
  process_tests(Fun, Tail, Num+1).

