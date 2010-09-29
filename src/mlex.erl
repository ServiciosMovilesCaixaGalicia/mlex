%%%----------------------------------------------------------------------
%%% File    : mlex.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : Simple lexical scanner
%%% Created :  2 Sep 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------
%%%
%%% $Id: mlex.erl,v 1.4 2002/10/04 00:03:52 svg Exp $
%%%
%%% $Log: mlex.erl,v $
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
%%% Revision 1.2  2002/10/01 22:32:04  svg
%%% *** empty log message ***
%%%
%%% Revision 1.1.1.1  2002/09/04 20:19:07  svg
%%% Imported sources
%%%
%%%

%% @doc MicroLex is a simple DFA based lexical scanner.
%% 
%% It supports mostly all frequently used lex regexps, predictive
%% operator, long (default) and short regexps. It works with Unix, DOS
%% and Mac files.
%% 
%% <h3>Grammar</h3>
%% 
%% <p>MicroLex grammar is a list of rules. Order is significant. If input
%% matches few rules the first in the list is chosen.</p>
%% 
%% <h4>Rules</h4>
%% 
%% <p>Rules have three forms:
%% <dl>
%%   <dt><code>{Class, Regexp, FormatFun}</code></dt>
%%     <dd>The longest string matched <code>Regexp</code> is chosen as
%%     token string</dd>
%%    <dt><code>{Class, Regexp, FormatFun, short}</code></dt>
%%      <dd>The same as above but the shortest string matched Regexp is
%%      chosen.</dd>
%%    <dt><code>{Class, Regexp1, '/', Regexp2, FormatFun}</code></dt>
%%      <dd>Predictive operator. Input matches
%%      <code>Regexp1Regexp2</code> but only the part matched
%%       <code>Regexp1</code> is chosen as token string and buffer
%%       position points to the next char after it.</dd>
%%    <dt><code>Class</code></dt>
%%      <dd>token class</dd>
%%    <dt><code>Regexp</code></dt>
%%      <dd>regular expression</dd>
%%    <dt><code>FormatFun</code></dt>
%%      <dd><code>Fun(Class, Line, String) -> 
%%                    {error, Error} | Token</code></dd>
%%    <dt><code>Line</code></dt>
%%      <dd>current line in input stream</dd>
%%    <dt><code>String</code></dt>
%%       <dd>string matched <code>Regexp</code></dd>
%% </dl>
%% </p>
%% 
%% <h4>Grammar Example</h4>
%% 
%% <p>Following simple grammar recognizes integers and floats
%% <pre>
%% %% Grammar
%% 
%% grammar() ->
%%  [{ws,             ws(),             ?skip},
%%   {float_num,      float_num(),      fun yeec_token/3},
%%   {integer_num,    integer_num(),    fun yeec_token/3}].
%% 
%% ws() ->	ci(" \t\f").
%% 
%% %%Float
%% %%(+|-)?[0-9]+\.[0-9]+((E|e)(+|-)?[0-9]+)?
%% float_num() ->
%%   '@'([integer_num(), c($.),'+'(digit()),'?'('@'([ci("Ee"), integer_num()]))]).
%% 
%% integer_num() ->
%%   '@'(['?'(ci("+-")), '+'(digit())]).
%% 
%% digit() ->
%%   ci($0, $9).
%% 
%% %% End of Grammar
%% </pre>
%% </p>
%% 
%% <h3>Regexps</h3>
%% <p>
%% <table>
%% <tr><th>MicroLex regexp</th><th>Lex analog</th></tr>
%% <tr><td><code>'@'([R1, R2, ...])</code></td><td>R1R2...    </td></tr>
%% <tr><td><code>'|'([R1, R2, ...])</code></td><td>R1|R2|...  </td></tr>
%% <tr><td><code>'*'(R)		</code></td><td>R*	   </td></tr>
%% <tr><td><code>'+'(R)		</code></td><td>R+	   </td></tr>
%% <tr><td><code>'?'(R)		</code></td><td>R?	   </td></tr>
%% <tr><td><code>'.'()		</code></td><td>.	   </td></tr>
%% <tr><td><code>sol(R)		</code></td><td>^R	   </td></tr>
%% <tr><td><code>eol(R)		</code></td><td>R$	   </td></tr>
%% <tr><td><code>btw(From, To, R)	</code></td><td>R{From, To}</td></tr>
%% <tr><td><code>c($a)		</code></td><td>a	   </td></tr>
%% <tr><td><code>nc($a)		</code></td><td>[^a]	   </td></tr>
%% <tr><td><code>ci($a, $z)	</code></td><td>[a-z]	   </td></tr>
%% <tr><td><code>ci("abc")		</code></td><td>[abc]	   </td></tr>
%% <tr><td><code>cni($a, $z)	</code></td><td>[^a-z]	   </td></tr>
%% <tr><td><code>cni("abc")	</code></td><td>[^abc]	   </td></tr>
%% <tr><td><code>str("abba")	</code></td><td>abba       </td></tr>
%% </table>
%% </p>
%% 
%% <h3>Scanner</h3>
%% 
%% <p>Scanner output is list of tokens. List ended with user defined end
%% token or <code>$end</code> for <code>yecc</code> compatibility.</p>
%% 
%% <p>Scanner can be used in batch mode when the whole input buffer is
%% processed and list of tokens is returned and in continuation style.</p>
%% 
%% <p>If rule's format function returns list it is appended to the output
%% list. Any other result is added to output list. Format function can
%% return empty list <code>[]</code> if you don't want rule result to be
%% present in the output.</p>
%% 
%% <h3>Errors</h3>
%% 
%% <p>On syntax error scanner returns tuple <code>{error, Error}</code>.
%% <dl>
%% <dt><code>Error</code></dt>
%%   <dd><code>scanError()</code></dd>
%% </dl>
%% </p>
%% 
%% @end
%%
%% @type scanError(LineNum, Char, Expect, Str) = {scan, LineNum, Char,
%% Expect, Str}.
%%       <dl>
%%       <dt><code>LineNum</code></dt>
%%         <dd>line number</dd>
%%       <dt><code>Char</code></dt>
%%         <dd>first unmatched character</dd>
%%       <dt><code>Expect</code></dt>
%%         <dd>character classes grammar expecting at that point</dd>
%%       <dt><code>Str</code></dt>
%%         <dd>last recognized characters</dd>
%%       </dl>
%% <p>It could be formatted to user friendly string with format_error/1.</p>
%% @end
%% 
%% @doc
%% 
%% <h3>Examples</h3>
%% 
%% <p>mlex_asn1.erl - subset of ASN.1 grammar.Test files are in 'priv/test'
%% directory.</p>
%% 
%% <p>Look also at mlex_test.erl</p>
%% 
%% @end

-module(mlex).
-author('svg@surnet.ru').

-export([scan/3,scan_token/3,scan_token/2,grammar/1,grammar/2,format_error/1]).
-export([c/1, nc/1, ci/1, ci/2, cni/1, cni/2, str/1, '.'/0]).
-export([match/1, match/2, nmatch/1, nmatch/2]). 
-export(['@'/1, '|'/1, '*'/1, '+'/1, '?'/1, sol/1, eol/1, btw/3]).

-define(ERROR_BEFORE_LEN, 25).
-define(skip, fun (_, _, _) -> [] end).
-define(DEFAULT_END, '$end').


-record(buf, {mod, buf, state, chrs=[], ints=[], pos=0, lnum=1}).
-record(dfa_pos, {pfun, class, follow, pred}).

				    
%% Scanner

%% @spec scan(ModBuffer::atom(), Buffer::term(), Grammar::grammar()) ->
%%   TokenList | {error, Error}
%% 
%%   TokenList = [term()]
%%   Error = scanError() | term()
%% 
%% @doc Scans whole <code>Buffer</code> and returns list of tokens or
%%     error. See <code>mlex_str_buf.erl</code> for buffer module
%%     example. <code>ModBuffer</code> is a buffer module name which
%%     operates on <code>Buffer</code>.
%% 
%% @see grammar/1
%% @see grammar/2

scan(ModBuffer, Buffer, Grammar) ->
  Ret =
    case catch scan1(create_buf(ModBuffer, Buffer), Grammar, []) of
      {ok, Result} ->
	Result;
      Error = {error, _} ->
	Error;
      Other ->
	{error, Other}
    end,
  Ret.

%% @spec scan_token(ModBuffer::atom(), Buffer::term(), Grammar::grammar()) ->
%%   {eof, Cont} | {ok, Cont} | {error, Error}
%%
%%   Cont = {Token | TokenList, NextBuffer} 
%%   TokenList = [Token]
%%   Token = term()
%%   NextBuffer = term()
%%   EndToken = term()
%% 
%% @doc Scans <code>Buffer</code> for the first recognized token
%%     Must be called first. Next calls must use <code>scan_token/2</code>
%%
%% @see scan/3
%% @see scan_token/3

scan_token(ModBuffer, Buffer, Grammar) ->
  scan_token(create_buf(ModBuffer, Buffer), Grammar).

%% @spec scan_token(Buf::term(), Grammar::grammar()) ->
%%   {eof, Cont} | {ok, Cont} | {error, Error}
%%
%%   Cont = {Token | TokenList, NextBuffer} 
%%   TokenList = [Token]
%%   Token = term()
%%   NextBuffer = term()
%%   Error = scanError() | term()
%% 
%% @doc The same as <code>scan_token/3</code> but uses buffer returned
%% by <code>scan_token/2</code> or <code>scan_token/3</code>
%%
%% @see scan/3
%% @see scan_token/3

scan_token(Buf, G={Grammar, EndToken}) ->
  case nxtchr(Buf) of
    _Ret = {eof, Buf1} ->
      {eof, {end_token(EndToken, lnum(Buf1)), Buf1}};
    {C, Buf1} ->
      case scan_token(C, Buf1, Grammar, [], [], []) of
	{ok, {[], Buf2}} ->
	  scan_token(Buf2, G);
	Result ->
	  Result
      end
  end.

scan_token(C, Buf, Grammar, Final, Pred, ScanChars) ->
  {Next, Grammar1} = grammar_next(C, Grammar),
  case Next of
    false when C == sol;
	       C == eol ->
      {C1, Buf1} = nxtchr(Buf),
      scan_token(C1, Buf1, Grammar, Final, Pred, ScanChars);
    false ->
      result_token(retchr(Buf), Grammar, Final, Pred, ScanChars);
    {_, {_, AddPred}} ->
      {Scan, Final1} =
	case state_final(Next) of
	  false -> {long, Final};
	  FinalFun -> {FinalFun(scan), {buf_pos(Buf), FinalFun}}
	end,
      ScanChars1 =
	case C of
	  _ when C == sof;
		 C == eof;
		 C == sol;
		 C == eol ->
	    ScanChars;
	  _ -> 
	    [C|ScanChars]
	end,

      case Scan of
	short ->
	  result_token(Buf, Grammar1, Final1, Pred, ScanChars1);
	long ->
	  {C1, Buf1} = nxtchr(Buf),
	  Pred1 = add_pred_pos(AddPred, Pred, buf_pos(Buf)),
	  scan_token(C1, Buf1, Grammar1, Final1, Pred1, ScanChars1);
	_Other ->
	  {_, F} = Final1,
	  exit({bad_scan_type, F(class)})
      end
  end.

scan1(Buf, Grammar, Tokens) ->
  case scan_token(Buf, Grammar) of
    {eof, {Token, _Buf1}} ->
      {ok, lists:reverse([Token|Tokens])};
    {ok, {Token, Buf1}} ->
      NxtTokens =
	case Token of
	  _ when is_list(Token) ->
	    lists:reverse(Token) ++ Tokens;
	  _ ->
	    [Token|Tokens]
	end,
      scan1(Buf1, Grammar, NxtTokens);
    Error ->
      Error
  end.

result_token(Buf, _Grammar, {StatePos, FinalFun}, Pred, ScanChars) ->
  Pos = get_pred_pos(FinalFun(final), Pred, StatePos),
  Buf1 = buf_pos(Buf, Pos),
  LNum = lnum(Buf1),
  RestBuf = forward(Buf1),
  {Class, Format} = final_params(FinalFun),
  TokenStr = lists:sublist(lists:reverse(ScanChars), Pos),
  case Format(Class, LNum, TokenStr) of
    {error, Error} ->
      return_error(LNum, Error);
    Result ->
      {ok, {Result, RestBuf}}
  end;
result_token(Buf, Grammar, [], _, Chars) ->
  token_error(Buf, Grammar, Chars).

end_token(Token, Line) ->
  {Token, Line}.

grammar_expect({{_, {Funs, _}}, _, _}) ->
  [F(class) || F <- Funs].

grammar_next(C, G={{N, {Funs, _}}, D, DT}) ->
  Success = lists:usort([F(class) || F <- Funs, F(C) == true]),
  case Success of
    [] -> {false, G};
    _ ->
      N1 = dfa_trans_get(N, Success, DT),
      StData = dfa_st_get(N1, D),
      St = {N1, StData},
      {St, {St, D, DT}}
  end.

get_pred_pos(Num, Pred, Default) ->
  case lists:keysearch(Num, 1, Pred) of
    {value, {_, Pos}} ->
	Pos;
      _ ->
      Default
  end.

add_pred_pos(Add, To, Pos) ->
  [{N, Pos} || N <- Add] ++
  [N || N <- To, lists:member(element(1, N), Add) =/= true].

final_params(Fun) ->
  {Fun(class), Fun(funformat)}.

state_final({_, {Funs, _}}) ->
  Funs1 = [F || F <- Funs, F(final) =/= false],
  case Funs1 of
    [] ->
      false;
    _ ->
      first_final(Funs1)
  end.

first_final([F1|Funs]) ->
  lists:foldl(fun (F, PF) ->
		  case (F(final) < PF(final)) of
		    true -> F;
		    _ -> PF
		  end
	      end, F1, Funs).

token_error(Buf, Grammar, Chars) ->
  Chars1 = if length(Chars) > ?ERROR_BEFORE_LEN ->
	       lists:sublist(Chars, ?ERROR_BEFORE_LEN);
	      true ->
	       Chars
	   end,
  Str = lists:reverse(Chars1),
  Expect = grammar_expect(Grammar),
  {Chr, _} = nxtchr(Buf),
  LNum = lnum(Buf),
  return_error(LNum, {scan, lnum(Buf), Chr, Expect, Str}).

return_error(LNum, Error) ->
  {error, {LNum, ?MODULE, Error}}.

format_error({scan, Line, Chr, Expect, Str}) ->
  FmtExp = lists:flatten([format_expected(E) || E <- Expect]),
  lists:flatten(
    io_lib:format("   Scan error at line: ~w;~n"
		  "                 Char: ~s;~n"
		  " Scanned chars before: ~s;~n"
		  "Expected char classes: ~s~n",
		  [Line, format_char(Chr), Str, FmtExp]));
format_error(Error) ->
  lists:flatten(io_lib:format("~w", [Error])).


format_char(C) when is_integer(C) ->
  io_lib:format("~c", [C]);
format_char(C) ->
  io_lib:format("~w", [C]).

format_expected({not_match, Exp}) ->
  "not " ++ format_expected(Exp);
format_expected(C) when is_integer(C) ->
  io_lib:format("~c;", [C]);
format_expected({From, To}) when is_integer(From), is_integer(To) ->
  io_lib:format("[~c-~c];", [From, To]);
format_expected(Str) when is_list(Str) ->
  io_lib:format("~s;", [Str]);
format_expected(Exp) ->
  io_lib:format("~w;", [Exp]).

%% Buffer

create_buf(M, B) ->
  #buf{mod=M, buf=B}.

nxtchr(Buf=#buf{ints=[I|Is], chrs=Cs, pos=P, lnum=L}) ->
  NL = if I == sol -> L+1;
	  true -> L
       end,
  {I, Buf#buf{ints=Is, chrs=[I|Cs], pos=P+1, lnum=NL}};
nxtchr(Buf=#buf{ints=[], mod=M, buf=B}) ->
  {C, B1} = M:nxtchr(B),
  nxtchr(C, Buf#buf{buf=B1}).

nxtchr(eof, Buf=#buf{chrs=Cs, pos=P}) ->
  {eol, Buf#buf{ints=[eof], chrs=[eol|Cs], pos=P+1}};
nxtchr(C=$\r, Buf=#buf{chrs=Cs, mod=M, buf=B, pos=P}) ->
  {C1, B1} = M:nxtchr(B),
  case C1 of
    $\n ->
      {eol, Buf#buf{ints=[C, C1, sol], chrs=[eol|Cs], buf=B1, pos=P+1}};
    _ ->
      {eol, Buf#buf{ints=[C, sol, C1], chrs=[eol|Cs], buf=B1, pos=P+1}}
  end;
nxtchr(C=$\n, Buf=#buf{chrs=Cs, mod=_M, buf=_B, pos=P}) ->
  {eol, Buf#buf{ints=[C, sol], chrs=[eol|Cs], pos=P+1}};
nxtchr(C, Buf=#buf{chrs=Cs, mod=_M, buf=_B, pos=P}) ->
  {C, Buf#buf{chrs=[C|Cs], pos=P+1}}.


retchr(Buf=#buf{ints=Is, chrs=[C|Cs], pos=P, lnum=L}) ->
  NL = if C == sol -> L - 1;
	 true -> L
       end,
  Buf#buf{ints=[C|Is], chrs=Cs, pos=P-1, lnum=NL}.

forward(Buf=#buf{buf=B, ints=Ints, lnum=L, mod=M}) ->
  B1=M:forward(B),
  LN=
    case Ints of
      [] ->
	M:lnum(B1);
      _ ->
	L
    end,
  Buf#buf{pos=0, chrs=[], buf=B1, lnum=LN}.

lnum(_Buf=#buf{buf=_B, mod=_M, lnum=L}) ->
  L.

buf_pos(_Buf=#buf{pos=P}) ->
  P.

buf_pos(Buf=#buf{buf=_B, mod=_M, pos=P}, P) ->
  Buf;
buf_pos(Buf, P) ->
  buf_pos(retchr(Buf), P).

%% Grammar

%% @spec grammar(Rules::list()) -> grammar()
%% 
%% @doc The same as <code>grammar/2</code> but use default terminating
%%  token <em>'$end'</em>.
%% 
%% @see grammar/2

grammar([]) ->
  grammar(internal_rules());
grammar(Rules) ->
  grammar(Rules, ?DEFAULT_END).

%% @spec grammar(Rules::list(), EndToken::term()) -> grammar()
%% 
%% @doc Compile list of <code>Rules</code> to internal grammar
%% representation

grammar(Rules, EndToken) when is_list(Rules) ->
  {compile_grammar('|'(compile_rules(Rules ++ internal_rules()))), EndToken}.

compile_grammar(Grammar) ->
  {First, D, DT} = dfa(Grammar),
  Funs = dfa_st_get(First, D),
  {{First, Funs}, D, DT}.

compile_rules(Ts) ->
  compile_rules(Ts, 0, []).

compile_rules([], _, Ret) ->
  lists:reverse(Ret);
compile_rules([T|Ts], Cnt, Acc) ->
  compile_rules(Ts, Cnt+1, [compile_rule(T, Cnt)|Acc]).

compile_rule({Class, Tree1, '/', Tree2, Format}, Num) ->
  compile_rule({Class, Tree1, '/', Tree2, Format, long}, Num);
compile_rule({Class, Tree1, '/', Tree2, Format, Scan}, Num) ->
  '@'([Tree1, pred(Num), Tree2, final(Class, Format, Num, Scan)]);
compile_rule({Class, Tree, Format}, Num) ->
  compile_rule({Class, Tree, Format, long}, Num);
compile_rule({Class, Tree, Format, Scan}, Num) ->
  '@'([Tree, final(Class, Format, Num, Scan)]).

final(Class, Format, Num, Scan) when Scan == short;
				     Scan == long ->
  Test = fun (final) -> Num;
	     (funformat) -> Format;
	     (class) -> Class;
	     (scan) -> Scan;
	     (_) -> false
	 end,
  fun (D) ->
      tree_pos({Class, Test}, D)
  end.

internal_rules() ->
  [{'$sof', match(sof), ?skip},
   {'$sol', match(sol), ?skip},
   {'$eol', match(eol), ?skip}
  ].

%% Helpers
nmatch(M) ->
  nmatch(M, fun (I) -> I == M end).

nmatch(Class, Test) when is_function(Test) ->
  match({not_match, Class}, fun (I) -> not Test(I) end).

match(M) ->
  match(M, fun (I) -> I == M end).
match(Class, TestFun) when is_function(TestFun) ->
  Test = fun (class) -> Class;
	     (final) -> false;
	     (eof) -> false;
	     (I) ->
	     TestFun(I)
	 end,
  fun (D) ->
      tree_pos({Class, Test}, D)
  end.

%% @spec nc(C::char()) -> function()
%% 
%% @doc Match any character excluding <code>C</code>

nc(C) when is_integer(C) ->
  nmatch(C).

%% @spec c(C::char()) -> function()
%% 
%% @doc Match character <code>C</code>

c(C) when is_integer(C) ->
  match(C).

%% @spec '.'() -> function()
%% 
%% @doc Match any character excluding new line.

'.'() ->
  cni([sol, eol, $\r, $\n]).

%% @spec str(Str::string()) -> function()
%% 
%% @doc Match string

str(Str) when is_list(Str) ->
  '@'([c(C) || C <- Str]).

%% @spec ci(Str::string()) -> function()
%% 
%% @doc Match any character in list

ci(Str) when is_list(Str) ->
  ci(Str, Str).

%% @spec ci(From::char(), To::char()) -> function()
%% 
%% @doc Match any character in range <em>From-To</em> 

ci(From, To) when is_integer(From),
		  is_integer(To),
		  From =< To ->
  Test =
    fun (C) when is_integer(C),
		 C >= From,
		 C =< To ->
	true;
	(_) ->
	false
    end,
  match({From, To}, Test);
ci(Class, Str) when is_list(Str), is_list(Class) ->
  Test =
    fun (C) -> lists:member(C, Str) end,
  match(Class, Test).

%% @spec cni(Str::string()) -> function()
%% 
%% @doc Match any character excluding chars in list

cni(Str) ->
  cni(Str, Str).

%% @spec cni(From::char(), To::char()) -> function()
%% 
%% @doc Match any character excluding chars in range <em>From-To</em> 

cni(From, To) when is_integer(From),
		   is_integer(To),
		   From =< To ->
  Test =
    fun (C) when is_integer(C),
		 C >= From,
		 C =< To ->
	true;
	(_) ->
	false
    end,
  nmatch({From, To}, Test);
cni(Class, Str) when is_list(Str), is_list(Class) ->
  Test = fun (C) -> lists:member(C, Str) end,
  nmatch(Class, Test).

%% @spec sol(Node::function) -> function()
%% 
%% @doc Match regexp at the start of line

sol(Node) ->
  '@'(['|'([match(sof),match(sol)]), Node]).

%% @spec eol(Node::function) -> function()
%% 
%% @doc Match regexp at the end of line

eol(Node) ->
  '@'([Node, match(eol)]).

%% @spec '@'(Nodes::list()) -> function()
%% 
%% @doc Regexps concatenation

'@'(Nodes) when is_list(Nodes) ->
  fun (D) ->
      {Ps, D1} = tree_compile(Nodes, D),
      tree_node(cat, Ps, D1)
  end.

%% @spec '|'(Nodes::list()) -> function()
%% 
%% @doc Match any regexp from list

'|'(Nodes) when is_list(Nodes) ->
  fun (D) ->
      {Ps, D1} = tree_compile(Nodes, D),
      tree_node('|', Ps, D1)
  end.

%% @spec '+'(Node::function()) -> function()
%% 
%% @doc Match one or more appearances of regexp

'+'(Node) when is_function(Node) ->
  '@'([Node, '*'(Node)]).

%% @spec '?'(Node::function()) -> function()
%% 
%% @doc Match one or zero appearances of regexp

'?'(Node) when is_function(Node) ->
  '|'([Node, e()]).

%% @spec '*'(Node::function()) -> function()
%% 
%% @doc Match zero or more appearances of regexp

'*'(Node) when is_function(Node) ->
  fun (D) ->
      {P, D1} = Node(D),
      tree_node('*', P, D1)
  end.

%% @spec btw(From, To, Node::function()) -> function()
%% 
%% @doc Match from <em>From</em> to <em>To</em> appearances of regexp

btw(From, To, Node) when is_integer(From),
			 is_integer(To),
			 From =< To,
			 From > 0 ->
  btw(From, To, Node, []).

btw(From, To, _Node, [SQ]) when To < From ->
  SQ;
btw(From, To, _Node, SQ) when To < From ->
  '|'(SQ);
btw(From, To=1, Node, SQ) ->
  btw(From, To-1, Node, [Node|SQ]);
btw(From, To, Node, SQ) ->
  btw(From, To-1, Node, ['@'(lists:duplicate(To, Node))|SQ]).

pred(N) ->
  fun (D) ->
      tree_pos({'$pred', N}, D)
  end.

e() ->
  fun (D) ->
      tree_pos('$e', D)
  end.

tree_compile(Nodes, D) ->
  tree_compile(Nodes, [], D).

tree_compile([], Ps, D) ->
  {lists:reverse(Ps), D};
tree_compile([Node|Rest], Ps, D) ->
  {P, NxtD} = Node(D),
  tree_compile(Rest, [P|Ps], NxtD).

dfa(Dfa) ->
  PsD = {create_counter(), create_dict()},
  {{First, _, _}, {_, PsD1}} = Dfa(PsD),
  DfaFirst = lists:usort(First),
  {D, DT} = dfa1([DfaFirst], PsD1, create_dict(), create_dict()),
  {DfaFirst, D, DT}.

dfa1([], _, D, DT) ->
  {D, DT};
dfa1([[]], _, D, DT) ->
  {D, DT};
dfa1([St|Rest], PsD, D, DT) ->
  {NxtD, NxtDT} = add_dfa_st(St, PsD, D, DT),
  dfa1(Rest, PsD, NxtD, NxtDT).

add_dfa_st(St, PsD, D, DT) ->
  case dfa_st_exist(St, D) of
    true ->
      {D, DT};
    false ->
      Ps = [dfa_pos_get(N, PsD) || N <- St],
      Pred = lists:usort([dfa_pos_pred(P)
			  || P <- Ps, dfa_pos_pred(P) =/= undefined]),
      Funs = [dfa_pos_fun(P) || P <- Ps],
      NxtD = dfa_st_add(St, {Funs, Pred}, D),
      Classes = class_intersects(
		  lists:usort([dfa_pos_class(P) || P <- Ps])),
      Edges = [mk_dfa_edge(Class, Ps) || Class <- Classes],
      NxtDT = dfa_trans_add(St, Edges, DT),
      dfa1([S || {_, S} <- Edges], PsD, NxtD, NxtDT)
  end.

class_intersects(Classes) ->
  class_intersects(Classes, []).

class_intersects([], Res) ->
  [lists:usort(N) || N <- Res];
class_intersects([C|Rest], Is) ->
  Test = fun (C1) -> class_intersect(C, C1) end,
  class_intersects(Rest,
		   [[C]|Is] ++ [[C|I] || I <- Is, lists:any(Test, I) == true]).
						    
class_intersect(C1, C2) ->
  case class_intersect1(C1, C2) of
    false -> class_intersect1(C2, C1);
    _ -> true
  end.

class_intersect1(C, {not_match, C}) ->
  false;
class_intersect1(_, {not_match, _}) ->
  true;
class_intersect1(C, C) ->
  true;
class_intersect1(C, Str) when is_integer(C), is_list(Str) ->
  lists:member(C, Str);
class_intersect1(C, {From, To}) when is_integer(C) ->
  (C >= From) and (C =< To);
class_intersect1(Str1, Str2) when is_list(Str1), is_list(Str2) ->
  lists:any(fun (E) ->
		lists:member(E, Str2)
	    end, Str1);
class_intersect1(Str, {From, To}) when is_list(Str) -> 
  lists:any(fun (E) ->
		class_intersect1(E, {From, To})
	    end, Str);
class_intersect1({_From1, To1}, {From2, To2}) ->
  (From2 =< To1) and (To1 =< To2);
class_intersect1(_, _) ->
  false.

%% State table

dfa_st_get(StName, D) ->
  dict_get(D, StName).

dfa_st_add(StName, Data, D) ->
  dict_set(D, StName, Data).

dfa_st_exist(Name, D) ->
  dict_exist(D, Name).

%% Trans table

%%dfa_class_sts(Class, DT) ->
%%  dict_select(DT,
%%	      fun (Key={_, C}, _) when C == Class -> true;
%%		  (_, _) -> false
%%	      end,
%%	      fun ({St, _}, _) ->
%%		  St
%%	      end).

dfa_trans_add(_St, [], DT) ->
  DT;
dfa_trans_add(St, [{Class, To}|Edges], DT) ->
  dfa_trans_add(St, Edges, dict_set(DT, {St, Class}, To)).

dfa_trans_get(St, Class, DT) -> 
  dict_get(DT, {St, Class}).

mk_dfa_edge(Class, Ps) ->
  Test = fun (P) -> lists:member(P, Class) end,
  {Class, lists:usort(
	    lists:flatten([dfa_pos_follow(P)
			   || P <- Ps, Test(dfa_pos_class(P)) == true]))}.
%% Tree

tree_pos({'$pred', N}, D) ->
  {{[], [], {pred, N}}, D};
tree_pos('$e', D) ->
  {{[], [], true}, D};
tree_pos({Class, Test}, D) ->
  {Pos, D1} = new_pos(Class, Test, D),
  Last = First = [Pos],
  Null = false,
  {{First, Last, Null}, D1}.

tree_node('|', [P1, P2|Ps], D) ->
  tree_node('|', [P2|Ps], P1, D);
tree_node(cat, [P1, P2|Ps], D) ->
  tree_node(cat, [P2|Ps], P1, D);
tree_node('*', {First, Last, _}, D) ->
  NxtD = add_follow(Last, First, D),
  {{First, Last, true}, NxtD}.

tree_node('|', [], Res, D) ->
  {Res, D};
tree_node('|', [Pos|Rest], {First, Last, Null}, D) ->
  tree_node('|', Rest,
	   {firstpos(Pos) ++ First, lastpos(Pos) ++ Last, (nullable(Pos) or Null)},
	   D);
tree_node(cat, [], Res, D) ->
  {Res, D};
%% Predictive node
tree_node(cat, [{[], [], {pred, N}}, Pos|Rest], Prev, D) ->
  NxtD = add_pred(firstpos(Pos), N, D),
  tree_node(cat, [Pos|Rest], Prev, NxtD);
tree_node(cat, [Pos|Rest], {First, Last, Null}, D) ->
  NxtFirst = case Null of
	       true ->
		 firstpos(Pos) ++ First;
	       false ->
		 First
	     end,
  NxtLast = case nullable(Pos) of
	      true ->
		lastpos(Pos) ++ Last;
	      false ->
		lastpos(Pos)
	     end,
  NxtNull = (nullable(Pos) and Null),
  NxtD = add_follow(Last, firstpos(Pos), D),
  tree_node(cat, Rest, {NxtFirst, NxtLast, NxtNull}, NxtD).

new_pos(Class, Test, D) ->
  {_Name, _NxtD} = add_pos(mk_dfa_pos(Test, Class, []), D).

firstpos({First, _, _}) ->  
  First.
lastpos({_, Last, _}) ->
  Last.

nullable({_, _, Null}) ->
  Null.

add_follow([], _, D) ->
  D;
add_follow([Name|Rest], Follow, {C, D}) ->
  D1 = dfa_pos_follow(Name, Follow, D),
  add_follow(Rest, Follow, {C, D1}).

add_pred([], _, D) ->
  D;
add_pred([Name|Rest], N, {C, D}) ->
  D1 = dfa_pos_pred(Name, N, D),
  add_pred(Rest, N, {C, D1}).

add_pos(Data, {C, D}) ->
  {Name, NxtC} = counter_next(C),
  NxtD = dict_set(D, Name, Data),
  {Name, {NxtC, NxtD}}.

%% Pos

mk_dfa_pos(Test, Class, Follow) ->
  #dfa_pos{pfun=Test, class=Class, follow=Follow}.

dfa_pos_get(Name, D) ->
  dict_get(D, Name).

dfa_pos_set(Name, Data, D) ->
  dict_set(D, Name, Data).

dfa_pos_class(_P=#dfa_pos{class=Class}) ->
  Class.

dfa_pos_fun(_P=#dfa_pos{pfun=Fun}) ->
  Fun.

dfa_pos_pred(Name, N, D) ->
  P = dfa_pos_get(Name, D),
  dfa_pos_set(Name, P#dfa_pos{pred=N}, D).

dfa_pos_pred(_P=#dfa_pos{pred=N}) ->
  N.

dfa_pos_follow(Name, Follow, D) ->
  P = dfa_pos_get(Name, D),
  dfa_pos_set(Name,
	      P#dfa_pos{follow=lists:usort(dfa_pos_follow(P) ++ Follow)}, D).

dfa_pos_follow(_P=#dfa_pos{follow=Follow}) ->
  Follow.

%% Counter
create_counter() ->
  0.

counter_next(C) ->
  {C+1, C+1}.

%% Dictionary

dict_get(Dict, Name) ->
  gb_trees:get(Name, Dict).

dict_set(Dict, Name, Val) ->
  _NewDict = gb_trees:enter(Name, Val, Dict).

dict_exist(Dict, Name) ->
  gb_trees:is_defined(Name, Dict).

%%dict_to_list(Dict) ->
%%  gb_trees:to_list(Dict).

%%dict_select(Dict, Test, Val) ->
%%  dict_select(Test, Val, [], gb_trees:next(gb_trees:iterator(Dict))).
%%
%%dict_select(Test, Val, Res, none) ->
%%  lists:reverse(Res);
%%dict_select(Test, Val, Acc, {K, V, NxtS}) ->
%%  case Test(K, V) of
%%    true ->
%%      dict_select(Test, Val, [Val(K, V)|Acc], gb_trees:next(NxtS));
%%    false ->
%%      dict_select(Test, Val, Acc, gb_trees:next(NxtS))
%%  end.

create_dict() ->
  gb_trees:empty().
