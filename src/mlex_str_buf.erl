%%%----------------------------------------------------------------------
%%% File    : mlex_str_buf.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : Mlex string buffer
%%% Created :  2 Sep 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------
%%%
%%% $Id: mlex_str_buf.erl,v 1.3 2002/10/02 20:43:56 svg Exp $
%%%
%%% $Log: mlex_str_buf.erl,v $
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

-module(mlex_str_buf).
-author('svg@surnet.ru').

-export([forward/1, nxtchr/1, lnum/1]).
-export([ str_to_buf/1]).

-record(buf, {str=[],  pos=0, lnum=0}).

str_to_buf(Str) when list(Str) ->
  #buf{str=Str, lnum=1}.

nxtchr(B=#buf{pos=0}) ->
  {sof, B#buf{pos=start}};
nxtchr(B=#buf{str=[]}) ->
  {eof, B};
nxtchr(B=#buf{str=[C=$\r,$\n|Cs], lnum=LN}) ->
  {C, B#buf{str=[$\n|Cs]}};
nxtchr(B=#buf{str=[C|Cs], lnum=LN}) when C == $\r; C == $\n ->
  {C, B#buf{str=Cs, lnum=LN+1}};
nxtchr(B=#buf{str=[C|Cs]}) ->
  {C, B#buf{str=Cs}}.

lnum(B=#buf{lnum=LN}) ->
  LN.

forward(B) ->
  B.
