%%% File    : mlex_asn1.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : Restricted ASN.1 test grammar
%%% Created : 25 Aug 2002 by  <svg@surnet.ru>
%%%
%%% $Id: mlex_asn1.erl,v 1.1.1.1 2002/09/04 20:19:07 svg Exp $
%%%
%%% $Log: mlex_asn1.erl,v $
%%% Revision 1.1.1.1  2002/09/04 20:19:07  svg
%%% Imported sources
%%%
%%%

-module(mlex_asn1).
-author('svg@surnet.ru').

-export([grammar/0]).

-import(mlex,[c/1, nc/1, ci/1, ci/2, cni/1, cni/2, str/1, btw/3]).
-import(mlex,['@'/1, '|'/1, '*'/1, '+'/1, '?'/1, '.'/0, sol/1, eol/1]).

-define(skip, fun (_, _, _) -> [] end).

grammar() ->	
 mlex:grammar(
   [
    {ws, ws(), ?skip},                 
    {sl_comment, sl_comment(), ?skip},
    {absent_kw		 ,absent_kw()           , fun yeec_token/3},
    {abstract_syntax_kw	 ,abstract_syntax_kw()  , fun yeec_token/3},
    {all_kw		 ,all_kw() 	        , fun yeec_token/3},
    {any_kw		 ,any_kw() 	        , fun yeec_token/3},
    {application_kw	 ,application_kw()      , fun yeec_token/3},
    {automatic_kw	 ,automatic_kw()        , fun yeec_token/3},	
    {based_num_kw	 ,based_num_kw()        , fun yeec_token/3},	
    {begin_kw		 ,begin_kw() 	        , fun yeec_token/3},
    {bit_kw		 ,bit_kw() 	        , fun yeec_token/3},
    {bmp_string_kw	 ,bmp_string_kw()       , fun yeec_token/3},
    {boolean_kw		 ,boolean_kw() 	        , fun yeec_token/3},
    {by_kw		 ,by_kw() 	        , fun yeec_token/3},
    {character_kw	 ,character_kw()        , fun yeec_token/3},	
    {choice_kw		 ,choice_kw() 	        , fun yeec_token/3},
    {class_kw		 ,class_kw() 	        , fun yeec_token/3},
    {components_kw	 ,components_kw()       , fun yeec_token/3},
    {component_kw	 ,component_kw()        , fun yeec_token/3},	
    {constrained_kw	 ,constrained_kw()      , fun yeec_token/3},
    {default_kw		 ,default_kw() 	        , fun yeec_token/3},
    {defined_kw		 ,defined_kw() 	        , fun yeec_token/3},
    {definitions_kw	 ,definitions_kw()      , fun yeec_token/3},
    {embedded_kw	 ,embedded_kw()         , fun yeec_token/3},	
    {end_kw		 ,end_kw() 	        , fun yeec_token/3},
    {enumerated_kw	 ,enumerated_kw()       , fun yeec_token/3},
    {except_kw		 ,except_kw() 	        , fun yeec_token/3},
    {explicit_kw	 ,explicit_kw()         , fun yeec_token/3},	
    {exports_kw		 ,exports_kw() 	        , fun yeec_token/3},
    {extensibility_kw	 ,extensibility_kw()    , fun yeec_token/3},
    {external_kw	 ,external_kw()         , fun yeec_token/3},	
    {false_kw		 ,false_kw() 	        , fun yeec_token/3},
    {from_kw		 ,from_kw() 	        , fun yeec_token/3},
    {generalized_time_kw ,generalized_time_kw() , fun yeec_token/3},	
    {general_str_kw	 ,general_str_kw()      , fun yeec_token/3},
    {graphic_str_kw	 ,graphic_str_kw()      , fun yeec_token/3},
    {ia5_string_kw	 ,ia5_string_kw()       , fun yeec_token/3},
    {identifier_kw	 ,identifier_kw()       , fun yeec_token/3},
    {implicit_kw	 ,implicit_kw()         , fun yeec_token/3},	
    {implied_kw		 ,implied_kw() 	        , fun yeec_token/3},
    {imports_kw		 ,imports_kw() 	        , fun yeec_token/3},
    {includes_kw	 ,includes_kw()         , fun yeec_token/3},	
    {instance_kw	 ,instance_kw()         , fun yeec_token/3},	
    {integer_kw		 ,integer_kw() 	        , fun yeec_token/3},
    {intersection_kw	 ,intersection_kw()     , fun yeec_token/3},
    {iso646string_kw	 ,iso646string_kw()     , fun yeec_token/3},
    {max_kw		 ,max_kw() 	        , fun yeec_token/3},
    {minus_infinity_kw	 ,minus_infinity_kw()   , fun yeec_token/3},
    {min_kw		 ,min_kw() 	        , fun yeec_token/3},
    {null_kw		 ,null_kw() 	        , fun yeec_token/3},
    {numeric_str_kw	 ,numeric_str_kw()      , fun yeec_token/3},
    {object_descriptor_kw,object_descriptor_kw(), fun yeec_token/3}, 	
    {object_kw		 ,object_kw() 	        , fun yeec_token/3},
    {octet_kw		 ,octet_kw() 	        , fun yeec_token/3},
    {of_kw		 ,of_kw() 	        , fun yeec_token/3},
    {optional_kw	 ,optional_kw()         , fun yeec_token/3},	
    {pdv_kw		 ,pdv_kw() 	        , fun yeec_token/3},
    {plus_infinity_kw	 ,plus_infinity_kw()    , fun yeec_token/3},
    {present_kw	 ,present_kw() 	        , fun yeec_token/3},
    {printable_str_kw	 ,printable_str_kw()    , fun yeec_token/3},
    {private_kw	 ,private_kw() 	        , fun yeec_token/3},
    {real_kw		 ,real_kw() 	        , fun yeec_token/3},
    {sequence_kw	 ,sequence_kw()         , fun yeec_token/3},	
    {set_kw		 ,set_kw() 	        , fun yeec_token/3},
    {size_kw		 ,size_kw() 	        , fun yeec_token/3},
    {string_kw		 ,string_kw() 	        , fun yeec_token/3},
    {tags_kw		 ,tags_kw() 	        , fun yeec_token/3},
    {teletex_str_kw	 ,teletex_str_kw()      , fun yeec_token/3},
    {true_kw		 ,true_kw() 	        , fun yeec_token/3},
    {type_identifier_kw ,type_identifier_kw()  , fun yeec_token/3},
    {union_kw		 ,union_kw() 	        , fun yeec_token/3},
    {unique_kw		 ,unique_kw() 	        , fun yeec_token/3},
    {universal_kw	 ,universal_kw()        , fun yeec_token/3},	
    {universal_str_kw	 ,universal_str_kw()    , fun yeec_token/3},
    {utc_time_kw	 ,utc_time_kw()         , fun yeec_token/3},	
    {utf8string_kw	 ,utf8string_kw()       , fun yeec_token/3},
    {videotex_str_kw	 ,videotex_str_kw()     , fun yeec_token/3},
    {visible_str_kw	 ,visible_str_kw()      , fun yeec_token/3},
    {with_kw		 ,with_kw() 	        , fun yeec_token/3},
    %%Operators
    {assign_op   ,assign_op()    , fun yeec_token/3},  
    {bar   	  , bar() 	  , fun yeec_token/3},  
    {colon	  , colon()       , fun yeec_token/3},	       
    {comma	  , comma()       , fun yeec_token/3},	       
    {dot	  , dot() 	  , fun yeec_token/3},  	 	      
    {dotdot	  , dotdot()      , fun yeec_token/3},	      
    {ellipsis    , ellipsis()    , fun yeec_token/3},	    
    {less	  , less() 	  , fun yeec_token/3},  	 	      
    {l_brace     , l_brace()     , fun yeec_token/3},	     
    {l_bracket   , l_bracket()   , fun yeec_token/3},	   
    {l_paren     , l_paren()     , fun yeec_token/3},	     
    {minus	  , minus()       , fun yeec_token/3},	       
    {plus	  , plus() 	  , fun yeec_token/3},  	 	      
    {r_brace     , r_brace()     , fun yeec_token/3},	     
    {r_bracket   , r_bracket()   , fun yeec_token/3},	   
    {r_paren     , r_paren()     , fun yeec_token/3},	     
    {semi	 , semi() 	 , fun yeec_token/3},
    {number      , num()         , fun yeec_token/3},
    {b_string    , b_string()    , fun yeec_token/3},
    {h_string    , h_string()    , fun yeec_token/3},
    {c_string    , c_string()    , fun yeec_token/3},
    {lower       , lower() 	 , fun yeec_token/3},
    {upper       , upper()       , fun yeec_token/3}
   ]).

absent_kw() 		-> str("ABSENT").
abstract_syntax_kw() 	-> str("ABSTRACT-SYNTAX").
all_kw() 		-> str("ALL").
any_kw() 		-> str("ANY").
application_kw() 	-> str("APPLICATION").
automatic_kw() 		-> str("AUTOMATIC").
based_num_kw() 		-> str("BASEDNUM").
begin_kw() 		-> str("BEGIN").
bit_kw() 		-> str("BIT").
bmp_string_kw() 	-> str("BMPString").
boolean_kw() 		-> str("BOOLEAN").
by_kw() 		-> str("BY").
character_kw() 		-> str("CHARACTER_KW").
choice_kw() 		-> str("CHOICE").
class_kw() 		-> str("CLASS").
components_kw() 	-> str("COMPONENTS").
component_kw() 		-> str("COMPONENT").
constrained_kw() 	-> str("CONSTRAINED_KW").
default_kw() 		-> str("DEFAULT").
defined_kw() 		-> str("DEFINED").
definitions_kw() 	-> str("DEFINITIONS").
embedded_kw() 		-> str("EMBEDDED").
end_kw() 		-> str("END").
enumerated_kw() 	-> str("ENUMERATED").
except_kw() 		-> str("EXCEPT").
explicit_kw() 		-> str("EXPLICIT").
exports_kw() 		-> str("EXPORTS").
extensibility_kw() 	-> str("EXTENSIBILITY").
external_kw() 		-> str("EXTERNAL").
false_kw() 		-> str("FALSE").
from_kw() 		-> str("FROM").
generalized_time_kw() 	-> str("GeneralizedTime").
general_str_kw() 	-> str("GeneralString").
graphic_str_kw() 	-> str("GraphicString").
ia5_string_kw() 	-> str("IA5String").
identifier_kw() 	-> str("IDENTIFIER").
implicit_kw() 		-> str("IMPLICIT").
implied_kw() 		-> str("IMPLIED").
imports_kw() 		-> str("IMPORTS").
includes_kw() 		-> str("INCLUDES").
instance_kw() 		-> str("INSTANCE").
integer_kw() 		-> str("INTEGER").
intersection_kw() 	-> str("INTERSECTION").
iso646string_kw() 	-> str("ISO646String").
max_kw() 		-> str("MAX").
minus_infinity_kw() 	-> str("MINUSINFINITY").
min_kw() 		-> str("MIN").
null_kw() 		-> str("NULL").
numeric_str_kw() 	-> str("NumericString").
object_descriptor_kw() 	-> str("ObjectDescriptor").
object_kw() 		-> str("OBJECT").
octet_kw() 		-> str("OCTET").
of_kw() 		-> str("OF").
optional_kw() 		-> str("OPTIONAL").
pdv_kw() 		-> str("PDV").
plus_infinity_kw() 	-> str("PLUSINFINITY").
present_kw() 		-> str("PRESENT").
printable_str_kw() 	-> str("PrintableString").
private_kw() 		-> str("PRIVATE").
real_kw() 		-> str("REAL").
sequence_kw() 		-> str("SEQUENCE").
set_kw() 		-> str("SET").
size_kw() 		-> str("SIZE").
string_kw() 		-> str("STRING").
tags_kw() 		-> str("TAGS").
teletex_str_kw() 	-> str("TeletexString").
true_kw() 		-> str("TRUE").
type_identifier_kw() 	-> str("TYPE-IDENTIFIER").
union_kw() 		-> str("UNION").
unique_kw() 		-> str("UNIQUE").
universal_kw() 		-> str("UNIVERSAL").
universal_str_kw() 	-> str("UniversalString").
utc_time_kw() 		-> str("UTCTime").
utf8string_kw() 	-> str("UTF8String").
videotex_str_kw() 	-> str("VideotexString").
visible_str_kw() 	-> str("VisibleString").
with_kw() 		-> str("WITH").

%% Operators

assign_op() 	 -> str("::=").
bar() 		 -> c($|).
colon() 	 -> c($:).
comma() 	 -> c($,).
comment() 	 -> str("--").
dot() 		 -> c($.).
dotdot() 	 -> str("..").
ellipsis() 	 -> str("...").
less() 		 -> c($<).
l_brace() 	 -> c(${).
l_bracket() 	 -> c($[).
l_paren() 	 -> c($().
minus() 	 -> c($-).
plus() 		 -> c($+).
r_brace() 	 -> c($}).
r_bracket() 	 -> c($]).
r_paren() 	 -> c($)).
semi() 		 -> c($;).
single_quote() 	 -> c($').
double_quote()   -> c($").


%% Whitespace -- ignored

ws() ->	ci(" \t\f\r\n").

%% Single-line comments

sl_comment() ->
  '@'([comment(), eol('*'(cni("\n\r")))]).

num()   -> '+'(ci($0, $9)).
upper() -> '@'([ci($A, $Z), '*'(alnum())]).
lower() -> '@'([ci($a, $z), '*'(alnum())]).
alnum() -> '|'([ci($a, $z), ci($A, $Z), ci($0, $9), ci("-_")]).
  
bdig() -> ci("01").
hdig() -> '|'([ci($0, $9), ci($A, $F), ci($a, $f)]).

b_string() ->
  '@'([single_quote(),  '+'(bdig()), single_quote(), c($B)]).
h_string() ->
  '@'([single_quote(),  '+'(btw(4, 4, hdig())), single_quote(), c($H)]).
c_string() ->
  '@'([double_quote(),
       '*'('|'([alnum(), ws(), ci("~`$#@!?*%/&=|:,.<>{[()}]+;'")])),
       double_quote()]).

%%
%% Format funs
%%
yeec_token(Class, Line, Data) ->
  {Class, Line, Data}.
