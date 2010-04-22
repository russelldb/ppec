%%% @author Russell Brown <russell@pango.lan>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% A cobbled together stealing of functions from mochiweb_util, yaws_api and erlang-format to 
%%% ease dealing with paypal NVP api
%%% @end
%%% Created : 17 Apr 2010 by Russell Brown <russell@pango.lan>
-module(ppec_util).

-export([percent_encode/1, url_decode/1, to_params/1, parse_result/1]).

-import(lists, [reverse/1, reverse/2, flatten/1]).

-define(is_uppercase_alpha(C), C >= $A, C =< $Z).
-define(is_lowercase_alpha(C), C >= $a, C =< $z).
-define(is_alpha(C), ?is_uppercase_alpha(C); ?is_lowercase_alpha(C)).
-define(is_digit(C), C >= $0, C =< $9).
-define(is_alphanumeric(C), ?is_alpha(C); ?is_digit(C)).
-define(is_unreserved(C), ?is_alphanumeric(C); C =:= $-; C =:= $_; C =:= $.; C =:= $~).

percent_encode(undefined) ->
	[];
percent_encode(Atom) when is_atom(Atom) ->
	percent_encode(atom_to_list(Atom));
percent_encode(Int) when is_integer(Int) ->
	percent_encode(integer_to_list(Int));
percent_encode(Chars) when is_list(Chars) ->
	percent_encode(Chars, []).

percent_encode([], Encoded) ->
	flatten(reverse(Encoded));
percent_encode([C|Etc], Encoded) when ?is_unreserved(C) ->
	percent_encode(Etc, [C|Encoded]);
percent_encode([C|Etc], Encoded) ->
	Value = flatten(io_lib:format("%~2.1.0s", [erlang:integer_to_list(C, 16)])),
	percent_encode(Etc, [Value|Encoded]).

%% Percent decode
url_decode([$%, Hi, Lo | Tail]) ->
	Hex = erlang:list_to_integer([Hi, Lo], 16),
	[Hex | url_decode(Tail)];
url_decode([H|T]) when is_integer(H) ->
	[H |url_decode(T)];
url_decode([]) ->
	[];
url_decode([H|T]) when is_list(H) ->
	[url_decode(H) | url_decode(T)].

%%Turn a proplist into a post body
to_params(Params) ->
	[["&",atom_to_list(Name), "=", percent_encode(Value)] || {Name, Value} <- Params].

%% Parse the paypal NVP response into a proplist
parse_result(Result) ->
	parse(Result, []).

parse([], Acc) ->
	lists:reverse(Acc);
parse(Result, Acc) ->
	{Key, Rest} = parse_key(Result, []),
	{Value, Remains} = parse_val(Rest, []),
	parse(Remains, [{Key, url_decode(Value)}|Acc]).

parse_key([$=|Rest], Yek) ->
	{lists:reverse(Yek), Rest};
parse_key([C|Rest], Acc) ->
	parse_key(Rest, [C|Acc]).

parse_val([], Lav) ->
	{lists:reverse(Lav), []};
parse_val([$&|Rest], Lav) ->
	{lists:reverse(Lav), Rest};
parse_val([C|Rest], Acc) ->
	parse_val(Rest, [C|Acc]).
