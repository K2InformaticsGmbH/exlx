-module(xlx_xml).

-export([parse/1]).

parse(XmlBin) ->
    case xmerl_sax_parser:stream(
		XmlBin,
		[{event_state, []}, % empty Stack as State
			{event_fun, fun pe/3}]
	) of
        {ok, RespMap, <<>>} -> RespMap;
        Error -> error(Error)
    end.

% ignores
pe(startDocument,            _, S) -> S;
pe(endDocument,              _, S) -> S;
pe({ignorableWhitespace, _}, _, S) -> S;

pe({startPrefixMapping,	_, _}, _, S) -> S;
pe({endPrefixMapping, _},      _, S) -> S;
pe({startElement, Uri, LocalName, {Prefix, LocalName}, Attributes}, _, S) ->
	[maps_merge([
		#{name => LocalName},
		#{prefix => Prefix},
		#{uri => Uri},
		attrs(Attributes)
	]) | S];
% end of top level element : State Stack NOP
pe({endElement, Uri, LocalName, {_Prefix, LocalName}}, _,
	[#{uri := Uri, name := LocalName}] = S) ->
	S;
% end of intermediate level element
% Elm 		= pop(State-Stack)
% Parent 	= pop(State-Stack)
% push(Parent, 		Elm)
% push(State-Stack, Parent)
pe({endElement, Uri, LocalName, {_Prefix, LocalName}}, _,
	[#{uri := Uri, name := LocalName} = Elm, Prev | S]) ->
	[Prev#{elms =>
		case Prev of
			#{elms := Elms} -> Elms ++ [Elm];
			_ -> [Elm]
		end} | S];
pe({characters, Chrs}, _, [Elm|Rest]) ->
	[Elm#{characters => Chrs} | Rest];
pe(Evt, _Location, State) ->
	Error =
		list_to_binary(
			io_lib:format(
				"~p ~p State = ~p",
				[{?MODULE,?FUNCTION_NAME,?LINE}, Evt, State]
			)
		),
	throw({unhandled, Error}).

attrs([]) -> #{};
attrs(Attributes) ->
	attrs(Attributes, #{}).
attrs([], Attr) -> #{attrs => Attr};
attrs([{[], [], AttributeName, Value} | Attrs], Attr) ->
	attrs(Attrs, Attr#{AttributeName => Value});
attrs([{Uri, Prefix, AttributeName, Value} | Attrs], Attr) ->
	attrs(
		Attrs,
		Attr#{AttributeName => maps_merge([
			#{uri => Uri}, #{prefix => Prefix, val => Value}
		])}
	).

maps_merge(Maps) -> maps_merge(Maps, #{}).
maps_merge([], Map) -> Map;
maps_merge([Map | Maps], Acc) ->
	maps_merge(
		Maps,
		maps:fold(fun(K, V, M) when V /= [] -> M#{K => V};
					 (_, _, M) -> M
				  end, Acc, Map)
	).

-ifdef(CONSOLE).

f().
{ok, Bin} = file:read_file("xlsx_test/SBSREP111a/_rels/.rels").
xlx_xml:parse(Bin).

-endif.