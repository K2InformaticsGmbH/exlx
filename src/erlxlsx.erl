-module(erlxlsx).
-include("xlx.hrl").

-export([create/4, create/5]).

-export([unpack/1, unpack/2, pack/1, write_xlx/2, unpack_part/5]).

unpack(XlxFile) ->
    {ok, XlxBin} = file:read_file(XlxFile),
    unpack(XlxFile, XlxBin).
unpack(XlxFile, XlxBin) ->
    ZipFile = filename:rootname(filename:basename(XlxFile)) ++ ".zip",
    case zip:foldl(
            fun (FileInArchive, GetInfo, GetBin, AccIn) ->
                [spawn(
                    ?MODULE, unpack_part,
                    [self(), FileInArchive,
                     string:to_lower(filename:extension(FileInArchive)),
                     GetInfo(), GetBin()]
                 ) | AccIn]
            end,
            [], {ZipFile, XlxBin}
        ) of
        {ok, ProducerPids} ->
            #{file => XlxFile,
              content => collect(ProducerPids, #{})};
        Error -> Error
    end.

unpack_part(ConsumerPid, FileInArchive, Extn, Info, Bin)
    when Extn == ".vml"; Extn == ".rels"; Extn == ".xml"  ->
    ConsumerPid !
        {self(), FileInArchive,
         #{info => Info, content => xlx_xml:parse(Bin)}};
unpack_part(ConsumerPid, FileInArchive, _Extn, Info, Bin)  ->
    ConsumerPid !
        {self(), FileInArchive,
         #{info => Info, content => Bin}}.

collect([], Map) -> Map;
collect(ProducerPids, Map) ->
    receive
        {Pid, FileInArchive, Content} ->
            collect(
                ProducerPids -- [Pid],
                Map#{FileInArchive => Content}
            )
    after 1000 ->
        collect(
            [P || P <- ProducerPids, catch is_process_alive(P) == true], Map
        )
    end.

pack(#{content := ContentMap}) -> pack(ContentMap);
pack(ContentMap) ->
    case zip:create(
            "temp.zip",
            maps:fold(
                fun(FileInArchieve, #{content := Bin}, Acc) ->
                    [{FileInArchieve, Bin} | Acc]
                end, [],  ContentMap
            ), [memory]
        ) of
        {ok, {"temp.zip", ZipBin}} -> {ok, ZipBin};
        Error -> Error
    end.

write_xlx(Target, Content) ->
    {ok, Bin} = pack(Content),
    file:write_file(Target, Bin).

% @doc
% * see create/5
create(File, SheetTitle, Sheet, Style) ->
    create(File, SheetTitle, Sheet, Style, #{}).

% @doc
% * ColumnID can only be one/two uppersace letters (^[A-Z]{1,2}$)
-spec create(File :: file:name(),
                  SheetTitle :: binary(),
                  Sheet :: #{RowId :: integer() =>
                             #{ColumnId :: list()|atom()|binary() =>
                               Value :: list()|binary()|integer()|float()|map()}
                            },
                  Style :: #{Fonts :: fonts => any()},
                  AutoFilterBin :: binary()) ->
%                  Style :: #{Fonts :: fonts|<<"fonts">> => [#{name|<<"name">> => list()|binary(),
%                                                     color|<<"color">> => list()|binary(),
%                                                     size|<<"size">> => float(),
%                                                     bold|<<"bold">> => true|false,
%                                                     italics|<<"italics">> => true|false,
%                                                     underline|<<"underline">> => true|false,
%                                                     strike|<<"strike">> => true|false}],
%                            fills|<<"fills">> => [#{type|<<"type">> => list()|binary(),
%                                                    fg|<<"fg">> => list()|binary(),
%                                                    bg|<<"bg">> => list()|binary()}],
%                            xfs|<<"xfs">> => [#{fill|<<"fill">> => integer(),
%                                                font|<<"font">> => integer()}]}) ->
    {ok, binary()} | {error, term()}.
create(_, _, _, _, #{autoFilter := AF}) when not is_binary(AF) ->
    error(bad_autofilter);
create(_, _, _, _, #{hyperlinks := H}) when not is_map(H) ->
    error(bad_hyperlinks);
create(File, SheetTitle, Sheet, Style, Opts)
  when is_binary(SheetTitle), is_map(Sheet), is_map(Style), is_map(Opts) ->
    try
        AutoFilterData = autofilter(Opts),
        {SheetData, RelLinks} = to_sheet_data(Sheet, AutoFilterData),
        StyleData = to_style_data(Style),
        case zip:zip(
               File,
               [{?CONTENT_TYPES_PATH, ?CONTENT_TYPES_BIN},
                {?DOT_RELS_PATH, ?DOT_RELS_BIN},
                {?WORKBOOK_RELS_PATH, ?WORKBOOK_RELS_BIN}]++
                if byte_size(RelLinks) > 0 ->
                    [{?WORKSHEET_RELS_PATH, ?WORKSHEET_RELS_BIN(RelLinks)}];
                    true -> []
                end ++
               [{?STYLES_PATH, ?STYLES_BIN(StyleData)},
                {?WORKBOOK_PATH, ?WORKBOOK_BIN(SheetTitle)},
                {?WORKSHEET_PATH, ?WORKSHEET_BIN(SheetData)}
               ], [memory]) of
            {ok, {File, FileContent}} -> {ok, FileContent};
            {error, Reason} -> {error, Reason}
        end
    catch
        error:E -> {error,E}
    end.

-spec autofilter(map()) -> binary().
autofilter(#{autoFilter := AutoFilter}) ->
    if
        byte_size(AutoFilter) == 0 -> <<>>;
        byte_size(AutoFilter) > 0 ->
            case re:run(AutoFilter, "^[A-Z]+[0-6]+:[A-Z]+[0-6]+$") of
                nomatch -> error({badfilter,AutoFilter});
                _ -> <<"<autoFilter ref=\"",AutoFilter/binary,"\"/>">>
            end
    end;
autofilter(_) -> <<>>.

-spec to_sheet_data(map(), binary()) -> binary().
to_sheet_data(Sheet, Extra) when is_map(Sheet), is_binary(Extra) ->
    {Res, HyperLinks} = extract_href(Sheet),
    {<<"<sheetData>",
        (lists:foldl(
            fold_sheet(Sheet), <<>>,
            lists:usort(maps:keys(Sheet))))/binary,
      "</sheetData>",
      Extra/binary,
      (if byte_size(HyperLinks) > 0 ->
        <<"<hyperlinks>",HyperLinks/binary,"</hyperlinks>">>;
        true -> <<>>
      end)/binary>>, Res}.

-spec extract_href(map()) -> {binary(), binary()}.
extract_href(Sheet) when is_map(Sheet) ->
    maps:fold(
        fun(R, RowMap, Buf) ->
            maps:fold(
                fun(C, #{href := Link}, IBuf) ->
                        hyperlink_i(R, C, Link, IBuf);
                   (_, _, IBuf) -> IBuf
                end, Buf, RowMap
            )
        end, {<<>>, <<>>}, Sheet
    ).
hyperlink_i(R, C, Link, {Rel, Data}) when is_integer(R), is_atom(C) ->
    Cell = list_to_binary(io_lib:format("~s~p", [atom_to_list(C), R])),
    RId = list_to_binary(io_lib:format("rId~s", [Cell])),
    HLink = list_to_binary(io_lib:format("~s", [Link])),
    {<< Rel/binary,
        "<Relationship Id=\"",RId/binary,"\""
                     " Type=\""?OXML_ODR"/hyperlink\""
                     " Target=\"",HLink/binary,"\""
                     " TargetMode=\"External\" />"
     >>,
     << Data/binary,
        "<hyperlink ref=\"",Cell/binary,"\" r:id=\"",RId/binary,"\"/>">>}.

fold_sheet(Sheet) ->
    fun(RowId, XML) ->
        RowIdBin = integer_to_binary(RowId),
        Row = maps:get(RowId, Sheet),
        <<XML/binary, "<row r=\"",RowIdBin/binary,"\">",
        (lists:foldl(
            fold_row(RowIdBin, Row), <<>>,
            lists:usort(maps:keys(Row))))/binary,
        "</row>">>
      end.

fold_row(RowIdBin, Row) ->
    fun(Col, ColXML) ->
        ColBin =
            if is_atom(Col) -> atom_to_binary(Col, utf8);
                is_list(Col) -> list_to_binary(Col);
                is_binary(Col) -> Col;
                true -> error({badcolumn, Col})
            end,
        case re:run(ColBin,"^[A-Z]{1,2}$") /= nomatch of
            true -> ok;
            _ -> error({badcolumnid, Col})
        end,
        Val = maps:get(Col,Row),
        {V, Stl} =
            case Val of
                #{style := S, data := D} -> {D, S+1};
                #{<<"style">> := S, <<"data">> := D} -> {D, S+1};
                D -> {D, 0}
            end,
        <<ColXML/binary,
          "<c s=\"",(integer_to_binary(Stl))/binary,
          "\" r=\"",ColBin/binary,RowIdBin/binary,"\"",
          (case V of
                V when is_binary(V) ->
                    <<" t=\"inlineStr\"><is><t>", V/binary, "</t></is>">>;
                V when is_list(V) ->
                    <<" t=\"inlineStr\"><is><t>", (list_to_binary(V))/binary, "</t></is>">>;
                V when is_integer(V) -> <<"><v>", (integer_to_binary(V))/binary, "</v>">>;
                V when is_float(V) -> <<"><v>", (float_to_binary(V))/binary, "</v>">>;
                V -> error({badvalue, V})
          end)/binary,
          "</c>">>
    end.

-define(MAPSGET(__K,__M,__D), maps:get(<<??__K>>, __M, maps:get(??__K, __M, maps:get(__K, __M, __D)))).
to_style_data(Style) ->
    FontBins =
        lists:foldl(
            fun(Font, Fonts) when is_map(Font) ->
                    [<<"<font>",
                    (list_to_binary(
                        [case ?MAPSGET(bold,Font,false) == true of
                            true -> "<b/>";
                            _ -> ""
                        end,
                        case ?MAPSGET(italics,Font,false) == true of
                            true -> "<i/>";
                            _ -> ""
                        end,
                        case ?MAPSGET(underline,Font,false) == true of
                            true -> "<u/>";
                            _ -> ""
                        end,
                        case ?MAPSGET(strike,Font,false) == true of
                            true -> "<strike/>";
                            _ -> ""
                        end,
                        case ?MAPSGET(size,Font,none) of
                            none -> "";
                            Size -> ["<sz val=\"",float_to_list(Size,[{decimals,2},compact]),"\"/>"]
                        end,
                        case ?MAPSGET(color,Font,none) of
                            none -> "";
                            Color -> ["<color rgb=\"",Color,"\"/>"]
                        end,
                        case ?MAPSGET(name,Font,none) of
                            none -> "";
                            Name -> ["<name val=\"",Name,"\"/>"]
                        end]))/binary,
                    "</font>">> | Fonts]
            end,
            [], ?MAPSGET(fonts, Style, [])),
    FontsCount = integer_to_binary(length(FontBins)+1),
    FillBins =
        lists:foldl(
            fun(Fill, Fills) when is_map(Fill) ->
                    [<<"<fill><patternFill patternType=\"",
                    (list_to_binary(
                        [case ?MAPSGET(type,Fill,none) of
                            none -> "solid";
                            Type -> Type
                        end,"\"><fgColor rgb=\"",
                        case ?MAPSGET(fg,Fill,none) of
                            none -> "FFFFFFFF";
                            FgCol -> FgCol
                        end,"\"/><bgColor rgb=\"",
                        case ?MAPSGET(bg,Fill,none) of
                            none -> "FFFFFFFF";
                            FgCol -> FgCol
                        end,"\"/>"]))/binary,
                    "</patternFill></fill>">> | Fills]
            end,
            [], ?MAPSGET(fills,Style,[])),
    FillsCount = integer_to_binary(length(FillBins)+2),
    XfsBins =
        lists:foldl(
            fun(Xf, Xfs) when is_map(Xf) ->
                    [<<"<xf fillId=\"",
                    (list_to_binary(
                        [case ?MAPSGET(fill,Xf,none) of
                            none -> "0";
                            FillId -> integer_to_list(FillId+2)
                        end,"\" fontId=\"",
                        case ?MAPSGET(font,Xf,none) of
                            none -> "0";
                            FontId -> integer_to_list(FontId+1)
                        end,"\""]))/binary,
                    " xfId=\"0\" applyFill=\"1\"/>">> | Xfs]
            end,
            [], ?MAPSGET(xfs,Style,[])),
    XfsCount = integer_to_binary(length(XfsBins)+1),
    
    <<"<fonts count=\"",FontsCount/binary,"\"><font/>",
      (list_to_binary(lists:reverse(FontBins)))/binary,"</fonts>"

      "<fills count=\"",FillsCount/binary,"\"><fill/>"
      "<fill><patternFill patternType=\"lightGray\"/></fill>",
      (list_to_binary(lists:reverse(FillBins)))/binary,
      "</fills>"

      "<borders count=\"1\"><border/></borders>"
      "<cellStyleXfs count=\"1\"><xf/></cellStyleXfs>"

      "<cellXfs count=\"",XfsCount/binary,"\"><xf/>",
      (list_to_binary(lists:reverse(XfsBins)))/binary,
      "</cellXfs>">>.
