-module(exlx).
-include("xlx.hrl").

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

-ifdef(CONSOLE).

f().
exlx:unpack("xlsx_test/SBSREP111a.xlsm").

-endif.