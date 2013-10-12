%% -*- erlang -*-
%%! -smp enable

-module(record_diagram).

-export([main/1]).

main(A) ->
	{ok, [[HomeDir]]} = init:get_argument(home),
	JarPath = filename:join([HomeDir, ".record_diagram/plantuml.jar"]),
	{Opts, Files} = proc_args(A, [], []),
	case proplists:get_bool(help, Opts) of
		true -> usage() ;
		false ->
			IoList = plantuml_text(Files, Opts),
			case proplists:get_value(out, Opts) of
				undefined ->
					case proplists:get_value(png, Opts) of
						undefined ->
							file:write(standard_io, IoList);
						PngFile ->
							TempFile = filename:rootname(PngFile) ++ ".tmp",
							extract_jar(JarPath),
							ok = file:write_file(TempFile, IoList),
							os:cmd(f("java -jar ~s ~s", [JarPath, TempFile])),
							file:delete(TempFile)
					end;
				IoDev ->
					file:write(IoDev, IoList),
					file:close(IoDev)
			end
	end
	.

plantuml_text(Files, Opts) ->
	RecordInfo = parallel_read(Files),

	try
		Rels = record_relationships(RecordInfo),
		UmlText = [
			f("@startuml~n"),
			case proplists:get_bool(linked, Opts) of
				false ->
					[record_text(Record) || Record <- RecordInfo]
					;
				true ->
					{Froms, Tos} = lists:unzip(Rels),
					RelatedRecs = uniq_list(Froms ++ Tos),
					[record_text(Record) || {record, RName, _} = Record <- RecordInfo, lists:member(RName, RelatedRecs)]
			end,
			[[f("\"~s\" *-- \"~s\"~n", [fmt_type(From), fmt_type(ToType)]) || ToType <- subtypes(To)] || {From, To} <- Rels],
			f("@enduml~n")
		]
	catch
		E:R ->
			io:fwrite(standard_error, "Error writing relationships:~n~p:~p~n~p", [E, R, erlang:get_stacktrace()])
	end
	.

usage() ->
	io:fwrite("Usage: record_diagram [options] files...~n~n"),
	io:fwrite("options:~n"),
	io:fwrite("\t--help           - Print this help~n"),
	io:fwrite("\t--linked         - Display only records containing/contained by other records~n"),
	io:fwrite("\t--out filename   - Write intermediate PlantUML text file.  No PNG file will be generated.~n"),
	io:fwrite("\t--png filename   - Name of output image file~n"),
	io:fwrite("~n")
	.

proc_args([], Opts, Files) ->
	{Opts, Files}
	;
proc_args(["--linked" | Args], Opts, Files) ->
	proc_args(Args, [linked | Opts], Files)
	;
proc_args(["--help" | Args], Opts, Files) ->
	proc_args(Args, [help | Opts], Files)
	;
proc_args(["--out" | Args], Opts, Files) ->
	{ok, IoDev} = file:open(hd(Args), [write]),
	proc_args(tl(Args), [{out, IoDev} | Opts], Files)
	;
proc_args(["--png" | Args], Opts, Files) ->
	proc_args(tl(Args), [{png, hd(Args)} | Opts], Files)
	;
proc_args([A | Args], Opts, Files) ->
	proc_args(Args, Opts, [A | Files])
	.

parallel_read(Paths) ->
	Self = self(),
	Pids = [spawn(fun() -> Self ! read_file_records(Path) end) || Path <- Paths],
	Responses = [receive X->X end || _ <- Pids],
	lists:flatten(Responses)
	.

read_file_records(Path) ->
	Modname = filename:rootname(filename:basename(Path)),
	{ok, Epp} = epp:open(Path, []),
	try
		RecordInfo = read_forms(Epp),
		epp:close(Epp),
		[{record, {list_to_atom(Modname), RName}, Fields} || {record, RName, Fields} <- RecordInfo]
	catch
		E:R ->
			io:fwrite(standard_error, "Error processing file '~s'~n~p:~p~n~p", [Path, E, R, erlang:get_stacktrace()]),
			{E,R}
	end
	.

extract_jar(JarPath) ->
	case filelib:is_dir(filename:dirname(JarPath)) of
		true -> ok;
		false -> ok = file:make_dir(filename:dirname(JarPath))
	end,
	case filelib:is_file(JarPath) of
		true -> already_created;
		false ->
			ArchiveFileName = "record_diagram/ebin/plantuml.jar",
			{ok, Sections} = escript:extract("record_diagram", []),
			Archive = proplists:get_value(archive, Sections),
			{ok, [{ArchiveFileName, PlantumlJar}]} = zip:extract(Archive, [{file_list, [ArchiveFileName]}, memory]),
			ok = file:write_file(JarPath, PlantumlJar)
	end
	.

f(Fmt) -> io_lib:format(Fmt, []) .
f(Fmt, L) -> io_lib:format(Fmt, L) .

record_text({record, {Modname, RecName}, Fields}) ->
	[
		f("class \"~s:~s\" {~n", [Modname, RecName]),
		[field_text(Field) || Field <- Fields],
		f("}~n")
	]
	.

field_text({field, FieldName, Types}) -> f("\t~p : ~s~n", [FieldName, fmt_type(Types)]) .

fmt_type({{}, Type}) -> "{" ++ string:join([f("~s", [fmt_type(T)]) || T <- Type], ", ") ++ "}" ;
fmt_type({[], Type}) -> f("~s[]", [fmt_type(Type)]) ;
fmt_type({union, Type}) -> fmt_type(Type) ;
fmt_type({Mod, Type}) -> f("~p:~p", [Mod, Type]) ;
fmt_type([]) -> "none" ;
fmt_type([T]) -> f("~s", [fmt_type(T)]) ;
fmt_type(Tl) when is_list(Tl) -> string:join([fmt_type(T) || T <- Tl -- [undefined]], " | ") ;
fmt_type(T) when is_atom(T) -> f("~p", [T]) .

record_relationships(RecordInfo) ->
	RecordTypes = [Rec || {record, Rec, _} <- RecordInfo],
	%io:fwrite("DBG: looking for relationships~nRecordTypes: ~p~nRecordInfo: ~p~n", [RecordTypes, RecordInfo]),
	Relationships = [record_rel(Record, RecordTypes) || Record <- RecordInfo],
	uniq_list(lists:flatten(Relationships))
	.

record_rel({record, {Mod, RecName}, Fields}, RecordTypes) ->
	FieldTypes = uniq_list(lists:flatten([subtypes(FieldTypes) || {field, _, FieldTypes} <- Fields])),
	%io:fwrite("DBG: ~p field types: ~p~n", [{Mod, RecName}, FieldTypes]),
	[{{Mod, RecName}, complete_type(Type, Mod)} || Type <- FieldTypes, typematch(Type, RecordTypes, Mod)]
	.

complete_type({_,_} = T, _Mod) -> T ;
complete_type(T, Mod) -> {Mod, T} .

subtypes({{}, Type}) -> subtypes(Type) ;
subtypes({[], Type}) -> subtypes(Type) ;
subtypes({union, Type}) -> subtypes(Type) ;
subtypes({M, _} = Type) when is_atom(M) -> [Type] ;
subtypes(Type) when is_atom(Type) -> [Type] ;
subtypes(Type) when is_atom(Type) -> [Type] ;
subtypes(Type) when is_list(Type) -> lists:flatten([subtypes(T) || T <- Type]) .

typematch({{}, Type}, RecordTypes, Mod) ->
	typematch(Type, RecordTypes, Mod)
	;
typematch({[], Type}, RecordTypes, Mod) ->
	typematch(Type, RecordTypes, Mod)
	;
typematch({union, Type}, RecordTypes, Mod) ->
	typematch(Type, RecordTypes, Mod)
	;
typematch({_, _} = Type, RecordTypes, _Mod) ->
	lists:member(Type, RecordTypes)
	;
typematch(Type, RecordTypes, Mod) when is_list(Type) ->
	lists:any(fun(Subtype) -> typematch(Subtype, RecordTypes, Mod) end, Type)
	;
typematch(undefined, _, _) ->
	false
	;
typematch(Type, RecordTypes, Mod) when is_atom(Type) ->
	lists:member({Mod, Type}, RecordTypes)
	.

uniq_list([]) -> [];
uniq_list(L) ->
	L1 = lists:sort(L),
	uniq_list0(hd(L1), tl(L1), [])
	.
uniq_list0(First, [], Acc) ->
	[First | Acc]
	;
uniq_list0(First, [Next | L], Acc) when First == Next ->
	uniq_list0(Next, L, Acc)
	;
uniq_list0(First, [Next | L], Acc) ->
	uniq_list0(Next, L, [First | Acc])
	.


read_forms(Epp) ->
	read_forms(epp:parse_erl_form(Epp), Epp, [])
	.

read_forms({eof, _Line}, _Epp, Acc) ->
	Acc
	;
read_forms({ok, {attribute, _, record, {Name, Fields}}}, Epp, Acc) ->
	read_forms(epp:parse_erl_form(Epp), Epp, [{record, Name, [field_info(F) || F <- Fields]} | Acc])
	;
read_forms(_, Epp, Acc) ->
	read_forms(epp:parse_erl_form(Epp), Epp, Acc)
	.

field_info({record_field, _, {atom, _, FieldName}}) ->
	{field, FieldName, []}
	;
field_info({record_field, _, {atom, _, FieldName}, _}) ->
	{field, FieldName, []}
	;
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
	{field, FieldName, [field_type(Type)]}
	;
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}, _}, Type}) ->
	{field, FieldName, [typename(Type)]}
	.


field_type({type, _, T, []}) when is_atom(T) ->
	T
	;
field_type({type, _, T, Ttype}) when is_atom(T) ->
	{T, union_types(Ttype, [])}
	;
field_type({type, _, union, UnionTypes}) ->
	{union, union_types(UnionTypes, [])}
	.

union_types([], Acc) ->
	Acc
	;
union_types([T | UnionTypes], Acc) ->
	union_types(UnionTypes, [typename(T) | Acc])
	.

typename({var, _, Lit}) ->
	Lit
	;
typename({atom, _, TypeName}) ->
	TypeName
	;
typename({type, _, list, Subtype}) ->
	{[], union_types(Subtype, [])}
	;
typename({type, _, TypeName, _}) ->
	TypeName
	;
typename({remote_type, _, [{atom, _, Mod}, {atom, _, Type}, L]}) when is_list(L) ->
	{Mod, Type}
	;
typename({A, _, _}) when is_atom(A) ->
	A
	.


