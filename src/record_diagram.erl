%% -*- erlang -*-
%%! -smp enable

-module(record_diagram).

-compile(export_all).

-export([main/1]).

main(A) ->
	{ok, [[HomeDir]]} = init:get_argument(home),
	JarPath = filename:join([HomeDir, ".record_diagram/plantuml.jar"]),
	{Opts, Files} = proc_args(A, [], []),
	case proplists:get_bool(help, Opts) of
		true -> usage() ;
		false ->
			IoList = plantuml_text(Files, Opts),
			Out = proplists:get_value(out, Opts),
			Png = proplists:get_value(png, Opts),
			Eps = proplists:get_value(eps, Opts),
			case {Out, Png, Eps} of
				{undefined, undefined, undefined} ->
					file:write(standard_io, IoList);
				{IoDev, undefined, undefined} ->
					file:write(IoDev, IoList),
					file:close(IoDev);
				{undefined, Png, undefined} ->
					TempFile = filename:rootname(Png) ++ ".tmp",
					extract_jar(JarPath),
					ok = file:write_file(TempFile, IoList),
					os:cmd(f("java -jar ~s ~s", [JarPath, TempFile])),
					file:delete(TempFile);
				{undefined, undefined, Eps} ->
					TempFile = filename:rootname(Eps) ++ ".tmp",
					extract_jar(JarPath),
					ok = file:write_file(TempFile, IoList),
					os:cmd(f("java -jar ~s -teps ~s", [JarPath, TempFile])),
					file:delete(TempFile)
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
					RelatedRecs = lists:usort(Froms ++ Tos),
					[record_text(Record) || {RName, _} = Record <- RecordInfo,
								lists:member(RName, RelatedRecs)]
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
	io:fwrite("\t--eps filename   - Name of output EPS file~n"),
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
proc_args(["-h" | Args], Opts, Files) ->
	proc_args(Args, [help | Opts], Files)
	;
proc_args(["--out" | Args], Opts, Files) ->
	{ok, IoDev} = file:open(hd(Args), [write]),
	proc_args(tl(Args), [{out, IoDev} | Opts], Files)
	;
proc_args(["--png" | Args], Opts, Files) ->
	proc_args(tl(Args), [{png, hd(Args)} | Opts], Files)
	;
proc_args(["--eps" | Args], Opts, Files) ->
	proc_args(tl(Args), [{eps, hd(Args)} | Opts], Files)
	;
proc_args([A | Args], Opts, Files) ->
	proc_args(Args, Opts, [A | Files])
	.

parallel_read(Paths) ->
	Self = self(),
	Pids = [spawn(fun() -> Self ! rd_extract_types:from_file(Path) end) || Path <- Paths],
	Responses = [receive X->X end || _ <- Pids],
	lists:flatten(Responses)
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
			{ok, Sections} = escript:extract(escript:script_name(), []),
			Archive = proplists:get_value(archive, Sections),
			{ok, [{ArchiveFileName, PlantumlJar}]} = zip:extract(Archive, [{file_list, [ArchiveFileName]}, memory]),
			ok = file:write_file(JarPath, PlantumlJar)
	end
	.

f(Fmt) -> io_lib:format(Fmt, []) .
f(Fmt, L) -> io_lib:format(Fmt, L) .

record_text({RecName, Fields}) ->
	[
		f("class \"~s\" {~n", [RecName]),
		[field_text(Field) || Field <- Fields],
		f("}~n")
	]
	.

field_text({FieldName, Types}) -> f("\t~p : ~s~n", [FieldName, fmt_type(Types)]) .

fmt_type({list, Type}) -> f("[~s]", [fmt_type(Type)]) ;
fmt_type({tuple, Type}) -> "{" ++ string:join([f("~s", [fmt_type(T)]) || T <- Type], ", ") ++ "}" ;
fmt_type({union, Type}) -> fmt_type(Type) ;
fmt_type({atom, A}) -> f("'~p'", [A]) ;
fmt_type({type, A}) when is_atom(A) -> fmt_type(A) ;
fmt_type({type, {Mod, Type}}) -> f("~p:~p", [Mod, Type]) ;
fmt_type([]) -> "none" ;
fmt_type([T]) -> f("~s", [fmt_type(T)]) ;
fmt_type(Tl) when is_list(Tl) -> string:join([fmt_type(T) || T <- Tl -- [{atom, undefined}]], " | ") ;
fmt_type(T) when is_atom(T) -> f("~p", [T]) .

record_relationships(RecordInfo) ->
	RecordTypes = [Rec || {Rec, _} <- RecordInfo],
	%io:fwrite("DBG: looking for relationships~nRecordTypes: ~p~nRecordInfo: ~p~n", [RecordTypes, RecordInfo]),
	Relationships = [record_rel(Record, RecordTypes) || Record <- RecordInfo],
	lists:usort(lists:flatten(Relationships))
	.

record_rel({RecName, Fields}, RecordTypes) ->
	FieldTypes = lists:usort(lists:flatten([subtypes(FieldTypes) || {_, FieldTypes} <- Fields])),
	% find anything in FieldTypes that matches something in RecordTypes
	[{RecName, DestType} || {type, DestType} <- FieldTypes, lists:member(DestType, RecordTypes)]
	.

subtypes({list, Type}) -> subtypes(Type) ;
subtypes({tuple, Type}) -> subtypes(Type) ;
subtypes({union, Type}) -> subtypes(Type) ;
subtypes({M, _} = Type) when is_atom(M) -> [Type] ;
subtypes(Type) when is_atom(Type) -> [Type] ;
subtypes(Type) when is_atom(Type) -> [Type] ;
subtypes(Type) when is_list(Type) -> lists:flatten([subtypes(T) || T <- Type]) .

typematch({list, Type}, RecordTypes, Mod) ->
	typematch(Type, RecordTypes, Mod)
	;
typematch({tuple, Type}, RecordTypes, Mod) ->
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

