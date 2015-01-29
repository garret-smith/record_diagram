-module(rd_extract_types).

-export([from_file/1,field_info/1,typename/1]).

-type record_type_info() :: {RecordName :: atom(), [field_type_info()]}.
-type field_type_info() :: {FieldName :: atom(), FieldType :: ft()}.
-type ft() :: atom() | [atom()] | {atom(), atom()}.

from_file(Path) ->
	{ok, Epp} = epp:open(Path, []),
	Forms = all_forms(Epp, []),
	epp:close(Epp),
	RecordInfo = lists:flatten([record_info(F) || F <- Forms]),
	RecordInfo.

all_forms(Epp, Acc) ->
	case epp:parse_erl_form(Epp) of
		{eof, _Line} -> lists:reverse(Acc);
		{error, Err} ->
			io:fwrite(standard_error, "parse error: ~p", [Err]),
			all_forms(Epp, Acc); % can't find an include, ignore
		{ok, Form} -> all_forms(Epp, [Form | Acc])
	end.

-spec record_info(Form :: term()) -> record_type_info() | [].
record_info({attribute, _, record, {Name, Fields}}) ->
	{Name, [field_info(F) || F <- Fields]};
record_info(_) ->
	[].

% field has no type info
field_info({record_field, _, {atom, _, FieldName}}) ->
	{FieldName, {atom, undefined}};
% field has no type info, but it does have an initializer
field_info({record_field, _, {atom, _, FieldName}, Init}) ->
	{FieldName, typename(Init)};
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
	{FieldName, typename(Type)};
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}, _}, Type}) ->
	{FieldName, typename(Type)}.

typename({var, _, Lit}) ->
	{literal, Lit};
typename({atom, _, Value}) ->
	{atom, Value};
typename({type, _, record, [{atom, _, RecType}]}) ->
	{record, RecType};
typename({type, _, list, [LType]}) ->
	{list, typename(LType)};
typename({type, _, Type, Subtypes}) when Type =:= union; Type =:= tuple ->
	{Type, [typename(T) || T <- Subtypes]};
typename({type, _, Type, Subtypes, []}) when Type =:= union; Type =:= tuple ->
	{Type, [typename(T) || T <- Subtypes]};
typename({type, _, TypeName, _}) ->
	{type, TypeName};
typename({remote_type, _, [{atom, _, Mod}, {atom, _, Type}, _]}) ->
	{type, {Mod, Type}};
typename({ann_type, _, [{var, _, _}, T]}) ->
	typename(T);
typename({A, _, _}) when is_atom(A) ->
	A.

