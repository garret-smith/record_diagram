-module(rd_extract_types).

-export([from_epp/1,from_file/1]).

-type record_type_info() :: {record, RecordName :: atom(), [field_type_info()]}.
-type field_type_info() :: {field, FieldName :: atom(), FieldType :: ft()}.
-type ft() :: atom() | [atom()] | {atom(), atom()}.

from_file(Path) ->
	{ok, Epp} = epp:open(Path, []),
	RecordInfo = from_epp(Epp),
	epp:close(Epp),
	RecordInfo.

-spec from_epp(epp:epp_handle()) -> [record_type_info()].
from_epp(Epp) ->
	from_forms(epp:parse_erl_form(Epp), Epp, []).

from_forms({eof, _Line}, _Epp, Acc) ->
	Acc;
from_forms({ok, {attribute, _, record, {Name, Fields}}}, Epp, Acc) ->
	from_forms(epp:parse_erl_form(Epp), Epp, [{record, Name, [field_info(F) || F <- Fields]} | Acc]);
from_forms(_, Epp, Acc) ->
	from_forms(epp:parse_erl_form(Epp), Epp, Acc).

% field has no type info
field_info({record_field, _, {atom, _, FieldName}}) ->
	{field, FieldName, []};
% field has no type info, but it does have an initializer
field_info({record_field, _, {atom, _, FieldName}, Init}) ->
	{field, FieldName, [typename(Init)]};
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
	{field, FieldName, [typename(Type)]};
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}, _}, Type}) ->
	{field, FieldName, [typename(Type)]}.

typename({var, _, Lit}) ->
	Lit;
typename({atom, _, TypeName}) ->
	TypeName;
typename({type, _, Type, Subtypes}) when is_list(Subtypes) andalso (Type == list orelse Type == union orelse Type == tuple) ->
	{Type, [typename(T) || T <- Subtypes]};
typename({type, _, TypeName, _}) ->
	TypeName;
typename({remote_type, _, [{atom, _, Mod}, {atom, _, Type}, L]}) when is_list(L) ->
	{Mod, Type};
typename({ann_type, _, [{var, _, _}, T]}) ->
	typename(T);
typename({A, _, _}) when is_atom(A) ->
	A.

