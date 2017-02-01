%% Copyright 2017 Jesse Gumm
%% MIT LICENSE

-module(rekt).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Forms, _Options) ->
    Recs = extract_records(Forms),
    NewForms = replace_extends(Forms, Recs),
    NewForms.

%%====================================================================
%% Internal functions
%%====================================================================

extract_records(Forms) ->
    [Rec || {attribute, _, record, Rec} <- Forms].

replace_extends(Forms, Recs) ->
    lists:map(fun
        ({attribute, LineNum, extend, {SourceRec, NewRec, Fields}}) ->
            replace_extend(Recs, LineNum, SourceRec, NewRec, Fields);
        (Other) ->
            Other
    end, Forms).

replace_extend(Recs, LineNum, SourceRec, NewRec, Fields) ->
    {_, OrigFields} = lists:keyfind(SourceRec, 1, Recs),
    NewFields = [field_to_form(F, LineNum) || F <- Fields],
    FullFields = OrigFields ++ NewFields,
    {attribute, LineNum, record, {NewRec, FullFields}}.
    
field_to_form(Field, LineNum) when is_atom(Field) ->
    {record_field, LineNum, {atom, LineNum, Field}};
field_to_form({Field, Value}, LineNum) when is_atom(Field) ->
    FormattedValue = erl_parse:abstract(Value, [{line, LineNum}]),
    {record_field, LineNum, {atom, LineNum, Field}, FormattedValue}.

