%% Copyright 2017 Jesse Gumm
%% MIT LICENSE

-module(rekt).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Forms, _Options) ->
    %io:fwrite("Forms: ~p",[Forms]),
    NewForms = transform_until_done(Forms),
    FinalForms = replace_unhandled_extends_with_errors(NewForms),
    %io:fwrite("Final Forms: ~p",[FinalForms]),
    FinalForms.


%%====================================================================
%% Internal functions
%%====================================================================

transform_until_done(Forms) ->
    Recs = extract_records(Forms),
    NewForms = replace_extends(Forms, Recs),
    case NewForms==Forms of
        true -> NewForms;
        false -> transform_until_done(NewForms)
    end.

replace_unhandled_extends_with_errors(Forms) ->
    lists:map(fun
        ({attribute, LineNum, extend, {SourceRec, _NewRec, _Fields}}) ->
            ErrMsg = lists:flatten(io_lib:format("rekt: Unable to extend undefined record '~p'", [SourceRec])),
            {error, {LineNum, erl_parse, ErrMsg}};
        (Other) ->
            Other
    end, Forms).


extract_records(Forms) ->
    [Rec || {attribute, _, record, Rec} <- Forms].

replace_extends(Forms, Recs) ->
    lists:map(fun
        ({attribute, LineNum, extend, {SourceRec, NewRec, Fields}}=RawFull) ->
            replace_extend(Recs, LineNum, SourceRec, NewRec, Fields, RawFull);
        (Other) ->
            Other
    end, Forms).

replace_extend(Recs, LineNum, SourceRec, NewRec, Fields, RawFull) ->
    case lists:keyfind(SourceRec, 1, Recs) of
        {_, OrigFields} ->
            NewFields = [field_to_form(F, LineNum) || F <- Fields],
            FullFields = merge_fields(OrigFields, NewFields),
            {attribute, LineNum, record, {NewRec, FullFields}};
        false ->
            RawFull
    end.
    
field_to_form(Field, LineNum) when is_atom(Field) ->
    {record_field, LineNum, {atom, LineNum, Field}};
field_to_form({Field, Value}, LineNum) when is_atom(Field) ->
    FormattedValue = erl_parse:abstract(Value, [{line, LineNum}]),
    {record_field, LineNum, {atom, LineNum, Field}, FormattedValue};
field_to_form({Field, Value, TypeStr}, LineNum) ->
    %% Types not implemented yet
    Str = lists:flatten(io_lib:format("-record(a, {~p=~p :: ~s}).", [Field, Value, TypeStr])),
    {attribute, _, record, {_Name, [FieldDef]}} = merl:quote(LineNum, Str),
    FieldDef.

merge_fields(OrigFields, NewFields) ->
    lists:foldl(fun(NewField, AccFields) ->
        replace_or_append_field(NewField, AccFields)
    end, OrigFields, NewFields).

replace_or_append_field(NewField, []) ->
    [NewField];
replace_or_append_field(NewField, [CurField|Rest]) ->
    NewFieldName = extract_field_name(NewField),
    CurFieldName = extract_field_name(CurField),
    case NewFieldName == CurFieldName of
        true ->
            %io:fwrite("Replacing: ~n >>>> ~p~n with~n <<<< ~p~n",[CurField, NewField]),
            [NewField | Rest];
        false ->
            [CurField | replace_or_append_field(NewField, Rest)]
    end.

extract_field_name({typed_record_field, Rec, _}) ->
    extract_field_name(Rec);
extract_field_name({record_field, _, {atom, _, FieldName}}) ->
    FieldName;
extract_field_name({record_field, _, {atom, _, FieldName}, _Default}) ->
    FieldName.
    

    
