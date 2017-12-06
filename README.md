rekt
=====

Erlang Record Extender Parse Transform

Build
-----

    $ rebar3 compile

Usage
-----

1) Add `rekt` to your rebar dependencies

2) Include in your modules(if using with Nitrogen 2.4+, it's included by default):

`-compile({parse_transform, rekt}).`

3) Extend records:

```erlang
-extend({original_record, new_record, [
	field1,   %% Just an atom for a field,
    {field2, 2},    %% A field with a default value of 2
    {field3, [a,b,c]},  %% a field with a default value of [a,b,c],
	{field4, "sometext", "string() | undefined"} %% A field with a default value of
                                                 %% "sometext", and supports types
                                                 %% string() and undefined. Note,
                                                 %% the typedef is the string
                                                 %% of the typedef
]}).
```

4) Enjoy being able to create new records from older records.

Notes
-----

* You can replace fields with new field definitions. The fields will be replaced
in-line.
* You can extend previously extended records.

Example
-------
```erlang
-module(rekt_sample).
-compile({parse_transform, rekt}).
-export([do/0]).

%% Create initial record
-record(rec_1, {a=1, b=2, c=3}).

%% Make new record called #rec_2{}, that is a copy of #rec_1{} with a new field called `y`
-extend({rec_1, rec_2, [
	{y, 1000}
]}).

%% Make a new record called #rec_3{} based on the previously created #rec_2{},
%% with the `a` field replaced with a new definition (having a default value of
%% "newthing")
-extend({rec_2, rec_3, [
	{a, "newthing"}
]}).

do() ->
	io:format("Rec_1: ~p~n",[#rec_1{}]),
	io:format("Rec_2: ~p~n",[#rec_2{}]),
	io:format("Rec_3: ~p~n",[#rec_3{}]).
```

Then run:

```erlang
> rekt_sample:do().
Rec_1: {rec_1, 1, 2, 3}
Rec_2: {rec_2, 1, 2, 3, 1000}
Rec_3: {rec_3, "newthing", 2, 3, 1000}
ok
```

License
-------
Copyright 2017 Jesse Gumm

MIT LICENSE
