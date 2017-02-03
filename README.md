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


License
-------

Copyright 2017 Jesse Gumm
MIT LICENSE
