-module(test).
-compile({parse_transform, rekt}).

-export([do/0]).

-record(testrec, {z, zz :: any(), a=5 :: integer() | list(), b = <<1,2>>, c="mystr", x={tuple}, y=[7,b]}).

-extend({testrec, testrec1, [
    {d, "what"},
    e,
    {f, 0.1},
    {h, undefined},
    {zz, "some string", "string()"}
]}).

do() ->
    #testrec1{}.
