-module(test).
-compile({parse_transform, rekt}).

-export([do/0]).

-record(testrec, {a=5, b = <<1,2>>, c="mystr", x={tuple}, y=[7,b]}).

-extend({testrec, testrec1, [
    {d, "what"},
    e,
    {f, 0.1},
    {h, undefined}
]}).

do() ->
    #testrec1{}.
