-module(test).
-compile({parse_transform, rekt}).

-export([do/0]).

-record(testrec, {z, zz :: any(), a=5 :: integer() | list(), b = <<1,2>>, c="mystr", x={tuple}, y=[7,b]}).

-extend({testrec, testrec1, [
    {d, "what"},
    e,
    {f, 0.1},
    {h, undefined},
    {zz, "my dong", "string()"}
]}).

%-blah(dfg, dfgd, dg}.

-define(wf_extend(From, To, Module, Fields), -extend({From, To, [{module, Module, "atom()"} | Fields]})).

?wf_extend(testrec1, testrec2, mymod, [j]).

-extend({testrec2, testrec3, []}).

do() ->
    #testrec1{}.
