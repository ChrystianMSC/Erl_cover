-module(contador).

-compile(export_all).

cria_contador()->
    Counter = counters:new(1, [atomics]),
    persistent_term:put(1, Counter),
    counters:add(persistent_term:get(1),1,1),
    counters:get(persistent_term:get(1),1).


[{call,1,{remote,1,{atom,1,counters},{atom,1,add}},[{call,1,{remote,1,{atom,1,persistent_term},{atom,1,get}},[{integer,1,1}]},{integer,1,1},{integer,1,1}]}]