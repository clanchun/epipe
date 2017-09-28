## Overview

Epipe provides the pipe operation for Erlang via the powerful parse transform, which is inspired by Elixir's pipe. For example this expression in Elixir:

``` Elixir
[1, 2, 3, 4] |> Enum.map(&(&1 + 1)) |> Enum.sum
```

can be simulated in Erlang as:

``` Erlang
?pipe([[1, 2, 3, 4],
       {lists:map(fun (X) -> X + 1 end), 2},
       lists:sum()
      ]).
```

Beyond Elixir's pipe, where the result of previous action is passed to next action as the first argument, there's an enhancement in Erlang's pipe, you can choose which position the result is passed to the next action,  the syntax is:

``` Erlang
?pipe([InitialValue,
       Action1,
       {Action2, Pos2},
       {Action3, Pos3},
       ...
      ]).
```

If no `PosN` provided, `1` is the default value.

## Usage

To use epipe in your application, you need to define it as a rebar dep or have some other way of including it in Erlang's path. You can then add the following attribute to the module you wish to compile:

``` Erlang
-compile([{parse_transform, epipe_transform}]).
```

Alternately, you can add the following option to `rebar.config`:

``` Erlang
{erl_opts, [{parse_transform, epipe_transform}]}.
```

For example:

``` Erlang
-module(pipe_demo).

-include_lib("epipe/include/epipe.hrl").

-compile([{parse_transform, epipe_transform}]).

-export([start/1]).

start(N) ->
    M = ?pipe([N,
               {lists:seq(1), 2},
               {lists:map(fun (X) -> X + 1 end), 2},
               lists:sum()
              ]),
    M.
```