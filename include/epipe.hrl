-ifndef(EPIPE_HRL).
-define(EPIPE_HRL, true).

-define(PIPE_MAGIC, '$pipe_magic$').
-define(pipe(L), [?PIPE_MAGIC | L]).

-endif.




