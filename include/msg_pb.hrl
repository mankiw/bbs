-ifndef(MSG_PB_H).
-define(MSG_PB_H, true).
-record(msg, {
    title = erlang:error({required, title}),
    reply_time = erlang:error({required, reply_time}),
    url = erlang:error({required, url}),
    reply_cout = erlang:error({required, reply_cout}),
    autor = erlang:error({required, autor}),
    reply_autor = erlang:error({required, reply_autor})
}).
-endif.

