-define(CONST_PAGS_LIST, ["http://bbs.dlut.edu.cn/nforum/board/DUT", 
                          "http://bbs.dlut.edu.cn/nforum/board/Focus", 
                          "http://bbs.dlut.edu.cn/nforum/board/Career",
                          "http://bbs.dlut.edu.cn/nforum/board/Secondhand"]).

-record(message, {
                  title = 0,
                  url = 0,
                  time = 0,
                  time_str = "00:00:00",
                  author = "",
                  reply_time = 0,
                  reply_time_str = "00:00:00",
                  reply_author = "",
                  reply_count = 0
                  }).
                  
-include("msg_pb.hrl").
