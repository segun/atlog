%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Mar 2015 8:59 AM
%%%-------------------------------------------------------------------
-module(atlog).
-author("aardvocate").

%% API
-export([start/0]).


start() ->
  inets:start(),
  crypto:start(),
  erlog:start(),
  application:start(atlog),
  Config = file:consult("./conf/db.conf"),
  erlog:load_config_file("./conf/erlog.conf"),
  erlog:info("~p~n", [Config]),
  file_reader:check_for_new_file(),
  ok.