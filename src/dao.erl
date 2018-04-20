%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jul 2014 7:24 PM
%%%-------------------------------------------------------------------
-module(dao).
-author("aardvocate").

-behaviour(gen_server).

%% API
-export([start_link/1, insert/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-compile(export_all).

-include("atlogheader.hrl").

start_link(PoolSize) ->
  process_flag(trap_exit, true),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [PoolSize], []).

init([_PoolSize]) ->
  Config = file:consult("./conf/db.conf"),
  erlog:load_config_file("./conf/erlog.conf"),
  erlog:info("~p~n", [Config]),
  application:start(odbc),
  {ok, [{odbc, [{uid, UID}, {pwd, Password}, {database, Database}, {server, Server}]}]} = Config,
  ConnString = Database ++ ";" ++ Server ++ ";" ++ UID ++ ";" ++ Password ++ ";Driver={SQL Server}",
  erlog:info("Connection String: ~p~n", [ConnString]),
  {ok, Ref} = odbc:connect(ConnString, []),
  erlog:info("Initialized  ~p Successfully", [?MODULE]),
  {ok, Ref}.

get_insert_id(Ref) ->
  QueryList = "SELECT @@IDENTITY AS LastID",
  erlog:info("~p~n", [list_to_binary(QueryList)]),
  {selected,["LastID"],[{InsertId}]} = odbc:sql_query(Ref, QueryList),
  erlog:info("Last ID: ~p", [InsertId]),
  InsertId.

handle_call({insert, session, FieldsList, ValuesList}, _From, Ref) ->
  QL = "insert into sessions (" ++ FieldsList ++ ") values (" ++ ValuesList ++ ")",
  QueryList = re:replace(QL, "\"", "", [global, {return, list}]),
  erlog:info("~p~n", [list_to_binary(QueryList)]),
  odbc:sql_query(Ref, QueryList),
  InsertId = get_insert_id(Ref),
  erlog:info("Insert Session ID: ~p", [InsertId]),
  {reply, {ok, InsertId}, Ref};

handle_call({update, file, FieldsValuesList, Where}, _From, Ref) ->
  QL = "update files set " ++ FieldsValuesList ++ " where " ++ Where,
  QueryList = re:replace(QL, "\"", "", [global, {return, list}]),
  erlog:info("~p~n", [list_to_binary(QueryList)]),
  odbc:sql_query(Ref, QueryList),
  odbc:commit(Ref, commit),
  {reply, ok, Ref};

handle_call({insert, file, FieldsList, ValuesList}, _From, Ref) ->
  QL = "insert into files (" ++ FieldsList ++ ") values (" ++ ValuesList ++ ")",
  QueryList = re:replace(QL, "\"", "", [global, {return, list}]),
  erlog:info("~p~n", [list_to_binary(QueryList)]),
  odbc:sql_query(Ref, QueryList),
  InsertId = get_insert_id(Ref),
  erlog:info("Insert File ID: ~p", [InsertId]),
  {reply, {ok, InsertId}, Ref};

handle_call({insert, transaction, FieldsList, ValuesList}, _From, Ref) ->
  QL = "insert into transactions (" ++ FieldsList ++ ") values (" ++ ValuesList ++ ")",
  QueryList = re:replace(QL, "\"", "", [global, {return, list}]),
  erlog:info("~p~n", [list_to_binary(QueryList)]),
  odbc:sql_query(Ref, QueryList),
  InsertId = get_insert_id(Ref),
  erlog:info("Insert Transaction ID: ~p", [InsertId]),
  {reply, {ok, InsertId}, Ref};

handle_call(_Request, _From, Ref) ->
  {reply, ok, Ref}.

handle_cast(_Request, Ref) ->
  {noreply, Ref}.

handle_info(_Info, Ref) ->
  {noreply, Ref}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, Ref, _Extra) ->
  {ok, Ref}.


%%%===================================================================
%%% Public API
%%%===================================================================

update(Type, FieldsValuesList, Where) ->
  gen_server:call(?SERVER, {update, Type, FieldsValuesList, Where}, infinity).

insert(Type, FieldsList, ValuesList) ->
  gen_server:call(?SERVER, {insert, Type, FieldsList, ValuesList}, infinity).


%%%===================================================================
%%% Private API
%%%===================================================================
