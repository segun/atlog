%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Mar 2015 9:07 AM
%%%-------------------------------------------------------------------
-module(file_reader).
-author("aardvocate").

-behaviour(gen_server).

%% API
-export([start_link/0, check_for_new_file/0, parse/0, test/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("atlogheader.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
  process_flag(trap_exit, true),
  erlog:load_config_file("conf/erlog.conf"),
  register(atlog_fr, spawn(file_reader, parse, [])),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([]) ->
  {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast(check_for_new_file, State) ->
  check(),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Public API
%%%===================================================================
check_for_new_file() ->
  gen_server:cast(?SERVER, check_for_new_file).

test() ->
  receive
    stop ->
      erlog:info("Stopped Testing");
    {ClientID, ping} ->
      erlog:info("Received Ping from ~p", [ClientID]),
      ClientID ! {self(), pong},
      test()
  end.

parse() ->
  erlog:info("Waiting for message....."),
  receive
    stop ->
      erlog:info("File Reader Finished");
    {ClientID, parse, Type, File} ->
      erlog:info("Got Message ~p", [list_to_binary(File)]),
      JSON = get_file_as_json(Type, "", list_to_binary(File)),
      ClientID ! {self(), JSON},
      parse()
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check() ->
  %FilesLocation = "/Users/aardvocate/atlog",
  {ok, FilesLocation} = application:get_env(atlog, files_location),
  erlog:info("Files Location : ~p~n", [FilesLocation]),
  case filelib:is_dir(FilesLocation) of
    false ->
      file:make_dir(FilesLocation);
    true ->
      ok
  end,

  %%WINCOR
  %WincorDir = "/Users/aardvocate/atlog/wincor",
  {ok, WD} = application:get_env(atlog, wincor_dir),
  WincorDir = filename:join(WD, "files"),
  erlog:info("Wincor Directory: ~p~n", [WincorDir]),
  case filelib:is_dir(WincorDir) of
    false ->
      file:make_dir(WincorDir);
    true ->
      ok
  end,

  WincorFiles = case file:list_dir(WincorDir) of
                  {ok, WF} ->
                    WF;
                  _ ->
                    []
                end,

  Files = do_recursive(WincorDir, WincorFiles, []),
  erlog:info("Found ~p Wincor Files in ~p~n", [length(Files), WincorDir]),
  read(wincor, WD, Files),
  backup(wincor, WD, WincorFiles),

  %%HYOSUNG
  {ok, HD} = application:get_env(atlog, hyosung_dir),
  HyosungDir = filename:join(HD, "files"),
  erlog:info("Hyosung Dir: ~p", [HyosungDir]),
  case filelib:is_dir(HyosungDir) of
    0 ->
      file:make_dir(HyosungDir);
    _ ->
      ok
  end,
  HyosungFiles = case file:list_dir(HyosungDir) of
                   {ok, HF} ->
                     HF;
                   _ ->
                     []
                 end,
  HyoFiles = do_recursive(HyosungDir, HyosungFiles, []),
  erlog:info("Found ~p Files in ~p~n", [length(HyosungFiles), HyosungDir]),
  read(hyosung, HD, HyoFiles),
  backup(hyosung, HD, HyosungFiles),

  {ok, DD} = application:get_env(atlog, diebold_dir),
  DieboldDir = filename:join(DD, "files"),
  erlog:info("Diebold Dir: ~p~n", [DieboldDir]),
  case filelib:is_dir(DieboldDir) of
    0 ->
      file:make_dir(DieboldDir);
    _ ->
      ok
  end,
  DieboldFiles = case file:list_dir(DieboldDir) of
                   {ok, DF} ->
                     DF;
                   _ ->
                     []
                 end,
  DieFiles = do_recursive(DieboldDir, DieboldFiles, []),
  erlog:info("Found ~p Files in ~p~n", [length(DieFiles), DieboldDir]),
  read(diebold, DD, DieFiles),
  backup(diebold, DD, DieboldFiles),

  io:format("Done"),

  {ok, RunAfter} = application:get_env(atlog, run_after),

  receive
    after RunAfter * 60 * 1000 ->
      file_reader:check_for_new_file()
  end.

do_recursive(BaseDir, [F | FilesAndDirs], Acc) ->
  File = filename:join(BaseDir, F),
  case filelib:is_dir(File) of
    false ->
      do_recursive(BaseDir, FilesAndDirs, [list_to_binary(filename:join(BaseDir, F)) | Acc]);
    true ->
      {ok, NewFilesAndDirs} = file:list_dir(File),
      NewFiles = do_recursive(filename:join(BaseDir, File), NewFilesAndDirs, []),
      do_recursive(BaseDir, FilesAndDirs, [NewFiles | Acc])
  end;

do_recursive(_BaseDir, [], Acc) ->
  lists:reverse(Acc).

read(Type, Dir, [Files | All]) ->
  read_files(Type, Dir, Files),
  read(Type, Dir, All);

read(_Type, _Dir, []) ->
  ok.

read_files(_Type, _Dir, []) ->
  ok;

read_files(Type, Dir, F) ->
  {File, Files} = case is_binary(F) of
                    true ->
                      {binary_to_list(F), []};
                    false ->
                      {binary_to_list(hd(F)), tl(F)}
                  end,

  erlog:info("Reading File: ~p~n", [File]),
  case file:open(filename:join(Dir, File), [read]) of
    {ok, Fd} ->
      JSON = read_file(Type, device, Dir, File, Fd),
      {ok, JSONDir} = application:get_env(atlog, json_dir),
      file:make_dir(JSONDir),
      file:write_file(filename:join(JSONDir, filename:basename(File) ++ ".json"), list_to_binary(JSON));
    _ ->
      ok
  end,
  read_files(Type, Dir, Files).

read_file(Type, device, _Dir, File, Device) ->
  Lines = read_lines(Device, []),
  case Lines of
    [] ->
      "{}";
    _ ->
      parse(Type, File, Lines, db)
  end.

read_file_as_json(Type, device, _Dir, File, Device) ->
  Lines = read_lines(Device, []),
  erlog:info(Lines),
  case Lines of
    [] ->
      "{}";
    _ ->
      parse(Type, File, Lines, json)
  end.

parse(wincor, File, Lines, RetType) ->
  erlog:info("Starting Parse Of ~p File ~p With Lines ~p~n", [wincor, File, length(Lines)]),
  case RetType of
    json ->
      wincor:parse_json(File, Lines);
    _ ->
      wincor:parse(File, Lines)
  end;

parse(hyosung, File, Lines, RetType) ->
  erlog:info("Starting Parse Of ~p File ~p With Lines ~p~n", [hyosung, File, length(Lines)]),
  case RetType of
    json ->
      hyosung:parse_json(File, Lines);
    _ ->
      hyosung:parse(File, Lines)
  end;

parse(diebold, File, Lines, RetType) ->
  erlog:info("Starting Parse Of ~p File ~p With Lines ~p~n", [diebold, File, length(Lines)]),
  case RetType of
    json ->
      diebold:parse_json(File, Lines);
    _ ->
      diebold:parse(File, Lines)
  end;

parse(_Type, _File, _Lines, _RetType) ->
  error("Unknown Type Supplied").

backup(wincor, WincorDir, Files) ->
  erlog:info("Starting Backup of ~p files", [wincor]),
  {ok, BackupDir} = application:get_env(atlog, backup_dir),
  file:make_dir(BackupDir),
  lists:foreach(fun(X) ->
    Src = filename:join(WincorDir, X),
    Dest = filename:join(BackupDir, X ++ ".back"),
    file:copy(Src, Dest),
    file:delete(Src)
  end, Files);

backup(diebold, DieboldDir, Files) ->
  erlog:info("Starting Backup of ~p files", [diebold]),
  {ok, BackupDir} = application:get_env(atlog, backup_dir),
  file:make_dir(BackupDir),
  lists:foreach(fun(X) ->
    Src = filename:join(DieboldDir, X),
    erlog:info("Backup Src == ~p~n", [Src]),
    Dest = filename:join(BackupDir, X ++ ".back"),
    erlog:info("Backup Dest == ~p~n", [Dest]),
    file:copy(Src, Dest),
    file:delete(Src)
  end, Files);

backup(hyosung, HyosungDir, Files) ->
  erlog:info("Starting Backup of ~p files", [hyosung]),
  {ok, BackupDir} = application:get_env(atlog, backup_dir),
  file:make_dir(BackupDir),
  lists:foreach(fun(X) ->
    Src = filename:join(HyosungDir, X),
    Dest = filename:join(BackupDir, X ++ ".back"),
    file:copy(Src, Dest),
    file:delete(Src)
  end, Files).


get_file_as_json(Type, Dir, F) ->
  File = binary_to_list(F),
  erlog:info("Reading File: ~p~n", [F]),
  case file:open(File, [read]) of
    {ok, Fd} ->
      read_file_as_json(Type, device, Dir, File, Fd);
    _ ->
      "{file: not_a_fd}"
  end.