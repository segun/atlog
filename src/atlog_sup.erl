
-module(atlog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 1000,
    Type = worker,

    %DAOChildDef = {"DAO", {dao, start_link, [2]}, Restart, Shutdown, Type, [dao]},
    FileReaderChildDef = {"file_reader", {file_reader, start_link, []}, Restart, Shutdown, Type, [file_reader]},
    {ok, {SupFlags, [FileReaderChildDef]}}.
    %{ok, {SupFlags, [FileReaderChildDef, DAOChildDef]}}.


