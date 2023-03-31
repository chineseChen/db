%%%-------------------------------------------------------------------
%% @doc c_db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(c_db_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},
    ChildSpecs = [
        #{id => 'db_sql_sup',
            start => {'db_sql_sup', start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => ['db_sql_sup']
        },
        #{id => 'writeSqlMgr',
            start => {'writeSqlMgr', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['writeSqlMgr']
        },
        #{id => 'db_sql_mgr',
            start => {'db_sql_mgr', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['db_sql_mgr']
        },
        #{id => 'db_cache_mgr',
            start => {'db_cache_mgr', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['db_cache_mgr']
        },
        #{id => 'db_cache_sup',
            start => {'db_cache_sup', start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => ['db_cache_sup']
        },
        #{id => 'db_channel_sup',
            start => {'db_channel_sup', start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => ['db_channel_sup']
        },
        #{id => 'db_channel_mgr',
            start => {'db_channel_mgr', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['db_channel_mgr']
        },
        #{id => 'dbLockQueueProc',
            start => {'dbLockQueueProc', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['dbLockQueueProc']
        },
        #{id => 'db_lock_proc',
            start => {'db_lock_proc', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['db_lock_proc']
        },
        #{id => 'db_handle_proc',
            start => {'db_handle_proc', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['db_handle_proc']
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
