-define(CHANNEL_TIME_OUT, 5000).
%% 数据变化排序集合
-define(ETS_DB_CACHE_CHANGE, '$db_cache_change').
%% 缓存从数据库初始化时一页读取最大数量
-define(CACHE_INIT_FROM_SQL_PAGE_NUM, 10000).
%% 系统错误缓存表
-define(DB_WRITE_ERROR_TAB_TAG, 'dbWriteErrorTab').
%% write sql 进程 state
-define(DB_WRITE_PROC_STATE_ETS, '$db_write_proc_state_ets').
%% 容错时间,单位毫秒
-define(LOCK_END_EXTRA_TIME, 500).

%% 渠道信息
-record(channel, {
    type = none :: atom(), %%渠道类型
    tab_keys = [] :: list(), %% 表键信
    from = none, %%最初来源
    s_time = 0 :: integer(), %%渠道开始时间
    e_time = 0 :: integer(), %%渠道结束时间
    time_out = ?CHANNEL_TIME_OUT :: integer(), %% 渠道失效
    timer_ref :: reference(), %%操作超时ref
    data = [] :: list(), %%数据
    insert_data = [] :: list(), %%插入数据
    info = none :: term() %%特殊数据
}).

%% 锁
-record(lock, {
    key = none :: tuple(), %%
    e_time = 0 :: integer(), %%解锁时间
    pid = none :: pid() %%执行渠道 pid
}).

%% 表中的key锁
-record(lock_run, {
    pid = none :: pid(), %%正在执行的进程
    eTime = 0 :: integer() %%结束时间用于清理
}).

%% 表中的key锁
-record(lock_queue, {
    uid = none :: {any(), integer(), pid()}, %%锁{key, endTime, pid}用于表清理
    eTime = 0 :: integer() %%结束时间用于清理
}).

%% channel写数据时的状态
-record(running, {
    pid = none :: pid(), %%执行channel进程
    data = none :: term() %%执前的数据
}).

%% 缓存数据
-record(cacheInfo, {
    key = none :: term(), %%
    running = none :: none|#running{}, %% 回滚数据
    mulCount = 0 :: integer(), %%复合数据条数
    active_time = 0 :: integer() %%活跃时间
}).

%% 以key为单位的数据变化依据
-record(changeInfo, {
    key = none :: any(),
    change_ref = none :: reference() %%最新变化的唯一标识
}).

%% 数据变化,每次变化生成排序列表
-record(cacheChange, {
    id = 0 :: integer(), %% id
    key = none :: term() %% changeInfo数据中的key
}).

%% 总数据变化
-record(cacheSupChange, {
    id = 0 :: integer(), %%id
    tag = none :: integer() %%cache tag
}).

%% 表管理数据
-record(cacheMgr, {
    tag = none :: tuple(), %%数据标记
    mod = none :: atom(), %%对映的mod
    pid = none :: pid(), %% 进程标记
    diskBool = false :: boolean(), %%是否持久化
    etsKey = none :: reference(), %%key表,disk表必然存存,内存表复合键存在此表
    etsIndex = [] :: [reference()], %%索引表
    etsLock = none :: reference(), %%锁表正在执行pid
    etsLockQueue = none :: reference(), %%锁表中排对表
    etsCache = none :: reference(), %%缓存数据
    etsCacheInfo = none :: reference(), %%缓存数据管理信息
    etsChange = none :: reference(), %%变化缓存信息
    etsChangeInfo = none :: reference() %%数据变化标识
}).

%%三目运算
-ifndef(BOOL_VALUE).
-define(BOOL_VALUE(Bool, Value1, Value2), case Bool of true -> Value1; false -> Value2 end).
-endif.

-define(DB_TIMER_TC_MSEC, 'db_timer_tc_msec').
-ifdef(DB_TIMER_TC_MSEC).
-define(DB_TIMER_TC_INIT(), db_channel:timerTcInit()).
-define(DB_TIMER_TC_CALC(V), db_channel:timerTcCalc(?MODULE, ?LINE, V)).

-else.
-define(DB_TIMER_TC_INIT(), ok).
-define(DB_TIMER_TC_CALC(V), ok).
-endif.

