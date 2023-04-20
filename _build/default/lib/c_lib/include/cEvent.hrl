-define(EVENT_FILE_MANAGE_ETS, '$event_file_manage_ets').
-define(EVENT_RUN_DATA_ETS, '$event_data_ets').

%% 事件信息
-record(event, {
    tag = none :: atom(), %%标记
    type = none :: timer|event|atom(), %%事件类型
    app = none :: atom(), %%生效app
    func = none :: function(), %%回调函数
    arg = [] :: any() %%初始参数
}).

%% 事件标记
-record(eventFile, {
    file = "" :: string(),
    modifyTime = 0 :: integer(), %%最近一次修改时间
    tag = [] :: list() %%所有
}).
