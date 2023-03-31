-record(dbAlterTab, {
	tabName = <<>> :: binary(), %%表名
	time = 0 :: integer(), %%最近一次变化时间,单位s
	version = {0,0,0} :: tuple() %%版本号
}).

