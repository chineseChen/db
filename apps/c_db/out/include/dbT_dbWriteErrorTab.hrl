-record(dbWriteErrorTab, {
	id = <<>> :: binary(), %%表名
	time = 0 :: integer(), %%最近一次变化时间,单位s
	error = <<>> :: binary(), %%错误原因
	value = <<>> :: binary() %%内容
}).

