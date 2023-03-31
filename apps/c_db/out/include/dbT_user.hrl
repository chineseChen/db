-record(user, {
	userUid = 0 :: integer(), %%角色uid
	sign = <<>> :: binary(), %%签名
	accountId = <<>> :: binary(), %%玩家帐号
	passWord = <<>> :: binary(), %%帐号密码
	pId = 0 :: integer(), %%平台
	channel = 0 :: integer(), %%渠道
	time = 0 :: integer(), %%最近一次登陆时间
	ip = <<>> :: binary(), %%最近一次登陆ip
	clientOs = none :: atom(), %%设备操作系统
	deviceId = <<>> :: binary() %%最近一次登陆设备id
}).

