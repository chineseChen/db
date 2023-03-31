-- alter必须在行首否则匹配不上
-- 删除字段请谨慎
-- ALTER TABLE `test`
--     ADD COLUMN `test_k1` VARCHAR(110) DEFAULT '' COMMENT '登陆设备' AFTER `var5`;
ALTER TABLE `ckc1_game`.`test`
    ADD COLUMN `test_k1` VARCHAR(110) DEFAULT '' COMMENT '登陆设备' AFTER `var5`;
ALTER TABLE `test`
    ADD COLUMN `test_k2` VARCHAR(110) DEFAULT '' COMMENT '登陆设备' AFTER `test_k1`;