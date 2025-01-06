CREATE TABLE "dev"."t_bot_group" (
                                     "id" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
                                     "group_id" int8 NOT NULL,
                                     "bot_id" int8 NOT NULL,
                                     "create_time" timestamp(6) NOT NULL DEFAULT now(),
                                     "update_time" timestamp(6) NOT NULL DEFAULT now(),
                                     "create_by" int8 NOT NULL,
                                     "update_by" int8 NOT NULL,
                                     "del_flag" int2 NOT NULL,
                                     CONSTRAINT "bot_group_pkey" PRIMARY KEY ("id")
)
;

-- ALTER TABLE "dev"."t_bot_group"
--     OWNER TO "ggbond";

CREATE INDEX "t_bot_group_group_id_bot_id_idx" ON "dev"."t_bot_group" USING btree (
                                                                                   "group_id" "pg_catalog"."int8_ops" ASC NULLS LAST,
                                                                                   "bot_id" "pg_catalog"."int8_ops" ASC NULLS LAST
    );

COMMENT ON COLUMN "dev"."t_bot_group"."id" IS 'id';

COMMENT ON COLUMN "dev"."t_bot_group"."group_id" IS 'QQ群号';

COMMENT ON COLUMN "dev"."t_bot_group"."bot_id" IS '机器人QQ号';

COMMENT ON COLUMN "dev"."t_bot_group"."create_time" IS '创建时间';

COMMENT ON COLUMN "dev"."t_bot_group"."update_time" IS '更新时间';

COMMENT ON COLUMN "dev"."t_bot_group"."create_by" IS '创建者';

COMMENT ON COLUMN "dev"."t_bot_group"."update_by" IS '更新者';

COMMENT ON COLUMN "dev"."t_bot_group"."del_flag" IS '删除标识';

COMMENT ON TABLE "dev"."t_bot_group" IS '机器人群权限表';