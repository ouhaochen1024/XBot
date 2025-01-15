CREATE TABLE "dev"."t_bot_group_keyword" (
                                             "id" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
                                             "group_id" int8 NOT NULL,
                                             "bot_id" int8 NOT NULL,
                                             "keyword" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
                                             "create_time" timestamp(6) NOT NULL DEFAULT now(),
                                             "update_time" timestamp(6) NOT NULL DEFAULT now(),
                                             "create_by" int8 NOT NULL,
                                             "update_by" int8 NOT NULL,
                                             "del_flag" int2 NOT NULL,
                                             CONSTRAINT "t_bot_group_keyword_pkey" PRIMARY KEY ("id")
)
;

ALTER TABLE "dev"."t_bot_group_keyword"
    OWNER TO "ggbond";

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."id" IS 'id';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."group_id" IS 'QQ群号';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."bot_id" IS '机器人QQ号';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."keyword" IS '加群通过的关键词';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."create_time" IS '创建时间';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."update_time" IS '更新时间';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."create_by" IS '创建者';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."update_by" IS '更新者';

COMMENT ON COLUMN "dev"."t_bot_group_keyword"."del_flag" IS '删除标识';