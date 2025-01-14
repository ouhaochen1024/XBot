package com.ouhaochen.bot.xbot.db.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.ouhaochen.bot.xbot.db.entity.BaseEntity;

import java.io.Serial;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 
 * </p>
 *
 * @author ouhaochen
 * @since 2025-01-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_bot_group_keyword")
public class BotGroupKeywordEntity extends BaseEntity<BotGroupKeywordEntity> {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId("id")
    private String id;

    /**
     * QQ群号
     */
    @TableField("group_id")
    private Long groupId;

    /**
     * 机器人QQ号
     */
    @TableField("bot_id")
    private Long botId;

    /**
     * 加群通过的关键词
     */
    @TableField("keyword")
    private String keyword;

    @Override
    public Serializable pkVal() {
        return this.id;
    }
}
