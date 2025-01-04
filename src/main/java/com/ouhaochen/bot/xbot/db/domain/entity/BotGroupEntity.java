package com.ouhaochen.bot.xbot.db.domain.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serial;
import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author ouhaochen
 * @since 2025-01-04
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_bot_group")
public class BotGroupEntity extends BaseEntity<BotGroupEntity> {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.ASSIGN_ID)
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

    @Override
    public Serializable pkVal() {
        return this.id;
    }
}
