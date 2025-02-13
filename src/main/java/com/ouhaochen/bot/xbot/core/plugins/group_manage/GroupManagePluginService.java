package com.ouhaochen.bot.xbot.core.plugins.group_manage;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupKeywordDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupKeywordEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class GroupManagePluginService {

    private final BotGroupKeywordDao botGroupKeywordDao;

    public BotContext<Object> addGroupKeyword(Long selfId, Long groupId, String keyword) {
        BotGroupKeywordEntity entity = new BotGroupKeywordEntity();
        entity.setBotId(selfId);
        entity.setGroupId(groupId);
        entity.setKeyword(keyword);
        boolean isExist = botGroupKeywordDao.exists(new LambdaQueryWrapper<BotGroupKeywordEntity>()
                .eq(BotGroupKeywordEntity::getBotId, selfId)
                .eq(BotGroupKeywordEntity::getGroupId, groupId)
                .eq(BotGroupKeywordEntity::getKeyword, keyword)
                .eq(BotGroupKeywordEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()));
        if (isExist) {
            return BotContext.ofMsg(String.format("入群关键词【%s】已经添加过了", keyword));
        } else {
            botGroupKeywordDao.save(entity);
            return BotContext.ofMsg(String.format("入群关键词【%s】添加成功", keyword));
        }
    }

    public BotContext<Object> delGroupKeyword(long selfId, Long groupId, String keyword) {
        BotGroupKeywordEntity botGroupKeywordEntity = botGroupKeywordDao.lambdaQuery()
                .eq(BotGroupKeywordEntity::getBotId, selfId)
                .eq(BotGroupKeywordEntity::getGroupId, groupId)
                .eq(BotGroupKeywordEntity::getKeyword, keyword)
                .eq(BotGroupKeywordEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupKeywordEntity) {
            botGroupKeywordEntity.setDelFlag(DelFlagEnum.DELETED.getCode());
            botGroupKeywordDao.updateById(botGroupKeywordEntity);
            return BotContext.ofMsg(String.format("入群关键词【%s】删除成功", keyword));
        } else {
            return BotContext.ofMsg(String.format("入群关键词【%s】还未添加", keyword));
        }
    }

    public BotContext<Object> handleAddGroup(Long selfId, Long userId, Long groupId, String comment) {
        List<String> keywordList = botGroupKeywordDao.list(new LambdaQueryWrapper<BotGroupKeywordEntity>()
                        .select(BotGroupKeywordEntity::getKeyword)
                        .eq(BotGroupKeywordEntity::getBotId, selfId)
                        .eq(BotGroupKeywordEntity::getGroupId, groupId)
                        .eq(BotGroupKeywordEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()))
                .stream()
                .map(BotGroupKeywordEntity::getKeyword)
                .toList();
        if (keywordList.isEmpty()) {
            return new BotContext<>(null);
        }
        boolean isExist = keywordList.stream().anyMatch(CommonUtil.getAddGroupAnswer(comment)::contains);
        if (isExist) {
            String sendMsg = MsgUtils.builder()
                    .at(userId)
                    .text(" 你好，非常欢迎\uD83D\uDC4F你加入本群聊, 请阅读本群公告并遵守QQ群规范，请勿发送违规信息，如违反规定将被禁言甚至移除群聊。")
                    .face(118)
                    .face(118)
                    .face(118)
                    .build();
            return BotContext.ofApprove(sendMsg, Boolean.TRUE, "答案正确，允许入群");
        } else {
            return BotContext.ofApprove("", Boolean.FALSE, "答案错误，拒绝入群");
        }
    }
}
