package com.ouhaochen.bot.xbot.core.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.core.context.PluginServiceContext;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupKeywordDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupKeywordEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class GroupManageService {

    private final BotGroupKeywordDao botGroupKeywordDao;

    public PluginServiceContext addGroupKeyword(Long selfId, Long groupId, String keyword) {
        PluginServiceContext context = new PluginServiceContext();
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
            context.setMsg(String.format("入群关键词【%s】已经添加过了", keyword));
        } else {
            botGroupKeywordDao.save(entity);
            context.setMsg(String.format("入群关键词【%s】添加成功", keyword));
        }
        return context;
    }

    public PluginServiceContext delGroupKeyword(long selfId, Long groupId, String keyword) {
        PluginServiceContext context = new PluginServiceContext();
        BotGroupKeywordEntity botGroupKeywordEntity = botGroupKeywordDao.lambdaQuery()
                .eq(BotGroupKeywordEntity::getBotId, selfId)
                .eq(BotGroupKeywordEntity::getGroupId, groupId)
                .eq(BotGroupKeywordEntity::getKeyword, keyword)
                .eq(BotGroupKeywordEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupKeywordEntity) {
            botGroupKeywordEntity.setDelFlag(DelFlagEnum.DELETED.getCode());
            botGroupKeywordDao.updateById(botGroupKeywordEntity);
            context.setMsg(String.format("入群关键词【%s】删除成功", keyword));
        } else {
            context.setMsg(String.format("入群关键词【%s】还未添加", keyword));
        }
        return context;
    }

    public PluginServiceContext handleAddGroup(Long selfId, Long groupId, String comment) {
        PluginServiceContext context = new PluginServiceContext();
        List<String> keywordList = botGroupKeywordDao.list(new LambdaQueryWrapper<BotGroupKeywordEntity>()
                        .select(BotGroupKeywordEntity::getKeyword)
                        .eq(BotGroupKeywordEntity::getBotId, selfId)
                        .eq(BotGroupKeywordEntity::getGroupId, groupId)
                        .eq(BotGroupKeywordEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()))
                .stream()
                .map(BotGroupKeywordEntity::getKeyword)
                .toList();
        if (keywordList.isEmpty()) {
            return context;
        }
        boolean isExist = keywordList.stream().anyMatch(CommonUtil.getAddGroupAnswer(comment)::contains);
        if (isExist) {
            context.setApprove(Boolean.TRUE);
        } else {
            context.setApproveReason("答案错误，拒绝入群");
            context.setApprove(Boolean.FALSE);
        }
        return context;
    }
}
