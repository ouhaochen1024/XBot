package com.ouhaochen.bot.xbot.extra.plugins.bailian_llm;

import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class BaiLianPluginService {

    private static final String BAILIAN_LLM_CURRENT_MODEL_KEY = "bailian_llm_current_model:";
    private final RedisTemplateClient redisTemplateClient;

    public BotContext<Object> currentModel(Long botId) {
        String currentModel = (String) redisTemplateClient.get(BAILIAN_LLM_CURRENT_MODEL_KEY + botId);
        if (currentModel == null) {
            currentModel = ModelTypeEnum.DEEPSEEK_R1.getName();
            redisTemplateClient.set(BAILIAN_LLM_CURRENT_MODEL_KEY + botId, currentModel);
        }
        return BotContext.ofMsg(String.format("当前模型为【%s】", currentModel));
    }

    public BotContext<Object> setModel(Long botId, String modelName) {
        ModelTypeEnum modelType = ModelTypeEnum.getByName(modelName);
        if (modelType == null) {
            return BotContext.ofMsg("不支持的模型");
        }
        redisTemplateClient.set(BAILIAN_LLM_CURRENT_MODEL_KEY + botId, modelType.getName());
        return BotContext.ofMsg(String.format("设置模型为【%s】", modelType.getName()));
    }
}
