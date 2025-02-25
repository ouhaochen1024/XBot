package com.ouhaochen.bot.xbot.extra.plugins.bailian_llm;

import com.alibaba.dashscope.aigc.generation.Generation;
import com.alibaba.dashscope.aigc.generation.GenerationParam;
import com.alibaba.dashscope.aigc.generation.GenerationResult;
import com.alibaba.dashscope.common.Message;
import com.alibaba.dashscope.common.Role;
import com.alibaba.fastjson2.JSON;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.collection.CollUtil;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class BaiLianPluginService {

    @Value("${third-party-api.aliyun.bailian.api-key}")
    private String baiLianApiKey;
    private static final String BAILIAN_LLM_LOCK_KEY = "bailian_llm_lock:";
    private static final String BAILIAN_CHAT_HISTORY_KEY = "bailian_chat_history:";
    private static final String BAILIAN_LLM_CURRENT_MODEL_KEY = "bailian_llm_current_model:";

    private static String BAILIAN_LLM_LOCK_KEY(Long botId, Long userId) {
        return BAILIAN_LLM_LOCK_KEY + botId + ":" + userId;
    }

    private static String BAILIAN_CHAT_HISTORY_KEY(Long botId, Long userId) {
        return BAILIAN_CHAT_HISTORY_KEY + botId + ":" + userId;
    }

    private final RedisTemplateClient redisTemplateClient;

    public BotContext<Object> currentModel(Long botId) {
        String currentModel = (String) redisTemplateClient.get(BAILIAN_LLM_CURRENT_MODEL_KEY + botId);
        if (currentModel == null) {
            currentModel = ModelTypeEnum.DEEPSEEK_R1.getName();
            redisTemplateClient.set(BAILIAN_LLM_CURRENT_MODEL_KEY + botId, currentModel);
        }
        return BotContext.ofData(String.format("当前模型为【%s】", currentModel), currentModel);
    }

    public BotContext<Object> setModel(Long botId, String modelName) {
        ModelTypeEnum modelType = ModelTypeEnum.getByName(modelName);
        if (modelType == null) {
            return BotContext.ofMsg("不支持的模型");
        }
        redisTemplateClient.set(BAILIAN_LLM_CURRENT_MODEL_KEY + botId, modelType.getName());
        return BotContext.ofMsg(String.format("设置模型为【%s】", modelType.getName()));
    }

    public BotContext<Message> chat(Long botId, Long userId, String keyword) {
        String lock = redisTemplateClient.tryLock(BAILIAN_LLM_LOCK_KEY(botId, userId), 5, TimeUnit.MINUTES);
        if (lock == null) {
            return BotContext.ofMsg("你的问题，模型还在思考中，请等待一会~");
        }
        String currentModel = (String) currentModel(botId).getData();
        String currentModelCode = ModelTypeEnum.getCodeByName(currentModel);
        if (currentModelCode == null) {
            return BotContext.ofMsg("不支持的模型");
        }
        try {
            List<Object> chatHistory = redisTemplateClient.getList(BAILIAN_CHAT_HISTORY_KEY(botId, userId));
            List<Message> messages = new ArrayList<>();
            if (CollUtil.isNotEmpty(chatHistory)) {
                messages = chatHistory.stream().map(item -> JSON.parseObject(item.toString(), Message.class)).collect(Collectors.toList());
            }
            messages.add(Message.builder().role(Role.USER.getValue()).content(keyword).build());
            Generation gen = new Generation();
            GenerationParam param = GenerationParam.builder()
                    .apiKey(baiLianApiKey)
                    .model(currentModelCode)
                    .messages(messages)
                    .resultFormat(GenerationParam.ResultFormat.MESSAGE)
                    .build();
            GenerationResult result = gen.call(param);
            if (result.getOutput() == null || CollUtil.isEmpty(result.getOutput().getChoices())) {
                return BotContext.ofMsg("百炼大模型服务异常，请稍后重试");
            }
            Message message = result.getOutput().getChoices().get(0).getMessage();
            redisTemplateClient.listPush(BAILIAN_CHAT_HISTORY_KEY(botId, userId), JSON.toJSONString(message));
            BotContext<Message> context;
            if (StrUtil.isNotBlank(message.getReasoningContent())) {
                context = BotContext.ofData("思考：\n" + message.getReasoningContent() + "\n总结：\n" + message.getContent(), message);
                context.setAtFlag(Boolean.TRUE);
                context.setAtId(userId);
                return context;
            } else {
                context = BotContext.ofData(message.getContent(), message);
                context.setAtFlag(Boolean.TRUE);
                context.setAtId(userId);
                return context;
            }
        } catch (Exception e) {
            log.error("百炼大模型服务异常，请稍后重试", e);
            return BotContext.ofMsg("百炼大模型服务异常，请稍后重试");
        } finally {
            redisTemplateClient.releaseLock(BAILIAN_LLM_LOCK_KEY(botId, userId), lock);
        }
    }

    public BotContext<Object> clearChatHistory(Long botId, Long userId) {
        redisTemplateClient.delete(BAILIAN_CHAT_HISTORY_KEY(botId, userId));
        return BotContext.ofMsg("已开启新对话");
    }
}
