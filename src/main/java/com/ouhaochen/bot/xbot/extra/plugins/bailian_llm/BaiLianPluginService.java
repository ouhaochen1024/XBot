package com.ouhaochen.bot.xbot.extra.plugins.bailian_llm;

import com.alibaba.dashscope.aigc.generation.Generation;
import com.alibaba.dashscope.aigc.generation.GenerationParam;
import com.alibaba.dashscope.aigc.generation.GenerationResult;
import com.alibaba.dashscope.common.Message;
import com.alibaba.dashscope.common.Role;
import com.alibaba.dashscope.exception.ApiException;
import com.alibaba.dashscope.exception.InputRequiredException;
import com.alibaba.dashscope.exception.NoApiKeyException;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import io.reactivex.Flowable;
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

    public BotContext<Object> viewModels() {
        String mgs = MsgUtils.builder()
                .text("支持的模型：\n")
                .text(ModelTypeEnum.getAllNames().stream().map(item -> "【" + item + "】").collect(Collectors.joining("\n")))
                .build();
        return BotContext.ofMsg(mgs);
    }

    public BotContext<Object> setModel(Long botId, String modelName) {
        ModelTypeEnum modelType = ModelTypeEnum.getByName(modelName);
        if (modelType == null) {
            return BotContext.ofMsg("不支持的模型");
        }
        redisTemplateClient.set(BAILIAN_LLM_CURRENT_MODEL_KEY + botId, modelType.getName());
        return BotContext.ofMsg(String.format("操作成功，当前模型为【%s】", modelType.getName()));
    }

    public BotContext<Message> chat(Long botId, Long userId, String keyword) {
        String lock = redisTemplateClient.tryLock(BAILIAN_LLM_LOCK_KEY(botId, userId), 10, TimeUnit.MINUTES);
        if (lock == null) {
            return BotContext.ofMsg("你的问题，模型还在思考中，再等等吧~");
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
                messages = chatHistory.stream().map(item -> (Message) item).collect(Collectors.toList());
            }
            messages.add(Message.builder().role(Role.USER.getValue()).content(keyword).build());

            Message result = new Message();
            result.setRole(Role.ASSISTANT.getValue());
            streamCallWithMessage(baiLianApiKey, currentModelCode, messages,result);

            if (StrUtil.isBlank(result.getReasoningContent()) || StrUtil.isBlank(result.getContent())) {
                return BotContext.ofMsg("百炼大模型服务异常，请稍后重试");
            }

            redisTemplateClient.listPush(BAILIAN_CHAT_HISTORY_KEY(botId, userId), result);
            redisTemplateClient.expire(BAILIAN_CHAT_HISTORY_KEY(botId, userId), 1, TimeUnit.HOURS);
            BotContext<Message> context;
            if (StrUtil.isNotBlank(result.getReasoningContent())) {
                context = BotContext.ofData("=========思考过程=========\n" + result.getReasoningContent() + "\n=========完整回复=========\n" + result.getContent(), result);
            } else {
                context = BotContext.ofData(result.getContent(), result);
            }
            context.setAtFlag(Boolean.TRUE);
            context.setAtId(userId);
            return context;
        } catch (Exception e) {
            log.error("百炼大模型服务异常，请稍后重试", e);
            return BotContext.ofMsg("百炼大模型服务异常，请稍后重试");
        } finally {
            redisTemplateClient.releaseLock(BAILIAN_LLM_LOCK_KEY(botId, userId), lock);
        }
    }

    public BotContext<Object> clearChatHistory(Long botId, Long userId) {
        redisTemplateClient.delete(BAILIAN_CHAT_HISTORY_KEY(botId, userId));
        redisTemplateClient.delete(BAILIAN_LLM_LOCK_KEY(botId, userId));
        return BotContext.ofMsg("已开启新对话");
    }

    private static GenerationParam buildGenerationParam(String baiLianApiKey, String modelCode, List<Message> msg) {
        return GenerationParam.builder()
                .apiKey(baiLianApiKey)
                .model(modelCode)
                .messages(msg)
                .resultFormat(GenerationParam.ResultFormat.MESSAGE)
                .incrementalOutput(true)
                .build();
    }

    private static void handleGenerationResult(StringBuilder reasoningContent, StringBuilder finalContent, GenerationResult message) {
        String reasoning = message.getOutput().getChoices().get(0).getMessage().getReasoningContent();
        String content = message.getOutput().getChoices().get(0).getMessage().getContent();
        if (!reasoning.isEmpty()) {
            reasoningContent.append(reasoning);
        }
        if (!content.isEmpty()) {
            finalContent.append(content);
        }
    }

    public static void streamCallWithMessage(String baiLianApiKey, String modelCode, List<Message> msg, Message result)
            throws NoApiKeyException, ApiException, InputRequiredException {
        GenerationParam param = buildGenerationParam(baiLianApiKey, modelCode, msg);
        Generation gen = new Generation();
        Flowable<GenerationResult> flowable = gen.streamCall(param);
        StringBuilder reasoningContent = new StringBuilder();
        StringBuilder finalContent = new StringBuilder();
        flowable.blockingForEach(message -> handleGenerationResult(reasoningContent, finalContent, message));
        result.setReasoningContent(reasoningContent.toString());
        result.setContent(finalContent.toString());
    }

}
