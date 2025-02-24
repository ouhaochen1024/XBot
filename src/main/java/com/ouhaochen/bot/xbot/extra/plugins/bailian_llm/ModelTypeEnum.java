package com.ouhaochen.bot.xbot.extra.plugins.bailian_llm;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
@AllArgsConstructor
public enum ModelTypeEnum {

    QWEN_PLUS("qwen-plus", "通义千问-Max-Latest"),
    DEEPSEEK_R1("deepseek-r1", "DeepSeek-R1"),
    DEEPSEEK_V3("deepseek-v3", "DeepSeek-V3"),
    DEEPSEEK_R1_DISTILL_LLAMA_70B("deepseek-r1-distill-llama-70b", "DeepSeek-R1-Distill-Llama-70B"),
    QWEN_MAX("qwen-max", "通义千问-Max");

    private final String code;
    private final String name;

    public static String getCodeByName(String name) {
        for (ModelTypeEnum modelTypeEnum : ModelTypeEnum.values()) {
            if (modelTypeEnum.getName().equals(name)) {
                return modelTypeEnum.getCode();
            }
        }
        return null;
    }

    public static ModelTypeEnum getByName(String modelName) {
        for (ModelTypeEnum modelTypeEnum : ModelTypeEnum.values()) {
            if (modelTypeEnum.getName().equals(modelName)) {
                return modelTypeEnum;
            }
        }
        return null;
    }

    public static List<String> getAllNames() {
        List<String> names = new ArrayList<>();
        for (ModelTypeEnum modelTypeEnum : ModelTypeEnum.values()) {
            names.add(modelTypeEnum.getName());
        }
        return names;
    }
}
