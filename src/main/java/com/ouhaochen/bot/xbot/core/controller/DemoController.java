package com.ouhaochen.bot.xbot.core.controller;

import com.ouhaochen.bot.xbot.commons.response.Response;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/apis/demo")
@Tag(name = "demo控制器", description = "demo控制器1")
public class DemoController {

    @GetMapping("/getAllDemo")
    @Operation(summary = "获取所有Demo", description = "返回所有demo表列表")
    public Response<String> getAllDemo() {
        return Response.success();
    }


}