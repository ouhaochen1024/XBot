package com.ouhaochen.bot.xbot.commons.utils;

import org.dromara.hutool.crypto.digest.DigestUtil;
import org.dromara.hutool.crypto.symmetric.SymmetricAlgorithm;
import org.dromara.hutool.crypto.symmetric.SymmetricCrypto;

import java.nio.charset.StandardCharsets;

/**
 * 加密算法工具类 AES DES MD5
 */
public final class CryptoUtil extends DigestUtil {
    /**
     * KEY
     */
    private static final byte[] key = "ouhaochen42454434".getBytes(StandardCharsets.UTF_8);
    /**
     * 初始化加密(AES加密方式)
     */
    private static final SymmetricCrypto aes = new SymmetricCrypto(SymmetricAlgorithm.AES, key);
    /**
     * 初始化加密(DES加密方式)
     */
    private static final SymmetricCrypto des = new SymmetricCrypto(SymmetricAlgorithm.DES, key);

    /**
     * AES加密
     *
     * @param str 加密之前的字符串
     */
    public static String encryptAESHex(String str) {
        try {
            return aes.encryptHex(str);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * AES解密
     *
     * @param str 加密后的字符串
     */
    public static String decryptAESStr(String str) {
		try {
			return aes.decryptStr(str);
		} catch (Exception e) {
			return null;
		}
    }

    /**
     * DES加密
     *
     * @param str 加密之前的字符串
     */
    public static String encryptDESHex(String str) {
		try {
			return des.encryptHex(str);
		} catch (Exception e) {
			return null;
		}
    }

    /**
     * DES解密
     *
     * @param str 加密后的字符串
     */
    public static String decryptDESStr(String str) {
		try {
			return des.decryptStr(str);
		} catch (Exception e) {
			return null;
		}
    }


}