#!/bin/sh
##!/usr/bin/env bash

#===========================================================================================
# custom Configuration
#===========================================================================================
SERVER="xbot"


#===========================================================================================
# Constant Configuration
#===========================================================================================
JAR="${SERVER}.jar"
RUN_NAME="${SERVER}"
JAVA_HOME="${JAVA_HOME}"

DIR_BIN="${DIR_HOME}/bin"
DIR_TARGET="${DIR_HOME}/target"
DIR_CONF="${DIR_HOME}/conf"
DIR_LOGS="${DIR_HOME}/logs"
DIR_GC="${DIR_HOME}/gc"


#===========================================================================================
# /bin/bash Configuration
#===========================================================================================
BASH_OPTS=""


#===========================================================================================
# JVM Configuration
#===========================================================================================
JVM_OPTS=""
JVM_OPTS="${JVM_OPTS} -server -Xms512m -Xmx512m -Xmn24m -Xss256K -XX:MetaspaceSize=128m "
JVM_OPTS="${JVM_OPTS} -XX:SurvivorRatio=4 -XX:MaxTenuringThreshold=15 "
JVM_OPTS="${JVM_OPTS} -XX:+UseParNewGC -XX:+UseConcMarkSweepGC "
JVM_OPTS="${JVM_OPTS} -XX:CMSInitiatingOccupancyFraction=75 -XX:+UseCMSCompactAtFullCollection -XX:CMSFullGCsBeforeCompaction=0 "
JVM_OPTS="${JVM_OPTS} -XX:+DoEscapeAnalysis -XX:-UseLargePages "
JVM_OPTS="${JVM_OPTS} -XX:+UseFastAccessorMethods -XX:+AggressiveOpts "
JVM_OPTS="${JVM_OPTS} -XX:+DisableExplicitGC "
JVM_OPTS="${JVM_OPTS} -XX:HeapDumpPath=${DIR_GC}/dump.hprof -XX:-OmitStackTraceInFastThrow -XX:+HeapDumpOnOutOfMemoryError "
JVM_OPTS="${JVM_OPTS} -Xloggc:${DIR_GC}/gc.log -verbose:gc -XX:+PrintGCDetails -XX:+PrintGCDateStamps -XX:+PrintGCTimeStamps -XX:+UseGCLogFileRotation -XX:NumberOfGCLogFiles=100 -XX:GCLogFileSize=100M "


#===========================================================================================
# JAVA -D Configuration
#===========================================================================================
JAR_D_OPTS=""
JAR_D_OPTS="${JAR_D_OPTS} -Dfile.encoding=UTF-8 "
JAR_D_OPTS="${JAR_D_OPTS} -Dapp.dir.home=${DIR_HOME} "

#===========================================================================================
# JAVA -jar Configuration
#===========================================================================================
JAR_OPTS=""
JAR_OPTS="${JAR_OPTS} -jar ${DIR_TARGET}/${JAR} "


#===========================================================================================
# Spring Configuration
#===========================================================================================
SPRING_OPTS=""
SPRING_OPTS="${SPRING_OPTS} --spring.config.additional-location=${DIR_CONF}/ "
SPRING_OPTS="${SPRING_OPTS} --logging.config=${DIR_CONF}/log4j2-spring.xml "
