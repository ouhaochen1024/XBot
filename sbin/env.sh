#!/bin/bash
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

DIR_BIN="${DIR_HOME}/sbin"
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
JVM_OPTS="${JVM_OPTS} -server -Xms4g -Xmx4g"
JVM_OPTS="${JVM_OPTS} -XX:MetaspaceSize=256m -XX:MaxMetaspaceSize=512m"
JVM_OPTS="${JVM_OPTS} -XX:+UseG1GC -XX:MaxGCPauseMillis=200"
JVM_OPTS="${JVM_OPTS} -XX:ParallelGCThreads=4 -XX:ConcGCThreads=2"
JVM_OPTS="${JVM_OPTS} -XX:+UseCompressedOops -XX:+DisableExplicitGC"
JVM_OPTS="${JVM_OPTS} -XX:HeapDumpPath=${DIR_GC}/dump.hprof -XX:+HeapDumpOnOutOfMemoryError"
JVM_OPTS="${JVM_OPTS} -Xlog:gc*,gc+heap=info,gc+age=trace:file=${DIR_GC}/gc.log:time,uptime,level,tags:filecount=100,filesize=100M"


#===========================================================================================
# JAVA -D Configuration
#===========================================================================================
JAR_D_OPTS=""
JAR_D_OPTS="${JAR_D_OPTS} -Dfile.encoding=UTF-8"
JAR_D_OPTS="${JAR_D_OPTS} -Dapp.dir.home=${DIR_HOME}"

#===========================================================================================
# JAVA -jar Configuration
#===========================================================================================
JAR_OPTS=""
JAR_OPTS="${JAR_OPTS} -jar ${DIR_TARGET}/${JAR}"


#===========================================================================================
# Spring Configuration
#===========================================================================================
SPRING_OPTS=""
SPRING_OPTS="${SPRING_OPTS} --spring.config.additional-location=${DIR_CONF}/"
SPRING_OPTS="${SPRING_OPTS} --logging.config=${DIR_CONF}/log4j2-spring.xml"
