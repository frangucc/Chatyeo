default:
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/*.java
test:
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/*.java
	java -cp classes/:lib/OtpErlang.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar LoadData
	##java -cp classes/:lib/OtpErlang.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar LoadData 127.0.0.1:3306 wikipedia root temp4now chats/chat1
load:
	java -cp classes/:lib/OtpErlang.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar LoadData
