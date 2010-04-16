#compiles the source into classes.
default:
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/*.java
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/org/wikipedia/miner/model/*.java
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/org/wikipedia/miner/annotation/*.java
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/org/wikipedia/miner/util/*.java
# Ed's testing code (works)
install:
	java -cp classes/:lib/OtpErlang.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar -Xmx1500m LoadData en_20090306/

# made for testing purposes.
test:
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/ChatyeoTopic.java
