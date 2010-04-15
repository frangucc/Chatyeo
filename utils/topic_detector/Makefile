#Jay's original code
default:
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/*.java

# Ed's testing code (works)
test:
	#javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/*.java
	java -cp classes/:lib/OtpErlang.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar -Xms256m -Xmx1024m LoadData en_20090306/

# Jay's original code
load:
	java -cp classes/:lib/OtpErlang.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar Test

# Call using make wikiminer to compile this to classes
wikiminer:
	javac -d classes/ -cp lib/log4j-1.2.14.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/trove-2.0.4.jar:lib/weka.jar:lib/wikipediaminer.jar:lib/OtpErlang.jar src/org/wikipedia/miner/model/*.java
