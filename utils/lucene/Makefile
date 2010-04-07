default:
	javac -d classes/ -cp lib/lucene-analyzers-2.4-dev.jar:lib/lucene-core-2.4-dev.jar:lib/lucene-queries-2.4-dev.jar:lib/xercesImpl.jar:lib/lucene-core-2.4.0.jar:lib/lucene-highlighter-2.4-dev.jar:lib/lucene-snowball-2.4-dev.jar:lib/mysql-connector-java-5.1.7-bin.jar:lib/OtpErlang.jar:$(CLASSPATH) src/com/chatyeo/lucene/util/*.java 

run:
	java -cp ${CLASSPATH} classes/com/chatyeo/lucene/util/FlexHighlighter

clean:
	rm -fr classes/com
