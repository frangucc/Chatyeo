default:
	javac -d classes/ -cp .:lib/lucene-core-2.4.1_jay.jar:$(CLASSPATH) src/com/defyned/chatyeo/lucene/*.java
test:
	java -cp ./classes/:lib/lucene-core-2.4.1_jay.jar:  org.apache.lucene.index.SearcherManager
