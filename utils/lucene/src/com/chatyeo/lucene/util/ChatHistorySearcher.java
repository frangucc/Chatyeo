package com.chatyeo.lucene.util;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.StopAnalyzer;
import org.apache.lucene.analysis.snowball.SnowballAnalyzer;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.Directory;
import org.apache.lucene.search.IndexSearcher;
import java.io.File;
import java.io.StringReader;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.Hits;
import org.apache.lucene.search.highlight.Highlighter;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.search.highlight.QueryScorer;
import java.io.*;

//Retrieves query results from Lucene
class ChatHistorySearcher
{
  public static String USAGE = "com.chatyeo.lucene.util.ChatHistorySearcher index_dir query";

  public static void main(String[] args) throws Exception
    {

      if(args.length != 2)
      {
        System.out.println(USAGE);
        System.exit(1);
      }

      String q = args[1];

      Directory ramDir = FSDirectory.getDirectory(new File(args[0]));

      String [] noStops = {""};
      Analyzer analyzer = new SnowballAnalyzer("English", noStops);

      IndexSearcher searcher = new IndexSearcher(ramDir);

      QueryParser parser = new QueryParser("message", analyzer);
      Query query = parser.parse(q);

      query = query.rewrite(searcher.getIndexReader()); //required to expand search terms
      Hits hits = searcher.search(query);

      System.out.println("Got " + hits.length() + " hits");

      Highlighter highlighter = new Highlighter(new QueryScorer(query));

      StringBuffer results = new StringBuffer();

      results.append("<results>" + "\n");
      //for (int i = 0; i < hits.length(); i++)
      for (int i = 0; ((i < 200) & (i < hits.length())); i++)
      {

        results.append("<doc>"+"\n");

        String id = hits.doc(i).get("id");
        String username = hits.doc(i).get("username");
        String room = hits.doc(i).get("room");
        String time = hits.doc(i).get("time");
        String message = hits.doc(i).get("message");

        results.append("<id>" + id + "</id>"+"\n");
        results.append("<username>" + username + "</username>"+"\n");
        results.append("<room>" + room + "</room>"+"\n");
        results.append("<time>" + time + "</time>"+"\n");
        results.append("<message>" + message + "</message>"+"\n");

        results.append("</doc>"+"\n");
      }

      results.append("</results>"+"\n");
      System.out.println(results.toString());

    }

}
