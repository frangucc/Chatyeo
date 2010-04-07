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
class RSSSearcher
{
  public static String USAGE = "com.chatyeo.lucene.util.Indexer dir_to_index/ index_dir";

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

      QueryParser parser = new QueryParser("text", analyzer);
      Query query = parser.parse(q);

      query = query.rewrite(searcher.getIndexReader()); //required to expand search terms
      Hits hits = searcher.search(query);

      Highlighter highlighter = new Highlighter(new QueryScorer(query));

      StringBuffer results = new StringBuffer();

      results.append("<results>" + "\n");
      //for (int i = 0; i < hits.length(); i++)
      for (int i = 0; ((i < 200) & (i < hits.length())); i++)
      {

        results.append("<doc>"+"\n");

        String title = hits.doc(i).get("title");
        String author = hits.doc(i).get("author");
        String link = hits.doc(i).get("link");
        String text = hits.doc(i).get("text");
        String score = hits.doc(i).get("score");

        results.append("<score>" + score + "</score>"+"\n");
        results.append("<title>" + title + "</title>"+"\n");
        results.append("<author>" + author + "</author>"+"\n");
        results.append("<link>" + link + "</link>"+"\n");
        results.append("<text>" + text + "</text>"+"\n");

        results.append("</doc>"+"\n");
      }

      results.append("</results>"+"\n");
      System.out.println(results.toString());

    }

}
