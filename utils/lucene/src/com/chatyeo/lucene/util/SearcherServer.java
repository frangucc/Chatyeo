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

import com.ericsson.otp.erlang.*;

import com.chatyeo.lucene.util.ChatHistoryIndexer;
import com.chatyeo.lucene.util.RSSIndexer;


public class SearcherServer
{
  public static String USAGE = "com.chatyeo.lucene.util.SearcherServer";

  private IndexSearcher rss_searcher = null;
  private QueryParser rss_parser = null;
  private IndexSearcher ch_searcher = null;
  private QueryParser ch_parser = null;

  private void OpenChatHistoryIndex(String indexDir) throws Exception
    {
      Directory ramDir = FSDirectory.getDirectory(new File(indexDir));

      ch_searcher = new IndexSearcher(ramDir);

      String [] noStops = {""};
      Analyzer analyzer = new SnowballAnalyzer("English", noStops);

      ch_parser = new QueryParser("message", analyzer);
    }
  private OtpErlangTuple SearchChatHistory(String q) throws Exception
    {
      Query query = ch_parser.parse(q);

      query = query.rewrite(ch_searcher.getIndexReader()); //required to expand search terms
      Hits hits = ch_searcher.search(query);

      System.out.println("Got " + hits.length() + " hits");

      Highlighter highlighter = new Highlighter(new QueryScorer(query));

      OtpErlangObject[] doc_list = new OtpErlangObject[hits.length()];

      for (int i = 0; ((i < 200) & (i < hits.length())); i++)
      {
        // TODO ADD SCORE
        //OtpErlangObject[] score = {new OtpErlangAtom("score"),new OtpErlangLong(Long.parseLong(hits.doc(i).get("score")))};
        OtpErlangObject[] id = {new OtpErlangAtom("id"),new OtpErlangLong(Long.parseLong(hits.doc(i).get("id")))};
        OtpErlangObject[] username = {new OtpErlangAtom("username"),new OtpErlangString(hits.doc(i).get("username"))};
        OtpErlangObject[] room = {new OtpErlangAtom("room"),new OtpErlangString(hits.doc(i).get("room"))};
        OtpErlangObject[] time = {new OtpErlangAtom("time"),new OtpErlangLong(Long.parseLong(hits.doc(i).get("time")))};
        OtpErlangObject[] message = {new OtpErlangAtom("message"),new OtpErlangString(hits.doc(i).get("message"))};
        OtpErlangObject[] doc = {/*new OtpErlangTuple(score), */new OtpErlangTuple(id), new OtpErlangTuple(username), new OtpErlangTuple(room), new OtpErlangTuple(time), new OtpErlangTuple(message)};
        OtpErlangObject[] doc_tuple = {new OtpErlangAtom("doc"), new OtpErlangList(doc)};

        doc_list[i] = new OtpErlangTuple(doc_tuple);
      }

      OtpErlangObject[] results = {new OtpErlangAtom("results"),new OtpErlangList(doc_list)};

      OtpErlangTuple result_tuple = new OtpErlangTuple(results);

      return result_tuple;

    }

  private void OpenRSSIndex(String indexDir) throws Exception
    {
      Directory ramDir = FSDirectory.getDirectory(new File(indexDir));

      rss_searcher = new IndexSearcher(ramDir);

      String [] noStops = {""};
      Analyzer analyzer = new SnowballAnalyzer("English", noStops);

      rss_parser = new QueryParser("text", analyzer);
    }
  private OtpErlangTuple SearchRSS(String q) throws Exception
    {
      Query query = rss_parser.parse(q);

      query = query.rewrite(rss_searcher.getIndexReader()); //required to expand search terms
      Hits hits = rss_searcher.search(query);

      System.out.println("Got " + hits.length() + " hits");

      Highlighter highlighter = new Highlighter(new QueryScorer(query));

      OtpErlangObject[] doc_list = new OtpErlangObject[hits.length()];

      for (int i = 0; ((i < 200) & (i < hits.length())); i++)
      {
        // TODO ADD SCORE
        //OtpErlangObject[] score = {new OtpErlangAtom("score"),new OtpErlangLong(Long.parseLong(hits.doc(i).get("score")))};
        OtpErlangObject[] title = {new OtpErlangAtom("title"),new OtpErlangString(hits.doc(i).get("title"))};
        OtpErlangObject[] author = {new OtpErlangAtom("author"),new OtpErlangString(hits.doc(i).get("author"))};
        OtpErlangObject[] link = {new OtpErlangAtom("link"),new OtpErlangString(hits.doc(i).get("link"))};
        OtpErlangObject[] text = {new OtpErlangAtom("text"),new OtpErlangString(hits.doc(i).get("text"))};
        OtpErlangObject[] doc = {/*new OtpErlangTuple(score), */new OtpErlangTuple(title), new OtpErlangTuple(author), new OtpErlangTuple(link), new OtpErlangTuple(text)};
        OtpErlangObject[] doc_tuple = {new OtpErlangAtom("doc"), new OtpErlangList(doc)};

        doc_list[i] = new OtpErlangTuple(doc_tuple);
      }

      OtpErlangObject[] results = {new OtpErlangAtom("results"),new OtpErlangList(doc_list)};

      OtpErlangTuple result_tuple = new OtpErlangTuple(results);

      return result_tuple;

    }

   public static void main (String[] args) throws Exception
    {

      OtpNode bar = new OtpNode("searcherserver1@cloud-laptop");
      OtpMbox mbox = bar.createMbox("searcher_server");

      OtpErlangObject o;
      OtpErlangTuple  msg;
      int  type;
      OtpErlangPid    from;
      //BigInteger      n;
      String query;
      String          s2;

      OtpErlangAtom ok = new OtpErlangAtom("ok");
      OtpErlangAtom error = new OtpErlangAtom("error");

      SearcherServer ss = new SearcherServer();

      if(args.length != 0)
      {
        System.out.println(USAGE);
        System.exit(1);
      }

      while(true)
      {
        try
        {
          o    = mbox.receive();
          msg  = (OtpErlangTuple)o;
          from = (OtpErlangPid)(msg.elementAt(0));
          type = ((OtpErlangLong)(msg.elementAt(1))).intValue();
          query = ((OtpErlangString)(msg.elementAt(2))).toString();
          query = query.substring(1, query.length()-1);
          System.out.println("SearchServer got2: " + type + " " + query);

          // Search Chat H istory
          if (type == 1)
          {
            OtpErlangObject[] reply = new OtpErlangObject[2];
            reply[0] = ok;
            reply[1] = ss.SearchChatHistory(query);

            OtpErlangTuple tuple = new OtpErlangTuple(reply);
            mbox.send(from,tuple);
          }
          // Search RSS
          else if (type == 2)
          {
            OtpErlangObject[] reply = new OtpErlangObject[2];
            reply[0] = ok;
            reply[1] = ss.SearchRSS(query);

            OtpErlangTuple tuple = new OtpErlangTuple(reply);
            mbox.send(from,tuple);
          }
          // Index Chat History
          else if (type == 101)
          {
            ChatHistoryIndexer.indexData(new File("/opt/chatyeo/data/ch_index"));
            mbox.send(from,ok);
          }
          // Index RSS
          else if (type == 102)
          {
            System.out.println("Going to rss index: " + query);
            RSSIndexer.indexDir(new File(query), new File("/optx/chatyeo/data/rss_index"));
            mbox.send(from,ok);
          }
          // Open indices
          else if (type == 501)
          {
            ss.OpenChatHistoryIndex("/opt/chatyeo/data/ch_index");
            ss.OpenRSSIndex("/opt/chatyeo/data/rss_index");
            mbox.send(from,ok);
          }
          else
          {
            System.out.println("Ignoring search type: " + type);
            OtpErlangObject[] reply = new OtpErlangObject[2];
            reply[0] = error;
            reply[1] = new OtpErlangString("Error: Search type: " + type + " is not supported");
            OtpErlangTuple tuple = new OtpErlangTuple(reply);
            mbox.send(from,tuple);
          }
        }
        catch(OtpErlangExit e)
        {
          break;
        }
      }
   }
}
