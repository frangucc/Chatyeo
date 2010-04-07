package com.chatyeo.lucene.util;

import com.chatyeo.lucene.util.ChatHistoryDocument;

import org.apache.lucene.analysis.*;
import org.apache.lucene.store.*;
import org.apache.lucene.analysis.snowball.*;
import org.apache.lucene.analysis.standard.*;
import org.apache.lucene.document.*;
import org.apache.lucene.index.*;

import java.io.*;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.xml.sax.*;

import javax.xml.parsers.*;

import java.sql.*;

//Builds Lucene index
public class ChatHistoryIndexer
{
  //public static File INDEX_DIR = new File("index");
  public static String USAGE = "com.chatyeo.lucene.util.ChatHistoryIndexer index_dir";
  public static boolean DEBUG = true;
  private static int STATE_INDOC = 1;
  private static int STATE_OUTDOC = 1;


  public static void main(String[] args) throws Exception
    {
      File INDEX_DIR = new File(args[0]);

      if(args.length != 1)
      {
        System.out.println(USAGE);
        System.exit(1);
      }

      indexData(INDEX_DIR);

    }

  public static void indexData(File index) throws Exception
    {
      //Directory directory = FSDirectory.getDirectory(INDEX_DIR);
      Directory directory = FSDirectory.getDirectory(index);
      Analyzer analyzer = new SnowballAnalyzer("English");
      IndexWriter iwriter = new IndexWriter(directory, analyzer, true);

      try
      {
        Statement stmt;
        ResultSet rs;

        // Register the JDBC driver for MySQL.
        Class.forName("com.mysql.jdbc.Driver");

        // Define URL of database server
        String url = "jdbc:mysql://localhost:3306/chatyeo";

        // Get a connection to the database
        Connection con = DriverManager.getConnection(url,"root", "temp4now");

        // Get a Statement object
        stmt = con.createStatement();


        // Query the database
        rs = stmt.executeQuery("SELECT messages.id, users.username, rooms.room, messages.time, messages.message from users, messages, rooms WHERE users.id=messages.who_from AND messages.user=rooms.id");

        ChatHistoryDocument chdoc = new ChatHistoryDocument();

        // Extract results and index
        while(rs.next())
        {
          chdoc.id= rs.getString("id");
          chdoc.username = rs.getString("username");
          chdoc.room = rs.getString("room");
          chdoc.time = rs.getString("time");
          chdoc.message = rs.getString("message");
          System.out.println(chdoc.id + " " + chdoc.username + " " + chdoc.room + " " + chdoc.time + " " + chdoc.message);
          try
          {
            indexChatHistoryDoc(chdoc, iwriter);
          }
          catch(Exception e)
          {
            e.printStackTrace();
            System.out.println("Caused by document " + chdoc.message);
          }
        }

        con.close();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

      iwriter.optimize();
      iwriter.close();

      directory.close();
    }

  public static void indexChatHistoryDoc(ChatHistoryDocument chdoc, IndexWriter iw) throws Exception
    {
      System.out.println("Adding document");
      Document doc = new Document();
      doc.add(new Field("id", chdoc.id, Field.Store.YES, Field.Index.UN_TOKENIZED));
      doc.add(new Field("username", chdoc.username, Field.Store.YES, Field.Index.TOKENIZED));
      doc.add(new Field("room", chdoc.room, Field.Store.YES, Field.Index.TOKENIZED));
      doc.add(new Field("time", chdoc.time, Field.Store.YES, Field.Index.UN_TOKENIZED));
      doc.add(new Field("message", chdoc.message, Field.Store.YES, Field.Index.TOKENIZED));
      iw.addDocument(doc);
    }
}
