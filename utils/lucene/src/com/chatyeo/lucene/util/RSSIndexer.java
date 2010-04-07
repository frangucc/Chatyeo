package com.chatyeo.lucene.util;

import com.chatyeo.lucene.util.RSSDocument;

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

//Builds Lucene index
public class RSSIndexer
{
  //public static File INDEX_DIR = new File("index");
  public static String USAGE = "com.chatyeo.lucene.util.RSSIndexer dir_to_index/ index_dir";
  public static boolean DEBUG = true;
  private static int STATE_INDOC = 1;
  private static int STATE_OUTDOC = 1;


  public static void main(String[] args) throws Exception
    {
      File INDEX_DIR = new File(args[1]);

      if(args.length != 2)
      {
        System.out.println(USAGE);
        System.exit(1);
      }

      indexDir(new File(args[0]), INDEX_DIR);



    }

  public static void indexDir(File dir_to_index, File index) throws Exception
    {
      //Directory directory = FSDirectory.getDirectory(INDEX_DIR);
      Directory directory = FSDirectory.getDirectory(index);
      Analyzer analyzer = new SnowballAnalyzer("English");
      IndexWriter iwriter = new IndexWriter(directory, analyzer, true);

      System.out.println("Dir to index: " + dir_to_index);

      System.out.println("Exists: " + dir_to_index.exists());
      System.out.println("Is directory: " + dir_to_index.isDirectory());
      System.out.println("Files to list: " + dir_to_index.listFiles());

      File[] files = dir_to_index.listFiles();

      System.out.println("Files: " + files);
      System.out.println("files.length: " + files.length);

      for(int i = 0; i < files.length; i++)
      {
        File f = files[i];
        if(DEBUG)
          System.out.println(f.getName());
        indexFile(f, iwriter);
      }

      iwriter.optimize();
      iwriter.close();

      directory.close();
    }

  public static void indexFile(File file_to_index, IndexWriter iwriter) throws Exception
   {
      DataInputStream in = null;
      int state = -1;

      try{
         in = new DataInputStream(new BufferedInputStream(new FileInputStream(file_to_index)));

         while(in.available() != 0) {
            String s = in.readLine();

            if(s.trim().equals("<DOC>"))
            {
               boolean readingText = false;
               RSSDocument rdoc = new RSSDocument();

               while((s = in.readLine().trim()) != null && !s.equals("</DOC>")){
                  if(!readingText){
                     if(s.startsWith("<TITLE>"))
                     {
                        int l_index = s.lastIndexOf("</TITLE>");
                        if(l_index == -1)
                           throw new RuntimeException("END TITLE TAG NOT ON SAME LINE");
                        else
                           rdoc.title = s.substring(7, l_index);
                     }

                     if(s.startsWith("<AUTHOR>"))
                     {
                        int l_index = s.lastIndexOf("</AUTHOR>");
                        if(l_index == -1)
                           throw new RuntimeException("END AUTHOR TAG NOT ON SAME LINE");
                        else
                           rdoc.author = s.substring(8, l_index);
                     }

                     if(s.startsWith("<LINK>"))
                     {
                        int l_index = s.lastIndexOf("</LINK>");
                        if(l_index == -1)
                           throw new RuntimeException("END LINK TAG NOT ON SAME LINE");
                        else
                           rdoc.link = s.substring(6, l_index);
                     }

                     if(s.startsWith("<TEXT>"))
                     {
                        readingText = true;
                        String sub = s.substring(6);
                        if(sub != null && sub.length() != 0)
                           rdoc.text = sub + "\n";
                        else
                           rdoc.text = new String();
                     }

                  }
                  else{
                     if(s.startsWith("</TEXT>"))
                           readingText = false;
                     else
                        rdoc.text += s + "\n";
                  }

               }
               try{
                  indexRSSDoc(rdoc, iwriter);
               }catch(Exception e){
                  e.printStackTrace();
                  System.out.println("Caused by document " + rdoc.title);
               }
            }
         }


      }catch(Exception e){
         e.printStackTrace();
         System.exit(1);
      }

   }

  public static void indexRSSDoc(RSSDocument rssDoc, IndexWriter iw) throws Exception
    {
      Document doc = new Document();
      doc.add(new Field("title", rssDoc.title, Field.Store.YES, Field.Index.TOKENIZED));
      doc.add(new Field("author", rssDoc.author, Field.Store.YES, Field.Index.TOKENIZED));
      doc.add(new Field("link", rssDoc.link, Field.Store.YES, Field.Index.TOKENIZED));
      doc.add(new Field("text", rssDoc.text, Field.Store.YES, Field.Index.TOKENIZED));
      iw.addDocument(doc);
    }
}
