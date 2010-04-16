import org.wikipedia.miner.model.*;
import org.wikipedia.miner.util.*;
import org.wikipedia.miner.util.text.*;
import org.wikipedia.miner.annotation.*;
import org.wikipedia.miner.annotation.preprocessing.*;
import org.wikipedia.miner.annotation.tagging.*;
import org.wikipedia.miner.annotation.weighting.*;
import java.io.*;
import java.util.*;
import gnu.trove.*;

public class ChatyeoTopicDetector{
  private static final String USAGE = "java ChatyeoTopicDetector databaseServer databaseName userName password article";
  private static final String DISAMBIGUATOR_FILE = "models/disambig.model";
  private static final String LINKDETECTOR_FILE = "models/linkDetect.model";
  public static int MICRO_CAP = 1;
  public static int MACRO_CAP = 2;
  private static final int MICRO_TREND = 0;
  private static final int MACRO_TREND = 1;

  private double minProbability = 0.35 ;
  private Disambiguator disambiguator ;
  private TopicDetector topicDetector ;
  private LinkDetector linkDetector ;
  private TextProcessor tp;
  private Wikipedia wikipedia;
  private DocumentPreprocessor dp;
  private TLongObjectHashMap<Trend[]> chats;
  //private HashSet banned_topics;

  public ChatyeoTopicDetector(String databaseServer, String databaseName, String userName, String password) throws Exception
  {
    this.wikipedia = new Wikipedia(databaseServer,databaseName,userName,password);
    System.out.println("Caching In Links");
    this.wikipedia.getDatabase().cacheInLinks(new File("en_20090306/"),null,null);
    System.out.println("Caching Pages");
    this.wikipedia.getDatabase().cachePages(new File("en_20090306/"),null,null);
    System.out.println("Caching Achors");
    this.wikipedia.getDatabase().cacheAnchors(new File("en_20090306/"), new CaseFolder(), null, 10, null);
    System.out.println("Caching Generality");
    this.wikipedia.getDatabase().cacheGenerality(new File("en_20090306/"),null,null);

    this.tp = new CaseFolder();

    this.disambiguator = new Disambiguator(this.wikipedia, tp, 0.01, 0, 25);
    this.disambiguator.loadClassifier(new File(DISAMBIGUATOR_FILE));

    this.topicDetector = new TopicDetector(this.wikipedia, disambiguator, null, true, false);

    this.linkDetector = new LinkDetector(this.wikipedia); 
    this.linkDetector.loadClassifier(new File(LINKDETECTOR_FILE));

    this.dp = new HtmlPreprocessor();

    this.chats = new TLongObjectHashMap<Trend[]>();

    //this.banned_topics = new Hashset<Integer>();
  }

  private void ban_topics(PreprocessedDocument d)
  {
  }

  public void set_window_size(long chan_id, int micro_size, int macro_size)
  {
      Trend[] trends = this.chats.get(chan_id);
      
      if(trends == null){
          micro_size = micro_size == -1 ? MICRO_CAP : micro_size;
          macro_size = macro_size == -1 ? MACRO_CAP : macro_size;

          trends = new Trend[2];
          trends[MICRO_TREND] = new Trend(chan_id, micro_size, dp, topicDetector, linkDetector, false);
          trends[MACRO_TREND] = new Trend(chan_id, macro_size, dp, topicDetector, linkDetector, true);
      }else{
          if(micro_size != -1)
              trends[MICRO_TREND] = new Trend(chan_id, micro_size, dp, topicDetector, linkDetector, false);
          if(macro_size != -1)
              trends[MACRO_TREND] = new Trend(chan_id, macro_size, dp, topicDetector, linkDetector, true);
      }
  }

  public void clearAll()
  {
      this.chats = new TLongObjectHashMap<Trend[]>();
  }

  public LinkedList<ChatyeoTopic> detect_topics(String message)
  {
      LinkedList<ChatyeoTopic> topics = new LinkedList<ChatyeoTopic>();
      try{
          PreprocessedDocument doc = dp.preprocess(message);
          doc.banTopic(143856) ;
          doc.banTopic(464907);
          doc.banTopic(290);
          doc.banTopic(1400413);
          doc.banTopic(48203);
          doc.banTopic(2329860);
          doc.banTopic(161019);
          SortedVector<Topic> allTopics = linkDetector.getWeightedTopics(topicDetector.getTopics(doc, null)) ;
          for(Topic dt:allTopics)
          {
              ChatyeoTopic chatyeo_topic = new ChatyeoTopic(dt.getTitle(), dt.getId(), dt.getWeight(),
                      dt.getRelatednessToContext(), dt.getRelatednessToOtherTopics(), dt.getGenerality(),false);
              topics.add(chatyeo_topic); 
          }
      }catch(Exception e){
          e.printStackTrace();
      }

      return topics;

  }
  public LinkedList<ChatyeoTopic>[] getMacroTopicsContributedBy(long chan_id, String username)
  {
      Trend[] trends = this.chats.get(chan_id);
      TIntObjectHashMap<ChatyeoTopic> h = trends[MACRO_TREND].getPrimaryTopicsForUser(username);
      TIntObjectIterator<ChatyeoTopic> i = h.iterator();
      return null;

  }

  public LinkedList<ChatyeoTopic>[] getMicroTopicsContributedBy(long chan_id, String username)
  {
      return null;
  }

  public void add(long chan_id, String username, String s)
  {
    Trend[] trends = this.chats.get(chan_id);
    if(trends == null){
      trends = new Trend[2];
      trends[MICRO_TREND] = new Trend(chan_id, MICRO_CAP, dp, topicDetector, linkDetector, false);
      trends[MACRO_TREND] = new Trend(chan_id, MACRO_CAP, dp, topicDetector, linkDetector, true);

      //trends[MICRO_TREND] = new Trend(chan_id, MICRO_CAP, dp, topicDetector, linkDetector, banned_topics, false);
      //trends[MACRO_TREND] = new Trend(chan_id, MACRO_CAP, dp, topicDetector, linkDetector, banned_topics, true);
    }

    trends[MICRO_TREND].add(username, s);
    trends[MACRO_TREND].add(username, s);
    this.chats.put(chan_id,trends);
  }

  public LinkedList<ChatyeoTopic> getPrimaryMacroTrendTopics(long chan_id)
  {
    Trend[] trends = this.chats.get(chan_id);
    if(trends == null || trends[MACRO_TREND] == null)
        return new LinkedList<ChatyeoTopic>();
    TIntObjectHashMap<ChatyeoTopic> h = trends[MACRO_TREND].getPrimaryTopics();
    TIntObjectIterator<ChatyeoTopic> i = h.iterator();
    LinkedList<ChatyeoTopic> l = new LinkedList<ChatyeoTopic>();
    while(i.hasNext()){
      i.advance();
      l.add(i.value());
    }
    return l;
  }

  public LinkedList<ChatyeoTopic> getSecondaryMacroTrendTopics(long chan_id)
  {
    Trend[] trends = this.chats.get(chan_id);
    if(trends == null || trends[MACRO_TREND] == null)
        return new LinkedList<ChatyeoTopic>();
    TIntObjectHashMap<ChatyeoTopic> h = trends[MACRO_TREND].getSecondaryTopics();
    TIntObjectIterator<ChatyeoTopic> i = h.iterator();
    LinkedList<ChatyeoTopic> l = new LinkedList<ChatyeoTopic>();
    while(i.hasNext()){
      i.advance();
      l.add(i.value());
    }
    return l;
  }
  public LinkedList<ChatyeoTopic> getPrimaryMicroTrendTopics(long chan_id)
  {
    Trend[] trends = this.chats.get(chan_id);
    if(trends == null || trends[MICRO_TREND] == null)
        return new LinkedList<ChatyeoTopic>();

    TIntObjectHashMap<ChatyeoTopic> h = trends[MICRO_TREND].getPrimaryTopics();
    TIntObjectIterator<ChatyeoTopic> i = h.iterator();
    LinkedList<ChatyeoTopic> l = new LinkedList<ChatyeoTopic>();
    while(i.hasNext()){
      i.advance();
      l.add(i.value());
    }
    return l;
  }

  public LinkedList<ChatyeoTopic> getSecondaryMicroTrendTopics(long chan_id)
  {
    Trend[] trends = this.chats.get(chan_id);
    if(trends == null || trends[MICRO_TREND] == null)
        return new LinkedList<ChatyeoTopic>();

    TIntObjectHashMap<ChatyeoTopic> h = trends[MICRO_TREND].getSecondaryTopics();
    TIntObjectIterator<ChatyeoTopic> i = h.iterator();
    LinkedList<ChatyeoTopic> l = new LinkedList<ChatyeoTopic>();
    while(i.hasNext()){
      i.advance();
      l.add(i.value());
    }
    return l;
  }

  public static void main(String[] args) throws Exception
  {
    if(args.length != 5){
      System.out.println(USAGE);
      System.exit(1);
    }
    System.err.println("Testing ChatyeoTopicDetector");
    ChatyeoTopicDetector c = new ChatyeoTopicDetector(args[0], args[1], args[2], args[3]);
    Scanner s = new Scanner(new File(args[4]));
    String conv = "";
    int i = 0;
    while(s.hasNextLine()){
      String l = s.nextLine();
      conv += l + "\n";
      if(i % 3 == 0){
        c.add(0,"test", conv);
        conv = "";
        LinkedList<ChatyeoTopic> list;
        System.err.println("Printing Primary Macro Topics:");
        list = c.getPrimaryMacroTrendTopics(0);
        for(ChatyeoTopic topic: list)
          System.err.println(color_green(topic));
        System.err.println("Printing Secondary Macro Topics:");
        list = c.getSecondaryMacroTrendTopics(0);
        for(ChatyeoTopic topic: list)
          System.err.println(color_green(topic));
        System.err.println("Printing Primary Micro Topics:");
        list = c.getPrimaryMicroTrendTopics(0);
        for(ChatyeoTopic topic: list)
          System.err.println(color_green(topic));
        System.err.println("Printing Secondary Micro Topics:");
        list = c.getSecondaryMicroTrendTopics(0);
        for(ChatyeoTopic topic: list)
          System.err.println(color_green(topic));

      }
      i++;
    }

  }
  public static String color_green(Object s)
  {
    return "\033[32m" + s.toString() + (char)27 + "[0m";
  }
}
