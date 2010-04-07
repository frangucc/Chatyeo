import java.util.*;
import org.wikipedia.miner.model.*;
import org.wikipedia.miner.util.*;
import org.wikipedia.miner.util.text.*;
import org.wikipedia.miner.annotation.*;
import org.wikipedia.miner.annotation.preprocessing.*;
import org.wikipedia.miner.annotation.tagging.*;
import org.wikipedia.miner.annotation.weighting.*;
import gnu.trove.*;
import org.apache.log4j.*;

public class Trend{
    private long chan_id;
    private int window_size;
    private SlidingWindow window;
    private TIntObjectHashMap<ChatyeoTopic> primary_topics;
    private TIntObjectHashMap<ChatyeoTopic> secondary_topics;
    private TIntObjectHashMap<ChatyeoTopic> ignored_topics;
    private DocumentPreprocessor dp;
    private TopicDetector topicDetector;
    private LinkDetector linkDetector;
    private boolean isMacro;
    static Logger cat = Logger.getLogger(Trend.class.getName());


    public Trend(long chan_id, int window_size, DocumentPreprocessor dp, TopicDetector topicDetector, LinkDetector linkDetector, boolean isMacro)
    {
        this.chan_id = chan_id;
        this.window_size = window_size;
        this.window = new SlidingWindow(window_size);
        this.dp = dp;
        this.topicDetector = topicDetector;
        this.linkDetector = linkDetector;
        this.isMacro = isMacro;

        this.primary_topics = new TIntObjectHashMap<ChatyeoTopic>();
        this.secondary_topics = new TIntObjectHashMap<ChatyeoTopic>();
        this.ignored_topics = new TIntObjectHashMap<ChatyeoTopic>();
    }

    public TIntObjectHashMap<ChatyeoTopic> getPrimaryTopics()
    {
        return this.primary_topics;
    }

    public TIntObjectHashMap<ChatyeoTopic> getSecondaryTopics()
    {
        return this.secondary_topics;
    }

    public TIntObjectHashMap<ChatyeoTopic> getPrimaryTopicsForUser(String username)
    {
        //cat.info("getting Primary Topics for " + username);
        TIntObjectHashMap topics = new TIntObjectHashMap<ChatyeoTopic>();
        try{
            String chat = this.window.getString(username);
            PreprocessedDocument doc = dp.preprocess(chat);
            doc.banTopic(143856) ;
            doc.banTopic(464907);
            doc.banTopic(290);
            doc.banTopic(1400413);
            doc.banTopic(48203);
            doc.banTopic(2329860);
            doc.banTopic(161019);
            SortedVector<Topic> allTopics = linkDetector.getWeightedTopics(topicDetector.getTopics(doc, null)) ;
            for (Topic dt:allTopics) 
            {
                ChatyeoTopic newTopic = new ChatyeoTopic(dt.getTitle(), dt.getId(),dt.getWeight(),
                        dt.getRelatednessToContext(),dt.getRelatednessToOtherTopics(),dt.getGenerality(), this.isMacro);
                topics.put(newTopic.topic_id,newTopic);
            }
        }catch(Exception e){
            cat.error("something bad happened", e);
        }
        return topics;
    }

    public TIntObjectHashMap<ChatyeoTopic> getPrimaryTopicsWithoutUser(String username){
        //cat.info("Getting primary topics without user " + username);
        TIntObjectHashMap topics = new TIntObjectHashMap<ChatyeoTopic>();
        try{
            String chat = this.window.getStringWithout(username);
            PreprocessedDocument doc = dp.preprocess(chat);
            doc.banTopic(143856) ;
            doc.banTopic(464907);
            doc.banTopic(290);
            doc.banTopic(1400413);
            doc.banTopic(48203);
            doc.banTopic(2329860);
            doc.banTopic(161019);

            SortedVector<Topic> allTopics = linkDetector.getWeightedTopics(topicDetector.getTopics(doc, null)) ;
            for (Topic dt:allTopics) 
            {
                ChatyeoTopic newTopic = new ChatyeoTopic(dt.getTitle(), dt.getId(),dt.getWeight(),
                        dt.getRelatednessToContext(),dt.getRelatednessToOtherTopics(),dt.getGenerality(), this.isMacro);
                topics.put(newTopic.topic_id,newTopic);
            }
        }catch(Exception e){
            cat.error("something bad happened", e);
        }
        return topics;
    }

    public void add(String user, String message)
    {
        if(this.window.size() == this.window_size)
            this.window.dequeue();
        this.window.enqueue(user, message);
        this.recalculateTopics(this.window.getString());
    }

    private void recalculateTopics(String s)
    {
        try{
            PreprocessedDocument doc = dp.preprocess(s);
            doc.banTopic(143856) ;
            doc.banTopic(464907);
            doc.banTopic(290);
            doc.banTopic(1400413);
            doc.banTopic(48203);
            doc.banTopic(2329860);
            doc.banTopic(161019);
            SortedVector<Topic> allTopics = linkDetector.getWeightedTopics(topicDetector.getTopics(doc, null)) ;

            /*
             *LinkedList<ChatyeoTopic> pt = new LinkedList<ChatyeoTopic>();
             *LinkedList<ChatyeoTopic> st = new LinkedList<ChatyeoTopic>();
             *
             */

            TIntObjectHashMap<ChatyeoTopic> pt = new TIntObjectHashMap<ChatyeoTopic>();
            TIntObjectHashMap<ChatyeoTopic> st = new TIntObjectHashMap<ChatyeoTopic>();
            TIntObjectHashMap<ChatyeoTopic> it = new TIntObjectHashMap<ChatyeoTopic>();

            double avg_pri_weight = 0;
            double avg_sec_weight = 0;
            double avg_igr_weight = 0;

            double avg_pri_rel_ctxt = 0;
            double avg_sec_rel_ctxt = 0;
            double avg_igr_rel_ctxt = 0;

            int pri_count = 0;
            int sec_count = 0;
            int igr_count = 0;


            for (Topic dt:allTopics) 
            {
                ChatyeoTopic newTopic = new ChatyeoTopic(dt.getTitle(), dt.getId(),dt.getWeight(),
                        dt.getRelatednessToContext(),dt.getRelatednessToOtherTopics(),dt.getGenerality(), this.isMacro);

                ChatyeoTopic p_oldTopic = this.primary_topics.get(newTopic.topic_id);
                ChatyeoTopic s_oldTopic = this.secondary_topics.get(newTopic.topic_id);
                ChatyeoTopic i_oldTopic = this.ignored_topics.get(newTopic.topic_id);

                double weight = 0;
                if(p_oldTopic != null)
                {
                    double d_weight = newTopic.topic_weight - p_oldTopic.topic_weight;
                    double avg_weight = (newTopic.topic_weight + p_oldTopic.topic_weight)/2;


                    //System.err.println("Primary[\033[34mName=" + newTopic.topic_name + ";Delta PTopic=" + d_weight + ";AvgWeight PTopic=" + 
                            //avg_weight + "]" + ";rel_to_ctxt=" + newTopic.rel_to_context + (char)27 + "[0m");
                    cat.info("Primary[" + newTopic + "]");
                    weight = p_oldTopic.topic_weight;
                }

                if(s_oldTopic != null)
                {
                    double d_weight = newTopic.topic_weight - s_oldTopic.topic_weight;
                    double avg_weight = (newTopic.topic_weight + s_oldTopic.topic_weight)/2;

                    //System.err.println("Secondary[\033[34mName=" + newTopic.topic_name + ";Delta STopic=" + d_weight + ";AvgWeight STopic=" + 
                            //avg_weight + "]" +(char)27 + "[0m");
                    cat.info("Secondary[" + newTopic + "]");
                    weight = s_oldTopic.topic_weight;
                }

                if(i_oldTopic != null)
                {
                    double d_weight = newTopic.topic_weight - i_oldTopic.topic_weight;
                    double avg_weight = (newTopic.topic_weight + i_oldTopic.topic_weight * i_oldTopic.rel_to_other);

                    //System.err.println("Ignored[\033[34mName=" + newTopic.topic_name + ";Delta STopic=" + d_weight + ";AvgWeight STopic=" + 
                            //avg_weight + "]" +(char)27 + "[0m");
                    cat.info("Ignored[" + newTopic + "]");
                    weight = i_oldTopic.topic_weight;
                }

                newTopic.topic_weight += weight + .05;

                if(newTopic.isPrimary()){
                    pt.put(newTopic.topic_id,newTopic);
                    avg_pri_weight += newTopic.topic_weight;
                    avg_pri_rel_ctxt += newTopic.rel_to_context;
                    pri_count++;

                }else if(newTopic.isSecondary()){
                    st.put(newTopic.topic_id, newTopic);
                    avg_sec_weight += newTopic.topic_weight;
                    avg_sec_rel_ctxt += newTopic.rel_to_context;
                    sec_count++;

                }else{
                    //System.err.println("\033[31mIgnoring topic: " + newTopic + (char)27 + "[0m");
                    cat.info("Ignoring Topic[" + newTopic + "]");
                    it.put(newTopic.topic_id, newTopic);
                    avg_igr_weight += newTopic.topic_weight;
                    avg_igr_rel_ctxt += newTopic.rel_to_context;
                    igr_count++;
                }

            }

            for(TIntObjectIterator<ChatyeoTopic> iterator = this.primary_topics.iterator();iterator.hasNext();){
                iterator.advance();
                int key = iterator.key();
                ChatyeoTopic t = iterator.value();
                if(!pt.containsKey(key) && !st.containsKey(key)){
                    t.topic_weight -= .1;
                    if(t.isPrimary())
                        pt.put(key,t);
                    else if(t.isSecondary())
                        st.put(key,t);
                }

            }

            for(TIntObjectIterator<ChatyeoTopic> iterator = this.secondary_topics.iterator();iterator.hasNext();){
                iterator.advance();
                int key = iterator.key();
                ChatyeoTopic t = iterator.value();
                if(!st.containsKey(key) && !pt.containsKey(key)){
                    t.topic_weight -= .1;
                    if(t.isPrimary())
                        pt.put(key,t);
                    else if(t.isSecondary())
                        st.put(key,t);
                }

            }

            this.primary_topics = pt;
            this.secondary_topics = st;
            this.ignored_topics = it;

            if(this.primary_topics.size() == 0 && this.secondary_topics.size() > 0)
                this.primary_topics = this.secondary_topics;

            avg_pri_weight /= pri_count;
            avg_sec_weight /= sec_count;
            avg_igr_weight /= igr_count;

            avg_pri_rel_ctxt /= pri_count;
            avg_sec_rel_ctxt /= sec_count;
            avg_igr_rel_ctxt /= igr_count;

            //System.err.print("\033[33m");
            //System.err.println("Average_Weight[pri=" + avg_pri_weight + ";sec=" + avg_sec_weight +
                    //";igr=" + avg_igr_weight + "]");
            //System.err.println("Average_Rel_To_CTXT[pri=" + avg_pri_rel_ctxt + ";sec" + avg_sec_rel_ctxt + 
                    //";igr=" + avg_igr_rel_ctxt + "]");
            //System.err.println((char)27 + "[0m");
        }catch(Exception e){
            cat.error("something bad happened", e);
        }
    }

}

