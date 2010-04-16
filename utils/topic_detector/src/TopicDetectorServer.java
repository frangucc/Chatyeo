import java.io.File;
import java.io.StringReader;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import org.apache.log4j.*;
import com.ericsson.otp.erlang.*;


public class TopicDetectorServer
{
    public static final int GET_TOPICS = 1;
    public static final int GET_ADMIN_TOPICS = 2;
    public static final int GET_TOPICS_BY_USER = 3;
    public static final int GET_TOPICS_EXCLUDING_USER = 4;
    public static final int DETECT_TOPICS = 5;
    public static final int CLEAR_ALL = 6;
    public static final int MWFP = 7;
    public static final int MWFS = 8;
    public static final int MCFP = 9;
    public static final int MCFS = 10;
    public static final int MRTOFP = 11;
    public static final int MRTOFS = 12;
    public static final int SET_DEFAULT_WS = 13;
    public static final int SET_WS= 14;

    public static String USAGE = "TopicDetectorServer server:port mysql_db mysql_user mysql_pw";
    public static int THREADS = 20;
    private ChatyeoTopicDetector c = null;
    private static ThreadPoolExecutor threadPool;
    private HashSet chanSet;
    static Logger cat = Logger.getLogger(TopicDetectorServer.class.getName());

    public TopicDetectorServer(String server, String db, String user, String pw) throws Exception
    {
        c = new ChatyeoTopicDetector(server,db,user,pw);
        chanSet = new HashSet();
	System.out.println("Topic Detector Server Listening...");
    }
    public Worker getWorker(OtpErlangObject o, OtpMbox mbox){
        return new Worker(o,mbox);
    }

    private class Worker extends Thread{
        OtpErlangObject o;
        OtpErlangTuple msg;
        int type;
        OtpErlangPid from;
        String query;
        String username;
        long room_id;
        OtpErlangAtom ok = new OtpErlangAtom("ok");
        OtpErlangAtom nil = new OtpErlangAtom("nil");
        OtpErlangAtom error = new OtpErlangAtom("error");
        OtpErlangAtom ignore = new OtpErlangAtom("ignore");


        OtpMbox mbox;

        public Worker(OtpErlangObject o, OtpMbox mbox){
            this.o = o;
            this.mbox = mbox;
        }

        public void run(){

            try{
                msg = (OtpErlangTuple)o;
                from = (OtpErlangPid)(msg.elementAt(0));
                type = ((OtpErlangLong)(msg.elementAt(1))).intValue();

                cat.info("Got Message of type " + type + " from " + from);

                // Check string for topics(can be multiple sentences)
                if (type == GET_TOPICS)
                {
                    cat.info("Handling GET_TOPICS");
                    //{Channel_Id, [{Username, Text},...]}
                    room_id = ((OtpErlangLong)(msg.elementAt(2))).longValue();
                    OtpErlangList list = (OtpErlangList)(msg.elementAt(3));

                    cat.info("TopicDetector got: " + type + " " + list);

                    if(!chanSet.contains(room_id)){
                        chanSet.add(room_id);
                        OtpErlangObject[] reply = new OtpErlangObject[2];

                        reply[0] = ok;
                        reply[1] = query(room_id, list);

                        OtpErlangTuple tuple = new OtpErlangTuple(reply);
                        cat.info("Sending Reply: " + tuple);
                        mbox.send(from,tuple);
                        chanSet.remove(room_id);
                    }else{
                        cat.info("Ignoring message in GET_TOPICS");
                        OtpErlangObject[] reply = new OtpErlangObject[2];
                        reply[0] = error;
                        reply[1] = ignore;
                        OtpErlangTuple tuple = new OtpErlangTuple(reply);
                        cat.info("Sending Reply: " + tuple);
                        mbox.send(from,tuple);
                    }
                }else if(type == GET_ADMIN_TOPICS)
                {
                    cat.info("Handling GET_ADMIN_TOPICS");
                    room_id = ((OtpErlangLong)(msg.elementAt(2))).longValue();
                    OtpErlangObject[] reply = new OtpErlangObject[2];
                    reply[0] = ok;
                    try{
                        reply[1] = query_admin(room_id, query);
                    }catch(Exception e){
                        cat.error("Something bad happened", e);
                        reply[1] = nil;
                    }
                    OtpErlangTuple tuple = new OtpErlangTuple(reply);
                    cat.info("Sending Reply: " + tuple);
                    mbox.send(from,tuple);
                }else if(type == DETECT_TOPICS){
                    cat.info("Handling DETECT_TOPICS");
                    OtpErlangObject[] reply = new OtpErlangObject[2];
                    reply[0] = ok;
                    reply[1] = detect_topics(new String(
                                ((OtpErlangBinary)(msg.elementAt(2))).binaryValue()));
                    OtpErlangTuple tuple = new OtpErlangTuple(reply);
                    cat.info("Sending Reply: " + tuple);
                    mbox.send(from,tuple);

                }else if(type == CLEAR_ALL){
                    c.clearAll();
                }else if(type == MWFP){
                    double weight = ((OtpErlangDouble)(msg.elementAt(2))).doubleValue();
                    set_min_weight_for_primary(weight);
                }else if(type == MWFS){
                    double weight = ((OtpErlangDouble)(msg.elementAt(2))).doubleValue();
                    set_min_weight_for_secondary(weight);
                }else if(type == MCFP){
                    double weight = ((OtpErlangDouble)(msg.elementAt(2))).doubleValue();
                    set_min_context_for_primary(weight);
                }else if(type == MCFS){
                    double weight = ((OtpErlangDouble)(msg.elementAt(2))).doubleValue();
                    set_min_context_for_secondary(weight);
                }else if(type == MRTOFP){
                    double weight = ((OtpErlangDouble)(msg.elementAt(2))).doubleValue();
                    set_min_rel_to_others_for_primary(weight);
                }else if(type == MRTOFS){
                    double weight = ((OtpErlangDouble)(msg.elementAt(2))).doubleValue();
                    set_min_rel_to_others_for_secondary(weight);
                }else if(type == SET_DEFAULT_WS){
                    int micro_size = ((OtpErlangInt)(msg.elementAt(2))).intValue();
                    int macro_size = ((OtpErlangInt)(msg.elementAt(3))).intValue();
                    set_default_window_size(micro_size, macro_size);
                }else if(type == SET_WS){
                    room_id = ((OtpErlangLong)(msg.elementAt(2))).longValue();
                    int micro_size = ((OtpErlangInt)(msg.elementAt(3))).intValue();
                    int macro_size = ((OtpErlangInt)(msg.elementAt(4))).intValue();
                    set_window_size(room_id, micro_size, macro_size);
                }
                else
                {
                    cat.warn("Ignoring search type: " + type);
                    OtpErlangObject[] reply = new OtpErlangObject[2];
                    reply[0] = error;
                    reply[1] = new OtpErlangString("Error: Search type: " + type + " is not supported");
                    OtpErlangTuple tuple = new OtpErlangTuple(reply);
                    mbox.send(from,tuple);
                }
            }catch(Exception e){
                cat.error("Something bad happened", e);
            }
        }
    }
    private OtpErlangString query_admin(long id, String q) throws Exception
    {
        LinkedList<ChatyeoTopic> macro_primary_topics = c.getPrimaryMacroTrendTopics(id);

        String mpt = "[";
        Iterator i = macro_primary_topics.iterator();
        while(i.hasNext())
        {
            ChatyeoTopic topic = (ChatyeoTopic)i.next();
            //OtpErlangObject[] tuple = {new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight)};
            mpt += "\"" + topic.topic_name + "(" + topic.topic_weight + ")\"";
            if(i.hasNext())
                mpt += ", ";
        }

        mpt += "]";
        mpt = "\"pt\": " + mpt + ",\n\"st\": [";


        LinkedList<ChatyeoTopic> macro_secondary_topics = c.getSecondaryMacroTrendTopics(id);
        i = macro_secondary_topics.iterator();
        while(i.hasNext())
        {
            ChatyeoTopic topic = (ChatyeoTopic)i.next();
            //OtpErlangObject[] tuple = {new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight)};
            mpt += "\"" + topic.topic_name + "(" + topic.topic_weight + ")\"";
            if(i.hasNext())
                mpt += ", ";
        }
        mpt += "]";

        LinkedList<ChatyeoTopic> micro_primary_topics = c.getPrimaryMicroTrendTopics(id);

        String mst = "[";
        i = micro_primary_topics.iterator();
        while(i.hasNext())
        {
            ChatyeoTopic topic = (ChatyeoTopic)i.next();
            //OtpErlangObject[] tuple = {new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight)};
            mst += "\"" + topic.topic_name + "(" + topic.topic_weight + ")\"";
            if(i.hasNext())
                mst += ", ";
        }

        mst += "]";
        mst = "\"pt\": " + mst + ",\n\"st\": [";


        LinkedList<ChatyeoTopic> micro_secondary_topics = c.getSecondaryMicroTrendTopics(id);
        i = micro_secondary_topics.iterator();
        while(i.hasNext())
        {
            ChatyeoTopic topic = (ChatyeoTopic)i.next();
            //OtpErlangObject[] tuple = {new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight)};
            mst += "\"" + topic.topic_name + "(" + topic.topic_weight + ")\"";
            if(i.hasNext())
                mst += ", ";
        }
        mst += "]";


        String s = "{\n\"macro\" : {" + mpt + "\n},\n\"micro\": {" + mst + "}\n}";
        cat.info(s);

        return new OtpErlangString(s);
    }


    /*
     *  private OtpErlangList query(long id, String q) throws Exception
     *  {
     *    c.add(id,q);
     *    LinkedList<ChatyeoTopic> primary_topics = c.getPrimaryMacroTrendTopics(id);
     *    OtpErlangObject[] results = new OtpErlangObject[primary_topics.size()];
     *
     *
     *    System.err.println("Detected Topics: ");
     *    int i = 0;
     *    for(ChatyeoTopic topic: primary_topics)
     *    {
     *      //OtpErlangObject[] tuple = {new OtpErlangString(l.get(i)[0]),new OtpErlangDouble(Double.parseDouble(l.get(i)[1]))};
     *      OtpErlangObject[] tuple = {new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight)};
     *      // results[i] = new OtpErlangString(l.get(i)[0]);
     *      results[i++] = new OtpErlangTuple(tuple);
     *    }
     *
     *    OtpErlangList result_list = new OtpErlangList(results);
     *
     *    return result_list;
     *  }
     */
    private OtpErlangTuple query(long id, OtpErlangList messages) throws Exception
    {
        java.util.Iterator<OtpErlangObject> iter = messages.iterator();
        while(iter.hasNext()){
            OtpErlangTuple message = (OtpErlangTuple)(iter.next());
            String username = new String(((OtpErlangBinary)(message.elementAt(0))).binaryValue());
            String q = new String(((OtpErlangBinary)(message.elementAt(1))).binaryValue());

            c.add(id,username, q);
        }
        LinkedList<ChatyeoTopic> primary_macro_topics = c.getPrimaryMacroTrendTopics(id);
        LinkedList<ChatyeoTopic> primary_micro_topics = c.getPrimaryMicroTrendTopics(id);
        OtpErlangObject[] results = new OtpErlangObject[primary_macro_topics.size()];
        OtpErlangObject[] results2 = new OtpErlangObject[primary_micro_topics.size()];

        System.err.println("Detected Topics: ");
        int i = 0;
        double sum = 0;

        for(ChatyeoTopic topic: primary_macro_topics)
        {
            sum += topic.topic_weight;
        }
        for(ChatyeoTopic topic: primary_macro_topics)
        {
            OtpErlangObject[] tuple = {new OtpErlangInt(topic.topic_id),
                new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight/sum),
                new OtpErlangDouble(topic.rel_to_context), new OtpErlangDouble(topic.rel_to_other),
                new OtpErlangDouble(topic.generality)
            };
            results[i++] = new OtpErlangTuple(tuple);
        }

        i = 0;
        sum = 0;

        for(ChatyeoTopic topic: primary_micro_topics)
        {
            sum += topic.topic_weight;
        }
        for(ChatyeoTopic topic: primary_micro_topics)
        {
            OtpErlangObject[] tuple = {new OtpErlangInt(topic.topic_id),
                new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight/sum),
                new OtpErlangDouble(topic.rel_to_context), new OtpErlangDouble(topic.rel_to_other),
                new OtpErlangDouble(topic.generality)
            };
            results2[i++] = new OtpErlangTuple(tuple);
        }

        //OtpErlangList[] result_list = new OtpErlangList[] {new OtpErlangList(results), new OtpErlangList(results2)};
        OtpErlangObject[] result_object = new OtpErlangObject[] {new OtpErlangList(results), new OtpErlangList(results2)};
        OtpErlangTuple result_tuple = new OtpErlangTuple(result_object);
        //OtpErlangObject[] tuple = {new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight)};
        //return result_list;
        return result_tuple;
    }


    private OtpErlangList detect_topics(String message) throws Exception
    {
        LinkedList<ChatyeoTopic> topics = c.detect_topics(message);
        OtpErlangObject[] results = new OtpErlangObject[topics.size()];

        int i = 0;
        Iterator<ChatyeoTopic> iterator = topics.iterator();

        while(iterator.hasNext()){
            ChatyeoTopic topic = iterator.next();
            results[i++] = new OtpErlangTuple(new OtpErlangObject[] {new OtpErlangInt(topic.topic_id),
                new OtpErlangString(topic.topic_name),new OtpErlangDouble(topic.topic_weight),
                new OtpErlangDouble(topic.rel_to_context), new OtpErlangDouble(topic.rel_to_other),
                new OtpErlangDouble(topic.generality)
            });
        }
        return new OtpErlangList(results);
    }

    private void set_min_weight_for_primary(double weight)
    {
        ChatyeoTopic.MIN_WEIGHT_FOR_PRI = weight;
    }

    private void set_min_weight_for_secondary(double weight)
    {
        ChatyeoTopic.MIN_WEIGHT_FOR_SEC = weight;
    }

    private void set_min_context_for_primary(double weight)
    {
        ChatyeoTopic.MIN_CONTXT_FOR_PRI = weight;
    }

    private void set_min_context_for_secondary(double weight)
    {
        ChatyeoTopic.MIN_CONTXT_FOR_SEC = weight;
    }

    private void set_min_rel_to_others_for_primary(double weight)
    {
        ChatyeoTopic.MIN_REL_TO_OTHRS_FOR_PRI = weight;
    }


    private void set_min_rel_to_others_for_secondary(double weight)
    {
        ChatyeoTopic.MIN_REL_TO_OTHRS_FOR_SEC = weight;
    }

    private void set_default_window_size(int micro_size, int macro_size)
    {
        macro_size = macro_size == -1 ? ChatyeoTopicDetector.MACRO_CAP : macro_size;
        micro_size = micro_size == -1 ? ChatyeoTopicDetector.MICRO_CAP : micro_size;
        ChatyeoTopicDetector.MICRO_CAP = micro_size;
        ChatyeoTopicDetector.MACRO_CAP = macro_size;
    }

    private void set_window_size(long room, int micro_size, int macro_size)
    {
        this.c.set_window_size(room, micro_size, macro_size);
    }

    

    public static void main (String[] args) throws Exception
    {
        OtpNode bar = new OtpNode("topicdetectorserver1");
        OtpMbox mbox = bar.createMbox("topicdetector_server");
        OtpErlangObject o;

        TopicDetectorServer td = new TopicDetectorServer(args[0], args[1], args[2], args[3]);
        threadPool = new ThreadPoolExecutor(THREADS,THREADS,1, TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(THREADS*100));
        if(args.length != 4)
        {
            System.out.println(USAGE);
            System.exit(1);
        }
        while(true)
        {
            try
            {
                o = mbox.receive();
                Worker w = td.getWorker(o,mbox);
                threadPool.execute(w);
            }
            catch(OtpErlangExit e)
            {
                e.printStackTrace();
                break;
            }
        }
    }
}
