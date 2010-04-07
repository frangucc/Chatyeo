import java.io.File;
import java.io.StringReader;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

public class AnalyzeFile 
{
    private ChatyeoTopicDetector c = null;
    public static String USAGE = "java AnalyzeFile server:port mysql_db mysql_user mysql_pw file";
    public static void main (String[] args) throws Exception
    {
        ChatyeoTopicDetector c = new 
            ChatyeoTopicDetector(args[0], args[1], args[2], args[3]);
        if(args.length != 5)
        {
            System.out.println(USAGE);
            System.exit(1);
        }
        try{
            FileInputStream fstream = new FileInputStream(args[4]);
            DataInputStream in = new DataInputStream(fstream);
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String strLine;
            String file = ""; 
            while ((strLine = br.readLine()) != null)   {
                file += strLine + "\n";
            }
            LinkedList<ChatyeoTopic> cts = c.detect_topics(file);
            Iterator<ChatyeoTopic> i = cts.iterator();

            while(i.hasNext())
            {
                System.out.println(i.next() + "\n");
            }

        }catch(Exception e)
        {
            e.printStackTrace();
        }
    }
}
