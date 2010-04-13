import org.wikipedia.miner.model.WikipediaDatabase;
import org.wikipedia.miner.model.*;
import java.io.*;

public class LoadData{

        public static void main(String args[]) throws Exception{

                WikipediaDatabase w = new WikipediaDatabase("localhost", "wikipedia", "wikiuser", "z8efx");
                w.loadData(new File(args[0]), true);
                w.prepareForTextProcessor(new org.wikipedia.miner.util.text.CaseFolder());

        }
}
