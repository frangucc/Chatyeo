import org.wikipedia.miner.model.WikipediaDatabase;
import org.wikipedia.miner.model.*;
import java.io.*;

public class Test{

        public static void main(String args[]) throws Exception{
		// This should connect to the Database and execute the query there to create the db;
		WikipediaDatabase w = new WikipediaDatabase("localhost", "wikipedia", "wikiuser", "z8efx");
	}
}
