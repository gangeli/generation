package generation;

import java.util.*;
import fig.basic.*;

public class EvalOptions implements java.io.Serializable{
	private static final long serialVersionUID = 001L;

	public enum Dataset { weather, robocup, nfl, sumtime };

  	@Option public ArrayList<String> inputPaths = new ArrayList();
	
	@Option(gloss="Port to use") public int port = 4242;
	
	@Option(gloss="Dataset to train on",required=true) public Dataset dataset;
	@Option(gloss="Test on training data too") public boolean evalTrain = false;
	@Option(gloss="Train Aligned Dataset file to load", required=true) public String trainPath = null;
	@Option(gloss="Test Aligned Dataset file to load", required=true) public String testPath = null;
	@Option(gloss="Path to alignments to beat") public String refPath = null;
	@Option(required=true) public String greetPath = null;
	@Option public boolean useRefs = true;

	@Option(gloss="") public int trainStart = 0;
	@Option(gloss="") public int testStart = 0;
	@Option(gloss="") public int trainNum = 0;
	@Option(gloss="") public int testNum = 0;

	@Option(gloss="") public String cssPath = "http://goobs.org/css/eval.css";
}
