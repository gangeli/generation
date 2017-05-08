package cortex;

import fig.basic.*;
import java.util.*;

/**
 * This example consists of the following items.
 *  - Input sentence inWords (x).
 *  - Output sentence outWords (y).
 *  - Hidden state: phrase segmentation and phrase alignment (z)
 * Features will be defined on all of (x, y, z).
 */
public class SentencePairExample {

	private String[] inWords;
	private String[] outWords;

	public SentencePairExample(List<String> inWords, List<String> outWords) {
		if(inWords != null){
			this.inWords = ListUtils.toArray(inWords);
		}
		if(outWords != null){
			this.outWords = ListUtils.toArray(outWords);
		}
	}
	public SentencePairExample(String[] inWords, String[] outWords) {
		this.inWords = inWords;
		this.outWords = outWords;
	}


	public String[] getInWords()  { return inWords; }
	public String[] getOutWords() { return outWords; }
	public int numInWords()    { return inWords.length; }
	public int numOutWords()   { return outWords.length; }

}
