/**
 * 
 */
package cortex;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;


import edu.berkeley.nlp.util.Counter;
import fig.basic.StrUtils;

/**
 * @author Alexandre Bouchard
 * 
 * <p>
 * A bleu score evaluator, as specified in:
 * <a href="http://www1.cs.columbia.edu/nlp/sgd/bleu.pdf">http://www1.cs.columbia.edu/nlp/sgd/bleu.pdf</a>
 * </p>
 *
 */
public class BleuScorer {
	
	private static final boolean isInt(String str){
		if(str.length() == 0) return false;
		for(int i=0; i<str.length(); i++){
			char c = str.charAt(i);
			if(i==0 && c=='-' && str.length() > 1){
				continue;	// '-' is ok at the beginning
			}
			if(c < '0' || c > '9'){
				return false;
			}
		}
		return true;
	}
	
	private class NGram{
		private List <String> grams;
		private NGram(List<String> grams){
			this.grams = grams;
		}
		public boolean equals(Object o){
			if(o instanceof NGram){
				NGram other = (NGram) o;
				//if they're different sizes, not the same
				if(this.grams.size() != other.grams.size()){
					return false;
				}
				//for each term in gram
				Iterator <String> iter1 = grams.iterator();
				Iterator <String> iter2 = other.grams.iterator();
				boolean good = true;
				while(iter1.hasNext()){
					//convert to string
					String a = iter1.next();
					String b = iter2.next();
					if(a != null && b != null && isInt(a) && isInt(b)){
						//if they're  both integers, see if they're within threshhold
						int ia = Integer.parseInt(a);
						int ib = Integer.parseInt(b);
						good = good && (Math.abs(ia-ib) <= numthreshold);
					}else if(a == null || b == null){
						return a == b;
					}else{
						//otherwise they have to be equal
						good = good && a.equals(b);
					}
				}
				return good;
			}else{
				return false;
			}
		}
		public int hashCode(){
			//TODO better hashcode, that works with numthreshhold
			int hc = grams.size();
			if(grams.size() > 0 && grams.get(0) != null && !isInt(grams.get(0))){
				hc ^= grams.get(0).hashCode();
			}
			if(grams.size() > 1 && grams.get(1) != null && !isInt(grams.get(1))){
				hc ^= grams.get(1).hashCode();
			}
			return hc;
		}
	}
	
	/**
	 * The threshold to which number terms are the same
	 */
	private int numthreshold = 0;
	
	/**
	 * The maximum length of the n-gram considered
	 * 
	 */
	private int N;
	
	/**
	 * The weights used to combine the individual n-grams
	 * 
	 */
	private List<Double> weights;

	/**
	 * Whether to normalize the text (join digits, tokenize punctuation, etc.)
   * to match the script.
	 */
  private boolean normalize = true;
	
	/**
	 * The setting usually preferred for reporting are 
	 * N=4 with the weights uniform.
	 *
	 */
	public BleuScorer()
	{
		this(4);	//changed from 4
	}
	
	/**
	 * 
	 * @param N The maximum length of the n-gram considered
	 */
	public BleuScorer(int N)
	{
		this.N = N;
		weights = new ArrayList<Double>();
		for (int i = 0; i < N; i++)
		{
			weights.add((double) 1/(double) N);
		}
	}
	
	/**
	 * If weights == null, this is equivalent to calling the anonymous constructor.
	 * @param weights The weigths used to combine the individual n-grams
	 */
	public BleuScorer(List<Double> weights)
	{
		this();
		if (weights != null)
		{
			this.weights = weights;
			this.N = weights.size();
		}
	}
	
	public void setThreshold(int set){
		this.numthreshold = set;
		if(set > 0){
			this.normalize = false;
		}
	}
	
	/**
	 * Wrapper to fit in the type hierarchy.
	 * 
	 * The sentences in the field outWords are evaluated.
	 * 
	 * Note that I am using different names for evaluateSentencePair, evaluateSentencePairs and evaluateBleu
	 * because of limitations of generics
	 * 
	 * @param candidates
	 * @return
	 */
	public BleuScore scoreSentencePairExamples(List<SentencePairExample> candidates, List<List<SentencePairExample>> referenceSets)
	{
		// transform the candidate into the appropriate format
		List<List<String>> transformedCandidates = sentencePairExamples2Sentences(candidates);
		// transform the referenceSets into the appropriate format
		List<List<List<String>>> transformedReferenceSets = new ArrayList<List<List<String>>>();
		for (List<SentencePairExample> currentReferenceSet : referenceSets)
		{
			transformedReferenceSets.add(sentencePairExamples2Sentences(currentReferenceSet));
		}
		return evaluateBleu(transformedCandidates, transformedReferenceSets);
	}
	
	/**
	 * Wrapper to fit in the type hierarchy.
	 * 
	 * The sentences in the field outWords are evaluated.
	 * 
	 * Note that I am using different names for evaluateSentencePair, evaluateSentencePairs and evaluateBleu
	 * because of limitations of generics
	 * 
	 * @param candidate
	 * @param reference
	 * @return
	 */
	public BleuScore scoreSentencePairExample(SentencePairExample candidate, SentencePairExample reference)
	{
		List<SentencePairExample> candidates = new ArrayList<SentencePairExample>();
		candidates.add(candidate);
		List<List<SentencePairExample>> referenceSets = new ArrayList<List<SentencePairExample>>();
		List<SentencePairExample> references = new ArrayList<SentencePairExample>();
		references.add(reference);
		referenceSets.add(references);
		return scoreSentencePairExamples(candidates, referenceSets);
	}
	
	/**
	 * 
	 * 
	 * @param sentencesPairs
	 * @return
	 */
	protected List<List<String>> sentencePairExamples2Sentences(List<SentencePairExample> sentencesPairs)
	{
		List<List<String>> sentences = new ArrayList<List<String>>();
		for (SentencePairExample currentSentencePair : sentencesPairs)
		{
			sentences.add(Arrays.asList(currentSentencePair.getOutWords()));
		}
		return sentences;
	}

  private static List<String> normalizeText(List<String> words) {
    String text = StrUtils.join(words);

    // We assume most of the tokenization has already been done properly

    text = text.toLowerCase();
	  // Language-independent part:
	  //text  =~ s/<skipped>//g; # strip "skipped" tags
	  //text  =~ s/-\n//g; # strip end-of-line hyphenation and join lines
	  //text  =~ s/\n/ /g; # join lines
    text = text.replaceAll("(\\d)\\s+(?=\\d)", "$1"); // join digits
	  //text  =~ s/&quot;/"/g;  # convert SGML tag for quote to "
	  //text  =~ s/&amp;/&/g;   # convert SGML tag for ampersand to &
	  //text  =~ s/&lt;/</g;    # convert SGML tag for less-than to >
	  //text  =~ s/&gt;/>/g;    # convert SGML tag for greater-than to <
	  // Language-dependent part (assuming Western languages):
	  //text  = " $norm_text ";
	  //text  =~ tr/[A-Z]/[a-z]/ unless $preserve_case;
	  text = text.replaceAll("([\\{-\\~\\[-\\` -\\&\\(-\\+\\:-\\@\\/])", " $1 "); // tokenize punctuation
	  text = text.replaceAll("([^0-9])([\\.,])", "$1 $2 "); // tokenize period and comma unless preceded by a digit
	  text = text.replaceAll("([\\.,])([^0-9])", " $1 $2"); // tokenize period and comma unless followed by a digit
	  text = text.replaceAll("([0-9])(-)", "$1 $2 "); // tokenize dash when preceded by a digit
	  //text  =~ s/\s+/ /g; # one space only between words
	  //text  =~ s/^\s+//;  # no leading space
	  //text  =~ s/\s+$//;  # no trailing space
    words = Arrays.asList(StrUtils.split(text.trim(), "\\s+"));
    //fig.basic.LogInfo.stderr.println(StrUtils.join(words));
    return words;
  }
	
	/**
	 * Evaluate a bleu score. 
	 * 
	 * The nesting of the lists has the following meaning:
	 * Define a Sentence as a List of String's, <br/>
	 * The parameter candidates should be a List of Sentence's <br/>
	 * Define a Reference as a List of Sentence's <br/>
	 * The parameter referenceSet should be a List of Reference's
	 * 
	 * Note that I am using different names for evaluateSentencePair, evaluateSentencePairs and evaluateBleu
	 * because of limitations of generics
	 * 
	 * TODO: integrate the string transformations that the nist perl script uses:
	 * # language-independent part:
	 * $norm_text =~ s/<skipped>//g; # strip "skipped" tags
	 * $norm_text =~ s/-\n//g; # strip end-of-line hyphenation and join lines
	 * $norm_text =~ s/\n/ /g; # join lines
	 * $norm_text =~ s/(\d)\s+(?=\d)/$1/g; #join digits
	 * $norm_text =~ s/&quot;/"/g;  # convert SGML tag for quote to "
	 * $norm_text =~ s/&amp;/&/g;   # convert SGML tag for ampersand to &
	 * $norm_text =~ s/&lt;/</g;    # convert SGML tag for less-than to >
	 * $norm_text =~ s/&gt;/>/g;    # convert SGML tag for greater-than to <
 	 * 
	 * # language-dependent part (assuming Western languages):
	 * $norm_text = " $norm_text ";
	 * $norm_text =~ tr/[A-Z]/[a-z]/ unless $preserve_case;
	 * $norm_text =~ s/([\{-\~\[-\` -\&\(-\+\:-\@\/])/ $1 /g;   # tokenize punctuation
	 * $norm_text =~ s/([^0-9])([\.,])/$1 $2 /g; # tokenize period and comma unless preceded by a digit
	 * $norm_text =~ s/([\.,])([^0-9])/ $1 $2/g; # tokenize period and comma unless followed by a digit
	 * $norm_text =~ s/([0-9])(-)/$1 $2 /g; # tokenize dash when preceded by a digit
	 * $norm_text =~ s/\s+/ /g; # one space only between words
	 * $norm_text =~ s/^\s+//;  # no leading space
	 * $norm_text =~ s/\s+$//;  # no trailing space
	 * 
	 * @param candidates A list of sentence translated by the mt system.
	 * @param referenceSets A list of set of "referenceSet"s.
	 * @return
	 */
	public BleuScore evaluateBleu(List<List<String>> candidates, List<List<List<String>>> referenceSets)
	{
	
    if(normalize) {
      List<List<String>> newCandidates = new ArrayList<List<String>>();
      for(List<String> candidate : candidates)
        newCandidates.add(normalizeText(candidate));
      candidates = newCandidates;

      List<List<List<String>>> newReferenceSets = new ArrayList<List<List<String>>>();
      for(List<List<String>> referenceSet : referenceSets) {
        List<List<String>> newReferenceSet = new ArrayList<List<String>>();
        for(List<String> reference : referenceSet)
          newReferenceSet.add(normalizeText(reference));
        newReferenceSets.add(newReferenceSet);
      }
      referenceSets = newReferenceSets;
    }

		List<Double> individualNGramScorings = new ArrayList<Double>();
		for (int i = 0; i < N; i++)
		{
			individualNGramScorings.add(computeIndividualNGramScoring(i + 1, candidates, referenceSets));
		}
		return new BleuScore(individualNGramScorings, weights, computeR(candidates, referenceSets), computeC(candidates));
	}
	
	public BleuScore evaluateBleuSentence(List<String> candidate, List<List<String>> referenceSet)
	{
		List<List<String>> candidates = new ArrayList<List<String>>();
		candidates.add(candidate);
		List<List<List<String>>> referenceSets = new ArrayList<List<List<String>>>();
		referenceSets.add(referenceSet);
		return evaluateBleu(candidates, referenceSets);
	}
	
	/**
	 * c is the total length of the candidate translation corpus.
	 * 
	 * @param candidates
	 * @return
	 */
	protected double computeC(List<List<String>> candidates)
	{
		double sum = 0.0;
		for (List<String> currentCandidate : candidates)
		{
			sum += currentCandidate.size();
		}
		return sum;
	}
	
	/**
	 * 
	 * The test corpus effective reference length, r, is computed by summing the best match lengths 
	 * for each candidate sentence in the corpus.
	 * 
	 * @param candidates
	 * @param referenceSets
	 * @return
	 */
	protected double computeR(List<List<String>> candidates, List<List<List<String>>> referenceSets)
	{
		double sum = 0.0;
		for (int i = 0; i < candidates.size(); i++)
		{
			double min = Double.POSITIVE_INFINITY;
			double argmin = 0.0;
			// find the best match
			for (List<List<String>> currentReferences : referenceSets)
			{
				double currentValue = Math.abs(currentReferences.get(i).size() - candidates.get(i).size());
				if (currentValue < min)
				{
					min = currentValue;
					argmin = currentReferences.get(i).size();
				}
			}
			sum += argmin;
		}
		return sum;
	}
	
	/**
	 * 
	 * Compute the modified unigram precisions. 
	 * 
	 * To compute this,
	 * one first counts the maximum number of times
	 * a word occurs in any single reference translation.
	 * Next, one clips the total count of each candidate
	 * word by its maximum reference count,
	 * adds these clipped counts up, and divides by the
	 * total (unclipped) number of candidate words.
	 * Modified n-gram precision is computed similarly
	 * for any n: all candidate n-gram counts
	 * and their corresponding maximum reference
	 * counts are collected. The candidate counts are
	 * clipped by their corresponding reference maximum
	 * value, summed, and divided by the total
	 * number of candidate n-grams.
	 * How do we compute modified n-gram precision
	 * on a multi-sentence test set? Although one typically
	 * evaluates MT systems on a corpus of entire
	 * documents, our basic unit of evaluation is the
	 * sentence. A source sentence may translate to
	 * many target sentences, in which case we abuse
	 * terminology and refer to the corresponding target
	 * sentences as a sentence. We first compute
	 * the n-gram matches sentence by sentence.
	 * Next, we add the clipped n-gram counts for all
	 * the candidate sentences and divide by the number
	 * of candidate n-grams in the test corpus to 
	 * compute a modified precision score, pn, for the
	 * entire test corpus.
	 * In other words, we use a word-weighted average
	 * of the sentence-level modified precisions rather
	 * than a sentence-weighted average. As an example,
	 * we compute word matches at the sentence
	 * level, but the modified unigram precision is the
	 * fraction of words matched in the entire test corpus.
	 * 
	 * @param n n in n-gram
	 * @param candidates 
	 * @param referenceSets
	 * @return
	 */
	protected double computeIndividualNGramScoring(int n, List<List<String>> candidates, List<List<List<String>>> referenceSets)
	{
		double denominator = 0.0;
		double numerator = 0.0;
		// loop over all (candidate, referenceSet) pairs
		for (int i = 0; i < candidates.size(); i++)
		{
			List<String> currentCandidate = candidates.get(i);
			// extract the counts of all the k-grams, where k=n....
			// ...in the candidate...
			Counter<NGram> candidateNGramCounts = extractNGramCounts(n, currentCandidate);
			// ...and in each of the references
			List<Counter<NGram>> referenceSetNGramCounts = new ArrayList<Counter<NGram>>();
			for (List<List<String>> currentReferenceSet : referenceSets)
			{
				referenceSetNGramCounts.add(extractNGramCounts(n, currentReferenceSet.get(i)));
			}
			// compute the modified n-gram precisions 
			for (NGram currentNGram : candidateNGramCounts.keySet())
			{
				// the count in the candidate sentence of the current n-gram is added to the denominator
				double currentCount = candidateNGramCounts.getCount(currentNGram);
				denominator += currentCount;
				// find, over all the references, the maximum number of occurrence of the current ngram 
				double max = 0.0;
				for (Counter<NGram> currentReferenceNGramCounts : referenceSetNGramCounts)
				{
					double tempCount = currentReferenceNGramCounts.getCount(currentNGram);
					if (tempCount > max)
					{
						max = tempCount;
					}
				}
				// the minimum of {max, currentCount} is added to the numerator
				if (max < currentCount)
				{
					numerator += max;
				}
				else
				{
					numerator += currentCount;
				}
			}
		}
		// if the sums were empty, return 0.0 (to mirror NIST standard)
		if (denominator == 0.0)
		{
			return 0.0;
		}
		else
		{
			return numerator / denominator;
		}
	}
	
	/**
	 * Extract all the ngrams and their counts in a given sentence.
	 * 
	 * @param n n in n-gram
	 * @param sentences
	 * @return
	 */
	protected Counter<NGram> extractNGramCounts(int n, List<String> sentences)
	{
		Counter<NGram> nGrams = new Counter<NGram>();
		for (int i = 0; i <= sentences.size() - n; i++)
		{
			nGrams.incrementCount(new NGram(sentences.subList(i, i + n)), 1.0);
		}
		return nGrams;
	}
	
	
	/**
	 * Example of usage of this class.
	 * 
	 * @param args
	 */
	public static void main(String [] args)
	{
		List<String> candidate = new ArrayList<String>();
		candidate.addAll(Arrays.asList("a0 a2391 a14 a1384 a14 a45 a7 a8 a9 a10".split(" ")));
		List<String> reference = new ArrayList<String>();
		reference.addAll(Arrays.asList("a0 a1 a2 a3 a1 a4 a5 a6 a7 a8 a9 a10".split(" ")));
		List<List<String>> refSet = new ArrayList<List<String>>();
		refSet.add(reference);
		
		BleuScorer s = new BleuScorer();
		s.setThreshold(3);
		System.out.println(s.evaluateBleuSentence(candidate, refSet)); // shoud be .2274
		
	}
}
