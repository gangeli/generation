package induction;

import java.util.*;
import fig.basic.*;

public class Options implements java.io.Serializable{
  private static final long serialVersionUID = 001L;

  public enum ModelType { gmm, pmmm, hmm, pcfg, dmv, seg, align, event3, rollins };
  public enum InitType { random, bait, supervised, uniformz, artificial };
  public enum InputFormat { raw, tag, mrg, seg };
  public enum AlignmentModel { m1, m2, hmm };

  @Option(gloss="Path to the configuration file") public String config = null;
  
  // Input
  @Option public ArrayList<String> inputPaths = new ArrayList();
  @Option public ArrayList<String> inputLists = new ArrayList();
  @Option public ArrayList<String> testInputPaths = new ArrayList();
  @Option public ArrayList<String> testInputLists = new ArrayList();
  @Option public String inputFileExt;
  @Option public String inputFileExt2; // For word alignment, the extension of the other language
  @Option(gloss="Description file for initializing artificial parameters") public String artificialDescriptionPath;
  @Option(gloss="Format of input", required=true) public InputFormat inputFormat;
  @Option(gloss="Maximum number of examples") public int maxExamples = Integer.MAX_VALUE;
  @Option(gloss="Maximum number of test examples") public int testMaxExamples = Integer.MAX_VALUE;
  @Option(gloss="Maximum length of an example") public int maxExampleLength = Integer.MAX_VALUE;
  @Option(gloss="For parsing") public boolean useTagsAsWords = false;
  @Option(gloss="Segment characters (default is segmenting words)") public boolean segChars = false;
  @Option(gloss="For segmentation") public String segBoundary = "||";
  @Option(gloss="Penalize words") public double segPenalty = 0.5;
  @Option(gloss="Maximum number of examples to use for extracting phrases (to limit # phrases)") public int maxExamplesForPhrases = Integer.MAX_VALUE;

  // Model
  @Option(gloss="Model", required=true) public ModelType modelType;
  @Option(gloss="Number of hidden states") public int K = 5;
  @Option(gloss="Maximum phrase length") public int maxPhraseLength = 5;
  @Option public AlignmentModel alignmentModel = AlignmentModel.m1;
  @Option(gloss="Threshold for posterior decoding") public double posteriorThreshold = 0.5;
  @Option public double gmmVariance = 1;
  @Option public double gmmGenRange = 10;
  @Option(gloss="PCFGs: tree structure is fixed") public boolean fixBracketing = false;
  @Option(gloss="PCFGs: fix the preterminal (POS) tags") public boolean fixPreTags = false;
  @Option(gloss="PCFGs: collapse the nonterminal tag set") public boolean collapseNonTags = false;
  @Option(gloss="PCFGs: number of hidden preterminal states") public int pK = 5;
  @Option(gloss="PCFGs: add CCM potentials (generate content and context)") public boolean useCCMPotentials = false;
  @Option(gloss="PCFGs: have the CCM generation depend on the state") public boolean ccmDependsOnState = false;
  @Option public boolean removePunctuation = false;
  @Option public boolean alignAgreement = true;

  //// Event3 model
  @Option(gloss="Output for external evaluation script") public boolean fullPredForEval = false;
  @Option(gloss="Random matching - baseline") public boolean fullPredRandomBaseline = false;
  @Option public Random fullPredRandom = new Random(1);

  @Option public String[] excludedFields = new String[0]; // List of <event type name>.<field name>
  @Option public String[] excludedEventTypes = new String[0]; // List of <event type name>
  @Option(gloss="Take the first event of each type (don't do this)") public boolean takeOneOfEventType = false;
  @Option public boolean treatCatAsSym = false;
  @Option public boolean useOnlyLabeledExamples = false;
  @Option public boolean alignProhibitNone = false;

  // Changes to the model can happen during training, so each of these specifies a starting and ending iteration for the
  // corresponding flag to be on
  @Option public Pair<Integer,Integer> indepEventTypes = new Pair(0, 0);
  @Option public Pair<Integer,Integer> indepFields = new Pair(0, 0);
  @Option(gloss="Each word chooses event type/field independently (word alignment, no segmentation)")
    public Pair<Integer,Integer> newEventTypeFieldPerWord = new Pair(0, 0);
  @Option(gloss="Each word chooses field independently (no segmentation at the field level)")
    public Pair<Integer,Integer> newFieldPerWord = new Pair(0, 0);
  @Option public Pair<Integer,Integer> oneEventPerExample = new Pair(0, 0);
  @Option public Pair<Integer,Integer> oneFieldPerEvent = new Pair(0, 0);

  @Option(gloss="p(t | w)") public boolean includeEventTypeGivenWord = false;
  @Option(gloss="Allow a field to show up twice in a row") public boolean disallowConsecutiveRepeatFields = false;

  // At event-type level
  @Option(gloss="p(include event? | e, v)") public boolean useEventSalienceModel = false;
  @Option(gloss="p(t) or p(t | t0)") public boolean useEventTypeDistrib = true;

  // At field level
  @Option(gloss="Generate and constrain the set of fields which are used for each event mentioned on these event types")
    public String[] useFieldSetsOnEventTypes = new String[0];
  @Option public Pair<Integer,Integer> useFieldSets = new Pair(0, 0);
  @Option public int minFieldSetSize = 0;
  @Option public int maxFieldSetSize = Integer.MAX_VALUE;

  // Tracks
  @Option(gloss="Tracks (for NFL): each track gets a subset of the event types")
    public String[] eventTypeTracks = new String[] { "ALL" }; // Default is one track with everything
  @Option(gloss="Jointly decide whether to have non-none event") public boolean jointEventTypeDecision = false;

  // Modify
  @Option public boolean includeEventTypeAsSymbol = false;
  @Option(gloss="Stem symbols, strings, and words") public boolean stemAll = false;
  @Option(gloss="Annotate and generate numeric quantities with labels (the word that follows)") public Pair<Integer,Integer> genLabels = new Pair(0, 0);

  @Option public String wordRolesPath;
  @Option(gloss="List of <event type name>.<field name>") public String[] useWordRolesOnFields = new String[0]; 

  // Specific control ofsmoothing
  @Option public double noneEventTypeSmoothing = 0;
  @Option public double fixedNoneEventTypeProb = Double.NaN;
  @Option public double fixedGenericProb = Double.NaN;
  @Option public double noneFieldSmoothing = 0;
  @Option public double fieldNameSmoothing = 0;
  @Option public boolean discountCatEmissions = false;

  @Option public boolean andIsPunctuation = false;
  @Option(gloss="A entity segment only break on punctuation") public boolean onlyBreakOnPunctuation = false;
  @Option(gloss="A entity segment cannot cross punctuation") public boolean dontCrossPunctuation = false;
  @Option(gloss="Limit field length (e.g. num and sym must be 1)") public boolean limitFieldLength = false;
  @Option(gloss="For each line, make a separate example (NFL data)") public boolean oneExamplePerLine = false;

  @Option public boolean debug = false;

  // Generic
  @Option public int trainStart = 0;
  @Option public int trainEnd = Integer.MAX_VALUE;
  @Option public int testStart = 0;
  @Option public int testEnd = 0;

  @Option public String alignedPath = null;

  // Learning
  @Option public InitType initType = InitType.random;
  @Option public double initSmoothing = 0.01;
  @Option public double initNoise = 1e-3;
  @Option public Random initRandom = new Random(1);
  @Option(gloss="Randomness for permuting data points in online") public Random onlinePermRandom = new Random(1);
  @Option public boolean onlinePerm = false;
  @Option(gloss="If batch size is larger this threshold, store counts rather than infer states") public int batchSizeNewCounts = 1000;

  @OptionSet(name="stage1") public LearnOptions stage1 = new LearnOptions();
  @OptionSet(name="stage2") public LearnOptions stage2 = new LearnOptions();
  @Option(gloss="Output every this number of iterations") public int outputIterFreq = 1;
  @Option(gloss="Output full predictions (for debugging)") public boolean outputFullPred = false;
  @Option(gloss="Output training objective and test predictions on current parameters") public boolean outputCurrentState = false;
  @Option(gloss="Output every this number of examples (for online)") public double outputExampleFreq = 1000;
  @Option public int outputNumParamsPerVec = Integer.MAX_VALUE;
  @Option(gloss="This computation involves lots of logs and can be slow") public boolean computeELogZEntropy = false;

  // Generate artificial data
  @Option public int genNumExamples = 0;
  @Option(gloss="Maximum number of tokens per example") public int genMaxTokens = 100;
  @Option public Random genRandom = new Random(1);
  @Option public Random genInitRandom = new Random(1);
  @Option public InitType genInitType = InitType.supervised;

  @Option public int artNumWords = 100;
  @Option public double artAlpha = 0.5;

  // General
  @Option(gloss="Number of parameters per state to print") public int numOutputParams = 10;
  @Option(gloss="Number of threads to use") public int numThreads = 4;
  
  
  

  //--GENERATION--
  @Option(gloss="Use templates") public boolean useTemplates = false;
  @Option(gloss="Heuristically align sumtime data") public boolean heuristicAlign = false;
  @Option(gloss="Filter out time fields for sumtime data") public boolean filterTimes = false;
  @Option(gloss="Force each parent to correspond to exactly one child template") public boolean oneChildTemplPerParent = false;
  @Option(gloss="Start interactive shell") public boolean interactive = false;
  @Option(gloss="Score results") public boolean scoreResults = true;
  @Option(gloss="re-train from inferred alignments") public int metaIters = 1;
  @Option(gloss="shuffle alignments") public boolean shuffleAlignments = false;
  @Option(gloss="seed for shuffle alignments") public int shuffleSeed = 42;
  @Option(gloss="drop to LM") public boolean dropToLM = false;
  @Option(gloss="Ignore (none) events") public boolean ignoreNone = false;
  @Option(gloss="Generate from gold alignments") public boolean genFromGold = false;
  @Option(gloss="Bait first event to true") public boolean trueFirstEvent = false;
  @Option(gloss="Test Index File") public String testIndexFile = "index";
  @Option(gloss="Entity Regex") public String entityRegex = "";
  @Option(gloss="The maximum number of templates to extract") public int maxTemplates = 500;
  @Option(gloss="Generate from true event") public boolean forceTrueEvents = false;
  @Option(gloss="Mininum count of templates for it to be considered") public int minTemplCount = 0;
  @Option(gloss="Log choices and their fired features") public boolean logChoices = false;
  @Option(gloss="Force using the largest possible field set") public boolean forceMaximalFS = false;
  @Option(gloss="Absolutely force the true events") public boolean forceStrongTrueEvents = false;
  @Option(gloss="Allow Vstr Fallback") public boolean allowVstrFallback = false;
  @Option(gloss="Sample from the classifier distribution") public boolean sample = false;
  //Saving Data
  @Option(gloss="Load parameters from file") public boolean loadParams = false;
  @Option(gloss="Path to save params to") public String paramSavePath;
  @Option(gloss="Load alignments from file") public boolean loadAligns = false;
  @Option(gloss="Save path for aligned data") public String alignSavePath;
  //Language Model
  public enum LanguageModel { simple, kneserney, rollins, event3 }
  @Option(gloss="Language model to use") public LanguageModel lm;
  @Option(gloss="n-gram length (default: trigram = 3)") public int numGrams = 3;
  @Option(gloss="Allow zero probability words") public boolean allowZeroProbSentence = true;

  @Option(gloss="Maximum number of sentences to read") public int lmMaxExamples = Integer.MAX_VALUE;
  @Option(gloss="Input path for sentences") public ArrayList <String> lmInputPaths = new ArrayList <String> ();
  @Option(gloss="Input file extension for Lanugage Model") public String lmFileExt;
  //Semantic Generation
  public static enum GenerationModel { simple, maxent, perceptron };
  @Option(gloss="Event Model", required=false) public GenerationModel eventModel = GenerationModel.maxent;
  @Option(gloss="Word Model", required=false) public GenerationModel fieldSetModel = GenerationModel.maxent;
  @Option(gloss="Word Model", required=false) public GenerationModel templateModel = GenerationModel.maxent;
  @Option(gloss="Field Model", required=false) public GenerationModel fieldModel = GenerationModel.maxent;
  @Option(gloss="Word Model", required=false) public GenerationModel wordModel = GenerationModel.perceptron;
  @Option(gloss="Number of iterations") public int maxentIterations = 40;
  @Option(gloss="Number of iterations (perceptron)") public int perceptronIterations = 5;
  @Option(gloss="Prohibit number generation from non-numeric field") public boolean prohibitNonNumericNum = false;
  @Option(gloss="Sigma (event choice)") public double sigmaEvent = 1.0;
  @Option(gloss="Sigma (field choice)") public double sigmaField = 1.0;
  @Option(gloss="Sigma (word choice)") public double sigmaWord = 1.0;
  @Option(gloss="Sigma (field set choice)") public double sigmaFieldSet = 1.0;
  @Option(gloss="Sigma (template choice)") public double sigmaTemplate = 1.0;
  @Option(gloss="Only generate from single event") public boolean singleEventGen = false;
  //Global Model
  @Option(gloss="Use the universal training model") public boolean globalTrain = false;
  @Option(gloss="Pre-train the parameters") public boolean globalPretrain = false;
  @Option(gloss="Maximum entropy iterations for pre-train") public int globalMaxentIters = 10;
  @Option(gloss="Percepton iterations for pre-train") public int globalPerceptronIters = 3;
  @Option(gloss="Number of iterations for global model") public int globalIterations = 10;
  @Option(gloss="Start of number of depths in gradual train") public int globalDepthStart = 0;
  @Option(gloss="End of number of depths in gradual train") public int globalDepthEnd = 0;
  @Option(gloss="Be as lazy as possible in global model") public boolean globalLazy = true;
  @Option(gloss="Probability for unknown terms at test") public double globalUnk = 0.0;
  @Option(gloss="Number of particles in training") public int trainParticles = 100;
  @Option(gloss="Number of particles at test time") public int testParticles = 100;
  @Option(gloss="Generate numbers at training, not from params") public boolean directNumGeneration = false;
  @Option(gloss="Use averaged weights") public boolean globalAverageWeights = false;
  @Option(gloss="Debug classifier") public boolean globalDebug = false;
  //Testing
  public static enum SearchType { greedy, kbest };
  @Option(gloss="Search type to use in testing") public SearchType searchType = SearchType.greedy;
  @Option(gloss="K to use in k-best") public int kbestK = 100;
  @Option(gloss="Treat each example generation independently (average score)") public boolean independentExamples = true;
  @Option(gloss="Treat all numbers as a single entity") public boolean squashNums = false;
  @Option(gloss="Threshhold to which numbers are the same") public int maxNumDif = Integer.MAX_VALUE;
  //Debugging
  @Option(gloss="Print as we generate") public boolean genPrint = false;
  @Option(gloss="Print cell width") public int printCellWidth = 20;
  @Option(gloss="Force actual alignments") public boolean forceActualAlignment = false;

  //Features
  public static enum ValueType { none, simple, bucket, fuzzy };
  @Option(gloss="Class for the event choice") public String eventChoice = "generation.EventChoice";
  @Option(gloss="Class for the field choice") public String fieldChoice = "generation.FieldChoice";
  @Option(gloss="Class for the word choice") public String wordChoice = "generation.WordChoice";
  @Option(gloss="Class for the field set choice") public String fieldSetChoice = "generation.FieldSetChoice";
  @Option(gloss="Class for the template choice") public String templateChoice = "generation.TemplateChoice";
  @Option(gloss="Class for the template fill choice") public String templateFillChoice = "generation.TemplateFillChoice";
  //(event)
  @Option(gloss="markov chain features for event") public boolean featEventMarkov = false;
  @Option(gloss="set of events seen before") public boolean featEventHistSet = false;
  @Option(gloss="length of tail to condition over") public int featEventTail = 1;
  @Option(gloss="include sub-tail features") public boolean featEventAllTails = false;
  @Option(gloss="generate event given events seen in past") public boolean featEventHist = false;
  @Option(gloss="repeated event boolean marker") public boolean featEventRep = false;
  @Option(gloss="repeated event type boolean marker") public boolean featEventTypeRep = false;
  @Option(gloss="position of event in the world state") public boolean featEventIndex = false;
  @Option(gloss="field values in world state under event") public ValueType featEventVal = ValueType.none;
  @Option(gloss="number of event types in world state") public boolean featEventDistinct = false;
  @Option(gloss="some temporal information on the event") public boolean featEventTemporal = false;
  @Option(gloss="what other event types are present") public boolean featEventOtherPresent = false;
  @Option(gloss="event overkill feature") public boolean featEventOverkill = false;
  @Option(gloss="event type is new") public boolean featEventNewType = false;
  @Option(gloss="language model stop feature for stop term") public boolean featEventLMStop = false;
  //(field)
  @Option(gloss="condition on the value of the last field") public boolean featFieldLastVal = false;
  @Option(gloss="markov chain features for field") public boolean featFieldMarkov = false;
  @Option(gloss="length of tail to condition over") public int featFieldTail = 1;
  @Option(gloss="include sub-tail features") public boolean featFieldAllTails = false;
  @Option(gloss="repeated field type boolean marker") public boolean featFieldRep = false;
  @Option(gloss="field value in world state for this field") public ValueType featFieldVal = ValueType.none;
  //(word)
  @Option(gloss="markov chain features for word") public boolean featWordMarkov = false;
  @Option(gloss="parent field value as input feature") public ValueType featWordVal = ValueType.none;
  @Option(gloss="word conditioned on other fields in event") public ValueType featWordEventVal = ValueType.none;
  @Option(gloss="feature for the length of generated words") public boolean featWordLen = false;
  @Option(gloss="language model as feature") public boolean featWordLM = false;
  @Option(gloss="condition on the parent event") public boolean featWordPEvent = false;
  @Option(gloss="condition on the parent field") public boolean featWordPField = false;
  @Option(gloss="language model feature weight") public double lmFactor = 1.0;
  //(field set)
  @Option(gloss="prior distribution on template frequency") public boolean featFSPrior = false;
  @Option(gloss="whether we're at the beginning of a generation") public boolean featFSFirst = false;
  @Option(gloss="field values as input feature") public ValueType featFSVal = ValueType.none;
  //(template)
  @Option(gloss="markov chain features for templates") public boolean featTemplMarkov = false;
  @Option(gloss="Language model score on template start") public boolean featTemplLM = false;
  @Option(gloss="markov chain features for templates") public boolean featTemplLength = false;
  @Option(gloss="prior distribution on template frequency") public boolean featTemplPrior = false;
  @Option(gloss="field values as input feature") public ValueType featTemplVal = ValueType.none;
  @Option(gloss="Difference between this field value and prev values of same type") public boolean featTemplValDiff = false;
  @Option(gloss="Threshold for 'same' value") public int valDiffThreshold = 3;
  @Option(gloss="less specific templates") public boolean featTemplHierarchical = false;


  //Depreciated
  @Option(gloss="Number of Particles") public int numParticles = 10000;
  @Option(gloss="Number of examples to generate") public int lmNumGenerated = 10;
  @Option(gloss="Weight of semantic component") public double semWeight = 1.0;
  @Option(gloss="Weight of linguistic component") public double linWeight = 1.0;
  @Option(gloss="Deterministic generation") public boolean deterministic = false;

  @Option(gloss="Initial step size") public double initStepSize = 1.0;
  @Option(gloss="Step-size reduction power") public double stepSizeReductPower = 0.5;
  
}
