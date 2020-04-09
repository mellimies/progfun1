package forcomp

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   *
   * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   *
   * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   *
   * Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val os = w.toLowerCase
      .toSeq
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .toList
      .sortBy(t => (t._1, t._2))
    //    println("wordOccurrences " + os)
    os
  }

  /** Converts a sentence into its character occurrence list. */
  //  def sentenceOccurrences(s: Sentence): Occurrences = ???
  //  def sentenceOccurrences(s: Sentence): Occurrences = s flatMap wordOccurrences
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   * the words that have that occurrence count.
   * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   * For example, the word "eat" has the following character occurrence list:
   *
   * `List(('a', 1), ('e', 1), ('t', 1))`
   *
   * Incidentally, so do the words "ate" and "tea".
   *
   * This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  //  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = ???
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val occDict = dictionary // val dictionary: List[Word]
      .map(w => (wordOccurrences(w), w))
      .groupBy(_._1) // occ -> Map(occ, List(Occurrences, List[Word]))
      .map(pairs => pairs._1 -> pairs._2.map(_._2)) // drop Occurrences from value pair
      .withDefaultValue(List())

    //    println("dictionaryByOccurrences: " + occDict)
    occDict
  }

  /** Returns all the anagrams of a given word. */
  //  def wordAnagrams(word: Word): List[Word] = ???
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word)).filter(!_.isEmpty)

  /** Returns the list of all subsets of the occurrence list.
   * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   * is a subset of `List(('k', 1), ('o', 1))`.
   * It also include the empty subset `List()`.
   *
   * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   * List(
   * List(),
   * List(('a', 1)),
   * List(('a', 2)),
   * List(('b', 1)),
   * List(('a', 1), ('b', 1)),
   * List(('a', 2), ('b', 1)),
   * List(('b', 2)),
   * List(('a', 1), ('b', 2)),
   * List(('a', 2), ('b', 2))
   * )
   *
   * Note that the order of the occurrence list subsets does not matter -- the subsets
   * in the example above could have been displayed in some other order.
   */

  type Occurrence = (Char, Int)

  //    def combinations(occurrences: Occurrences): List[Occurrences] = ???
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    def mapAccumulator(elem: Occurrence, acc: Set[Occurrences]): Set[Occurrences] = {
      val mappedAcc = for {
        occs <- acc
        newEntry = elem :: occs
        if !occs.exists(elem._1 == _._1)
      } yield newEntry.sorted
      mappedAcc
    }

    def newAccumulator(elem: Occurrence, acc: Set[Occurrences]): Set[Occurrences] = {
      acc + List(elem) ++ mapAccumulator(elem, acc)
    }

    @scala.annotation.tailrec
    def loop(occurrences: Occurrences, acc: Set[Occurrences]): List[Occurrences] = {
      //      println(s"OCS $occurrences ACC ${acc.toList.mkString("\n")}")
      occurrences match {
        case Nil => acc.toList
        case o :: os =>
          val (char, count) = o
          if (count > 1) loop((char, count - 1) :: os, newAccumulator(o, acc))
          else loop(os, newAccumulator(o, acc))
      }
    }
    loop(occurrences, Set(List()))
  }


  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   * The precondition is that the occurrence list `y` is a subset of
   * the occurrence list `x` -- any character appearing in `y` must
   * appear in `x`, and its frequency in `y` must be smaller or equal
   * than its frequency in `x`.
   *
   * Note: the resulting value is an occurrence - meaning it is sorted
   * and has no zero-entries.
   */
  //  def subtract(x: Occurrences, y: Occurrences): Occurrences = ???
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val step1 = (x ++ y).groupBy(_._1).toList
    val ans = step1.map {
      case (key, List(v)) => (key, v._2)
      case (key, List(e1, e2)) => (key, e1._2 - e2._2)
    }
      .filter(p => p._2 > 0)
    ans.sorted

  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   * An anagram of a sentence is formed by taking the occurrences of all the characters of
   * all the words in the sentence, and producing all possible combinations of words with those characters,
   * such that the words have to be from the dictionary.
   *
   * The number of words in the sentence and its anagrams does not have to correspond.
   * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   * Also, two sentences with the same words but in a different order are considered two different anagrams.
   * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   * `List("I", "love", "you")`.
   *
   * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   * List(
   * List(en, as, my),
   * List(en, my, as),
   * List(man, yes),
   * List(men, say),
   * List(as, en, my),
   * List(as, my, en),
   * List(sane, my),
   * List(Sean, my),
   * List(my, en, as),
   * List(my, as, en),
   * List(my, sane),
   * List(my, Sean),
   * List(say, men),
   * List(yes, man)
   * )
   *
   * The different sentences do not have to be output in the order shown above - any order is fine as long as
   * all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   * so it has to be returned in this list.
   *
   * Note: There is only one anagram of an empty sentence.
   */

    /**
     * My unoptimized implementation for sentenceAnagrams:
     * 0: check for special case of empty sentence
     * 1: get the full list of words from combinations (contains word anagrams, therefore unoptimized)
     * 2: subtract sentenceOccurrences(List(word)) from original occurrences, if we have characters left then
     * 3: get all words from characters left (occs -> combs -> map(dict...)
     * 4: for each word left repeat loop except that word list is now List(newWord, word)
     * 5: terminate when:
     *    a: list of word matches occurrences -> anagram
     *    b: we have no more new words from characters that are still left (not anagram)
     * 6: return flatMapped value for each word into loop -> all tests pass, grade 10/10.
     *
     * Implementation is compact but has taken almost two weeks, a lot of attempts to get the algorithm
     * on paper, excel etc. Coursera estimate of 3 hours is closer to 30 but very happy I did not give
     * up and was able to get the solution. It's not exactly what's in the guide but works.
     *
     * It could still be optimized by taking only a single word from dictionary values, any list
     * returned contains word anagrams so occurrences is the same for each word in list. Actually initially
     * tried to use that but then the result would need to be processed for each word and expanded
     * to word anagrams, maybe later, moving this mountain is enough for now.

     */
  //  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    if (sentence.isEmpty) List(sentence) else {

      def realWords(combs: List[Occurrences]): List[Word] = combs.flatMap(dictionaryByOccurrences).filterNot(_.isEmpty)

      val occs = sentenceOccurrences(sentence)

      def loop(anagramCandidateList: Sentence, acc: Set[Sentence]): Set[Sentence] = {
        val newOccs = subtract(occs, sentenceOccurrences(anagramCandidateList))
        //        println(s"Candidates: $anagramCandidateList, occs left: $newOccs")
        if (newOccs == Nil) acc + anagramCandidateList // anagram :)
        else // still occs left or not anagram
        realWords(combinations(newOccs)).flatMap(w => loop(w :: anagramCandidateList, acc)).toSet
      }

      realWords(combinations(occs)).flatMap(w => loop(List(w), Set.empty))
    }

  }
}

  object Dictionary {
    def loadDictionary: List[String] = {
      val wordstream = Option {
        getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
      } getOrElse {
        sys.error("Could not load word list, dictionary file not found")
      }
      try {
        val s = scala.io.Source.fromInputStream(wordstream)
        s.getLines.toList
      } catch {
        case e: Exception =>
          println("Could not load word list: " + e)
          throw e
      } finally {
        wordstream.close()
      }
    }
  }
