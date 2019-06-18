package patmat

/**
  * Huffman coding is an algorithm to do data compression and it forms the basic idea behind file compression.
  * Read more about it https://www.techiedelight.com/huffman-coding/
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
    abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  /**
    * Gets the weight of the given codetree.
    * We use pattern matching here to match the type of object we are dealing with
    * @param tree The input CodeTree object
    * @return the weight of the tree as an integer
    */
    def weight(tree: CodeTree): Int = tree match {
      case Fork(_,_,_,w) => w
      case Leaf(_, w) => w
    }

  /**
    * Gets the list of characters in a given code tree.
    * If the CodeTree passed is a Fork, then we return the characters in that object
    * else we pass the character in the leaf as a singleton list.
    * @param tree CodeTree object
    * @return List of characters in that tree
    */
    def chars(tree: CodeTree): List[Char] = tree match {
      case Fork(_,_,chars,_) => chars
      case Leaf(char, _) => List(char)
    }

  /**
    * This method returns the merged CodeTree for the given left and right subtree
    * To do this, we create a Fork object of the left and right subtree and merge the list of characters in the subtrees
    * And for the weight of the tree we simply add the weights of the right and left subtree.
    * @param left Left subtree
    * @param right Right subtree
    * @return CodeTree object
    */
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Generating Huffman trees

  /**
    * Just a utility method to convert string to a list of characters.
    * @param str input string
    * @return List of characters
    */
  def string2Chars(str: String): List[Char] = str.toList

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    *   times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    *   List(('a', 2), ('b', 1))
    *
    * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
    * character and an integer. Pairs can be constructed easily using parentheses:
    *
    *   val pair: (Char, Int) = ('c', 1)
    *
    * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
    *
    *   val theChar = pair._1
    *   val theInt  = pair._2
    *
    * Another way to deconstruct a pair is using pattern matching:
    *
    *   pair match {
    *     case (theChar, theInt) =>
    *       println("character is: "+ theChar)
    *       println("integer is  : "+ theInt)
    *   }
    *   This method is implemented using the foldLeft concept in scala.
    *   Foldleft method in scala lets you iterate over each element in the collection where you can write your own function
    *   for each element in the collection. In the method below, the foldleft function is applied to the list of input characters to
    *   create a Map of [Char, Int].
    *   chars.foldLeft(Map[Char, Int]()) generates function with the signature (Map[Char, Int],Char) => Map[Char, Int]
    *   The intial values passed to this function are an empty map and the first character in the list of characters.
    *   In the iterate function, we store the count of each character by checking if the map already contains the given character,
    *   if yes, we add one to the existing count, if not we initialize the value to 0.
    *   Then we add the character 'c' and its count.
    *   We repeat this for all the characters in the list.
    *   This method returns a list, not a map, so we convert the map to list using the iterator method of map.
    * @param chars the input list of characters
    * @return List[(Char, Int)]
    */
    def times(chars: List[Char]): List[(Char, Int)] = {
      def iterate(map: Map[Char, Int], c: Char) = {
        val count = (map.get(c)).getOrElse(0) + 1
        map + ((c, count))
      }
      chars.foldLeft(Map[Char, Int]())(iterate).iterator.toList
    }

  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    *
    * To generate the leaf nodes from the given list of (Char, Int) pair,
    * we first sort this list by using the sortBy method on the weight of the character and the character itself
    * For example, (c,10),(b,10),(e,2) should be sorted to (e,2),(b,10), (c,10) and not (e,2),(c,10),(b,10)
    * Then we map each of this list to create a Leaf object with the character and weight as its attributes.
    *
    * @param freqs List[(Char, Int)]
    * @return ordered list of leaf nodes
    */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      freqs.sortBy(pair => (pair._2, pair._1)).map(pair => Leaf(pair._1, pair._2))
    }

  /**
    * Checks whether the list `trees` contains only one single code tree.
    * This method checks if the length of the tree list is one. If yes, it returns true else false.
    * @param trees List[CodeTree]
    * @return true if the list is singleton, false otherwise
    */
    def singleton(trees: List[CodeTree]): Boolean = {
      if (trees.length == 1) true else false
    }

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    *
    * We combine the first two trees of the list using pattern matching in scala.
    * the first case left :: right :: rest will match the first CodeTree, second CodeTree and the rest of the tree.
    * left is a CodeTree type of object, right is also a CodeTree type object and the rest is a list of CodeTree type objects.
    * We call the makeCodeTree method to create a Fork of the two subtrees, then we add the codetree to the remaining list of CodeTree objects.
    * Then we sort this list on weight of the trees.
    * @param trees List[CodeTree]
    * @return combined codetree List[CodeTree]
    */
    def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
      case left :: right :: rest =>
        (makeCodeTree(left, right):: rest).sortBy(t => weight(t))
      case _ => trees
    }

  /**
    * This function will be called in the following way:
    *
    *   until(singleton, combine)(trees)
    * This function combines the CodeTree objects in the given list until it is a singleton tree.
    * The above line means call combine function until the list is of length one.
    * To implement this function, we create the following method, which takes three parameters
    * 1. A function that takes a List[CodeTree] and returns a Boolean value
    * 2. A function that takes a List[CodeTree] and returns a List[CodeTree]
    * 3. A List[CodeTree] objects.
    *
    * The combine function has to be called until the list is of length one.
    *
    * @param singletonTree Function with the signature List[CodeTree] => Boolean
    * @param combineTrees Function with the signature List[CodeTree] => List[CodeTree]
    * @param trees List[CodeTree]
    * @return List[CodeTree] that is a Singleton.
    */
    def until(singletonTree: List[CodeTree] => Boolean, combineTrees: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
      if(singletonTree(trees)) trees
      else until(singletonTree, combineTrees)(combineTrees(trees))
    }

  /**
    * This function creates a code tree which is optimal to encode the text `chars`.
    *
    * The parameter `chars` is an arbitrary text. This function extracts the character
    * frequencies from that text and creates a code tree based on them.
    *
    * Here we have to use all the methods that we created above to create a CodeTree from a given list of characters.
    * The first action is to create a List of Leafs that has each character and the frequency of its occurence in the given list.
    * Then we pass this List[CodeTree] objects to the until function to combine the leafs until a whole tree is built.
    * @param chars List of characters to be encoded
    * @return CodeTree object
    */
    def createCodeTree(chars: List[Char]): CodeTree = {
      until(singleton, combine)(makeOrderedLeafList(times(chars))).head
    }
  

  // Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    *
    * We implement this method using tail recursion.
    * We have an accumulator for storing the characters of the decoded characters.
    * The logic here is if the encountered bit is 0, we move to the left codetree. If the encountered bit is 1, we move to the left codetree.
    * We keep moving until the leaf node is reached. While we reach a leaf node, we store the character in an accumulator list and proceed with the next bit
    * in the sequence and start from the top of the tree.
    *
    * For each CodeTree type, we do the following using scala's pattern matching.
    * 1. If it is a Leaf object, we check if the bit list is empty. If yes, we add the character to the accumulator and return the list.
    *    If the bit list is not empty, we recursively call the same method for the rest of the bits in the decoding sequence.
    * 2. If it is a Fork object, we check the list head value. If it is 0, we call the same method for the left subtree.
    *    If it is 1, we call the same method for the right subtree. While calling the same method we only pass the list of bits' tail not the entire list.
    *
    * @param tree CodeTree
    * @param bits Bits to be decoded.
    * @return Decoded list of characters.
    */
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
      def decodeTree(t: CodeTree, b: List[Bit], acc: List[Char]): List[Char] = t match {
        case Leaf(c , _) => if (b.isEmpty) c :: acc else decodeTree(tree, b, c :: acc)
        case Fork(l, r, _, _) => if (b.head == 0) decodeTree(l, b.tail, acc) else decodeTree(r, b.tail, acc)
      }
      decodeTree(tree, bits, List()).reverse
    }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
    def decodedSecret: List[Char] = decode(frenchCode, secret) // Answer is "huffmanestcool"
  

  // Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    * The encode method encodes the input text using the huffman's encoding.
    * To implement this method, we need to iterate over each character in the text and trannsform it to a list of Bits.
    * To achieve this, we will be using the foldmeft method on the input text.
    * text.foldLeft(List[Bit]()) creates a function that iterates over every character in the input that has the signature (Char, List[Bit]) => List[Bit]
    * Within this function we create another function to implement tail recursion.
    * We have to write a function that iterates on each character of the given text and go through the given CodeTree until we reach the
    * Leaf node that matches the character.
    * The function that moves through the tree to reach the leaf node is the encodeChar function written within the foldleft function.
    *
    * If the encountered tree object matches the Leaf object, we just return the list of bits.
    * If the encountered tree object matches the Fork object, we have to decide if we should move to the left or right subtree.
    *   This decision is taken based on which subtree contains the character we want to encode.
    *   If the right subtree contains the character, we add 1 to the accumulator list and call the encodeChar function for the right subtree.
    *   If the left subtree contains the character, we add 0 to the accumulator list and call the encodeChar function for the left subtree.
    *
    *  We reverse the list finally because the bits we add for each character are prepended to the existing list of bits.
    *
    * @param tree CodeTree
    * @param text Input text to be encoded into a sequence of bits
    * @return encoded sequence of bits.
    */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      text.foldLeft(List[Bit]()){(acc, c) =>
        def encodeChar(t: CodeTree, bits: List[Bit]): List[Bit] = t match {
          case Leaf(c, _) => bits
          case Fork(l, r, _ , _) => if(chars(l) contains(c)) encodeChar(l, 0 :: bits) else encodeChar(r, 1 :: bits)
        }
        encodeChar(tree, acc)
      }.reverse
    }
  
  // Encoding using code table
  // Code table is an object that holds the encoding bits sequence for a character.

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    *
    * In this method, we just have to get the element in the list which matches the character and return the second element of the pair
    * @param table CodeTable object
    * @param char Character to be encoded
    * @return encoding Sequence of bits
    */
    def codeBits(table: CodeTable)(char: Char): List[Bit] = {
      table.find(_._1 == char).get._2
    }

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * To implement this method, we have to create a list of (Char, List[Bit]) tuple.
    * We create this tuple by getting the list of characters of the root node and mapping it to the List[Bit] generated using the encode fucntion
    * implemented above.
    *
    * @param tree CodeTree Object
    * @return CodeTable object
    */
    def convert(tree: CodeTree): CodeTable = {
      chars(tree).map(char => (char -> encode(tree)(List(char))))
    }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
      a ::: b
    }
  
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      def a = convert(tree)
      text.flatMap(codeBits(a)(_))
    }
  }
