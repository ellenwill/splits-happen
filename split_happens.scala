/**
  * Ellen Will
  * Programming Exercise from Enlighten IT Consulting/Alion
  * This program will take in a valid sequence of rolls and use pattern matching and
  * recursion to produce the total score of an American Ten-Pin bowling game
  * Written in Scala
  *
  */

object split_happens {
  //Constants
  //the end of the game if the user gets a strike or a spare
  //ends when there are 3 more elements in our list
  // this is 10 pin bowling, so each frame it is possible to knock down
  //at most 10 pins
  val EndOfGame = 3
  val AllPins = 10

  /**
    * Main method
    */
  def main(args:Array[String]): Unit ={
    //Test cases
    //No need to check for valid rolls
    //No need to check for correct number of rolls and frames
    //no need to provide scores for intermediate frames
    println("Bowling Score!")
    val test1 = "XXXXXXXXXXXX"
    val test2 = "9-9-9-9-9-9-9-9-9-9-"
    val test3 = "5/5/5/5/5/5/5/5/5/5/5"
    val test4 = "X7/9-X-88/-6XXX81"

    val list1 = toList(test1)
    val list2 = toList(test2)
    val list3 = toList(test3)
    val list4 = toList(test4)
    println("Intput: "+ test1 + " = "+  addFrames(list1))
    println("Intput: "+ test2 + " = "+ addFrames(list2))
    println("Intput: "+ test3 +" = "+  addFrames(list3))
    println("Intput: "+ test4 +" = "+  addFrames(list4))
  }
  /**
    * addFrames
    * addFrames passes in a Char list, which is the line, and returns the int value
    * score of the game. it uses recursion to get each element of the list and assess
    * what the score will be
    *
    */
  def addFrames(list: List[Char]):Int =
    list match {
      //base case
      case Nil => 0
      //recursive step
      case listHead :: listTail =>
        if(list.length == EndOfGame && listHead.equals('X')) strike(list)
        else if(list.length == EndOfGame && listTail.head.equals('/')) spare(list)
        else if(list.length != 1 && listTail.head.equals('/')) spare(list) + addFrames(listTail)
        else if(listHead.equals('X')) strike(list) + addFrames(listTail)
        else convert(listHead)+addFrames(listTail)

    }
  /**
    * convert
    * convert passes in a char and returns the integer value of that character
    * X -> a strike is 10 pins
    * - -> a miss is zero
    * / -> a spare is knocking all the pins down in two tries, calculated with spare methods
    * so 0 is dummy value.
    * number -> score of a frame that is not a miss, spare or strike. it is converted with the
    * asDigit method which could be implemented with pattern matching with each number 1-9 as
    * a case.
    *
    */
  def convert(frame:Char) : Int = frame match {
    case 'X' => AllPins
    case '-' => 0
    case '/' => 0
    case number => number.asDigit
  }

  /**
    * strike
    * strike passes in the char list of the line starting with the X(strike) it is evaluating
    * this will calculate the score when a user gets a strike, a strike
    * is the simple score of the ten pins originally knocked down
    * plus the next two frames.
    * if in the next two frames a user gets a spare, the score for the strike frame
    * will be 20.
    * we do not want to change the list
    *
    */
  def strike(list: List[Char]): Int = {
    if(list.tail.tail.head.equals('/'))
      AllPins+AllPins
    else
      convert(list.head)+ convert(list.tail.head) + convert(list.tail.tail.head)
  }
  /**
    * spare
    * spare takes in the char list starting with the element before the
    * spare character
    * this will calculate the score by adding 10 to the score of the next frame
    *
    */
  def spare(list: List[Char]): Int = {
    convert(list.head) + AllPins-convert(list.head) + convert(list.tail.tail.head)
  }


  /**
    * toList
    * toList passes in the string of the line and converts it
    * into a char list.
    *
    */
  def toList(str: String): List[Char] = str.length match{
    case 0 => Nil
    case default => str.head :: toList(str.tail)

  }
}

