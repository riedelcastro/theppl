package com.github.riedelcastro.theppl.parse

object InferCLIParse {

  val usage = """
    Usage: infer -c -i univ-out.mln -e univ-test.db -r univ.results -q advisedBy;student;professor -c -maxSteps 20000
              """
  type OptionMap = Map[Symbol, Any]

  def nextOption(map: OptionMap, list: List[String]): OptionMap = {

    def isSwitch(s: String) = (s(0) == '-')
    list match {
      case Nil => map
      case "-i" :: value :: tail =>
        nextOption(map ++ Map('input -> value), tail)
      case "-o" :: value :: tail =>
        nextOption(map ++ Map('output -> value), tail)
      case "-r" :: value :: tail =>
        nextOption(map ++ Map('result -> value), tail)
      case "-e" :: value :: tail =>
        nextOption(map ++ Map('evidence -> value), tail)
      case "-q" :: value :: tail =>
        nextOption(map ++ Map('query -> value), tail)
      case "-ma" :: tail =>
        nextOption(map ++ Map('ma -> "ma"), tail)
      case "-maxSteps" :: value :: tail =>
        nextOption(map ++ Map('maxsize -> value.toInt), tail)
      case string :: opt2 :: tail if isSwitch(opt2) =>
        nextOption(map ++ Map('flag -> string), list.tail)
      case string :: Nil => nextOption(map ++ Map('infile -> string), list.tail)
      case option :: tail => throw new Error("Unknown option " + option)
      //todo: more options comming!
    }

  }

  def main(args: Array[String]) {
    val arglist = "-ma -i univ-out.mln -e univ-test.db -r univ.results -q advisedBy;student;professor -c -maxSteps 20000".split(" ").toList
    val options = nextOption(Map(), arglist)
    println(options)
    println(options.get('input).get)

    val arglist1 = "-i univ-out.mln -e univ-test.db -r univ.results -q AdvisedBy".split(" ").toList
    val options1 = nextOption(Map(), arglist1)
    println(options1)
  }
}


