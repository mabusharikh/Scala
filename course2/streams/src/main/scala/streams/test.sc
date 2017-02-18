import streams.{GameDef, Solver, StringParserTerrain}

class SolutionChecker extends GameDef with Solver with StringParserTerrain {

  val level =
    """ooo-------
    |oSoooo----
    |ooooooooo-
    |-ooooooooo
    |-----ooToo
    |------ooo-""".stripMargin

  }

val a = new SolutionChecker

(a.pathsFromStart take 100).toList

