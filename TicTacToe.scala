  // flags
  var finished = false
  val replacement = false 
  val ai = false

  // generate the board
  val a = Array(0,1,2)
  val board = a.map(_ => Array(0,0,0))

  /** main game loop */
  while(!finished){
    
    showBoard
    println("Player 1")
    val move = getInput
    
    if(wins(move._1,move._2, 1)){
      println("You Won, Player 1!")
      finished = true
    }
    updateBoard(move,1)
    showBoard

    if(!finished){
      val move = if (!ai) moveOther else moveHal
      if(wins(move._1,move._2, 2)){
        println("You suck, Player 1!")
        finished = true
      }
      updateBoard(move,2)
      showBoard
    }
   
    if(!finished && boardIsFull){
      println("There was a tie!")
      finished = true
    }

  }

  /** Get move from player as Tuple2 
    *
    * prompt player for input and check
    * if inside board and free (if replacement
    * is set to false) */
  def getInput(): (Int, Int)= {
    var r = -1
    var c = -1
    var valid = false;
    var taken = false;

    while(!valid || (!replacement && taken)){ // only accept a valid cell. If replacement is disabled, ensure it is free
      println("Please enter row and column")
      println("Row:")
      r = readInt-1
      println("Column")
      c = readInt-1
      valid = ((a contains r) && (a contains c))
      taken = board(r)(c)!=0

      if(!replacement && taken) println("That cell is taken, choose another one")
      if(!valid) println("Those are not valid")
    }
    (r,c)
  }


  /** Check if given move (r,c)  wins the game for the given player */
  def wins(r:Int,c:Int,player:Int):Boolean = {

    lazy val otherXs = a.filter(_!=r) // rows not occupied by this move
    lazy val otherYs = a.filter(_!=c) // cols not occupied by this move
    lazy val colFull = otherXs.forall( // victory condition 1: are all other Xs...
      other => board(other)(c)==player)  // ...in this column taken by this player? 
    lazy val lineFull  = otherYs.forall(other => board(r)(other)==player) // VC 2: same thing, now for this row
    lazy val diagFull = (r==c) && // VC 3: check only if this cell is in a diagonal 
      (otherXs.forall(other => board(other)(other)== player) // VC for diagonal 1: cells with the same r and c
        || (otherXs.forall(other => board(other)(2-other) ==player))) // VC for diagonal 2: cells whose r & c add to 2
    
    colFull || lineFull || diagFull //check and return if any of the victory conditions is met
  }

  /** get move from human player 2 */
  def moveOther():(Int,Int)= {
    println("Player2")
    getInput
  }

  /** get a move from HAL, our friendly AI
    *
    * Work in progres
    */
  def moveHal():(Int,Int)= {
    println("I will crush you now!")
    
 //   for 
 //     r <-a;c <- a 
 //     val   possible = (r,c)
 //    match wins(move)==true =
    
 //   } yield move


    (0,0)
  }

  /** record move to board */
  def updateBoard (move:(Int,Int),player:Int){
    board(move._1)(move._2)= player
  }

  /** self expl */
  def boardIsFull() = {
    board.flatten.forall(_!=0)  
  }

  /** shows a graphical (we've got a GUI!) repr of the board
    * 
    * It would be nice if this used pattern matching, to be
    * more Scala-y
    */
  def showBoard(){
    print("\n\n\n")
    for(i <- 0 to 12; j <- 0 to 13) {
      val row = i/4
      val col = j/4
      val centerTile = (i-2)%4==0 && (j-2)%4==0
      if(j==13){
        print("\n")
      }else if  (i%4 ==0 || j%4 ==0) {
        print ("#")
      }else if (centerTile && board(row)(col) == 1){
        print("X")
      }else if (centerTile &&  board(row)(col) == 2){  
        print("O")
      }else print(" ")

    }
  
  }


