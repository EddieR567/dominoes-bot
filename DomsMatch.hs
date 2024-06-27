{- 
   DomsMatch: code to play a dominoes match between two players.
   
   The top level function is domsMatch - it takes five arguments:
       games - the number of games to play
       target - the target score to reach
       player1, player2 - two DomsPlayer functions, representing the two players
       seed - an integer to seed the random number generator
   The function returns a pair showing how many games were won by each player.

   The functions of type DomsPlayer must take four arguments:
       The current Hand
       The current Board
       The Player (which will be one of P1 or P2)
       The current Scores
   The function returns a tuple containing the Domino to play and End to play it on.

   Stub with types provided by Emma Norling (October 2023).
   Full implementation by Edward Ranyard (November 2023).


  simplePlayer and smartPlayer DomsPlayers have been implemented.
  scoreBoard and other functions also implemented to correctly run a domsMatch.


 -}

module DomsMatch where
    import System.Random
    import Data.List
    import Data.Ord (comparing)




    -- types used in this module
    type Domino = (Int, Int) -- a single domino
    {- Board data type: either an empty board (InitState) or the current state as represented by
        * the left-most domino (such that in the tuple (x,y), x represents the left-most pips)
        * the right-most domino (such that in the tuple (x,y), y represents the right-most pips)
        * the history of moves in the round so far
     -}
    data Board = InitState | State Domino Domino History deriving (Eq, Show)
    {- History should contain the *full* list of dominos played so far, from leftmost to
       rightmost, together with which player played that move and when they played it
     -}
    type History = [(Domino, Player, MoveNum)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    {- DomsPlayer is a function that given a Hand, Board, Player and Scores will decide
       which domino to play where. The Player information can be used to "remember" which
       moves in the History of the Board were played by self and which by opponent
     -}
    type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- shuffleDoms: returns a shuffled set of dominoes, given a number generator
       It works by generating a random list of numbers, zipping this list together
       with the ordered set of dominos, sorting the resulting pairs based on the random
       numbers that were generated, then outputting the dominos from the resulting list.
     -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen = [ d | (r,d) <- sort (zip (randoms gen :: [Int]) domSet)]

    {- domsMatch: play a match of n games between two players,
        given a seed for the random number generator
       input: number of games to play, number of dominos in hand at start of each game,
              target score for each game, functions to determine the next move for each
              of the players, seed for random number generator
       output: a pair of integers, indicating the number of games won by each player
     -}
    domsMatch :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    domsMatch games handSize target p1 p2 seed
        = domsGames games p1 p2 (mkStdGen seed) (0, 0)
          where
          domsGames 0 _  _  _   wins               = wins
          domsGames n p1 p2 gen (p1_wins, p2_wins)
            = domsGames (n-1) p1 p2 gen2 updatedScore
              where
              updatedScore
                | playGame handSize target p1 p2 (if odd n then P1 else P2) gen1 == P1 = (p1_wins+1,p2_wins)
                | otherwise                                            = (p1_wins, p2_wins+1)
              (gen1, gen2) = split gen
              {- Note: the line above is how you split a single generator to get two generators.
                 Each generator will produce a different set of pseudo-random numbers, but a given
                 seed will always produce the same sets of random numbers.
               -}

    {- playGame: play a single game (where winner is determined by a player reaching
          target exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame handSize target p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' p1 p2 firstPlayer gen (s1, s2)
            | s1 == target = P1
            | s2 == target = P2
            | otherwise
                = let
                      newScores = playDomsRound handSize target p1 p2 firstPlayer currentG (s1, s2)
                      (currentG, nextG) = split gen
                  in
                  playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

    {- playDomsRound: given the starting hand size, two dominos players, the player to go first,
        the score at the start of the round, and the random number generator, returns the score at
        the end of the round.
        To complete a round, turns are played until either one player reaches the target or both
        players are blocked.
     -}
    playDomsRound :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound handSize target p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitState, scores)
          where
          -- shuffle the dominoes and generate the initial hands
          shuffled = shuffleDoms gen
          hand1 = take handSize shuffled
          hand2 = take handSize (drop handSize shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked -}
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | (score1 == target) || (score2 == target) || (p1_blocked && p2_blocked) = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               =  playDomsRound' p1 p2 P2 newGameState
            | otherwise                =  playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end)          -- get next move from appropriate player
                  | turn == P1 = p1 hand1 board turn (score1, score2)
                  | turn == P2 = p2 hand2 board turn (score1, score2)
                                     -- attempt to play this move
              maybeBoard             -- try to play domino at end as returned by the player
                  | turn == P1 && not (elem domino hand1) = Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (elem domino hand2) = Nothing
                  | otherwise = playDom turn domino board end
              newGameState           -- if successful update board state (exit with error otherwise)
                 | maybeBoard == Nothing = error ("Player " ++ show turn ++ " attempted to play an invalid move.")
                 | otherwise             = (newHand1, newHand2, newBoard,
                                              (limitScore score1 newScore1, limitScore score2 newScore2))
              (newHand1, newHand2)   -- remove the domino that was just played
                 | turn == P1 = (hand1\\[domino], hand2)
                 | turn == P2 = (hand1, hand2\\[domino])
              score = scoreBoard newBoard (newHand1 == [] || newHand2 == [])
              (newScore1, newScore2) -- work out updated scores
                 | turn == P1 = (score1+score,score2)
                 | otherwise  = (score1,score2+score)
              limitScore old new     -- make sure new score doesn't exceed target
                 | new > target = old
                 | otherwise    = new
              Just newBoard = maybeBoard -- extract the new board from the Maybe type
   
{-scoreBoard: given the current board and Boolean isLastDomino, returns the score scored by the player
 at the current turn. Calls on countPips to determine how many pips are on either side.-}
    scoreBoard :: Board -> Bool -> Int
    scoreBoard InitState isLastDomino = 0 + if isLastDomino then 1 else 0
    scoreBoard (State (ll, lr) (rl, rr) _) isLastDomino
      | (mod pipCount 3 == 0) && (mod pipCount 5 == 0) = div pipCount 3 + div pipCount 5 + bonus
      | mod pipCount 5 == 0 = div pipCount 5 + bonus
      | mod pipCount 3 == 0 = div pipCount 3 + bonus
      | otherwise = bonus
      where
        pipCount = countPips (State (ll, lr) (rl, rr) [])
        bonus = if isLastDomino then 1 else 0

{-countPips: given a board, this function calculates how many pips are to be counted either side 
(sum of left and right as well as any doubles) it calls on isDouble to determine any double dominos.-}
    countPips :: Board -> Int
    countPips InitState = 0
    countPips (State (ll, lr) (rl, rr) _)
      = ll + rr + isDouble (ll,lr) + isDouble (rl,rr)

{-isDouble: Given a domino, returns the number of pips to be added from a double domino, or 0 if not a double.-}
    isDouble :: (Int, Int) -> Int
    isDouble (l, r)
      | l == r = r
      | otherwise = 0

{-blocked: given a hand and a board, checks if a player is blocked from playing and returns True or False depending.
   Calls boolean function canPlayHand to check each end separately.-}
    blocked :: Hand -> Board -> Bool
    blocked _ InitState = False
    blocked hand (State lDom rDom _)
     | canPlayHand hand lDom L = False
     | canPlayHand hand rDom R = False
     | otherwise = True

{-canPlayHand: given a hand, board and an end, checks if any domino in the hand can be played at the 
  board end specified. Returns True if possible.-}
    canPlayHand :: Hand -> Domino -> End -> Bool
    canPlayHand hand (lPip, rPip) end
      | any (\(x, y) -> x == lPip || y == lPip ) hand && end == L = True
      | any (\(x, y) -> x == rPip || y == rPip ) hand && end == R = True
      | otherwise = False

{-playDom: given a the player number, domino to play, board on which to play it, and the end, 
returns a maybe board of the updated board state. Calls on functions canPlay and canPlayReverse to check 
which way round to play the domino, and calls reverseDom in the latter case to swap the (x,y) values of the domino. 
Also updates the history to show the domino played, who played it, and which turn.-}
    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom player (l,r) InitState L = Just (State (l,0) (0,r) [((l,r), player, 1)])
    playDom player (l,r) InitState R = Just (State (l,0) (0,r) [((l,r), player, 1)])
    playDom player domino (State leftDomino rightDomino history) end
        | end == L && canPlay domino leftDomino end = Just (State domino rightDomino updatedHistory)
        | end == L && canPlayReverse domino leftDomino end = Just (State (reverseDom domino) rightDomino updatedHistoryReverse)
        | end == R && canPlay domino rightDomino end  = Just (State leftDomino domino updatedHistory)
        | end == R && canPlayReverse domino rightDomino end  = Just (State leftDomino (reverseDom domino) updatedHistoryReverse)
        | otherwise = Nothing
      where
        updatedHistory = (domino, player, length history + 1) : history
        updatedHistoryReverse = (reverseDom domino, player, length history + 1) : history
      

{-canPlay: takes a domino to be played and a domino on the board (at the specified end) and returns true if that domino
  in that configuration can be played on the other.-}
    canPlay :: Domino -> Domino -> End -> Bool
    canPlay(l1, r1) (l2, r2) end
      | end == L && r1 == l2  = True
      | end == R && l1 == r2 = True
      | otherwise = False

{-canPlayReverse: Same as previous, but with the pips swapped around on the domino to be played.-}
    canPlayReverse :: Domino -> Domino -> End -> Bool
    canPlayReverse(l1, r1) (l2, r2) end
      | end == L &&  l1 == l2 = True
      | end == R &&  r1 == r2  = True
      | otherwise = False

{-reverseDom: given a domino, swaps the ends, returns reversed domino-}
    reverseDom :: Domino -> Domino
    reverseDom (x,y) = (y,x)

{-simplePlayer: this domsPlayer will call possPlays to find which dominos can be played at which end, and returns 
 the first domino in the list of end L, unless there are none to be placed on the left, in which case it will
 return the first domino in the list of R.-}
    simplePlayer :: DomsPlayer
    simplePlayer hand board _ _
      | not (null leftPoss) = (head leftPoss, L)
      | not (null rightPoss) = (head rightPoss, R)
      | otherwise = error "No Possible Dominos" -- Theoretically this should never occur
      where
        (leftPoss, rightPoss) = possPlays hand board

{-possPlays: given a hand and a board, return 2 lists, one of possible dominos in the hand to be played
on the left hand side of the board, and one for the right. Calls canPlay and canPlayReverse to check 
what can be played, and filters accordingly.-}
    possPlays :: Hand -> Board -> ([Domino], [Domino])
    possPlays hand InitState = (hand, hand) -- returns all doms in the left and right lists
    possPlays hand (State lDom rDom _)
      | not (null hand) = (lPoss,rPoss)
      | otherwise = ([], [])
      where
        lPoss = filter (\dom -> canPlay dom lDom L || canPlayReverse dom lDom L) hand
        rPoss = filter (\dom -> canPlay dom rDom R || canPlayReverse dom rDom R) hand

{-smartPlayer: this domsPlayer takes the current hand and board, if board is empty will call maxFirstDrop to 
  find the best starting domino, otherwise will call findMaxPoss to find the highest scoring domino 
  from all playable dominos in the hand. Playable domino lists are found by calling possPlays and concatenating 
  leftPoss and rightPoss -}
    smartPlayer :: DomsPlayer
    smartPlayer hand InitState _ _ = maxFirstDrop hand
    smartPlayer hand board _ _
      | not (null leftPoss) ||  not (null rightPoss) = findMaxPoss board (leftPoss ++ rightPoss) hand 
      | otherwise = error "No Possible Dominos"
      where
        (leftPoss, rightPoss) = possPlays hand board

{-findMaxPoss: given a board, a list of playable dominos from the hand, and the full hand, firstly calls findMaxList 
to return the highest scoring domino, and in case there are multiple playable dominos with the same max score, filter 
further by calling canBeKnockedOff to return the highest scoring domino which the player can play on top of. If none can 
be played on top of, find the next highest scoring domino which can be played on top of.-}
    findMaxPoss :: Board -> [Domino] -> Hand  -> (Domino, End)
    findMaxPoss board dominos hand 
      | not (null filteredList) = head filteredList
      | not (null filteredList2) = head filteredList2
      | otherwise = head maxDomEndPairs
      where
        maxDomEndPairs = findMaxList board dominos
        filteredList = canBeKnockedOff board hand maxDomEndPairs
        exclusionList = map fst maxDomEndPairs
        filteredList2 = canBeKnockedOff board hand (findMaxList board (filter (`notElem` exclusionList) dominos))

{-findMaxList: given a board and a list of playable dominos, calls calcPossScore to calculates individually 
the possible score each domino would gain, and returns a list of the dominos with the highest score.-}
    findMaxList :: Board -> [Domino] -> [(Domino, End)]
    findMaxList board dominos = maxDomEndPairs
      where 
        maxScore = maximum (map (fst . calcPossScore board) dominos)
        maxDoms = filter (\dom -> fst (calcPossScore board dom) == maxScore) dominos
        ends = map (snd . calcPossScore board) maxDoms
        maxDomEndPairs = zip maxDoms ends

{-canBeKnockedOff: given the board, hand, and the highest scoring (domino,end) pairs, calls the blocked function 
to check if the current hand (minus the domino in question) can be played on the board next if this domino is played.
  Returns a list of (domino,end) pairs.-}
    canBeKnockedOff :: Board -> Hand -> [(Domino, End)] -> [(Domino, End)]
    canBeKnockedOff (State (lBoard, _) (_, rBoard) _) hand domEndList =
      filter (\((lDom, rDom), end) ->
                case end of
                  L -> not (blocked (filter (/= (lDom,rDom)) hand) (State (lDom,0)(0,rBoard)[]))
                  R -> not (blocked (filter (/= (lDom,rDom)) hand) (State (lBoard,0)(0,rDom)[]))) domEndList

{-maxFirstDrop: given a hand, checks if it contains (4,5) and returns it if so, otherwise
   calculates the maximum possible score and returns it.-}
    maxFirstDrop :: Hand -> (Domino,End)
    maxFirstDrop hand 
      | elem (5,4) hand || elem (4,5) hand = ((5,4),L)
      | otherwise = (max, L)
      where max = maximumBy (comparing (fst . calcPossScore InitState)) hand

{-calcPossScore: given a board and a domino, calculates what score this domino would achieve if placed by calling 
  scoreBoard, canPlay and canPlayReverse functions, alongside the end at which it should be placed. -}
    calcPossScore :: Board -> Domino -> (Int,End)
    calcPossScore InitState (left,right) = (scoreBoard (State (left,0) (0,right) []) False,L)
    calcPossScore (State (ll,lr) (rl,rr) history) dom
      | canPlay dom (ll,lr) L = (scoreBoard (State dom (rl,rr) history) False,L)
      | canPlayReverse dom (ll,lr) L = (scoreBoard (State (reverseDom dom) (rl,rr) history) False,L)
      | canPlay dom (rl,rr) R = (scoreBoard (State (ll,lr) dom history) False,R)
      | canPlayReverse dom (rl,rr) R = (scoreBoard (State (ll,lr) (reverseDom dom) history) False,R)
      | otherwise = (0,L)
    
    



