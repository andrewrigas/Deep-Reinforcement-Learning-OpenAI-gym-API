package QLearning


import OpenAI.gym._
import scala.util.Random

case class QLearning(env: Environment) {

  //Rename Data types Variables
  type qValue = Double
  type Action = Int
  type Reward = Double

  //State is a subtype of Observation
  type State = Observation
  type NextState = Observation

  //Q-Values Table Key (State and Action) => Values qValue
  type QValues = Map[(State,Action), qValue]


  //Discount Rate
  val gamma: Double = 0.9
  //Learning Rate
  val learning_rate: Double = 0.01
  //Explore Rate
  val epsilon: Int = 60
  //Epsilon Min to Explore
  val epsilonMin: Int = 10


  private def Trials(qValues: QValues,episodes: Int,trials: Int,trial: Int = 0): QValues = {
    //Check trial if is larger than trials
    if(trial >= trials){
      //return Q-values table
      qValues
    }else {
      //Execute Episodes get new Q-Values Table
      val newQValues = Episodes(qValues,episodes)
      //Recall Trials
      Trials(newQValues,episodes,trials,trial+1)
    }
  }

  private def Episodes(qValues: QValues,episodes: Int,episode: Int = 1): QValues = {

    def qLearning(currState: Observation,qValues: QValues,done: Boolean): QValues = {
      if (done) {
        qValues
      } else {
        //Get an action using E greedy method
        val action = E_greedy(qValues,currState)
        // Make an action to the environment
        val reply = env.step(action,true)
        //Get the next state

        //Set the next State as the current State
        val nextState = Observation(reply.observation)
        //Get newQValue
        val newQValues = updateQValues(reply.reward,currState,nextState,action,qValues)
        //Update the value done if the game is over
        qLearning(nextState, newQValues,reply.done)
      }
    }

    //Check if episode is larger then episodes
    if(episode >= episodes) {
      //Return Q-values Table
      qValues
    } else {
      //Reset and get observation
      val currState = env.reset()
      //Get new W-values Table
      val newQValues = qLearning(currState,qValues,false)
      //Recall Episode Function
      Episodes(newQValues,episode+1)
    }

  }


  private def E_greedy(qValues: QValues,currState: Observation): Action = {

    if(Random.nextInt(100) <= epsilon){
      //Explore take random Action
      env.action_space().sample()
    }else {
      val rewards = getQValues(qValues,currState)

      if(rewards.isEmpty){
        //If is the first time then take a random Action
        env.action_space().sample()
      }else{
        //Max by qValue
        rewards.maxBy(_._1)._2
      }
    }
  }

  private def getQValues(qValues: QValues, currState: Observation): Seq[(qValue,Action)] = {
    //Some rewards
    val someRewards: Seq[(Option[qValue], Action)] = for(i <- 1 to env.action_space().info.n ) yield (qValues.get((currState,i)), i)
    someRewards.filter(x => x._1.isDefined).map(t => (t._1.get,t._2))
  }

  private def updateQValues(reward: Reward,currState: Observation,nextState: Observation,action: Action,qValues: QValues): QValues = {

    val rewards = getQValues(qValues,nextState)

    //Pattern Matching check if is Empty return 0 or the max value
    val nextStateMaxQValue = rewards match {
      case Nil => 0.0
      case x => x.maxBy(_._1)._1
    }

    //Get Monad
    val currStateQValue = qValues.get(currState,action) match {
      case Some(value) => value
      case None => 0.0
    }

    //Updated QValue calculation
    val newQvalue =  currStateQValue + learning_rate * ( reward + gamma * nextStateMaxQValue -  currStateQValue)

    //Update value in table and return
    qValues.updated((currState,action),newQvalue)
  }


  def runQlearning(trials: Int = 100,episodes: Int = 1000): QValues = {
    // Create a private QValues Map
    val qValues: QValues= Map()
    //Pattern Matching
    Trials(qValues,episodes,trials)
  }



}
