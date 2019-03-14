package QLearning

import OpenAI.gym._
import scala.util.Random

class QLearning(trials: Int = 100,episodes: Int = 1000, gamma: Double = 0.9, learning_rate: Double = 0.01, epsilon: Int = 90) {

  type qValue = Double
  type Action = Int
  type Reward = Double
  // State is a subtype of Observation
  type State = Observation
  type NextState = Observation

  //Q-Values Table Key (State and Action) => Values (NextState, Reward)
  type QValues = Map[(State,Action), (NextState,qValue)]


  def TrialsClassic(env: ClassicControlEnvironment,qValues: QValues,trial: Int = 1): QValues = {
    if(trial >= trials){
      qValues
    }else {
      val newQValues = EpisodesClassic(env,qValues)
      TrialsClassic(env,newQValues,trial+1)
    }

  }


  def EpisodesClassic(env: ClassicControlEnvironment,qValues: QValues,episode: Int = 1): QValues = {


    def qLearning(currState: ObservationClassicControl,qValues: QValues,done: Boolean): QValues = {
      if (done) {
        return qValues
      } else {
        //Get an action using E greedy method
        val action = E_greedyClassic(env,qValues,currState)
        // Make an action to the environment
        val reply = env.step(action)
        //Get the next state
        val nextState = ObservationClassicControl(reply.observation)

        val newQValues = updateQValues(env,reply.reward,currState,nextState,action,qValues)
        //Set the next State as the current State

        //Update the value done if the game is over
        qLearning(nextState, newQValues,reply.done)
      }
    }

    if(episode >= episodes) {
      qValues
    } else {

      val currState = env.reset()
      val q = qLearning(currState,qValues,false)

      EpisodesClassic(env,q,episode+1)
    }
    
  }


    def E_greedyClassic(env: ClassicControlEnvironment,qValues: QValues,currState: ObservationClassicControl): Action = {

    if(Random.nextInt(100) > epsilon){
      //Explore take random Action
      env.action_space().sample()
    }else {
      val rewards: Seq[((NextState, qValue), Action)] = getQValues(env,qValues,currState)

      if(rewards.isEmpty){
        //If is the first time then take a random Action
        env.action_space().sample()
      }else{
        //Max by qValue
        rewards.maxBy(_._1._2)._2
      }
    }
  }

  def getQValues(env: ClassicControlEnvironment,qValues: QValues, currState: ObservationClassicControl): Seq[((NextState, qValue), Action)] = {
    val someRewards: Seq[(Option[(NextState, qValue)], Action)] = for(i <- 1 to env.action_space().info.n ) yield (qValues.get((currState,i)), i)
    someRewards.filter(x => x._1.isDefined).map(t => (t._1.get,t._2))
  }

  def updateQValues(env:ClassicControlEnvironment,reward: Reward,currState: ObservationClassicControl,nextState: ObservationClassicControl,action: Action,qValues: QValues): QValues ={
    val rewards = getQValues(env,qValues,nextState)

    val nextStateMaxQValue = rewards match {
      case Nil => 0.0
      case x => x.maxBy(_._1._2)._1._2
    }

    val currStateQValue = qValues.get(currState,action) match {
      case Some(value) => value._2
      case None => 0.0
    }

    val newQvalue =  currStateQValue + learning_rate * ( reward + gamma * nextStateMaxQValue -  currStateQValue)

    qValues.updated((currState,action),(nextState,newQvalue))
  }




  def runQlearning(env: Environment): QValues = {
    // Create a private QValues Map
    val QValues: Map[(State,Action), (NextState,Reward)] = Map()
    //Pattern Matching
    env match {
      case ClassicControlEnvironment(i) => TrialsClassic(ClassicControlEnvironment(i),QValues)
    }
  }

}
