package QLearning

import OpenAI.gym._
import scala.util.Random

class QLearning(trials: Int = 100,episodes: Int = 1000, gamma: Double = 0.9, learning_rate: Double = 0.01, epsilon: Int = 10) {

  type qValue = Double
  type Action = Int
  type Reward = Double
  // State is a subtype of Observation
  type State = Observation
  type NextState = Observation

  //Q-Values Table Key (State and Action) => Values qValue
  type QValues = Map[(State,Action), qValue]


  def Trials(env: Environment,qValues: QValues,trial: Int = 1): QValues = {
    if(trial >= trials){
      qValues
    }else {
      val newQValues = Episodes(env,qValues)
      Trials(env,newQValues,trial+1)
    }
  }

  def Episodes(env: Environment,qValues: QValues,episode: Int = 1): QValues = {

    def qLearning(currState: Observation,qValues: QValues,done: Boolean): QValues = {
      if (done) {
        println("Done")
        return qValues
      } else {
        //Get an action using E greedy method
        val action = E_greedy(env,qValues,currState)

        // Make an action to the environment
        val reply = env.step(action,false)



        val (observe,reward,end,info) = reply match {
          case StepReplyClassicControl(o,r,d,i) => (o,r,d,i)
          case StepReplyAtari(o,r,d,i) => (o,r,d,i)
        }

        val observation = env match {
          case AtariEnvironment(i) => observe match {
            case atari: List[List[(Double,Double,Double)]] => ObservationAtari(atari)
          }
          case ClassicControlEnvironment(i) => observe match {
            case classic: List[Double] => ObservationClassicControl(classic)
          }
        }
        //Get the next state
        val nextState = observation

        val newQValues = updateQValues(env,reward,currState,nextState,action,qValues)
        //Set the next State as the current State

        newQValues.foreach(x => println(x._2))
        //Update the value done if the game is over
        qLearning(nextState, newQValues,end)
      }
    }

    if(episode >= episodes) {
      qValues
    } else {

      val currState = env.reset()
      val newQValues = qLearning(currState,qValues,false)

      Episodes(env,newQValues,episode+1)
    }

  }


  def E_greedy(env: Environment,qValues: QValues,currState: Observation): Action = {

    if(Random.nextInt(100) <= epsilon){
      //Explore take random Action
      env.action_space().sample()
    }else {
      val rewards = getQValues(env,qValues,currState)

      if(rewards.isEmpty){
        //If is the first time then take a random Action
        env.action_space().sample()
      }else{
        //Max by qValue
        rewards.maxBy(_._1)._2
      }
    }
  }

  def getQValues(env: Environment,qValues: QValues, currState: Observation): Seq[(qValue,Action)] = {
    val someRewards: Seq[(Option[qValue], Action)] = for(i <- 1 to env.action_space().info.n ) yield (qValues.get((currState,i)), i)
    someRewards.filter(x => x._1.isDefined).map(t => (t._1.get,t._2))
  }

  def updateQValues(env: Environment,reward: Reward,currState: Observation,nextState: Observation,action: Action,qValues: QValues): QValues ={
    val rewards = getQValues(env,qValues,nextState)

    val nextStateMaxQValue = rewards match {
      case Nil => 0.0
      case x => x.maxBy(_._1)._1
    }

    val currStateQValue = qValues.get(currState,action) match {
      case Some(value) => value
      case None => 0.0
    }

    val newQvalue =  currStateQValue + learning_rate * ( reward + gamma * nextStateMaxQValue -  currStateQValue)

    qValues.updated((currState,action),newQvalue)
  }


  def runQlearning(env: Environment): QValues = {
    // Create a private QValues Map
    val qValues: QValues= Map()
    //Pattern Matching
    Trials(env,qValues)
  }



}
