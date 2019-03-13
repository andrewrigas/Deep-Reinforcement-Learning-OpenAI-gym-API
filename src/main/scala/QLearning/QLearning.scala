package QLearning

import OpenAI.gym._
import scala.util.Random

class QLearning(trials: Int = 100,episodes: Int = 1000, gamma: Double = 0.9, learning_rate: Double = 0.01, epsilon: Int = 90) {

  type Reward = Double
  type Action = Int

  // State is a subtype of Observation
  type State = Observation

  // Create a private QValues Map
  private var QValues: Map[(State,Action), Reward] = Map()

  def TrialsClassic(env: ClassicControlEnvironment) ={

    for(i <- 0 to trials){
      val observation = env.reset()
      EpisodesClassic(env,observation)

    }
  }


  def EpisodesClassic(env: ClassicControlEnvironment,observation: ObservationClassicControl) ={

    var currentState: ObservationClassicControl = observation


    for(i <- 0 to episodes){

      def qLearning(done: Boolean){
        if(done){
          return
        }else {
          //Get an action using E greedy method
          val action = E_greedyClassic(env,observation)
          // Make an action to the environment
          val reply = env.step(action)
          //Get the next state
          val nextState = ObservationClassicControl(reply.observation)
          //Set the next State as the current State
          currentState = nextState
          //Update the value done if the game is over
          qLearning(reply.done)
        }
      }
    }
  }


  def E_greedyClassic(env: ClassicControlEnvironment,currState: ObservationClassicControl): Action = {

    if(Random.nextInt(100) > epsilon){
      //Explore take random Action
      env.action_space().sample()
    }else {
      val someRewards: Seq[(Option[Reward], Action)] = for(i <- 1 to env.action_space().info.n ) yield (QValues.get((currState,i)), i)

      val rewards: Seq[(Reward, Action)] = someRewards.filter(x => x._1.isDefined).map(t => (t._1.get,t._2))

      if(rewards.isEmpty){
        //If is the first time then take a random Action
        env.action_space().sample()
      }else{
        //Max by reward
        rewards.maxBy(_._1)
      }
    }
  }

  def updateQValues(): Unit ={

  }

  def TrialsAtari(env: AtariEnvironment) ={

    for(i <- 0 to trials){
      EpisodesAtari(env)
      env.reset()
    }
  }


  def EpisodesAtari(env: AtariEnvironment) ={

    def episodes(i: Int) ={

    }


  }


  def E_greedyAtari(env: AtariEnvironment) = {

  }


  def runQlearning(env: Environment, QValues: Map[(Observation,Action),Reward]) ={
    env match {
      case AtariEnvironment(i) => TrialsAtari(AtariEnvironment(i))
      case ClassicControlEnvironment(i) => TrialsClassic(ClassicControlEnvironment(i))
    }
  }

}
