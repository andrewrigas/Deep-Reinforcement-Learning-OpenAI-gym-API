

import Agent.Agent
import OpenAI.gym._
import QLearning.QLearning



object main extends App{

  val env = gym.make("CartPole-v1")

  val state_size = env.observation_space().info.shape
  val action_size = env.action_space().info.n

  env.reset()
  val agent = new Agent(state_size,action_size)

  val qlearner = new QLearning(100,1000,0.9,0.01,90)

  var step: StepReply = env.step(env.action_space().sample())
  for(i <- 1 to 10000){
    val done = step match {
      case StepReplyAtari(o,r,d,i) => (d,r)
      case StepReplyClassicControl(o,r,d,i) => (d,r)
    }
    println(done._2)
    if(done._1){
      env.reset()
    }else{
      step = env.step(env.action_space().sample())
    }
  }
  gym.shutDown() //Drop Python Server API
  gymClient.terminate // Terminate Actor System
}
