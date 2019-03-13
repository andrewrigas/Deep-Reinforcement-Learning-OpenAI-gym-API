

import Agent.Agent
import OpenAI.gym._
import QLearning.QLearning



object main extends App{

  val env = gym.make("Assault-v0")

  val state_size = env.observation_space().info.shape
  val action_size = env.action_space().info.n

  val agent = new Agent(state_size,action_size)

  val qlearner = new QLearning(100,1000,0.9,0.01,90)

  gym.shutDown() //Drop Python Server API
  gymClient.terminate // Terminate Actor System
}
