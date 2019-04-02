

import Agent.Agent
import OpenAI.gym._
import QLearning.QLearning
import NeuralNetwork._
import breeze.linalg._
import breeze.numerics._
import scala.concurrent.duration.Duration

import scala.concurrent.{Await,Future}

object main extends App{

//  val env = gym.make("CartPole-v1")
//
//  val state_size = env.observation_space().info.shape
//
//  val action_size = env.action_space().info.n
//
//  env.reset()
//
//  val agent = new Agent(state_size,action_size)
//
//  val qlearner = new QLearning(10,100,0.9,0.01,90)
//
//  qlearner.runQlearning(env)

  val model = new Model

  //Input Layer
  val inputLayer = Layer(3,true,"relu")
  //Hidden Layer
  val hidden1Layer = Layer(2,true)

  val hidden2Layer = Layer(2,true)
  //Output Layer
  val outputLayer = Layer(3)


  //Add to our model each layer
  model.addToModel(inputLayer)
  model.addToModel(hidden1Layer)
  model.addToModel(hidden2Layer)
  model.addToModel(outputLayer)

  model.buildModel()

  model.getWeights.foreach(x => println(x))
  println(model)
  model.fit(DenseMatrix.rand[Double](3,1),DenseMatrix.rand[Double](3,1))



//  gym.shutDown() //Drop Python Server API
//  gymClient.terminate // Terminate Actor System
}
