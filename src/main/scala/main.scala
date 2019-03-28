

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
  val inputLayer = new Layer(3,true,"relu")
  //Hidden Layer
  val hiddenLayer = new Layer(2,true,"sigmoid")
  //Output Layer
  val outputLayer = new Layer(3)


  //Add to our model each layer
  model.addToModel(inputLayer)
  model.addToModel(hiddenLayer)
  model.addToModel(outputLayer)

  val x1 =  DenseMatrix.rand[Double](4,1) + 2.0
  val x2 =  DenseMatrix.rand[Double](2,4) + 1.4
  val b = x2 * x1

  val g = sum(x2(::,*))

  val d = DenseMatrix.zeros[Double](2,4)

  val bia = where(x2 >:> d,x2,d)

  val f = sigmoid(x1)
  val s = pow(x1,2)
  println(x1)
  println(x2)
  println(b)
  println(bia)
  println(d)
  println(f)
  println(s)
  println(g)
  println(model)



//  gym.shutDown() //Drop Python Server API
//  gymClient.terminate // Terminate Actor System
}
