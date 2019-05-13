

import java.io.File

import Agent.Agent
import OpenAI.gym._
import akka.actor.Props
import AkkaActors.ExecuteAgent
import NeuralNetwork.{Layer, Model}
import QLearning.QLearning
import org.apache.spark.sql.SparkSession


object main extends App{

  //Create or get a SparkSession instance make it implicit to pass to all needed functions
  implicit val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Total cost")
      .config("spark.master", "local")
      .getOrCreate()


  //Create an Atari Environment
  val atariEnv = gym.make("Boxing-v0")

  //Create a Classic Control Environment
  val classicControlEnv1 = gym.make("Acrobot-v1")

  //Create a Classic Control Environment
  val classicControlEnv2 = gym.make("CartPole-v1")



  //Get Akka actor System
  val system = gymClient.system

  //Atari Actor
  val atariActor = system.actorOf(Props(new ExecuteAgent(1000,5000)),"AtariActor")

  //Classic Control Actor1
  val classicControlActor1 = system.actorOf(Props(new ExecuteAgent(1000,5000)),"ClassicControlActor1")
  //Classic Control Actor2
  val classicControlActor2 = system.actorOf(Props(new ExecuteAgent(1000,5000)),"ClassicControlActor2")


  //Create an Atari Agent
  val atariAgent = Agent(atariEnv)

  //Create a Classic Control Agent
  val classicControlAgent = Agent(classicControlEnv1)

  //Create a Q-Learnig agent
  val qAgent = QLearning(classicControlEnv2)

  //Bang AtariAgent
  atariActor ! atariAgent

  //Bang ClassicControlAgent1
  classicControlActor1 ! classicControlAgent

  //Bang ClassicControlAgent2
  classicControlActor2 ! qAgent

   //gym.shutDown() //Drop Python Server API
   //system.terminate // Terminate Actor System

}
