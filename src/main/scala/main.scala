

import java.io.File

import Agent.Agent
import OpenAI.gym._
import akka.actor.Props
import AkkaActors.ExecuteAgent
import QLearning.QLearning

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql._

object main extends App{

  //Create or get a SparkSession instance
  implicit val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Total cost")
      .config("spark.master", "local")
      .getOrCreate()


  //Create an Atari Environment
  val atariEnv = gym.make("Boxing-v0")

  //Create a Classic Control Environment
  val classicControlEnv = gym.make("Acrobot-v1")

  //Get Akka actor System
  val system = gymClient.system

  //Atari Actor
  val atariActor = system.actorOf(Props(new ExecuteAgent(100,5000)),"AtariActor")

  //Classic Control Actor
  val classicControlActor = system.actorOf(Props(new ExecuteAgent(1000,5000)),"ClassicControlActor")

  //Create an Atari Agent
  val atariAgent = Agent(atariEnv)

  //Create a Classic Control Agent
  val classicControlAgent = Agent(classicControlEnv)

  //Bang AtariAgent
  atariActor ! atariAgent

  //Bang ClassicControlAgent
  classicControlActor ! classicControlAgent

   //gym.shutDown() //Drop Python Server API
   //system.terminate // Terminate Actor System
}
