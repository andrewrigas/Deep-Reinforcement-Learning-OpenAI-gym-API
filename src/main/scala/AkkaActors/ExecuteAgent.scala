package AkkaActors


import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props}
import Agent._
import QLearning.QLearning
import akka.actor.SupervisorStrategy.{Restart, Resume}


//Actor represented by an ActorRef which is basically a pointer
//It has an Ordered mailbox with messages
//Processing asynchronously immutable messages
//One message handled at a time
//Each actor has a unique addresses
//They dont have shared state
//It can Change behaviour at runtime

class ExecuteAgent(trials: Int, episodes: Int) extends Actor {
  //Create Actors counters
  private var number = 0

  override def receive = {
    case agent: Agent=>
      //Create a child Actor
     val childActor = creatChild()
      //Pass msg to the child
      passMSG(childActor,agent)
    case qLearner: QLearning =>
      //Create a child Actor
      val childActor = creatChild()
      //Pass msg to the child
      passMSG(childActor,qLearner)
    case _ => println("Process only agents!!")
  }

  def creatChild() ={
    //Create a child Actor
    val childActor = context.actorOf(Props(new ActorAgent(trials,episodes)),"ChildActor"+ number)
    //Increase number
    number += 1
    //Return child reference
    childActor
  }

  def passMSG(childActor: ActorRef,agent: Any)= {
    //Send ChildActor the agent class
    childActor ! agent
  }

  //Handle Different Exceptions of Children
  override val supervisorStrategy = OneForOneStrategy(loggingEnabled = false) {
    case ae: ArithmeticException => Resume
    case ne: NullPointerException => Resume
    case _: Exception => Restart
  }

}

class ActorAgent(trials: Int, episodes: Int) extends Actor {
  //Child Actor run Agent
  override def receive: Receive = {
    case agent: Agent=> agent.runDeepQN(trials, episodes)
    case qLearner: QLearning => qLearner.runQlearning(trials,episodes)
    case _ => println("Process only Agents and Q-Learners")
  }
}


