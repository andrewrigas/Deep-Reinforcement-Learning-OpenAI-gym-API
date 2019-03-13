package OpenAI.gym

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.unmarshalling.Unmarshal
import gymClient.requestToApi
import spray.json.DeserializationException

import scala.concurrent.duration._
import scala.concurrent.Await
import OpenAI.gym.gymClient.materializer
import spire.syntax.action


abstract class Environment(val instance: EnvInstance) {

  //Action Space Response, Return an ActionSpace Object
  def action_space(): ActionSpace ={
    val request = actionSpace(instance.instance_id)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ActionSpace], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }
  }


  //Observation Space Response , Return an Observation Space Object
  def observation_space(): ObservationSpace = {

    val request = obsSpace(instance.instance_id)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ObservationSpace], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }
  }




  //Abstract Method Reset
  def reset(): Observation

  //Abstract Method step
  def step(action: Int, render: Boolean = true): StepReply

}

case class AtariEnvironment(override val instance: EnvInstance) extends Environment(instance) {



  override def reset(): ObservationAtari = {
    val request = resetEnv(instance.instance_id)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ObservationAtari], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }


  }

  override def step(action: Int, render: Boolean = true): StepReplyAtari = {

    val request = stepEnv(instance.instance_id,action,render)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[StepReplyAtari], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }
  }

}

case class ClassicControlEnvironment(override val instance: EnvInstance) extends Environment(instance) {


  override def reset(): ObservationClassicControl= {
    val request = resetEnv(instance.instance_id)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ObservationClassicControl], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }


  }

  override def step(action: Int, render: Boolean = true): StepReplyClassicControl = {

    val request = stepEnv(instance.instance_id,action,render)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[StepReplyClassicControl], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }
  }

}
