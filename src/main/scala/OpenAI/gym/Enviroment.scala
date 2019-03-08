package OpenAI.gym

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.unmarshalling.Unmarshal
import gymClient.requestToApi

import scala.concurrent.duration._
import scala.concurrent.Await
import OpenAI.gym.gymClient.materializer


case class Environment(implicit val instance: EnvInstance) {

  def reset(): Observation = {
    val request = resetEnv(instance.instance_id)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[Observation], gymClient.timeout.second)
      case _ => throw new Exception
    }
  }

  def reset1(): Unit = {
    val request = resetEnv(instance.instance_id)

    val res = requestToApi(request)

    val json = res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[String], gymClient.timeout.second)
      case _ => throw new Exception
    }

    println(json)
  }

  def step(action: Int, render: Boolean = true): StepReply = {

    val request = stepEnv(instance.instance_id,action,render)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[StepReply], gymClient.timeout.second)
      case _ => throw new Exception
    }
  }

  def step1(action: Int, render: Boolean = true): Unit = {

    val request = stepEnv(instance.instance_id,action,render)

    val res = requestToApi(request)

    val json = res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[String], gymClient.timeout.second)
      case _ => throw new Exception
    }

    println(json)
  }


  def action_space(): ActionSpace ={
    val request = actionSpace(instance.instance_id)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ActionSpace], gymClient.timeout.second)
      case _ => throw new Exception
    }
  }

  def action_space1(): Unit ={
    val request = actionSpace(instance.instance_id)

    val res = requestToApi(request)

    val json = res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[String], gymClient.timeout.second)
      case _ => throw new Exception
    }

    println(json)
  }


  def observation_space(): ObservationSpace = {

    val request = obsSpace(instance.instance_id)

    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ObservationSpace], gymClient.timeout.second)
      case _ => throw new Exception
    }
  }

  def observation_space1(): Unit = {

    val request = obsSpace(instance.instance_id)

    val res = requestToApi(request)

    val json = res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[String], gymClient.timeout.second)
      case _ => throw new Exception
    }

    println(json)
  }

}
