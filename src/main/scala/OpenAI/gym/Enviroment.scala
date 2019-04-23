package OpenAI.gym

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.unmarshalling.Unmarshal
import gymClient.requestToApi
import spray.json.DeserializationException

import scala.concurrent.duration._
import scala.concurrent.Await
import OpenAI.gym.gymClient.materializer
import org.apache.spark.sql.SparkSession



abstract class Environment(val instance: EnvInstance) {

  //Action Space Response, Return an ActionSpace Object
  def action_space(): ActionSpace ={
    //Create an Action Space Request
    val request = actionSpace(instance.instance_id)
    //Make the request to the API
    val res = requestToApi(request)
    //Get Result
    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ActionSpace], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }
  }

  //Observation Space Response , Return an Observation Space Object
  def observation_space(): ObservationSpace = {
    //Create an Observation Space Request
    val request = obsSpace(instance.instance_id)
    //Make the request to the API
    val res = requestToApi(request)
    //Get Result
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

case class AtariEnvironment(override val instance: EnvInstance)(implicit spark: SparkSession) extends Environment(instance) {



  override def reset(): Observation = {
    //Create an Reset request
    val request = resetEnv(instance.instance_id)
    //Make the request to the API
    val res = requestToApi(request)
    //Get Result in Observation
    val obs = res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ObservationAtari], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }

    val obsFlat = obs.observation.flatten

    //Convert observation RGB to GrayScale
    val observationRDD = spark.sparkContext.parallelize(obsFlat).map(c => (c._1+c._2+c._3)/ 3).collect().toSeq

    //Return Observation
    Observation(observationRDD)

  }

  override def step(action: Int, render: Boolean = true): StepReply = {
    //Create Step Request
    val request = stepEnv(instance.instance_id,action,render)
    //Make the request to the API
    val res = requestToApi(request)
    //Get Result in StepReply
    val stepReply = res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[StepReplyAtari], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }

    val obsFlat = stepReply.observation.flatten

    //Convert observation RGB to GrayScale
    val observationRDD = spark.sparkContext.parallelize(obsFlat).map(c => (c._1+c._2+c._3)/ 3).collect().toSeq

    //Return StepReply
    StepReply(observationRDD ,stepReply.reward,stepReply.done,stepReply.info)
  }


}

case class ClassicControlEnvironment(override val instance: EnvInstance) extends Environment(instance) {


  override def reset(): Observation= {
    //Create an Reset request
    val request = resetEnv(instance.instance_id)
    //Make the request to the API
    val res = requestToApi(request)

    //Get Result in Observation
    val obs = res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ObservationClassicControl], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }
    //Return Observation
    Observation(obs.observation)
  }

  override def step(action: Int, render: Boolean = true): StepReply = {
    //Create Step Request
    val request = stepEnv(instance.instance_id,action,render)
    //Make the request to the API
    val res = requestToApi(request)
    //Get Result in StepReply
    val stepReply =res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[StepReplyClassicControl], gymClient.timeout.second)
      case _ => throw new Exception("Http response Failed")
    }
    //Return StepReply
    StepReply(stepReply.observation,stepReply.reward,stepReply.done,stepReply.info)
  }

}
