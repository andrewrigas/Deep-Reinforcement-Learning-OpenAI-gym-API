package OpenAI.gym

import OpenAI.gym.gymClient.requestToApi
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.concurrent.duration._

import OpenAI.gym.gymClient.materializer

import scala.concurrent.Await

object gym {

  //Classic Control Enviroments
  private val classicControl = List("Acrobot","CartPole","MountainCar","MountainCar","Pendulum")


  def make(env: String): Environment = {

    //Create Environment request
    val request: createEnv = createEnv(env)

    //Http Post Request to api
    val res = requestToApi(request)

    val inc = classicControl.foldLeft(false)((acc, envi) => acc || env.contains(envi))

    if(inc){
      implicit val EnvInstance = res match {
        //Manipulate The Response
        case HttpResponse(StatusCodes.OK, headers, entity, _) =>  Await.result(Unmarshal(entity).to[EnvInstance], gymClient.timeout.second)
        case _ => throw new Exception
      }

      //Create New Classic Environment with EnvInstance
      new ClassicControlEnvironment(EnvInstance)

    } else {
      implicit val EnvInstance = res match {
        //Manipulate The Response
        case HttpResponse(StatusCodes.OK, headers, entity, _) =>  Await.result(Unmarshal(entity).to[EnvInstance], gymClient.timeout.second)
        case _ => throw new Exception
      }

      //Create New Atari Environment with EnvInstance
      new AtariEnvironment(EnvInstance)
    }
  }


  def getListEnvs(): ListEnvs = {
    //Create listEnvs request
    val request: listEnvs = listEnvs()

    //Http Get Request to api
    val res = requestToApi(request)

    res match {
      case HttpResponse(StatusCodes.OK, headers, entity, _) => Await.result(Unmarshal(entity).to[ListEnvs], gymClient.timeout.second)
    }
  }


  def shutDown(): Unit = {
    val request = shutdown()

    requestToApi(request)
  }
}
