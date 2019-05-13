package OpenAI.gym


import akka.actor.{ActorSystem, Terminated}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpEntity, _}
import akka.stream.ActorMaterializer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object gymClient{



  implicit val system: ActorSystem = ActorSystem.create("system") // Create an Actor System for messages communication
  implicit def materializer: ActorMaterializer = ActorMaterializer() //Implicit Materializer use to encode Http Entity responses

  val executionContext: ExecutionContextExecutor = system.dispatcher //Implicit dispatcher for future function calls

  private val host: String = "http://127.0.0.1" // Local Host address
  private val port: Int = 5000 // Port Number
  val timeout: Int = 20 // Timeout of the request

  private val contentType = ContentTypes.`application/json` //Data Content Type

  def terminate: Future[Terminated] = system.terminate() // Terminate Actor System

  def requestToApi(request: gymApi): HttpResponse = {
    //create the url
    val url = host + ":" + port + request.url
    //Create the request
    val httpRequest = HttpRequest(uri = url).withMethod(request.method).withEntity(HttpEntity(contentType, request.json))
    //Send the request
    val responseFuture: Future[HttpResponse] = Http().singleRequest(httpRequest)
    //Wait a specific timeout to arrive and return the entity
    Await.result(responseFuture, timeout.second)
  }



}
